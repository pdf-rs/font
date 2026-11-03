use std::collections::HashMap;
use nom::{IResult,
    number::complete::{be_u8, le_u8, be_i32, le_u32},
};
use tuple::{TupleElements};
use itertools::Itertools;
use indexmap::IndexMap;
use crate::{Font, Glyph, State, Seac, v, R, IResultExt, Context, HMetrics, TryIndex, GlyphId, Name, Value, Info, FontError, MAX_SUBR_DEPTH};
use crate::postscript::{Vm, RefItem};
use crate::eexec::Decoder;
use crate::parsers::parse;
use pdf_encoding::{glyphname_to_unicode, Encoding};
use pathfinder_geometry::{
    transform2d::Transform2F,
    vector::Vector2F,
    rect::RectF
};
use istring::TinyString;

#[derive(Clone)]
pub struct Type1Font {
    glyphs: IndexMap<String, Glyph>, // namee -> glyph
    pub codepoints: HashMap<u32, u32>, // codepoint -> glyph id
    pub unicode_map: HashMap<TinyString, u32>,
    font_matrix: Transform2F,
    bbox: Option<RectF>,
    name: Name,
    info: Info,
}
impl Font for Type1Font {
    fn num_glyphs(&self) -> u32 {
        self.glyphs.len() as u32
    }
    fn font_matrix(&self) -> Transform2F {
        self.font_matrix
    }
    fn glyph(&self, gid: GlyphId) -> Option<Glyph> {
        self.glyphs.get_index(gid.0 as usize).map(|(_, glyph)| glyph.clone())
    }
    fn is_empty_glyph(&self, gid: GlyphId) -> bool {
        self.glyphs.get_index(gid.0 as usize).map(|(_, glyph)| glyph.path.len() == 0).unwrap_or(true)
    }
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        let &index = self.codepoints.get(&codepoint)?;
        Some(GlyphId(index as u32))
    }
    fn gid_for_name(&self, name: &str) -> Option<GlyphId> {
        self.glyphs.get_full(name).map(|(id, _, _)| GlyphId(id as u32))
    }
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        let s = TinyString::from(char::from_u32(codepoint)?);
        self.unicode_map.get(&s).map(|&id| GlyphId(id))
    }
    fn bbox(&self) -> Option<RectF> {
        self.bbox
    }
    fn name(&self) -> &Name {
        &self.name
    }
    fn info(&self) -> &Info {
        &self.info
    }
}

impl Type1Font {
    pub fn parse_pfa(data: &[u8]) -> Result<Self, FontError> {
        let mut vm = Vm::new();
        vm.parse_and_exec(data)?;
        Self::from_vm(vm)
    }
    pub fn parse_pfb(data: &[u8]) -> Result<Self, FontError> {
        let mut vm = Vm::new();
        parse_pfb(&mut vm, data)?;
        Self::from_vm(vm)
    }
    pub fn parse_postscript(data: &[u8]) -> Result<Self, FontError> {
        let mut vm = Vm::new();
        vm.parse_and_exec(data)?;
        Self::from_vm(vm)
    }
        
    pub fn from_vm(vm: Vm) -> Result<Self, FontError> {
        let (_font_name, font_dict) = expect!(vm.fonts().nth(0), "no font in vm");
        
        let private_dict = expect!(font_dict.get_dict("Private"), "no /Private dict");
        // `lenIV` is the number of random bytes prepended before encryption; a negative
        // value (conventionally -1) means the charstrings are *not* encrypted at all,
        // so they must be used verbatim (TN5015 §2, PLRM `lenIV`).
        let len_iv_value = private_dict.get_int("lenIV").unwrap_or(4);
        let len_iv = if len_iv_value < 0 {
            warn!("Type 1 font has lenIV {len_iv_value}: charstrings are stored unencrypted");
            None
        } else {
            Some(len_iv_value as usize)
        };
        let font_info = font_dict.get_dict("FontInfo");
        
        debug!("FontDict keys: {:?}", font_dict.iter().map(|(k, _)| k).format(", "));
        debug!("Private keys: {:?}", private_dict.iter().map(|(k, _)| k).format(", "));
        debug!("FontName: {:?}", font_dict.get("FontName"));
        debug!("FontInfo: {:?}", font_dict.get("FontInfo"));
        
        let postscript_name = font_dict.get_str("FontName").map(|s| s.into());
        let full_name = font_dict.get_dict("FontInfo")
            .and_then(|d| d.get_str("FullName"))
            .map(|s| s.into());
        let name = Name {
            full_name,
            postscript_name,
            .. Name::default()
        };
        let weight = font_info.and_then(|d| d.get_str("Weight")).and_then(|s| match s {
            "Light" => Some(300),
            "Regular" => Some(400),
            "Medium" => Some(500),
            "Semibold" => Some(600),
            "Bold" => Some(700),
            "Black" => Some(900),
            _ => None
        });

        let char_strings = expect!(font_dict.get_dict("CharStrings"), "no /CharStrings");
        
        let mut subrs = Vec::new();
        if let Some(arr) = private_dict.get_array("Subrs") {
            for (i, item) in arr.iter().enumerate() {
                let decoded = match item.as_bytes() {
                    Some(data) => {
                        let r = decode_charstring(data, len_iv);
                        if r.is_none() {
                            warn!("subr {i} is shorter than lenIV; skipped");
                        }
                        r
                    }
                    None => {
                        warn!("subr {i} is not a string; skipped");
                        None
                    }
                };
                subrs.push(decoded);
            }
        }

        let context = Context {
            subr_bias: 0,
            subrs,
            global_subr_bias: 0,
            global_subrs: ()
        };

        // PLRM §5.6: `/Encoding` is optional, and it may be a name (`/StandardEncoding`)
        // instead of an array. This VM does not resolve such a name back to an array, so
        // "no encoding" means the glyphs are only reachable by name — it must not abort
        // the font.
        let encoding = font_dict.get("Encoding").and_then(|item| item.as_array());
        if encoding.is_none() {
            warn!("Type 1 font has no /Encoding array; codepoint lookup is empty");
        }
        
        let mut glyphs = IndexMap::with_capacity(char_strings.len());
        let mut unicode_map = HashMap::with_capacity(char_strings.len());
        let mut seac_glyphs = Vec::new();
        let mut state = State::new();
        for (name, item) in char_strings.string_entries() {
            let data = expect!(item.as_bytes(), "data is not bytes");

            let decoded = match decode_charstring(&data, len_iv) {
                Some(decoded) => decoded,
                None => {
                    warn!("charstring for glyph {name} is shorter than lenIV; skipped");
                    continue;
                }
            };
            //debug!("{} decoded: {:?}", name, String::from_utf8_lossy(&decoded));
            
            if let Err(e) = charstring(&decoded, &context, &mut state) {
                warn!("Failed to decode charstring for glyph {name}: {e:?}");
                continue;
            }

            let (index, _) = glyphs.insert_full(name.to_owned(), Glyph {
                metrics: HMetrics {
                    advance: expect!(state.char_width, "CharWidth not set"),
                    lsb: state.lsb.unwrap_or_default()
                },
                path: state.take_path(),
            });
            if let Some(seac) = state.seac {
                seac_glyphs.push((index, seac));
            }
            state.clear();

            if let Some(unicode) = glyphname_to_unicode(name) {
                debug!("{} -> {} @ {}", name, unicode, index);
                unicode_map.insert(TinyString::new(unicode).unwrap(), index as u32);
            } else {
                if let Some(s) = name.strip_prefix("uni")
                    .and_then(|id| u16::from_str_radix(id, 16).ok())
                    .and_then(|num| char::from_u32(num as u32))
                    .map(|c| TinyString::from(c))
                {
                        unicode_map.insert(s, index as u32);
                } else {
                    debug!("canot map {}", name);
                }
            }
        }

        // T1 §6 second pass: a `seac` glyph is empty on its own — it is the base
        // character with the accent drawn on top, the accent shifted by (adx, ady)
        // from the origin of the base. The components can be defined after the
        // composite, so this cannot be done while decoding the charstrings.
        for (index, seac) in seac_glyphs {
            let base = match seac_gid(seac.base, &unicode_map) {
                Some(gid) => gid,
                None => {
                    warn!("seac: no glyph for StandardEncoding base character {}", seac.base);
                    continue;
                }
            };
            let accent = match seac_gid(seac.accent, &unicode_map) {
                Some(gid) => gid,
                None => {
                    warn!("seac: no glyph for StandardEncoding accent character {}", seac.accent);
                    continue;
                }
            };
            if base == index || accent == index {
                warn!("seac: glyph composes itself ({} + {})", base, accent);
                continue;
            }
            let (base_path, accent_path) = match (glyphs.get_index(base), glyphs.get_index(accent)) {
                (Some((_, base)), Some((_, accent))) => (base.path.clone(), accent.path.clone()),
                _ => continue,
            };
            let path = seac.compose(base_path, accent_path);
            if let Some((_, glyph)) = glyphs.get_index_mut(index) {
                glyph.path = path;
            }
        }

        let mut codepoints = HashMap::with_capacity(encoding.map(|e| e.len()).unwrap_or(0));
        if let Some(encoding) = encoding {
            let mut codepoint = 0;
            for item in encoding.iter() {
                match item {
                    RefItem::Null => {},
                    RefItem::Literal(b".notdef") => {},
                    RefItem::Literal(name) => {
                        match std::str::from_utf8(name) {
                            Ok(name) => {
                                if let Some((index, _, _)) = glyphs.get_full(name) {
                                    codepoints.insert(codepoint, index as u32);
                                }
                            }
                            Err(_) => {
                                warn!("name not utf8: {:?}", name);
                            }
                        }
                    }
                    _ => {}
                }
                codepoint += 1;
            }
        }

        let font_matrix = expect!(font_dict.get("FontMatrix"), "no FontMatrix");
        let font_matrix = expect!(font_matrix.as_array(), "FontMatrix not an array");
        require_eq!(font_matrix.len(), 6);
        
        let bbox = font_dict.get("FontBBox")
            .map(|val| {
                let arr = expect!(val.as_array(), "FontBox not an array");
                require!(arr.len() == 4);
                let (a, b, c, d) = expect!(TupleElements::from_iter(arr.iter().filter_map(|v| v.as_f32())), "needs 4 numbers");
                Ok(RectF::from_points(Vector2F::new(a, b), Vector2F::new(c, d)))
            }).transpose()?;
        
        let matrix: Vec<f32> = font_matrix.iter().filter_map(|item| item.as_f32()).collect();
        let (a, b, c, d, e, f) = expect!(TupleElements::from_iter(matrix.into_iter()), "FontMatrix needs 6 numbers");
        
        Ok(Type1Font {
            font_matrix: Transform2F::row_major(a, b, e, c, d, f),
            glyphs,
            codepoints,
            unicode_map,
            bbox,
            name,
            info: Info {
                weight,
                .. Info::default()
            }
        })
    }

    pub fn unicode_names(&self) -> impl Iterator<Item=(GlyphId, &str)> + '_ {
        self.unicode_map.iter().map(|(s, &g)| (GlyphId(g), s.as_str()))
    }
}

/// The glyph a StandardEncoding character code stands for. T1 §6 identifies the
/// `seac` components by code, not by name: the code is mapped to the character it
/// encodes, and that character to the glyph whose name maps to it.
fn seac_gid(code: u8, unicode_map: &HashMap<TinyString, u32>) -> Option<usize> {
    let ch = Encoding::AdobeStandard.forward_map()?.get(code)?;
    unicode_map.get(&TinyString::from(ch)).map(|&gid| gid as usize)
}

fn parse_binary<'a>(vm: &mut Vm, data: &'a [u8]) {
    let mut decoder = Decoder::file();
    let decoded = decoder.decode(data, 4);
    
    vm.parse_and_exec(&decoded);
}

#[test]
fn test_parser() {
    let mut vm = Vm::new();
    vm.parse_and_exec(b"/FontBBox{-180 -293 1090 1010}readonly ");
    vm.print_stack();
    assert_eq!(vm.stack().len(), 2);
}
fn parse_pfb(mut vm: &mut Vm, i: &[u8]) -> Result<(), FontError> {
    let mut input = i;
    while input.len() > 0 {
        let (i, magic) = le_u8(input)?;
        require_eq!(magic, 0x80);
        let (i, block_type) = le_u8(i)?;
        match block_type {
            1 | 2 => {} // continue
            3 => break,
            n => error!("unknown PFB block type {}", n)
        }
        
        let (i, block_len) = le_u32(i)?;
        info!("block type {}, length: {}", block_type, block_len);
    
        let block = slice!(i, .. block_len as usize);
        match block_type {
            1 => {
                vm.parse_and_exec(block);
            }
            2 => parse_binary(&mut vm, block),
            _ => unreachable!()
        }
        
        input = slice!(i, block_len as usize ..);
    }
    
    Ok(())
}
/// Decrypt one charstring and drop its `lenIV` random leading bytes.
///
/// `None` in `len_iv` means the font stores its charstrings unencrypted (`lenIV < 0`,
/// TN5015 §2), so the bytes are returned untouched. A charstring shorter than `lenIV`
/// cannot be a valid one — the caller skips it instead of getting an empty string and
/// failing later with "CharWidth not set".
fn decode_charstring(data: &[u8], len_iv: Option<usize>) -> Option<Vec<u8>> {
    match len_iv {
        None => Some(data.to_vec()),
        Some(skip) if skip > data.len() => None,
        Some(skip) => Some(Decoder::charstring().decode(data, skip)),
    }
}

/// Append one cubic Bézier to the contour.
///
/// Type 1 flex does **not** come through here: the standard flex protocol (T1 §6,
/// TN5015 §10) collects its seven points as `rmoveto`s between `1 callsubr` and
/// `0 callsubr`, never as curve operators.
#[inline]
fn push_curve(s: &mut State, c1: Vector2F, c2: Vector2F, p: Vector2F) {
    s.contour.push_cubic(c1, c2, p);
}


pub fn charstring<'a, 'b, T, U>(mut input: &'a [u8], ctx: &'a Context<T, U>, s: &'b mut State) -> Result<(), FontError>
    where T: TryIndex + 'a, U: TryIndex + 'a
{
    let mut ps_stack = vec![];
    while input.len() > 0 {
        //trace!("input: {:?}", slice!(input, .. input.len().min(10)));
        trace!("current: {:?}", s.current);
        let b0 = parse(&mut input, be_u8)?;
        match b0 {
            1 => { // ⊦ y dy hstem (1) ⊦
                trace!("hstem");
                s.stack.clear();
            }
            3 => { // ⊦ x dx vstem (3) ⊦
                trace!("vstem");
                s.stack.clear();
            }
            4 => { // ⊦ dy vmoveto (4) ⊦
                trace!("vmoveto");
                require!(s.stack.len() >= 1);
                s.flush();

                let p = s.current + v(0., s.stack[0]);
                s.contour.push_endpoint(p);
                s.stack.clear();
                s.current = p;
            }
            5 => { // ⊦ dx dy rlineto (5) ⊦
                trace!("rlineto");
                require!(s.stack.len() >= 2);
                let p = s.current + v(s.stack[0], s.stack[1]);
                s.contour.push_endpoint(p);
                s.stack.clear();
                s.current = p;
            }
            6 => { // ⊦ dx hlineto (6) ⊦
                trace!("hlineto");
                require!(s.stack.len() >= 1);
                let p = s.current + v(s.stack[0], 0.);
                s.contour.push_endpoint(p);
                s.stack.clear();
                s.current = p;
            }
            7 => { // dy vlineto (7)
                trace!("vlineto");
                require!(s.stack.len() >= 1);
                let p = s.current + v(0., s.stack[0],);
                s.contour.push_endpoint(p);
                s.stack.clear();
                s.current = p;
            }
            8 => { // ⊦ dx1 dy1 dx2 dy2 dx3 dy3 rrcurveto (8) ⊦
                trace!("rrcurveto");
                require!(s.stack.len() >= 6);
                let c1 = s.current + v(s.stack[0], s.stack[1]);
                let c2 = c1 + v(s.stack[2], s.stack[3]);
                let p = c2 + v(s.stack[4], s.stack[5]);
                push_curve(s, c1, c2, p);
                s.stack.clear();
                s.current = p;
            }
            9 => { // –closepath (9) ⊦
                trace!("closepath");
                s.contour.close();
                s.stack.clear();
            }
            10 => { // subr# callsubr (10) –
                let subr_nr = s.pop()?.to_int()?;
                trace!("callsubr {}", subr_nr);
                let subr = ctx.subr(subr_nr)?;
                // A subr calling itself would recurse until the native stack dies.
                if s.subr_depth >= MAX_SUBR_DEPTH {
                    error!("subroutine nesting deeper than {} levels (recursive subr?)", MAX_SUBR_DEPTH);
                }
                s.subr_depth += 1;
                let result = charstring(subr, ctx, s);
                s.subr_depth -= 1;
                result?;
            }
            11 => { // return
                trace!("return");
                break;
            }
            12 => {
                let b1 = parse(&mut input, be_u8)?;
                match b1 {
                    0 => { // – dotsection (12 0) ⊦
                        trace!("dotsection");
                        s.stack.clear();
                    }
                    1 => { // ⊦ x0 dx0 x1 dx1 x2 dx2 vstem3 (12 1) ⊦
                        trace!("vstem3");
                        s.stack.clear();
                    }
                    2 => { // ⊦ y0 dy0 y1 dy1 y2 dy2 hstem3 (12 2) ⊦
                        trace!("hstem3");
                        s.stack.clear();
                    }
                    6 => { // ⊦ asb adx ady bchar achar seac (12 6) ⊦
                        trace!("seac");
                        // T1 §6: an accented character is not drawn by its own
                        // charstring — it is the base character plus the accent, both
                        // named by their StandardEncoding code and composed in
                        // `Type1Font::from_vm` once every charstring is decoded.
                        require!(s.stack.len() >= 5);
                        let first = s.stack.len() - 5;
                        let asb = s.stack[first].to_float();
                        let adx = s.stack[first + 1].to_float();
                        let ady = s.stack[first + 2].to_float();
                        let bchar = s.stack[first + 3].to_float();
                        let achar = s.stack[first + 4].to_float();
                        if !(0.0 ..= 255.0).contains(&bchar) || !(0.0 ..= 255.0).contains(&achar) {
                            warn!("seac: character code out of range ({bchar}, {achar})");
                        } else {
                            // F41/audit C4: `adx` is measured from this glyph's own left
                            // side bearing, and `asb` is the accent's left side bearing,
                            // which the accent's outline already carries. FreeType's
                            // `t1operator_seac` does `adx += left_bearing.x` and then
                            // places the accent at `adx - asb`.
                            s.seac = Some(Seac {
                                adx, ady,
                                sbx: s.lsb.unwrap_or_default(),
                                asb,
                                base: bchar as u8,
                                accent: achar as u8,
                            });
                        }
                        s.stack.clear();
                    }
                    7 => { // ⊦ sbx sby wx wy sbw (12 7) ⊦
                        let (sbx, sby, wx, _wy, _sbw) = s.args()?;
                        trace!("sbw");
                        s.char_width = Some(wx.to_float());
                        s.current = v(sbx, sby);
                        s.stack.clear();
                    }
                    12 => { // num1 num2 div (12 12) quotient
                        trace!("div");
                        let num2 = s.pop()?.to_float();
                        let num1 = s.pop()?.to_float();
                        s.push(num1 / num2);
                    }
                    16 => { //  arg1 . . . argn n othersubr# callothersubr (12 16) –
                        let subr_nr = s.pop()?.to_int()?;
                        trace!("callothersubr {}", subr_nr);
                        let n = s.pop()?.to_int()? as usize;
                        
                        match (subr_nr, n) {
                            // flex start: Subr 1 is `1 0 callothersubr return` (T1 §6,
                            // TN5015 §10). The seven `dx dy rmoveto 2 callsubr` steps
                            // that follow are collected in `flex_sequence` by `rmoveto`
                            // instead of being drawn.
                            (1, 0) => {
                                // The curve starts where the charstring currently is
                                // (FreeType t1decode.c, othersubr 1: `start_point`).
                                // Everything the seven following `rmoveto`s collect is
                                // relative to it; the first of them only moves the
                                // current point to the flex reference point and is not
                                // part of the outline.
                                if s.contour.is_empty() {
                                    s.contour.push_endpoint(s.current);
                                }
                                s.flex_sequence = Some(Vec::with_capacity(7));
                            }
                            // Subr 2 is `0 2 callothersubr return` and runs after *every*
                            // one of those rmovetos. It only acknowledges the point that
                            // has just been added, so it must leave `flex_sequence` alone.
                            (2, 0) => {}
                            // flex end: Subr 0 is
                            // `3 0 callothersubr pop pop setcurrentpoint return`.
                            (0, 3) => {
                                if s.stack.len() < 3 {
                                    warn!("flex end with {} operands, expected flexheight and a point", s.stack.len());
                                    s.stack.clear();
                                } else {
                                    let y = s.pop()?;
                                    let x = s.pop()?;
                                    let _flex_height = s.pop()?;
                                    match s.flex_sequence.take() {
                                        Some(points) if points.len() == 7 => {
                                            // points[0] is the flex reference point: it
                                            // exists only for the flex-height test and is
                                            // not part of the outline (FreeType ignores
                                            // index 0 of the seven flex vectors). The
                                            // first cubic starts where the contour already
                                            // is — the position at flex start.
                                            s.contour.push_cubic(points[1], points[2], points[3]);
                                            s.contour.push_cubic(points[4], points[5], points[6]);
                                            s.current = points[6];
                                        }
                                        Some(points) => {
                                            warn!("flex sequence has {} points, expected 7", points.len());
                                        }
                                        None => {
                                            warn!("flex end without a flex sequence");
                                        }
                                    }
                                    // the charstring continues with `pop pop setcurrentpoint`
                                    ps_stack.push(y);
                                    ps_stack.push(x);
                                }
                            }
                            // otherSubr 3 returns a value to the PostScript side
                            (3, 1) => {
                                ps_stack.push(s.pop()?);
                                ps_stack.push(Value::Int(3));
                            }
                            _ => {
                                let m = s.stack.len();
                                ps_stack.clear();
                                if n <= m {
                                    ps_stack.extend(s.stack.drain(m - n ..).rev());
                                } else {
                                    warn!("callothersubr {} wants {} operands, {} on the stack", subr_nr, n, m);
                                }
                            }
                        }
                    }
                    17 => { // – pop (12 17) number
                        trace!("pop");
                        let n = expect!(ps_stack.pop(), "PS stack is empty");
                        s.push(n);
                    }
                    33 => { // ⊦ x y setcurrentpoint (12 33) ⊦
                        trace!("setcurrentpoint");
                        let (x, y) = s.args()?;
                        let p = v(x, y);
                        s.current = p;
                        s.stack.clear();
                    },
                    n => error!("invalid Type 1 charstring operator (12 {})", n)
                }
            }
            13 => { // ⊦ sbx wx hsbw (13) ⊦
                trace!("hsbw");
                let (sbx, wx) = s.args()?;
                let lsb = sbx.to_float();
                s.lsb = Some(lsb);
                s.current = Vector2F::new(lsb, 0.0);
                s.char_width = Some(wx.to_float());
                s.stack.clear();
            }
            14 => { //– endchar (14) ⊦
                trace!("endchar");
                break;
            }
            21 => { // ⊦ dx dy rmoveto (21) ⊦
                trace!("rmoveto");
                let (dx, dy) = s.args()?;
                let p = s.current + v(dx, dy);
                
                // hack to counter the flex sequences hack by adobe
                if let Some(ref mut points) = s.flex_sequence {
                    points.push(p);
                } else {
                    s.flush();
                    s.contour.push_endpoint(p);
                }
                s.current = p;
                s.stack.clear();
            }
            22 => { // ⊦ dx hmoveto (22) ⊦
                trace!("hmoveto");
                let (dx, ) = s.args()?;
                let p = s.current + v(dx, 0.);
                s.flush();
                s.contour.push_endpoint(p);
                s.current = p;
                s.stack.clear();
            }
            30 => { // ⊦ dy1 dx2 dy2 dx3 vhcurveto (30) ⊦
                trace!("vhcurveto");
                let (dy1, dx2, dy2, dx3) = s.args()?;
                let c1 = s.current + v(0., dy1);
                let c2 = c1 + v(dx2, dy2);
                let p = c2 + v(dx3, 0.);
                push_curve(s, c1, c2, p);
                s.stack.clear();
                s.current = p;
            }
            31 => { // ⊦ dx1 dx2 dy2 dy3 hvcurveto (31) ⊦
                trace!("hvcurveto");
                let (dx1, dx2, dy2, dy3) = s.args()?;
                let c1 = s.current + v(dx1, 0.);
                let c2 = c1 + v(dx2, dy2);
                let p = c2 + v(0., dy3);
                push_curve(s, c1, c2, p);
                s.stack.clear();
                s.current = p;
            },
            v @ 32 ..= 246 => {
                s.push(v as i32 - 139);
            }
            v @ 247 ..= 250 => {
                let w = parse(&mut input, be_u8)?;
                s.push((v as i32 - 247) * 256 + w as i32 + 108);
            }
            v @ 251 ..= 254 => {
                let w = parse(&mut input, be_u8)?;
                s.push(-(v as i32 - 251) * 256 - w as i32 - 108);
            }
            255 => {
                let v = parse(&mut input, be_i32)?;
                s.push(v);
            }
            c => error!("unknown code {}", c)
        }
        
        trace!("stack: {:?}", s.stack);
    };
    
    Ok(())
}
