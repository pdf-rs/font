use std::collections::HashMap;
use nom::{IResult,
    number::complete::{be_u8, le_u8, be_i32, le_u32},
};
use tuple::{TupleElements};
use itertools::Itertools;
use indexmap::IndexMap;
use crate::{Font, Glyph, State, v, R, IResultExt, Context, HMetrics, TryIndex, GlyphId, Name, Value, Info, FontError};
use crate::postscript::{Vm, RefItem};
use crate::eexec::Decoder;
use crate::parsers::parse;
use pdf_encoding::{glyphname_to_unicode};
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
        let len_iv = private_dict.get_int("lenIV").unwrap_or(4) as usize;
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
            for item in arr.iter() {
                subrs.push(item.as_bytes().map(|data| Decoder::charstring().decode(data, len_iv)));
            }
        }

        let context = Context {
            subr_bias: 0,
            subrs,
            global_subr_bias: 0,
            global_subrs: ()
        };

        let encoding = font_dict.get("Encoding").expect("no /Encoding").as_array().expect("/Encoding is not an array");
        
        let mut glyphs = IndexMap::with_capacity(char_strings.len());
        let mut unicode_map = HashMap::with_capacity(char_strings.len());
        let mut state = State::new();
        for (name, item) in char_strings.string_entries() {
            let data = expect!(item.as_bytes(), "data is not bytes");

            let decoded = Decoder::charstring().decode(&data, len_iv);
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
        
        let mut codepoints = HashMap::with_capacity(encoding.len());
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
        
        let (a, b, c, d, e, f) = TupleElements::from_iter(
                font_matrix.iter().map(|item| item.as_f32().unwrap())
        ).unwrap();
        
        Ok(Type1Font {
            font_matrix: Transform2F::row_major(a, b, e, c, d, f),
            glyphs,
            codepoints,
            unicode_map,
            bbox,
            name,
            info: Info {
                weight,
            }
        })
    }

    pub fn unicode_names(&self) -> impl Iterator<Item=(GlyphId, &str)> + '_ {
        self.unicode_map.iter().map(|(s, &g)| (GlyphId(g), s.as_str()))
    }
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
            n => panic!("unknown block type {}", n)
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
                s.contour.push_cubic(c1, c2, p);
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
                charstring(subr, ctx, s)?;
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
                        
                        match subr_nr {
                            1 => {
                                require_eq!(n, 0);
                                s.flex_sequence = Some(Vec::with_capacity(7));
                                
                                // first moveto: referece point
                                // then first control point of first curve
                                // second control point of first curve
                                // joining point
                                // first control point of second curve
                                // second control point of second curve
                                // endpoint of second curve
                                // (flex height, final x, final y) 0 callsubr
                            }
                            2 => {
                                require_eq!(n, 0);
                            }
                            0 => {
                                // end of flex sequences
                                require_eq!(n, 3);
                                let (_flex_height, x, y) = s.pop_tuple()?;
                                let flex_sequence = expect!(s.flex_sequence.take(), "no flex sequence");
                                let (_ref, c0, c1, p2, c3, c4, p5) = expect!(TupleElements::from_iter(flex_sequence.into_iter()), "can't parse flex sequence");
                                //require_eq!(p5, v(x, y));
                                s.contour.push_cubic(c0, c1, p2);
                                s.contour.push_cubic(c3, c4, p5);
                                ps_stack.push(y);
                                ps_stack.push(x);
                            }
                            3 => {
                                require_eq!(n, 1);
                                ps_stack.push(s.pop()?);
                                ps_stack.push(Value::Int(3));
                            }
                            _ => {
                                let m = s.stack.len();
                                ps_stack.clear();
                                ps_stack.extend(s.stack.drain(m - n ..).rev());
                            }
                        }
                    }
                    17 => { // – pop (12 17) number
                        trace!("pop");
                        let n = ps_stack.pop().expect("PS stack is empty");
                        s.push(n);
                    }
                    33 => { // ⊦ x y setcurrentpoint (12 33) ⊦
                        trace!("setcurrentpoint");
                        let (x, y) = s.args()?;
                        let p = v(x, y);
                        s.current = p;
                        s.stack.clear();
                    },
                    _ => panic!("invalid operator")
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
                s.contour.push_cubic(c1, c2, p);
                s.stack.clear();
                s.current = p;
            }
            31 => { // ⊦ dx1 dx2 dy2 dy3 hvcurveto (31) ⊦
                trace!("hvcurveto");
                let (dx1, dx2, dy2, dy3) = s.args()?;
                let c1 = s.current + v(dx1, 0.);
                let c2 = c1 + v(dx2, dy2);
                let p = c2 + v(0., dy3);
                s.contour.push_cubic(c1, c2, p);
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
