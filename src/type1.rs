use std::collections::HashMap;
use nom::{IResult,
    number::complete::{be_u8, le_u8, be_i32, le_u32},
};
use tuple::{TupleElements};
use itertools::Itertools;
use indexmap::IndexMap;
use crate::{Font, Glyph, State, v, R, IResultExt, Context, HMetrics, TryIndex, GlyphId};
use crate::postscript::{Vm, RefItem};
use crate::eexec::Decoder;
use crate::parsers::parse;
use vector::{Outline, Transform, Rect, Vector};
use encoding::{glyphname_to_unicode};

pub struct Type1Font<O: Outline> {
    glyphs: IndexMap<u32, Glyph<O>>, // codepoint -> glyph
    names: HashMap<String, u32>, // name -> glyph id
    unicode_map: HashMap<&'static str, u32>,
    font_matrix: Transform,
    bbox: Option<Rect>,
    postscript_name: Option<String>,
    full_name: Option<String>
}
impl<O: Outline> Font<O> for Type1Font<O> {
    fn num_glyphs(&self) -> u32 {
        self.glyphs.len() as u32
    }
    fn font_matrix(&self) -> Transform {
        self.font_matrix
    }
    fn glyph(&self, gid: GlyphId) -> Option<Glyph<O>> {
        self.glyphs.get_index(gid.0 as usize).map(|(_, glyph)| glyph.clone())
    }
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        self.glyphs.get_full(&codepoint).map(|(index, _, _)| GlyphId(index as u32))
    }
    fn gid_for_name(&self, name: &str) -> Option<GlyphId> {
        self.names.get(name).map(|&id| GlyphId(id))
    }
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        let c = std::char::from_u32(codepoint)?;
        let mut buf = [0; 4];
        let s = c.encode_utf8(&mut buf);
        self.unicode_map.get(&*s).map(|&id| GlyphId(id))
    }
    fn bbox(&self) -> Option<Rect> {
        self.bbox
    }
    fn full_name(&self) -> Option<&str> {
        self.full_name.as_ref().map(|s| s.as_str())
    }
    fn postscript_name(&self) -> Option<&str> {
        self.postscript_name.as_ref().map(|s| s.as_str())
    }
}

impl<O: Outline> Type1Font<O> {
    pub fn parse_pfa(data: &[u8]) -> Self {
        let mut vm = Vm::new();
        vm.parse_and_exec(data);
        Self::from_vm(vm)
    }
    pub fn parse_pfb(data: &[u8]) -> Self {
        let mut vm = Vm::new();
        parse_pfb(&mut vm, data).get();
        Self::from_vm(vm)
    }
    pub fn parse_postscript(data: &[u8]) -> Self {
        let mut vm = Vm::new();
        vm.parse_and_exec(data);
        Self::from_vm(vm)
    }
        
    pub fn from_vm(vm: Vm) -> Self {
        let (_font_name, font_dict) = vm.fonts().nth(0).unwrap();
        
        let private_dict = font_dict.get("Private").unwrap()
            .as_dict().unwrap();
        let len_iv = private_dict.get("lenIV")
            .map(|i| i.as_int().unwrap()).unwrap_or(4) as usize;
        
        debug!("FontDict keys: {:?}", font_dict.iter().map(|(k, _)| k).format(", "));
        debug!("Private keys: {:?}", private_dict.iter().map(|(k, _)| k).format(", "));
        debug!("FontName: {:?}", font_dict.get("FontName"));
        debug!("FontInfo: {:?}", font_dict.get("FontInfo"));
        
        let postscript_name = font_dict.get("FontName").map(|i| i.as_str().unwrap().into());
        let full_name = font_dict.get("FontInfo").and_then(|i|
            i.as_dict().unwrap().get("FullName").map(|i| i.as_str().unwrap().into())
        );
        
        let char_strings = font_dict.get("CharStrings").expect("no /CharStrings").as_dict().unwrap();
        
        let subrs: Vec<Vec<u8>> = private_dict.get("Subrs").expect("no /Subrs")
            .as_array().unwrap().iter()
            .map(|item| Decoder::charstring().decode(item.as_bytes().unwrap(), len_iv).into())
            .collect();
        
        let context = Context {
            subr_bias: 0,
            subrs,
            global_subr_bias: 0,
            global_subrs: ()
        };

        let encoding = font_dict.get("Encoding").expect("no /Encoding").as_array().expect("/Encoding is not an array");
        
        let mut names = HashMap::new();
        let mut unicode_map = HashMap::new();
        let mut glyphs = IndexMap::new();
        let mut codepoint = 0;
        for item in encoding.iter() {
            match item {
                RefItem::Null => {},
                RefItem::Literal(b".notdef") => {},
                RefItem::Literal(name) => {
                    let name = std::str::from_utf8(name).unwrap();
                    let unicode = glyphname_to_unicode(name);
                    debug!("glyph: {} {:?}, gid={}", name, unicode, glyphs.len());
                    let char_string = match char_strings.get(name) {
                        Some(item) => item.as_bytes().unwrap(),
                        None => {
                            warn!("no charstring for {}", name);
                            continue;
                        }
                    };
                    
                    let decoded = Decoder::charstring().decode(&char_string, len_iv);
                    //debug!("{} decoded: {:?}", name, String::from_utf8_lossy(&decoded));
                    
                    let mut state = State::new();
                    charstring(&decoded, &context, &mut state).unwrap();
                    
                    let (index, _) = glyphs.insert_full(codepoint, Glyph {
                        path: state.path.into_outline(),
                        metrics: HMetrics {
                            advance: Vector::new(state.char_width.unwrap(), 0.0),
                            lsb: state.lsb.unwrap_or_default()
                        }
                    });
                    names.insert(name.to_owned(), index as u32);
                    
                    if let Some(unicode) = unicode {
                        unicode_map.insert(unicode, index as u32);
                    }
                }
                _ => {}
            }
            codepoint += 1;
        }
        let font_matrix = font_dict.get("FontMatrix").unwrap().as_array().unwrap();
        assert_eq!(font_matrix.len(), 6);
        
        let bbox = font_dict.get("FontBBox")
            .map(|val| {
                let (a, b, c, d) = TupleElements::from_iter(val.as_array().unwrap().iter().map(|v| v.as_f32().unwrap())).unwrap();
                Rect::from_points(Vector::new(a, b), Vector::new(c, d))
            });
        
        let (a, b, c, d, e, f) = TupleElements::from_iter(
                font_matrix.iter().map(|item| item.as_f32().unwrap())
        ).unwrap();
        
        Type1Font {
            font_matrix: Transform::row_major(a, b, c, d, e, f),
            glyphs,
            names,
            unicode_map,
            bbox,
            postscript_name,
            full_name
        }
    }
}

fn parse_binary<'a>(vm: &mut Vm, data: &'a [u8]) {
    let mut decoder = Decoder::file();
    let decoded = decoder.decode(data, 4);
    
    vm.parse_and_exec(&decoded)
}

#[test]
fn test_parser() {
    let mut vm = Vm::new();
    vm.parse_and_exec(b"/FontBBox{-180 -293 1090 1010}readonly ");
    vm.print_stack();
    assert_eq!(vm.stack().len(), 2);
}
fn parse_pfb<'a>(mut vm: &mut Vm, i: &'a [u8]) -> R<'a, ()> {
    let mut input = i;
    while input.len() > 0 {
        let (i, magic) = le_u8(input)?;
        assert_eq!(magic, 0x80);
        let (i, block_type) = le_u8(i)?;
        match block_type {
            1 | 2 => {} // continue
            3 => break,
            n => panic!("unknown block type {}", n)
        }
        
        let (i, block_len) = le_u32(i)?;
        info!("block type {}, length: {}", block_type, block_len);
    
        let block = &i[.. block_len as usize];
        match block_type {
            1 => vm.parse_and_exec(block),
            2 => parse_binary(&mut vm, block),
            _ => unreachable!()
        }
        
        input = &i[block_len as usize ..];
    }
    
    Ok((input, ()))
}
pub fn charstring<'a, 'b, O, T, U>(mut input: &'a [u8], ctx: &'a Context<T, U>, s: &'b mut State<O>) -> IResult<&'a [u8], ()>
    where O: Outline, T: TryIndex + 'a, U: TryIndex + 'a
{
    let mut ps_stack = vec![];
    while input.len() > 0 {
        //trace!("input: {:?}", &input[.. input.len().min(10)]);
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
                let p = s.current + v(0., s.stack[0]);
                s.path.move_to(p);
                s.stack.clear();
                s.current = p;
            }
            5 => { // ⊦ dx dy rlineto (5) ⊦
                trace!("rlineto");
                let p = s.current + v(s.stack[0], s.stack[1]);
                s.path.line_to(p);
                s.stack.clear();
                s.current = p;
            }
            6 => { // ⊦ dx hlineto (6) ⊦
                trace!("hlineto");
                let p = s.current + v(s.stack[0], 0.);
                s.path.line_to(p);
                s.stack.clear();
                s.current = p;
            }
            7 => { // dy vlineto (7)
                trace!("vlineto");
                let p = s.current + v(0., s.stack[0],);
                s.path.line_to(p);
                s.stack.clear();
                s.current = p;
            }
            8 => { // ⊦ dx1 dy1 dx2 dy2 dx3 dy3 rrcurveto (8) ⊦
                trace!("rrcurveto");
                let c1 = s.current + v(s.stack[0], s.stack[1]);
                let c2 = c1 + v(s.stack[2], s.stack[3]);
                let p = c2 + v(s.stack[4], s.stack[5]);
                s.path.cubic_curve_to(c1, c2, p);
                s.stack.clear();
                s.current = p;
            }
            9 => { // –closepath (9) ⊦
                trace!("closepath");
                s.path.close();
                s.stack.clear();
            }
            10 => { // subr# callsubr (10) –
                let subr_nr = s.pop().to_int();
                trace!("callsubr {}", subr_nr);
                let subr = ctx.subr(subr_nr);
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
                        let (sbx, sby, wx, _wy, _sbw) = s.args();
                        trace!("sbw");
                        s.char_width = Some(wx.to_float());
                        s.current = v(sbx, sby);
                        s.stack.clear();
                    }
                    12 => { // num1 num2 div (12 12) quotient
                        trace!("div");
                        let num2 = s.pop().to_float();
                        let num1 = s.pop().to_float();
                        s.push(num1 / num2);
                    }
                    16 => { //  arg1 . . . argn n othersubr# callothersubr (12 16) –
                        let subr_nr = s.pop().to_int();
                        trace!("callothersubr {}", subr_nr);
                        let n = s.pop().to_int() as usize;
                        
                        match subr_nr {
                            1 => {
                                assert_eq!(n, 0);
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
                                assert_eq!(n, 0);
                            }
                            0 => {
                                // end of flex sequences
                                assert_eq!(n, 3);
                                let (flex_height, x, y) = s.pop_tuple();
                                let (_ref, c0, c1, p2, c3, c4, p5) = TupleElements::from_iter(s.flex_sequence.take().unwrap().into_iter()).unwrap();
                                //assert_eq!(p5, v(x, y));
                                s.path.cubic_curve_to(c0, c1, p2);
                                s.path.cubic_curve_to(c3, c4, p5);
                                ps_stack.push(y);
                                ps_stack.push(x);
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
                        let (x, y) = s.args();
                        let p = v(x, y);
                        s.current = p;
                        s.stack.clear();
                    },
                    _ => panic!("invalid operator")
                }
            }
            13 => { // ⊦ sbx wx hsbw (13) ⊦
                trace!("hsbw");
                let (sbx, wx) = s.args();
                let lsb = v(sbx, 0.);
                s.lsb = Some(lsb);
                s.current = lsb;
                s.char_width = Some(wx.to_float());
                s.stack.clear();
            }
            14 => { //– endchar (14) ⊦
                trace!("endchar");
                break;
            }
            21 => { // ⊦ dx dy rmoveto (21) ⊦
                trace!("rmoveto");
                let (dx, dy) = s.args();
                let p = s.current + v(dx, dy);
                
                // hack to counter the flex sequences hack by adobe
                if let Some(ref mut points) = s.flex_sequence {
                    points.push(p);
                } else {
                    s.path.move_to(p);
                }
                s.current = p;
                s.stack.clear();
            }
            22 => { // ⊦ dx hmoveto (22) ⊦
                trace!("hmoveto");
                let (dx, ) = s.args();
                let p = s.current + v(dx, 0.);
                s.path.move_to(p);
                s.current = p;
                s.stack.clear();
            }
            30 => { // ⊦ dy1 dx2 dy2 dx3 vhcurveto (30) ⊦
                trace!("vhcurveto");
                let (dy1, dx2, dy2, dx3) = s.args();
                let c1 = s.current + v(0., dy1);
                let c2 = c1 + v(dx2, dy2);
                let p = c2 + v(dx3, 0.);
                s.path.cubic_curve_to(c1, c2, p);
                s.stack.clear();
                s.current = p;
            }
            31 => { // ⊦ dx1 dx2 dy2 dy3 hvcurveto (31) ⊦
                trace!("hvcurveto");
                let (dx1, dx2, dy2, dy3) = s.args();
                let c1 = s.current + v(dx1, 0.);
                let c2 = c1 + v(dx2, dy2);
                let p = c2 + v(0., dy3);
                s.path.cubic_curve_to(c1, c2, p);
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
                s.push(v as f32 / 65536.);
            }
            c => panic!("unknown code {}", c)
        }
        
        trace!("stack: {:?}", s.stack);
    };
    
    Ok((input, ()))
}
