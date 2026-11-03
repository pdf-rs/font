use crate::{State, Seac, v, Value, Context, TryIndex, FontError, MAX_SUBR_DEPTH, TRANSIENT_SIZE};
use nom::{IResult,
    bytes::complete::{take},
    number::complete::{be_u8, be_i16, be_i32}
};

macro_rules! point {
    ($iter:ident, x) => ({
        let &x = expect!($iter.next(), "stack empty");
        v(x, 0.0)
    });
    ($iter:ident, y) => ({
        let &y = expect!($iter.next(), "stack empty");
        v(0.0, y)
    });
    ($iter:ident, xy) => ({
        let &x = expect!($iter.next(), "stack empty");
        let &y = expect!($iter.next(), "stack empty");
        v(x, y)
    });
    ($iter:ident, yx) => ({
        let &y = expect!($iter.next(), "stack empty");
        let &x = expect!($iter.next(), "stack empty");
        v(x, y)
    });
}

macro_rules! bezier {
    ($s:ident, $slice:ident, $($a:tt $b:tt $c:tt)*) => ({
        let mut iter = $slice.iter();
        $(
            let c1 = $s.current + point!(iter, $a);
            let c2 = c1 + point!(iter, $b);
            let p = c2 + point!(iter, $c);
            $s.contour.push_cubic(c1, c2, p);
            $s.current = p;
        )*
        iter.as_slice()
    });
}
macro_rules! lines {
    ($s:ident, $slice:ident, $($a:tt)*) => ({
        let mut iter = $slice.iter();
        $(
            let p = $s.current + point!(iter, $a);
            $s.contour.push_endpoint(p);
            $s.current = p;
        )*
        iter.as_slice()
    });
}

fn alternating_curve(s: &mut State, mut horizontal: bool) -> Result<(), FontError> {
    let mut slice = s.stack.as_slice();
    while slice.len() > 0 {
        slice = match (slice.len(), horizontal) {
            (5, false) => bezier!(s, slice, y xy xy),
            (5, true)  => bezier!(s, slice, x xy yx),
            (_, false)  => bezier!(s, slice, y xy x),
            (_, true) => bezier!(s, slice, x xy y),
        };
        horizontal = !horizontal;
    }
    Ok(())
}

/// An operand used as a boolean counts as true when it is non-zero (TN5177 §4.4).
fn is_true(value: Value) -> bool {
    match value {
        Value::Int(i) => i != 0,
        Value::Float(f) => f != 0.
    }
}

#[inline]
fn maybe_width(state: &mut State, cond: impl Fn(usize) -> bool) {
    if state.first_stack_clearing_operator {
        state.first_stack_clearing_operator = false;
        if !cond(state.stack.len()) {
            let w = state.stack.remove(0);
            state.delta_width = Some(w.to_float());
        }
    }
}

/// Run one subroutine, guarding against subrs that call themselves.
///
/// The depth is restored afterwards even when the subr fails, so one bad glyph does
/// not poison the counter for the next one.
fn call_subr<'a, 'b, T, U>(subr: &'a [u8], ctx: &'a Context<T, U>, s: &'b mut State) -> Result<(), FontError>
    where T: TryIndex + 'a, U: TryIndex + 'a
{
    if s.subr_depth >= MAX_SUBR_DEPTH {
        error!("subroutine nesting deeper than {} levels (recursive subr?)", MAX_SUBR_DEPTH);
    }
    s.subr_depth += 1;
    let result = charstring(subr, ctx, s);
    s.subr_depth -= 1;
    result
}

pub fn charstring<'a, 'b, T, U>(mut input: &'a [u8], ctx: &'a Context<T, U>, s: &'b mut State) -> Result<(), FontError>
    where T: TryIndex + 'a, U: TryIndex + 'a
{
    while input.len() > 0 && !s.done {
        let (i, b0) = be_u8(input)?;
        let i = match b0 {
            0 => {
                warn!("reserved Type2 operator 0");
                //error!("reserved {}", b0);
                i
            }
            1 => { // ⊦ y dy hstem (1) ⊦
                trace!("hstem");
                maybe_width(s, |n| n % 2 == 0);
                s.stem_hints += (s.stack.len() / 2) as u32;
                s.stack.clear();
                i
            }
            2 => error!("reserved {}", b0),
            3 => { // ⊦ x dx vstem (3) ⊦
                trace!("vstem");
                maybe_width(s, |n| n % 2 == 0);
                s.stem_hints += (s.stack.len() / 2) as u32;
                s.stack.clear();
                i
            }
            4 => { // ⊦ dy vmoveto (4) ⊦
                trace!("vmoveto");
                require!(s.stack.len() >= 1);
                s.flush();

                maybe_width(s, |n| n == 1);
                let p = s.current + v(0., s.stack[0]);
                s.contour.push_endpoint(p);
                s.stack.clear();
                s.current = p;
                i
            }
            5 => { // |- {dxa dya}+ rlineto (5) |-
                trace!("rlineto");
                let mut slice = s.stack.as_slice();
                while slice.len() >= 2 {
                    slice = lines!(s, slice, xy);
                }
                s.stack.clear();
                i
            }
            6 => { // |- dx1 {dya dxb}* hlineto (6) |-
                   // |- {dxa dyb}+ hlineto (6) |-
                trace!("hlineto");
                for (i, &d) in s.stack.iter().enumerate() {
                    let dv = if i % 2 == 0 {
                        v(d, 0.)
                    } else {
                        v(0., d)
                    };
                    let p = s.current + dv;
                    s.contour.push_endpoint(p);
                    s.current = p;
                }
                s.stack.clear();
                i
            }
            7 => { // |- dy1 {dxa dyb}* vlineto (7) |-
                   // |- {dya dxb}+ vlineto (7) |-
                trace!("vlineto");
                for (i, &d) in s.stack.iter().enumerate() {
                    let dv = if i % 2 == 0 {
                        v(0., d)
                    } else {
                        v(d, 0.)
                    };
                    let p = s.current + dv;
                    s.contour.push_endpoint(p);
                    s.current = p;
                }
                s.stack.clear();
                i
            }
            8 => { // ⊦ {dxa dya dxb dyb dxc dyc}+ rrcurveto (8) ⊦
                trace!("rrcurveto");
                let mut slice = s.stack.as_slice();
                while slice.len() >= 6 {
                    slice = bezier!(s, slice, xy xy xy);
                }
                s.stack.clear();
                i
            }
            9 => error!("reserved {}", b0),
            10 => { // subr# callsubr (10) –
                trace!("callsubr");
                let subr_nr = s.pop()?.to_int()?;
                
                let subr = ctx.subr(subr_nr)?;
                call_subr(subr, ctx, s)?;
                i
            }
            11 => { // – return (11) –
                trace!("return");
                return Ok(());
            }
            12 => {
                let (i, b1) = be_u8(i)?;
                match b1 {
                    0 => { // dotsection (NOOP)
                        s.stack.clear();
                        i
                    }
                    1 | 2 => error!("reserved: 12 {}", b1),
                    3 => { // num1 num2 and (12 3) bool
                        // TN5177 §4.4: 1 when *both* operands are non-zero, else 0.
                        trace!("and");
                        let num2 = s.pop()?;
                        let num1 = s.pop()?;
                        s.push(if is_true(num1) && is_true(num2) { 1 } else { 0 });
                        i
                    }
                    4 => { // num1 num2 or (12 4) bool
                        trace!("or");
                        let num2 = s.pop()?;
                        let num1 = s.pop()?;
                        s.push(if is_true(num1) || is_true(num2) { 1 } else { 0 });
                        i
                    }
                    5 => { // num1 not (12 5) bool
                        trace!("not");
                        let num1 = s.pop()?;
                        s.push(if is_true(num1) { 0 } else { 1 });
                        i
                    }
                    6 | 7 | 8 => error!("reserved"),
                    9 => { // num abs (12 9) num2
                        trace!("abs");
                        match s.pop()? {
                            Value::Int(i) => s.push(i.abs()),
                            Value::Float(f) => s.push(f.abs())
                        }
                        i
                    }
                    10 => { // num1 num2 add (12 10) sum
                        trace!("add");
                        match (s.pop()?, s.pop()?) {
                            (Value::Int(num2), Value::Int(num1)) => s.push(num1 + num2),
                            (num2, num1) => s.push(num2.to_float() + num1.to_float())
                        }
                        i
                    }
                    11 => { // num1 num2 sub (12 11) difference
                        trace!("sub");
                        match (s.pop()?, s.pop()?) {
                            (Value::Int(num2), Value::Int(num1)) => s.push(num1 - num2),
                            (num2, num1) => s.push(num2.to_float() - num1.to_float())
                        }
                        i
                    }
                    12 => { // num1 num2 div (12 12) quotient
                        trace!("div");
                        let num2 = s.pop()?.to_float();
                        let num1 = s.pop()?.to_float();
                        s.push(num1 / num2);
                        i
                    }
                    13 => error!("reserved {}", b1),
                    14 => { // num neg (12 14) num2
                        trace!("neg");
                        match s.pop()? {
                            Value::Int(i) => s.push(-i),
                            Value::Float(f) => s.push(-f)
                        }
                        i
                    }
                    15 => error!("unimplemented: eq"),
                    16 | 17 => error!("reserved {}", b1),
                    18 => { // num drop (12 18)
                        trace!("drop");
                        s.pop()?;
                        i
                    }
                    19 => error!("reserved {}", b1),
                    20 => { // val i put (12 20) –
                        trace!("put");
                        let idx = s.pop()?.to_int()?;
                        let val = s.pop()?;
                        if idx < 0 || idx as usize >= TRANSIENT_SIZE {
                            error!("put: transient array index {} out of range (0..{})", idx, TRANSIENT_SIZE);
                        }
                        s.transient[idx as usize] = val;
                        i
                    }
                    21 => { // i get (12 21) val
                        trace!("get");
                        let idx = s.pop()?.to_int()?;
                        if idx < 0 || idx as usize >= TRANSIENT_SIZE {
                            error!("get: transient array index {} out of range (0..{})", idx, TRANSIENT_SIZE);
                        }
                        s.push(s.transient[idx as usize]);
                        i
                    }
                    22 => { // num1 num2 v1 v2 ifelse (12 22) v
                        // TN5177 §4.4: `v1` when `num1 <= num2`, otherwise `v2`.
                        trace!("ifelse");
                        let v2 = s.pop()?.to_float();
                        let v1 = s.pop()?.to_float();
                        let num2 = s.pop()?.to_float();
                        let num1 = s.pop()?.to_float();
                        s.push(if num1 <= num2 { v1 } else { v2 });
                        i
                    }
                    23 => { // random (12 23) num2
                        trace!("random");
                        use rand::{thread_rng, Rng};
                        use rand::distributions::OpenClosed01;
                        
                        let val: f32 = thread_rng().sample(OpenClosed01);
                        s.push(val);
                        i
                    }
                    24 => { // num1 num2 mul (12 24) product
                        trace!("mul");
                        let num2 = s.pop()?.to_float();
                        let num1 = s.pop()?.to_float();
                        s.push(num1 * num2);
                        i
                    }
                    25 => error!("reserved {}", b1),
                    26 => { // num sqrt (12 26) num2
                        trace!("sqrt");
                        let num1 = s.pop()?.to_float();
                        s.push(num1.sqrt());
                        i
                    }
                    27 => { // any dup (12 27) any any
                        trace!("dup");
                        let any = s.pop()?;
                        s.push(any);
                        s.push(any);
                        i
                    }
                    28 => { // num1 num2 exch (12 28) num2 num1
                        trace!("exch");
                        let num2 = s.pop()?;
                        let num1 = s.pop()?;
                        s.push(num2);
                        s.push(num1);
                        i
                    }
                    29 => { // numX ... num0 i index (12 29) numX ... num0 numi
                        trace!("index");
                        let j = s.pop()?.to_int()?;
                        // TN5177 §4.4: "if i is negative, the top element is copied".
                        let j = if j < 0 { 0 } else { j as usize };
                        require!(j < s.stack.len());
                        let idx = s.stack.len() - j - 1;
                        let val = s.stack[idx];
                        s.push(val);
                        i
                    }
                    30 => { // num(N–1) ... num0 N J roll (12 30) num((J–1) mod N) ... num0 num(N–1) ... num(J mod N)
                        trace!("roll");
                        let j = s.pop()?.to_int()?;
                        let n = s.pop()?.to_usize()?;
                        let len = s.stack.len();
                        require!(n <= len);
                        let slice = &mut s.stack[len - n .. len];
                        if j > 0 {
                            let j = j as usize;
                            require!(j < n);
                            slice.rotate_left(j);
                        } else if j < 0 {
                            let j = (-j) as usize;
                            require!(j < n);
                            slice.rotate_right(j);
                        }
                        i
                    }
                    31 | 32 | 33 => error!("reserved {}", b1),
                    34 => { // |- dx1 dx2 dy2 dx3 dx4 dx5 dx6 hflex (12 34) |-
                        trace!("hflex");
                        let slice = s.stack.as_slice();
                        bezier!(s, slice, x xy x  x x x);
                        s.stack.clear();
                        i
                    }
                    35 => { // |- dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 dx6 dy6 fd flex (12 35) |-
                        debug!("flex");
                        let slice = s.stack.as_slice();
                        bezier!(s, slice, xy xy xy  xy xy xy);
                        s.stack.clear();
                        i
                    }
                    36 => { // |- dx1 dy1 dx2 dy2 dx3 dx4 dx5 dy5 dx6 hflex1 (12 36) |-
                        trace!("hflex1");
                        let slice = s.stack.as_slice();
                        require!(slice.len() >= 9);

                        // `hflex1` is only legal when the starting and ending points
                        // share their y-value, so the last delta is not 0 but the one
                        // that brings the curve back: dy6 = -(dy1 + dy2 + dy5).
                        let dy6 = -(slice[1].to_float()
                            + slice[3].to_float()
                            + slice[7].to_float());

                        let mut iter = slice.iter();
                        let c1 = s.current + point!(iter, xy);
                        let c2 = c1 + point!(iter, xy);
                        let join = c2 + point!(iter, x);
                        s.contour.push_cubic(c1, c2, join);

                        let c3 = join + point!(iter, x);
                        let c4 = c3 + point!(iter, xy);
                        let dx6 = {
                            let &dx6 = expect!(iter.next(), "stack empty");
                            dx6.to_float()
                        };
                        let end = c4 + v(dx6, dy6);
                        s.contour.push_cubic(c3, c4, end);
                        s.current = end;
                        s.stack.clear();
                        i
                    }
                    37 => { // |- dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 d6 flex1 (12 37) |-
                        trace!("flex1");
                        let slice = s.stack.as_slice();
                        
                        // process first bezier
                        bezier!(s, slice, xy xy xy);
                        
                        // figure out the second
                        let mut iter = slice.iter();
                        let mut sum = point!(iter, xy);
                        for _ in 0 ..  4 {
                            sum = sum + point!(iter, xy);
                        }
                        let horizontal = sum.x().abs() > sum.y().abs();
                        
                        let mut iter = slice[6..].iter();
                        let d4 = s.current + point!(iter, xy);
                        let d5 = d4 + point!(iter, xy);
                        let d6 = d5 + match horizontal {
                            true => point!(iter, x),
                            false => point!(iter, y)
                        };
                        s.contour.push_cubic(d4, d5, d6);
                        s.current = d6;
                        s.stack.clear();
                        i
                    }
                    38 ..= 255 => error!("reserved {}", b1),
                }
            }
            13 => error!("reserved {}", b0),
            14 => { //– endchar (14) ⊦
                trace!("endchar");
                // TN5177 Appendix C: with four arguments (and optionally a leading
                // width) `endchar` is the deprecated `seac` form, adx ady bchar achar;
                // the composition happens where the other glyphs are known.
                maybe_width(s, |n| n == 0 || n == 4);
                if s.stack.len() >= 4 {
                    let first = s.stack.len() - 4;
                    let adx = s.stack[first].to_float();
                    let ady = s.stack[first + 1].to_float();
                    let bchar = s.stack[first + 2].to_float();
                    let achar = s.stack[first + 3].to_float();
                    if !(0.0 ..= 255.0).contains(&bchar) || !(0.0 ..= 255.0).contains(&achar) {
                        warn!("endchar seac: character code out of range ({bchar}, {achar})");
                    } else {
                        s.seac = Some(Seac {
                            adx, ady,
                            base: bchar as u8,
                            accent: achar as u8,
                        });
                    }
                }
                s.contour.close();
                s.done = true;
                i
            }
            15 | 16 | 17 => error!("reserved {}", b0),
            18 => { // |- y dy {dya dyb}* hstemhm (18) |-
                trace!("hstemhm");
                maybe_width(s, |n| n % 2 == 0);
                s.stem_hints += (s.stack.len() / 2) as u32;
                s.stack.clear();
                i
            }
            19 => { // |- hintmask (19 + mask) |-
                trace!("hintmask");
                maybe_width(s, |n| n == 0);
                s.stem_hints += (s.stack.len() / 2) as u32;
                let (i, _) = take((s.stem_hints + 7) / 8)(i)?;
                s.stack.clear();
                i
            }
            20 => { // cntrmask |- cntrmask (20 + mask) |-
                trace!("cntrmask");
                maybe_width(s, |n| n == 0);
                s.stem_hints += (s.stack.len() / 2) as u32;
                let (i, _) = take((s.stem_hints + 7) / 8)(i)?;
                s.stack.clear();
                i
            }
            21 => { // ⊦ dx dy rmoveto (21) ⊦
                trace!("rmoveto");
                require!(s.stack.len() >= 2);
                maybe_width(s, |n| n == 2);
                s.flush();
                let p = s.current + v(s.stack[0], s.stack[1]);
                s.contour.push_endpoint(p);
                s.current = p;
                s.stack.clear();
                i
            }
            22 => { // ⊦ dx hmoveto (22) ⊦
                trace!("hmoveto");
                require!(s.stack.len() >= 1);
                maybe_width(s, |n| n == 1);
                s.flush();
                let p = s.current + v(s.stack[0], 0.);
                s.contour.push_endpoint(p);
                s.current = p;
                s.stack.clear();
                i
            }
            23 => { // |- x dx {dxa dyx}* vstemhm (23) |-
                trace!("vstemhm");
                maybe_width(s, |n| n % 2 == 0);
                s.stem_hints += (s.stack.len() / 2) as u32;
                s.stack.clear();
                i
            }
            24 => { // |- {dxa dya dxb dyb dxc dyc}+ dxd dyd rcurveline (24) |-
                trace!("rcurveline");
                let mut slice = s.stack.as_slice();
                while slice.len() >= 8 {
                    slice = bezier!(s, slice, xy xy xy);
                }
                lines!(s, slice, xy);
                
                s.stack.clear();
                i
            }
            25 => { // |- {dxa dya}+ dxb dyb dxc dyc dxd dyd rlinecurve (25) |-
                trace!("rlinecurve");
                let mut slice = s.stack.as_slice();
                while slice.len() >= 8 {
                    slice = lines!(s, slice, xy);
                }
                bezier!(s, slice, xy xy xy);
                
                s.stack.clear();
                i
            }
            26 => { // |- dx1? {dya dxb dyb dyc}+ vvcurveto (26) |-
                trace!("vvcurveto");
                let mut slice = s.stack.as_slice();
                if slice.len() % 2 == 1 { // odd 
                    slice = bezier!(s, slice, xy xy y);
                }
                while slice.len() >= 4 {
                    slice = bezier!(s, slice, y xy y);
                }
                s.stack.clear();
                i
            }
            27 => { // ⊦ dy1? {dxa dxb dyb dxc}+ hhcurveto (27) ⊦
                trace!("hhcurveto");
                let mut slice = s.stack.as_slice();
                if slice.len() % 2 == 1 { // odd 
                    slice = bezier!(s, slice, yx xy x);
                }
                while slice.len() >= 4 {
                    slice = bezier!(s, slice, x xy x);
                }
                s.stack.clear();
                i
            }
            29 => { // globalsubr# callgsubr (29) –
                let subr_nr = s.pop()?.to_int()?;
                trace!("globalsubr#{}", subr_nr as i32 + ctx.global_subr_bias);
                
                let subr = ctx.global_subr(subr_nr)?;
                call_subr(subr, ctx, s)?;
                i
            }
            30 => { // |- dy1 dx2 dy2 dx3 {dxa dxb dyb dyc dyd dxe dye dxf}* dyf? vhcurveto (30) |-
                    // |- {dya dxb dyb dxc dxd dxe dye dyf}+ dxf? vhcurveto (30) |-
                trace!("vhcurveto");
                alternating_curve(s, false)?;
                
                s.stack.clear();
                i
            }
            31 => { // |- dx1 dx2 dy2 dy3 {dya dxb dyb dxc dxd dxe dye dyf}* dxf? hvcurveto (31) |-
                    // |- {dxa dxb dyb dyc dyd dxe dye dxf}+ dyf? hvcurveto (31) |-
                trace!("hvcurveto");
                alternating_curve(s, true)?;
                
                s.stack.clear();
                i
            },
            28 => {
                let (i, v) = be_i16(i)?;
                s.push(v);
                i
            }
            v @ 32 ..= 246 => {
                s.push(v as i32 - 139);
                i
            }
            v @ 247 ..= 250 => {
                let (i, w) = be_u8(i)?;
                s.push((v as i32 - 247) * 256 + w as i32 + 108);
                i
            }
            v @ 251 ..= 254 => {
                let (i, w) = be_u8(i)?;
                s.push(-(v as i32 - 251) * 256 - w as i32 - 108);
                i
            }
            255 => {
                let (i, v) = be_i32(i)?;
                s.push(v as f32 / 65536.);
                i
            }
        };
        
        input = i;
    };
    
    Ok(())
}
