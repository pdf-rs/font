#![allow(non_snake_case)]

use std::collections::{HashMap, HashSet};
use crate::R;
use crate::opentype::coverage_table;

use nom::number::complete::{be_i16, be_u16};
pub mod assembly;

pub fn parse_math(data: &[u8]) -> R<MathHeader> {
    MathHeader::parse(data)
}

pub trait Parser {
    type Output;
    fn parse(data: &[u8])-> R<Self::Output>;
}
pub trait FixedSize {
    const SIZE: usize;
}

macro_rules! parser {
    ($name:ident : $fun:ident -> $out:ty) => (
        pub struct $name;
        impl Parser for $name {
            type Output = $out;
            fn parse(data: &[u8])-> R<Self::Output> {
                $fun(data)
            }
        }
        impl FixedSize for $name {
            const SIZE: usize = std::mem::size_of::<$out>();
        }
    )
}
parser!(int16 : be_i16 -> i16);
parser!(uint16 : be_u16 -> u16);

macro_rules! parse_field {
    ($start:expr, $input:expr, ?$ptr:ident $parser:ident, $field:expr) => ({
        let (i, offset) = <$ptr as Parser>::parse($input)?;
        if offset != 0 {
            let data = &$start[offset as usize ..];
            let (_, value) = <$parser as Parser>::parse(data)?;
            Ok((i, value))
        } else {
            Ok((i, Default::default()))
        }
    });
    ($start:expr, $input:expr, @ $ptr:ident $parser:ident, $field:expr) => ({
        let (i, offset) = <$ptr as Parser>::parse($input)?;
        assert_ne!(offset, 0, stringify!($field));

        let data = &$start[offset as usize ..];
        let (_, value) = <$parser as Parser>::parse(data)?;
        Ok((i, value))
    });
    ($start:expr, $input:expr, $parser:ident, $field:expr) => (
        <$parser as Parser>::parse($input)
    );
}
macro_rules! field_size {
    (@ $ptr:ident $(?)* $parser:ident) => (<$ptr as FixedSize>::SIZE);
    ($parser:ident) => (<$parser as FixedSize>::SIZE);
}

macro_rules! table {
    ($name:ident { $( $(#[$meta:meta])* $(?$ptr_opt:ident)* $(@$ptr:ident)* $parser:ident $field:tt, )* } ) => (
        #[derive(Clone, Debug)]
        pub struct $name {
            $(
                $(#[$meta])*
                pub $field: <$parser as Parser>::Output,
            )*
        }
        impl Parser for $name {
            type Output = $name;
            fn parse(input: &[u8]) -> R<$name> {
                let i = input;
                $(
                    let (i, $field) = parse_field!(input, i, $(?$ptr_opt)* $(@$ptr)* $parser, $field)?;
                )*
                Ok((i, $name { $( $field, )* }))
            }
        }
        impl FixedSize for $name {
            const SIZE: usize = 0 $(+ field_size!($(@$ptr_opt)* $(@$ptr)* $parser) )*;
        }
    );
}

fn array_iter<'a, P: Parser + FixedSize>(input: &'a [u8], count: usize) -> R<impl Iterator<Item=P::Output> + ExactSizeIterator + 'a> {
    let (ours, remaining) = input.split_at(P::SIZE * count);
    let iter = ours.chunks(P::SIZE).map(|chunk| P::parse(chunk).unwrap().1);
    Ok((remaining, iter))
}

#[derive(Default, Clone, Debug)]
pub struct MathValueRecord {
    pub value: i16
}
impl Parser for MathValueRecord {
    type Output = Self;
    fn parse<'a>(i: &'a [u8])-> R<'a, Self::Output> {
        let (i, value) = be_i16(i)?;
        let (i, _offset) = be_i16(i)?;
        Ok((i, MathValueRecord { value }))
    }
}
impl FixedSize for MathValueRecord {
    const SIZE: usize = 4;
}

table!(MathHeader {
    /// Major version of the MATH table, = 1.
    uint16 majorVersion,

    /// Minor version of the MATH table, = 0.
    uint16 minorVersion,

    /// Offset to MathConstants table - from the beginning of MATH table.
    @uint16 MathConstants constants,

    /// Offset to MathGlyphInfo table - from the beginning of MATH table.
    @uint16 MathGlyphInfo glyph_info,

    /// Offset to MathVariants table - from the beginning of MATH table.
    @uint16 MathVariants variants,
});

table!(MathConstants {
    /// Percentage of scaling down for level 1 superscripts and subscripts. Suggested value: 80%.
    int16 script_percent_scale_down,

    /// Percentage of scaling down for level 2 (scriptScript) superscripts and subscripts. Suggested value: 60%.
    int16 script_script_percent_scale_down,

    /// Minimum height required for a delimited expression (contained within parentheses, etc.) to be treated as a sub-formula. Suggested value: normal line height × 1.5.
    uint16 delimited_sub_formula_min_height,

    /// Minimum height of n-ary operators (such as integral and summation) for formulas in display mode (that is, appearing as standalone page elements, not embedded inline within text).
    uint16 display_operator_min_height,

    /// White space to be left between math formulas to ensure proper line spacing. For example, for applications that treat line gap as a part of line ascender, formulas with ink going above (os2.sTypoAscender + os2.sTypoLineGap - MathLeading) or with ink going below os2.sTypoDescender will result in increasing line height.
    MathValueRecord math_leading,

    /// Axis height of the font. In math typesetting, the term axis refers to a horizontal reference line used for positioning elements in a formula. The math axis is similar to but distinct from the baseline for regular text layout. For example, in a simple equation, a minus symbol or fraction rule would be on the axis, but a string for a variable name would be set on a baseline that is offset from the axis. The axisHeight value determines the amount of that offset.
    MathValueRecord axis_height,

    /// Maximum (ink) height of accent base that does not require raising the accents. Suggested: x‑height of the font (os2.sxHeight) plus any possible overshots
    MathValueRecord accent_base_height,

    /// Maximum (ink) height of accent base that does not require flattening the accents. Suggested: cap height of the font (os2.sCapHeight).
    MathValueRecord flattened_accent_base_height,

    /// The standard shift down applied to subscript elements. Positive for moving in the downward direction. Suggested: os2.ySubscriptYOffset.
    MathValueRecord subscript_shift_down,

    /// Maximum allowed height of the (ink) top of subscripts that does not require moving subscripts further down. Suggested: 4/5 x- height.
    MathValueRecord subscript_top_max,

    /// Minimum allowed drop of the baseline of subscripts relative to the (ink) bottom of the base. Checked for bases that are treated as a box or extended shape. Positive for subscript baseline dropped below the base bottom.
    MathValueRecord subscript_baseline_drop_min,

    /// Standard shift up applied to superscript elements. Suggested: os2.ySuperscriptYOffset.
    MathValueRecord superscript_shift_up,

    /// Standard shift of superscripts relative to the base, in cramped style.
    MathValueRecord superscript_shift_up_cramped,

    /// Minimum allowed height of the (ink) bottom of superscripts that does not require moving subscripts further up. Suggested: ¼ x-height.
    MathValueRecord superscript_bottom_min,

    /// Maximum allowed drop of the baseline of superscripts relative to the (ink) top of the base. Checked for bases that are treated as a box or extended shape. Positive for superscript baseline below the base top.
    MathValueRecord superscript_baseline_drop_max,

    /// Minimum gap between the superscript and subscript ink. Suggested: 4 × default rule thickness.
    MathValueRecord sub_superscript_gap_min,

    /// The maximum level to which the (ink) bottom of superscript can be pushed to increase the gap between superscript and subscript, before subscript starts being moved down. Suggested: 4/5 x-height.
    MathValueRecord superscript_bottom_max_with_subscript,

    /// Extra white space to be added after each subscript and superscript. Suggested: 0.5 pt for a 12 pt font.
    MathValueRecord space_after_script,

    /// Minimum gap between the (ink) bottom of the upper limit, and the (ink) top of the base operator.
    MathValueRecord upper_limit_gap_min,

    /// Minimum distance between baseline of upper limit and (ink) top of the base operator.
    MathValueRecord upper_limit_baseline_rise_min,

    /// Minimum gap between (ink) top of the lower limit, and (ink) bottom of the base operator.
    MathValueRecord lower_limit_gap_min,

    /// Minimum distance between baseline of the lower limit and (ink) bottom of the base operator.
    MathValueRecord lower_limit_baseline_drop_min,

    /// Standard shift up applied to the top element of a stack.
    MathValueRecord stack_top_shift_up,

    /// Standard shift up applied to the top element of a stack in display style.
    MathValueRecord stack_top_display_style_shift_up,

    /// Standard shift down applied to the bottom element of a stack. Positive for moving in the downward direction.
    MathValueRecord stack_bottom_shift_down,

    /// Standard shift down applied to the bottom element of a stack in display style. Positive for moving in the downward direction.
    MathValueRecord stack_bottom_display_style_shift_down,

    /// Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element. Suggested: 3 × default rule thickness.
    MathValueRecord stack_gap_min,

    /// Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element in display style. Suggested: 7 × default rule thickness.
    MathValueRecord stack_display_style_gap_min,

    /// Standard shift up applied to the top element of the stretch stack.
    MathValueRecord stretch_stack_top_shift_up,

    /// Standard shift down applied to the bottom element of the stretch stack. Positive for moving in the downward direction.
    MathValueRecord stretch_stack_bottom_shift_down,

    /// Minimum gap between the ink of the stretched element, and the (ink) bottom of the element above. Suggested: same value as upperLimitGapMin.
    MathValueRecord stretch_stack_gap_above_min,

    /// Minimum gap between the ink of the stretched element, and the (ink) top of the element below. Suggested: same value as lowerLimitGapMin.
    MathValueRecord stretch_stack_gap_below_min,

    /// Standard shift up applied to the numerator.
    MathValueRecord fraction_numerator_shift_up,

    /// Standard shift up applied to the numerator in display style. Suggested: same value as stackTopDisplayStyleShiftUp.
    MathValueRecord fraction_numerator_display_style_shift_up,

    /// Standard shift down applied to the denominator. Positive for moving in the downward direction.
    MathValueRecord fraction_denominator_shift_down,

    /// Standard shift down applied to the denominator in display style. Positive for moving in the downward direction. Suggested: same value as stackBottomDisplayStyleShiftDown.
    MathValueRecord fraction_denominator_display_style_shift_down,

    /// Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar. Suggested: default rule thickness.
    MathValueRecord fraction_numerator_gap_min,

    /// Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar in display style. Suggested: 3 × default rule thickness.
    MathValueRecord fraction_num_display_style_gap_min,

    /// Thickness of the fraction bar. Suggested: default rule thickness.
    MathValueRecord fraction_rule_thickness,

    /// Minimum tolerated gap between the (ink) top of the denominator and the ink of the fraction bar. Suggested: default rule thickness.
    MathValueRecord fraction_denominator_gap_min,

    /// Minimum tolerated gap between the (ink) top of the denominator and the ink of the fraction bar in display style. Suggested: 3 × default rule thickness.
    MathValueRecord fraction_denom_display_style_gap_min,

    /// Horizontal distance between the top and bottom elements of a skewed fraction.
    MathValueRecord skewed_fraction_horizontal_gap,

    /// Vertical distance between the ink of the top and bottom elements of a skewed fraction.
    MathValueRecord skewed_fraction_vertical_gap,

    /// Distance between the overbar and the (ink) top of he base. Suggested: 3 × default rule thickness.
    MathValueRecord overbar_vertical_gap,

    /// Thickness of overbar. Suggested: default rule thickness.
    MathValueRecord overbar_rule_thickness,

    /// Extra white space reserved above the overbar. Suggested: default rule thickness.
    MathValueRecord overbar_extra_ascender,

    /// Distance between underbar and (ink) bottom of the base. Suggested: 3 × default rule thickness.
    MathValueRecord underbar_vertical_gap,

    /// Thickness of underbar. Suggested: default rule thickness.
    MathValueRecord underbar_rule_thickness,

    /// Extra white space reserved below the underbar. Always positive. Suggested: default rule thickness.
    MathValueRecord underbar_extra_descender,

    /// Space between the (ink) top of the expression and the bar over it. Suggested: 1¼ default rule thickness.
    MathValueRecord radical_vertical_gap,

    /// Space between the (ink) top of the expression and the bar over it. Suggested: default rule thickness + ¼ x-height.
    MathValueRecord radical_display_style_vertical_gap,

    /// Thickness of the radical rule. This is the thickness of the rule in designed or constructed radical signs. Suggested: default rule thickness.
    MathValueRecord radical_rule_thickness,

    /// Extra white space reserved above the radical. Suggested: same value as radicalRuleThickness.
    MathValueRecord radical_extra_ascender,

    /// Extra horizontal kern before the degree of a radical, if such is present.
    MathValueRecord radical_kern_before_degree,

    /// Negative kern after the degree of a radical, if such is present. Suggested: −10/18 of em.
    MathValueRecord radical_kern_after_degree,

    /// Height of the bottom of the radical degree, if such is present, in proportion to the ascender of the radical sign. Suggested: 60%.
    int16 radical_degree_bottom_raise_percent,
});

table!(MathGlyphInfo {
    /// Offset to MathItalicsCorrectionInfo table, from the beginning of the MathGlyphInfo table.
    @uint16 MathItalicsCorrectionInfo italics_correction_info,

    /// Offset to MathTopAccentAttachment table, from the beginning of the MathGlyphInfo table.
    @uint16 MathTopAccentAttachment top_accent_attachment,

    /// Offset to ExtendedShapes coverage table, from the beginning of the MathGlyphInfo table. When the glyph to the left or right of a box is an extended shape variant, the (ink) box should be used for vertical positioning purposes, not the default position defined by values in MathConstants table. May be NULL.
    @uint16 ExtendedShapes extended_shape_coverage,

    /// Offset to MathKernInfo table, from the beginning of the MathGlyphInfo table.
    ?uint16 MathKernInfo kern_info,
});

#[derive(Clone, Debug)]
pub struct MathItalicsCorrectionInfo {
    map: HashMap<u16, MathValueRecord>
}
impl MathItalicsCorrectionInfo {
    pub fn get(&self, gid: u16) -> Option<&MathValueRecord> {
        self.map.get(&gid)
    }
}
impl Parser for MathItalicsCorrectionInfo {
    type Output = MathItalicsCorrectionInfo;
    fn parse(data: &[u8]) -> R<Self> {
        let (i, italics_correction_coverage_offset) = be_u16(data)?;
        let (_, italics_correction_coverage) = coverage_table(&data[italics_correction_coverage_offset as usize ..])?;
        let (i, italics_correction_count) = be_u16(i)?;
        let (i, italics_correction) = array_iter::<MathValueRecord>(i, italics_correction_count as usize)?;
        let map = italics_correction_coverage.zip(italics_correction).collect();
       Ok((i, MathItalicsCorrectionInfo { map }))
    }
}

#[derive(Clone, Debug)]
pub struct MathTopAccentAttachment {
    map: HashMap<u16, MathValueRecord>
}
impl MathTopAccentAttachment {
    pub fn get(&self, gid: u16) -> Option<&MathValueRecord> {
        self.map.get(&gid)
    }
}
impl Parser for MathTopAccentAttachment {
    type Output = MathTopAccentAttachment;
    fn parse(data: &[u8]) -> R<Self> {
        let (i, top_accent_coverage_offset) = be_u16(data)?;
        let (_, top_accent_coverage) = coverage_table(&data[top_accent_coverage_offset as usize ..])?;
        let (i, top_accent_attachment_count) = be_u16(i)?;
        let (i, top_accent_attachment) = array_iter::<MathValueRecord>(i, top_accent_attachment_count as usize)?;
        let map = top_accent_coverage.zip(top_accent_attachment).collect();
       Ok((i, MathTopAccentAttachment { map }))
    }
}

table!(MathGlyphVariantRecord {
    /// Glyph ID for the variant.
    uint16 variant_glyph,

    /// Advance width/height, in design units, of the variant, in the direction of requested glyph extension.
    uint16 advance_measurement,
});

table!(GlyphPartRecord {
    /// Glyph ID for the part.
    uint16 glyph_id,

    /// Advance width/ height, in design units, of the straight bar connector material at the start of the glyph in the direction of the extension (the left end for horizontal extension, the bottom end for vertical extension).
    uint16 start_connector_length,

    /// Advance width/ height, in design units, of the straight bar connector material at the end of the glyph in the direction of the extension (the right end for horizontal extension, the top end for vertical extension).
    uint16 end_connector_length,

    /// Full advance width/height for this part in the direction of the extension, in design units.
    uint16 full_advance,
    /// Part qualifiers. PartFlags enumeration currently uses only one bit:
    /// 0x0001 fExtender If set, the part can be skipped or repeated.
    /// 0xFFFE Reserved.
    uint16 part_flags,
});

impl GlyphPartRecord {
    #[inline]
    pub fn required(&self) -> bool {
        self.part_flags & 1 == 0
    }
    #[inline]
    pub fn optional(&self) -> bool {
        self.part_flags & 1 != 0
    }
}

#[derive(Clone, Debug)]
pub struct GlyphAssembly {
    pub italics_correction: MathValueRecord,
    pub parts: Vec<GlyphPartRecord>
}
impl Parser for GlyphAssembly {
    type Output = Self;
    fn parse(data: &[u8]) -> R<Self> {
        let (i, italics_correction) = MathValueRecord::parse(data)?;
        let (i, part_count) = be_u16(i)?;
        let (i, parts) = array_iter::<GlyphPartRecord>(i, part_count as usize)?;

        Ok((i, GlyphAssembly {
            italics_correction,
            parts: parts.collect()
        }))
    }
}

#[derive(Clone, Debug)]
pub struct MathGlyphConstruction {
    pub glyph_assembly: Option<GlyphAssembly>,
    pub variants: Vec<MathGlyphVariantRecord>,
}
impl Parser for MathGlyphConstruction {
    type Output = Self;
    fn parse(data: &[u8]) -> R<Self> {
        let (i, glyph_assembly_offset) = be_u16(data)?;
        let glyph_assembly = match glyph_assembly_offset {
            0 => None,
            off => Some(GlyphAssembly::parse(&data[off as usize ..])?.1)
        };
        
        let (i, variant_count) = be_u16(i)?;
        let (i, variants) = array_iter::<MathGlyphVariantRecord>(i, variant_count as usize)?;
        Ok((i, MathGlyphConstruction {
            glyph_assembly,
            variants: variants.collect()
        }))
    }
}

#[derive(Clone, Debug)]
pub struct MathVariants {
    pub min_connector_overlap: u16,
    pub vert_glyph_construction: HashMap<u16, MathGlyphConstruction>,
    pub horiz_glyph_construction: HashMap<u16, MathGlyphConstruction>,
}
impl Parser for MathVariants {
    type Output = MathVariants;
    fn parse(data: &[u8]) -> R<Self> {
        let (i, min_connector_overlap) = be_u16(data)?;
        let (i, vert_glyph_coverage_offset) = be_u16(i)?;
        let (i, horiz_glyph_coverage_offset) = be_u16(i)?;

        let (_, vert_glyph_coverage) = coverage_table(&data[vert_glyph_coverage_offset as usize ..])?;
        let (_, horiz_glyph_coverage) = coverage_table(&data[horiz_glyph_coverage_offset as usize ..])?;

        let (i, vert_glyph_count) = be_u16(i)?;
        let (i, horiz_glyph_count) = be_u16(i)?;

        let (i, vert_glyph_construction_offsets) = array_iter::<uint16>(i, vert_glyph_count as usize)?;
        let (i, horiz_glyph_construction_offsets) = array_iter::<uint16>(i, horiz_glyph_count as usize)?;

        let vert_glyph_construction = vert_glyph_construction_offsets.map(|off| MathGlyphConstruction::parse(&data[off as usize ..]).unwrap().1);
        let horiz_glyph_construction = horiz_glyph_construction_offsets.map(|off| MathGlyphConstruction::parse(&data[off as usize ..]).unwrap().1);

        Ok((i, MathVariants {
            min_connector_overlap,
            vert_glyph_construction: vert_glyph_coverage.zip(vert_glyph_construction).collect(),
            horiz_glyph_construction: horiz_glyph_coverage.zip(horiz_glyph_construction).collect(),
        }))
    }
}

#[derive(Default, Debug, Clone)]
pub struct MathKern {
    pub pairs: Vec<(MathValueRecord, MathValueRecord)>,
    pub last: MathValueRecord
}
impl Parser for MathKern {
    type Output = Self;
    fn parse(i: &[u8]) -> R<Self> {
        let (i, height_count) = be_u16(i)?;
        let (i, heights) = array_iter::<MathValueRecord>(i, height_count as usize)?;
        let (i, kerns) = array_iter::<MathValueRecord>(i, height_count as usize)?;
        let (i, last) = MathValueRecord::parse(i)?;
        let pairs = heights.zip(kerns).collect();
        Ok((i, MathKern { pairs,last }))
    }
}

impl MathKern {
    pub fn kern_for_height(&self, height: i16) -> i16 {
        for (h, k) in self.pairs.iter() {
            if height < h.value {
                return k.value;
            }
        }
        self.last.value
    }
}

#[derive(Debug, Clone)]
pub struct MathKernInfoRecord {
    pub top_right: MathKern,
    pub top_left: MathKern,
    pub bottom_right: MathKern,
    pub bottom_left: MathKern,
}

#[derive(Clone, Debug, Default)]
pub struct MathKernInfo {
    pub entries: HashMap<u16, MathKernInfoRecord>
}
impl Parser for MathKernInfo {
    type Output = Self;
    fn parse(data: &[u8]) -> R<Self> {
        use itertools::Itertools;

        let (i, coverage_offset) = be_u16(data)?;
        let (_, coverage) = coverage_table(&data[coverage_offset as usize ..])?;
        let (i, kern_count) = be_u16(i)?;
        let (i, records) = array_iter::<uint16>(i, 4 * kern_count as usize)?;

        let parse_kern = |off| if off > 0 {
            MathKern::parse(&data[off as usize ..]).unwrap().1
        } else {
            MathKern::default()
        };

        let records = records.tuples().map(|(a, b, c, d)| {
            MathKernInfoRecord {
                top_right: parse_kern(a),
                top_left: parse_kern(b),
                bottom_right: parse_kern(c),
                bottom_left: parse_kern(d),
            }
        });
        let entries = coverage.zip(records).collect();

        Ok((i, MathKernInfo { entries }))
    }
}

#[derive(Clone, Debug)]
pub struct ExtendedShapes {
    pub glyphs: HashSet<u16>
}

impl Parser for ExtendedShapes {
    type Output = Self;
    fn parse(data: &[u8]) -> R<Self> {
        let (i, glyphs) = coverage_table(data)?;
        Ok((i, ExtendedShapes { glyphs: glyphs.collect() }))
    }
}
