//! `OS/2` (OT §OS/2): the layout metrics Windows asks for, `sTypo*` included.
//!
//! The table is a fixed 78-byte prefix plus versioned tails: `ulCodePageRange1/2` from
//! version 1, `sxHeight`/`sCapHeight`/`usDefaultChar`/`usBreakChar`/`usMaxContext` from
//! version 2. A table may be shorter than its version announces (producers truncate the
//! tail); that only costs the optional fields, never the font.

use nom::{
    bytes::complete::take,
    number::complete::{be_i16, be_u16, be_u32},
};
use crate::{FontError};

pub struct Os2 {
    pub version: u16,
    pub weight: u16,
    pub width_class: u16,
    pub fs_type: u16,
    pub fs_selection: u16,
    /// `ulUnicodeRange1..4`
    pub unicode_ranges: [u32; 4],
    pub typo_ascender: i16,
    pub typo_descender: i16,
    pub typo_line_gap: i16,
    pub win_ascent: u16,
    pub win_descent: u16,
    /// `sxHeight`, in font units (version ≥ 2 and a table long enough).
    pub x_height: Option<i16>,
    /// `sCapHeight`, in font units (version ≥ 2 and a table long enough).
    pub cap_height: Option<i16>,
}

impl Os2 {
    /// `fsSelection` bit 7, USE_TYPO_METRICS (version ≥ 4): the `sTypo*` values, not
    /// `hhea`, are the line metrics the font wants to be laid out with.
    pub fn use_typo_metrics(&self) -> bool {
        self.version >= 4 && self.fs_selection & 0x0080 != 0
    }

    /// The line metrics this table asks for, if it asks for them.
    pub fn typo_metrics(&self) -> Option<crate::VMetrics> {
        if !self.use_typo_metrics() {
            return None;
        }
        Some(crate::VMetrics {
            ascent: self.typo_ascender as f32,
            descent: self.typo_descender as f32,
            line_gap: self.typo_line_gap as f32,
        })
    }
}

pub fn parse_os2(data: &[u8]) -> Result<Os2, FontError> {
    let (i, version) = be_u16(data)?;
    let (i, _x_avg_char_width) = be_i16(i)?;
    let (i, weight) = be_u16(i)?;
    let (i, width_class) = be_u16(i)?;
    let (i, fs_type) = be_u16(i)?;

    // ySubscriptXSize … yStrikeoutPosition: ten int16, then sFamilyClass.
    let mut i = i;
    for _ in 0 .. 10 {
        let (rest, _) = be_i16(i)?;
        i = rest;
    }
    let (i, _s_family_class) = be_i16(i)?;
    let (i, _panose) = take(10usize)(i)?;

    let mut i = i;
    let mut unicode_ranges = [0u32; 4];
    for range in unicode_ranges.iter_mut() {
        let (rest, value) = be_u32(i)?;
        *range = value;
        i = rest;
    }
    let (i, _ach_vend_id) = take(4usize)(i)?;
    let (i, fs_selection) = be_u16(i)?;
    let (i, _us_first_char_index) = be_u16(i)?;
    let (i, _us_last_char_index) = be_u16(i)?;
    let (i, typo_ascender) = be_i16(i)?;
    let (i, typo_descender) = be_i16(i)?;
    let (i, typo_line_gap) = be_i16(i)?;
    let (i, win_ascent) = be_u16(i)?;
    let (i, win_descent) = be_u16(i)?;

    // The tails are optional in practice, so they are read off the raw slice.
    let mut tail = i;
    if version >= 1 {
        // ulCodePageRange1, ulCodePageRange2. Either all eight bytes are there or the
        // table stops inside them: a partial tail must not shift `sxHeight` onto the
        // code-page bits (audit C7).
        tail = tail.get(8..).unwrap_or_default();
    }
    let (mut x_height, mut cap_height) = (None, None);
    if version >= 2 && tail.len() >= 4 {
        x_height = Some(i16::from_be_bytes([tail[0], tail[1]]));
        cap_height = Some(i16::from_be_bytes([tail[2], tail[3]]));
    }

    Ok(Os2 {
        version,
        weight,
        width_class,
        fs_type,
        fs_selection,
        unicode_ranges,
        typo_ascender,
        typo_descender,
        typo_line_gap,
        win_ascent,
        win_descent,
        x_height,
        cap_height,
    })
}
