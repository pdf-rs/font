use crate::{R, parsers::*, opentype::gpos::KernTable, FontError};
use nom::{
    number::complete::{be_u8, be_u16, be_i16, be_u32},
    bytes::complete::take,
    sequence::tuple,
};

fn parse_kern_format0<'a>(i: &'a [u8], table: &mut KernTable) -> Result<(), FontError> {
    let (i, n_pairs) = be_u16(i)?;
    let (i, _search_range) = be_u16(i)?;
    let (i, _entry_selector) = be_u16(i)?;
    let (i, _range_shift) = be_u16(i)?;
    
    table.glyph_pairs.reserve(n_pairs as usize);
    for (left, right, kern) in iterator(i, tuple((be_u16, be_u16, be_i16))).take(n_pairs as usize) {
        table.glyph_pairs.insert((left, right), kern);
    }
    Ok(())
}

/// Read one format-2 class subtable: `firstGlyph` plus one byte offset per glyph.
///
/// The offsets are relative to the start of the *subtable data* (`data`), not to this
/// per-glyph table (OT/Microsoft legacy `kern`, format 2).
fn class_table<'a>(data: &'a [u8], off: u16) -> Result<(u16, Vec<u16>), FontError> {
    let (i, first_glyph) = be_u16(slice!(data, off as usize ..))?;
    let (mut i, n_glyphs) = be_u16(i)?;
    let mut offsets = Vec::with_capacity(n_glyphs as usize);
    for _ in 0 .. n_glyphs {
        let (rest, value) = be_u16(i)?;
        offsets.push(value);
        i = rest;
    }
    Ok((first_glyph, offsets))
}

fn parse_kern_format2<'a>(data: &'a [u8], table: &mut KernTable) -> Result<(), FontError> {
    let (i, _row_width) = be_u16(data)?;
    let (i, left_class_table_off) = be_u16(i)?;
    let (i, right_class_table_off) = be_u16(i)?;
    let (i, array_off) = be_u16(i)?;
    let array_off = array_off as usize;

    let (left_first, left_offsets) = class_table(data, left_class_table_off)?;
    let (right_first, right_offsets) = class_table(data, right_class_table_off)?;

    // MS §kern format 2 (`KernSubtableFormat2`): a class value is the row (left) or column
    // (right) already multiplied by `rowWidth` resp. by the size of one value, and the
    // kerning value lives at `kerningArrayOffset + leftClass + rightClass`. Class 0 is the
    // "does not kern" row/column the spec says always holds zeros: keeping it as a class
    // made `array + 0 + column` read the subtable header as kerning values (audit C5).
    for (n, &class) in left_offsets.iter().enumerate() {
        // `firstGlyph + n` can overflow a `u16` in a hostile table, and the glyph is past
        // the class range anyway.
        if let (true, Some(gid)) = (class != 0, left_first.checked_add(n as u16)) {
            table.left_classes.insert(gid, class);
        }
    }
    for (n, &class) in right_offsets.iter().enumerate() {
        if let (true, Some(gid)) = (class != 0, right_first.checked_add(n as u16)) {
            table.right_classes.insert(gid, class);
        }
    }

    // One `class_pairs` entry per distinct row/column pair — that is the size of the
    // array the subtable really carries, not `num_left_glyphs * num_right_glyphs` (F45).
    let mut rows = left_offsets;
    rows.retain(|&class| class != 0);
    rows.sort_unstable();
    rows.dedup();
    let mut columns = right_offsets;
    columns.retain(|&class| class != 0);
    columns.sort_unstable();
    columns.dedup();
    let mut skipped = 0usize;
    for &left_class in &rows {
        for &right_class in &columns {
            // The cross product invents combinations the font may never have written, so an
            // address past the end of the subtable skips that cell instead of failing the
            // whole table (audit C5).
            let address = array_off + left_class as usize + right_class as usize;
            let kern = match data.get(address ..) {
                Some(rest) => {
                    let parsed: R<i16> = be_i16(rest);
                    match parsed {
                        Ok((_, kern)) => kern,
                        Err(_) => {
                            skipped += 1;
                            continue;
                        }
                    }
                }
                None => {
                    skipped += 1;
                    continue;
                }
            };
            table.class_pairs.insert((left_class, right_class), kern);
        }
    }
    if skipped > 0 {
        warn!("kern: format 2 class array is truncated; {} cell(s) skipped", skipped);
    }
    Ok(())
}

pub fn parse_kern(input: &[u8]) -> Result<KernTable, FontError> {
    let (_i, version) = be_u16(input)?;
    debug!("kern table version {}", version);
    match version {
        0 => parse_kern_ms(input),
        1 => parse_kern_apple(input),
        _ => error!("stahp!")
    }
}

pub fn parse_kern_apple(i: &[u8]) -> Result<KernTable, FontError> {
    let (i, version) = be_u32(i)?;
    require_eq!(version, 0x00010000);
    
    let mut table = KernTable::default();
    let (mut input, n_tables) = be_u32(i)?;
    for _ in 0 .. n_tables {
        let (i, length) = be_u32(i)?;
        let (i, coverage) = be_u8(i)?;
        let (i, format) = be_u8(i)?;
        let (i, tuple_index) = be_u16(i)?;

        let (new_input, data) = take(length as usize)(i)?;
        // Apple §kern coverage: bit 7 (0x80) selects vertical; format 0 only carries
        // horizontal pairs here.
        if coverage & 0x80 != 0 {
            warn!("kern: skipping vertical Apple subtable format={}", format);
            input = new_input;
            continue;
        }
        match format {
            0 => apple_kern_0(data, &mut table)?,
            f => warn!("kern: Apple subtable format {} is not implemented", f),
        }
        input = new_input;
    }
    Ok(table)
}

fn apple_kern_0(i: &[u8], table: &mut KernTable) -> Result<(), FontError> {
    let (i, n_pairs) = be_u16(i)?;
    let (i, _) = be_u16(i)?;
    let (i, _) = be_u16(i)?;
    let (i, _) = be_u16(i)?;

    for _ in 0 .. n_pairs {
        let (i, left) = be_u16(i)?;
        let (i, right) = be_u16(i)?;
        let (i, value) = be_i16(i)?;
        table.glyph_pairs.insert((left, right), value);
    }

    Ok(())
}

pub fn parse_kern_ms(i: &[u8]) -> Result<KernTable, FontError> {
    let (i, version) = be_u16(i)?;
    require_eq!(version, 0);
    
    let mut table = KernTable::default();
    let (mut i, n_tables) = be_u16(i)?;
    for _ in 0 .. n_tables {
        let (_version, length, format, coverage) = parse(&mut i, tuple((be_u16, be_u16, be_u8, be_u8)))?;
        debug!("format={}, coverage={:02x}", format, coverage);
        // OT/MS `kern`: `length` covers the whole subtable including this 6-byte header.
        require!(length >= 6);
        let data = parse(&mut i, take(length as usize - 6))?;
        // Coverage is a bit field, not an enumeration: bit 0 horizontal, bit 1 minimum,
        // bit 2 cross-stream, bit 3 override. Minimum and override do not change the
        // interpretation of the pair data, so only vertical/cross-stream is unusable here.
        let horizontal = coverage & 0x01 != 0;
        let cross_stream = coverage & 0x04 != 0;
        if !horizontal || cross_stream {
            warn!("kern: skipping subtable format={} coverage={:02x} (vertical/cross-stream)", format, coverage);
            continue;
        }
        match format {
            0 => parse_kern_format0(data, &mut table)?,
            2 => parse_kern_format2(data, &mut table)?,
            f => warn!("kern: subtable format {} is not implemented", f),
        }
    }
    Ok(table)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opentype::gpos::KernTable;

    fn be_words(words: &[u16]) -> Vec<u8> {
        words.iter().flat_map(|w| w.to_be_bytes().to_vec()).collect()
    }

    /// A format-2 subtable body (the bytes after the 6-byte subtable header).
    ///
    /// Laid out as: 4-word header, left class table at 8, right class table at
    /// `right_table_at`, array at `array_at`. The left table maps `first_left ..` to the
    /// absolute byte offset of its row, the right table maps glyphs to the column offset
    /// *inside* the row — so the value of a pair is at `left_offset + right_offset`.
    fn format2_body(
        row_width: u16,
        left_first: u16,
        left_classes: &[u16],
        right_first: u16,
        right_classes: &[u16],
        right_table_at: u16,
        array_at: u16,
        values: &[i16],
    ) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(&be_words(&[row_width, 8, right_table_at, array_at]));
        // left class table at 8: the class values are premultiplied row offsets
        data.extend_from_slice(&be_words(&[left_first, left_classes.len() as u16]));
        data.extend_from_slice(&be_words(left_classes));
        // pad to `right_table_at`
        while data.len() < right_table_at as usize {
            data.push(0);
        }
        // right class table: the class values are premultiplied column offsets
        data.extend_from_slice(&be_words(&[right_first, right_classes.len() as u16]));
        data.extend_from_slice(&be_words(right_classes));
        // pad to the array
        while data.len() < array_at as usize {
            data.push(0);
        }
        data.extend(values.iter().flat_map(|v| v.to_be_bytes().to_vec()).collect::<Vec<_>>());
        data
    }

    #[test]
    fn kern_format2_maps_classes_to_values() {
        // Two left classes (rows 1 and 2 of a three-value, six-byte row) and two right
        // classes (columns 1 and 2). Row 0 and column 0 are the "glyph does not kern"
        // ones the spec reserves. The array starts at 46 and holds three rows of three.
        let values = [
            0, 0, 0,      // row 0: glyphs that do not kern
            0, -5, 3,     // row 1 (left class 6)
            0, 7, -20,    // row 2 (left class 12)
        ];
        let data = format2_body(6, 10, &[6, 12], 6, &[2, 4], 20, 46, &values);
        let mut table = KernTable::default();
        parse_kern_format2(&data, &mut table).expect("format 2 parses");

        // The values live in the class maps, not in a per-glyph-pair map.
        assert!(table.glyph_pairs.is_empty());
        assert_eq!(table.left_classes.len(), 2);
        assert_eq!(table.right_classes.len(), 2);
        assert_eq!(table.class_pairs.len(), 4);
        assert_eq!(table.get(10, 6), Some(-5));
        assert_eq!(table.get(10, 7), Some(3));
        assert_eq!(table.get(11, 6), Some(7));
        assert_eq!(table.get(11, 7), Some(-20));
        // A glyph outside the class tables has no kerning.
        assert_eq!(table.get(12, 6), None);
        assert_eq!(table.get(10, 5), None);
    }

    #[test]
    fn kern_format2_memory_follows_the_array_not_the_glyph_count() {
        // F45: 5 000 glyphs sharing one row used to fill `glyph_pairs` with 25 M entries
        // (and reserve them up front). The storage is the array: one row times two
        // columns.
        let rows: Vec<u16> = std::iter::repeat(6u16).take(5000).collect();
        let data = format2_body(6, 100, &rows, 6, &[2, 4], 10012, 10020, &[0, 0, 0, 0, -5, 9]);
        let mut table = KernTable::default();
        parse_kern_format2(&data, &mut table).expect("format 2 parses");

        assert!(table.glyph_pairs.is_empty(), "no per-glyph cross product");
        assert_eq!(table.left_classes.len(), 5000);
        assert_eq!(table.right_classes.len(), 2);
        assert_eq!(table.class_pairs.len(), 2, "one row, two columns");
        assert_eq!(table.get(100, 6), Some(-5));
        assert_eq!(table.get(5099, 7), Some(9));
    }

    #[test]
    fn kern_format2_skips_the_do_not_kern_class() {
        // Audit C5: class 0 is the "glyph does not kern" row/column. Keeping it as a class
        // made `array + 0 + column` read the subtable header — rowWidth and the table
        // offsets — as kerning values.
        let data = format2_body(6, 10, &[0, 6], 6, &[0, 2], 20, 46, &[0, 0, 0, 0, -5, 3]);
        let mut table = KernTable::default();
        parse_kern_format2(&data, &mut table).expect("format 2 parses");

        assert_eq!(table.left_classes.get(&10), None, "class 0 is not a class");
        assert_eq!(table.left_classes.get(&11), Some(&6));
        assert_eq!(table.right_classes.get(&6), None);
        assert_eq!(table.right_classes.get(&7), Some(&2));
        assert_eq!(table.class_pairs.len(), 1, "only the one real row/column");
        assert_eq!(table.get(10, 7), None, "the glyph that does not kern");
        assert_eq!(table.get(11, 7), Some(-5));
    }

    #[test]
    fn kern_format2_skips_unreadable_array_addresses() {
        // The row/column cross product invents pairs the font never wrote; an address past
        // the end of the subtable must cost that cell, not the whole table (audit C5).
        let values = [0, 0, 0, 0, -5, 3];
        let data = format2_body(6, 10, &[6, 12], 6, &[2, 4], 20, 46, &values);
        let mut table = KernTable::default();
        parse_kern_format2(&data, &mut table).expect("format 2 parses");

        assert_eq!(table.get(10, 6), Some(-5));
        assert_eq!(table.get(10, 7), Some(3));
        assert_eq!(table.get(11, 6), None, "row 2 is past the end");
        assert_eq!(table.get(11, 7), None);
    }

    #[test]
    fn kern_format2_class_range_does_not_overflow_a_glyph_id() {
        // `firstGlyph + n` overflowed a u16 and panicked in a debug build (audit C5).
        let data = format2_body(6, 65534, &[6, 6, 6], 6, &[2], 20, 46, &[0, 0, 0, 0, -5, 3]);
        let mut table = KernTable::default();
        parse_kern_format2(&data, &mut table).expect("format 2 parses");

        assert_eq!(table.left_classes.get(&65535), Some(&6));
        assert_eq!(table.left_classes.len(), 2, "the id that does not fit is dropped");
        assert_eq!(table.get(65535, 6), Some(-5));
    }
}
