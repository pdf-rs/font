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
    let (i, _array_off) = be_u16(i)?;

    let (left_first, left_offsets) = class_table(data, left_class_table_off)?;
    let (right_first, right_offsets) = class_table(data, right_class_table_off)?;

    // Each entry is the offset of that glyph's row (left table) or column (right table),
    // so a glyph's class is its offset. The values only exist for the rows/columns that
    // are actually in the tables — expanding every *glyph* pair instead made a legitimate
    // 5 000-glyph class table allocate 25 M map entries (F45).
    for (n, &offset) in left_offsets.iter().enumerate() {
        table.left_classes.insert(left_first + n as u16, offset);
    }
    for (n, &offset) in right_offsets.iter().enumerate() {
        table.right_classes.insert(right_first + n as u16, offset);
    }

    // One `class_pairs` entry per distinct row/column pair — that is the size of the
    // array the subtable really carries, not `num_left_glyphs * num_right_glyphs`.
    let mut rows = left_offsets;
    rows.sort_unstable();
    rows.dedup();
    let mut columns = right_offsets;
    columns.sort_unstable();
    columns.dedup();
    for &left_off in &rows {
        for &right_off in &columns {
            let address = left_off as usize + right_off as usize;
            let (_, kern) = be_i16(slice!(data, address ..))?;
            table.class_pairs.insert((left_off, right_off), kern);
        }
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
        left_glyphs: u16,
        left_row_offsets: &[u16],
        right_glyphs: u16,
        right_table_at: u16,
        array_at: u16,
        values: &[i16],
    ) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(&be_words(&[4, 8, right_table_at, array_at])); // rowWidth, left, right, array
        // left class table at 8
        data.extend_from_slice(&be_words(&[left_glyphs, left_row_offsets.len() as u16]));
        data.extend_from_slice(&be_words(left_row_offsets));
        // pad to `right_table_at`
        while data.len() < right_table_at as usize {
            data.push(0);
        }
        // right class table: one column offset per glyph
        let column_offsets: Vec<u16> = (0 .. right_glyphs).map(|n| n * 2).collect();
        data.extend_from_slice(&be_words(&[6, right_glyphs]));
        data.extend_from_slice(&be_words(&column_offsets));
        // pad to the array
        while data.len() < array_at as usize {
            data.push(0);
        }
        data.extend(values.iter().flat_map(|v| v.to_be_bytes().to_vec()).collect::<Vec<_>>());
        data
    }

    #[test]
    fn kern_format2_maps_classes_to_values() {
        // Two glyphs on each side (10/11 left, 6/7 right), a 2x2 array: 10/6 -> 0,
        // 10/7 -> -5, 11/6 -> 3, 11/7 -> 7. Rows start at 40 and 44 (rowWidth 4),
        // columns at 0 and 2.
        let data = format2_body(10, &[40, 44], 2, 28, 40, &[0, -5, 3, 7]);
        let mut table = KernTable::default();
        parse_kern_format2(&data, &mut table).expect("format 2 parses");

        // The values live in the class maps, not in a per-glyph-pair map.
        assert!(table.glyph_pairs.is_empty());
        assert_eq!(table.left_classes.len(), 2);
        assert_eq!(table.right_classes.len(), 2);
        assert_eq!(table.class_pairs.len(), 4);
        assert_eq!(table.get(10, 6), Some(0));
        assert_eq!(table.get(10, 7), Some(-5));
        assert_eq!(table.get(11, 6), Some(3));
        assert_eq!(table.get(11, 7), Some(7));
        // A glyph outside the class tables has no kerning.
        assert_eq!(table.get(12, 6), None);
        assert_eq!(table.get(10, 5), None);
    }

    #[test]
    fn kern_format2_memory_follows_the_array_not_the_glyph_count() {
        // F45: 5 000 glyphs sharing one row used to fill `glyph_pairs` with 25 M entries
        // (and reserve them up front). The storage is the array: one row times two
        // columns.
        let rows: Vec<u16> = std::iter::repeat(10020u16).take(5000).collect();
        let data = format2_body(100, &rows, 2, 10012, 10020, &[0, -5]);
        let mut table = KernTable::default();
        parse_kern_format2(&data, &mut table).expect("format 2 parses");

        assert!(table.glyph_pairs.is_empty(), "no per-glyph cross product");
        assert_eq!(table.left_classes.len(), 5000);
        assert_eq!(table.right_classes.len(), 2);
        assert_eq!(table.class_pairs.len(), 2, "one row, two columns");
        assert_eq!(table.get(100, 6), Some(0));
        assert_eq!(table.get(5099, 7), Some(-5));
    }
}
