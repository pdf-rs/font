use nom::{
    number::complete::{be_i16, be_u16},
    sequence::{tuple},
};
use crate::{R, FontError};
use crate::parsers::{*};
use crate::opentype::{tag, Tag};

parser!(int16 : be_i16 -> i16);
parser!(uint16 : be_u16 -> u16);


pub fn parse_base(data: &[u8]) -> Result<(), FontError> {
    let (i, major) = be_u16(data)?;
    let (i, minor) = be_u16(i)?;

    require_eq!(major, 1);
    let (i, horiz_offset) = offset(i)?;
    let (i, vertical_offset) = offset(i)?;

    if let Ok(horiz) = horiz_offset.of(data) {
        let _ = parse_axis_table(horiz)?;
    }
    if let Ok(vert) = vertical_offset.of(data) {
        let _ = parse_axis_table(vert)?;
    }

    Ok(())
}
fn parse_axis_table(data: &[u8]) -> Result<(), FontError> {
    let (i, base_tag_list_offset) = be_u16(data)?;
    let (i, base_script_list_offset) = be_u16(i)?;

    let base_tag_list = parse_base_tag_list(offset!(data, base_tag_list_offset))?;
    let _ = parse_base_script_list(offset!(data, base_script_list_offset), base_tag_list)?;

    Ok(())
}

fn parse_base_tag_list(data: &[u8]) -> Result<impl Array<Item=Tag> + '_, FontError> {
    let (i, base_tag_count) = be_u16(data)?;
    let (i, array) = array::<Tag, _>(base_tag_count)(i)?;
    Ok(array)
}

fn parse_base_script_list<'a>(data: &'a [u8], tags: impl Array<Item=Tag>) -> Result<(), FontError> {
    let mut tags = tags.iter();
    let (i, base_script_count) = be_u16(data)?;
    for (script_tag, offset) in iterator_n(i, tuple((tag, offset)), base_script_count) {
        let (default_baseline_idx, baselines) = parse_base_script_table(offset.of(data)?)?;
        for (base_pos, base_tag) in baselines.zip(tags.by_ref()) {
            //println!("{:?} @ {}", base_tag?, base_pos);
        }
    }
    Ok(())
}
fn parse_base_script_table(data: &[u8]) -> Result<(u16, impl Iterator<Item=Result<i16, FontError>> + '_), FontError> {
    let (i, base_values_offset) = offset(data)?;
    let (i, default_min_max_offset) = offset(data)?;
    let (default_baseline_idx, base_values) = parse_base_values_table(base_values_offset.of(data)?)?;

    Ok((default_baseline_idx, base_values))
}
fn parse_base_values_table(data: &[u8]) -> Result<(u16, impl Iterator<Item=Result<i16, FontError>> + '_), FontError> {
    let (i, default_baseline_idx) = be_u16(data)?;
    let (i, base_coord_count) = be_u16(i)?;
    let array = iterator_n(i, offset, base_coord_count)
        .map(move |off| parse_base_coord(off.of(data)?));
    Ok((default_baseline_idx, array))
}
fn parse_base_coord(i: &[u8]) -> Result<i16, FontError> {
    let (i, _format) = be_u16(i)?;
    let (i, x) = be_i16(i)?;
    Ok(x)
}
