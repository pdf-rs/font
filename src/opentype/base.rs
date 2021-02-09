use nom::{
    number::complete::{be_i16, be_u16},
    sequence::{tuple},
};
use crate::{R};
use crate::parsers::{*};
use crate::opentype::{tag, Tag};

pub fn parse_base(data: &[u8]) -> R<()> {
    let (i, major) = be_u16(data)?;
    let (i, minor) = be_u16(i)?;

    assert_eq!(major, 1);
    let (i, horiz_offset) = offset(i)?;
    let (i, vertical_offset) = offset(i)?;

    let (_, ()) = parse_axis_table(horiz_offset.of(data))?;
    let (_, ()) = parse_axis_table(vertical_offset.of(data))?;

    Ok((i, ()))
}
fn parse_axis_table(data: &[u8]) -> R<()> {
    let (i, base_tag_list_offset) = offset(data)?;
    let (i, base_script_list_offset) = offset(i)?;

    let (_, base_tag_list) = parse_base_tag_list(base_tag_list_offset.of(data))?;
    let (_, ()) = parse_base_script_list(base_script_list_offset.of(data), base_tag_list)?;

    Ok((i, ()))
}

fn parse_base_tag_list(data: &[u8]) -> R<Array<Tag>> {
    let (i, base_tag_count) = be_u16(data)?;
    let (i, array) = array::<Tag, _>(base_tag_count)(i)?;
    Ok((i, array))
}

fn parse_base_script_list<'a>(data: &'a [u8], tags: Array<Tag>) -> R<'a, ()> {
    let (i, base_script_count) = be_u16(data)?;
    for (script_tag, offset) in iterator_n(i, tuple((tag, offset)), base_script_count) {
        let (_, (default_baseline_idx, baselines)) = parse_base_script_table(offset.of(data))?;
        for (base_tag, base_pos) in tags.iter().zip(baselines) {
            println!("{:?} @ {}", base_tag, base_pos);
        }
    }
    Ok((i, ()))
}
fn parse_base_script_table(data: &[u8]) -> R<(u16, impl Iterator<Item=i16> + '_)> {
    let (i, base_values_offset) = offset(data)?;
    let (i, default_min_max_offset) = offset(data)?;
    let (_, base_values) = parse_base_values_table(base_values_offset.of(data))?;

    Ok((i, base_values))
}
fn parse_base_values_table(data: &[u8]) -> R<(u16, impl Iterator<Item=i16> + '_)> {
    let (i, default_baseline_idx) = be_u16(data)?;
    let (i, base_coord_count) = be_u16(i)?;
    let array = iterator_n(i, offset, base_coord_count)
        .map(move |off| parse_base_coord(off.of(data)).unwrap().1);
    Ok((i, (default_baseline_idx, array)))
}
fn parse_base_coord(i: &[u8]) -> R<i16> {
    let (i, _format) = be_u16(i)?;
    let (i, x) = be_i16(i)?;
    Ok((i, x))
}
