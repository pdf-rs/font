use nom::{
    number::complete::{be_u16, be_i16},
};
use crate::{R};

pub struct Os2 {
    pub weight: u16
}

pub fn parse_os2(data: &[u8]) -> R<Os2> {
    let (i, version) = be_u16(data)?;
    let (i, _x_avg_char_width) = be_i16(i)?;
    let (i, weight) = be_u16(i)?;
    Ok((i, Os2 {
        weight
    }))
}
