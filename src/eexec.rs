use std::io::{self, Read};

pub struct Decoder {
    r: u16,
}
impl Decoder {
    pub fn new(r: u16) -> Decoder {
        Decoder { 
            r
        }
    }
    pub fn file() -> Decoder {
        Decoder::new(55665)
    }
    pub fn charstring() -> Decoder {
        Decoder::new(4330)
    }
    pub fn decode_byte(&mut self, cipher: u8) -> u8 {
        const C1: u16 = 52845;
        const C2: u16 = 22719;
        
        let plain = cipher ^ (self.r >> 8) as u8;
        self.r = (cipher as u16).wrapping_add(self.r).wrapping_mul(C1).wrapping_add(C2);
        
        return plain;
    }
    pub fn decode(&mut self, data: &[u8], skip: usize) -> Vec<u8> {
        let mut vec = Vec::with_capacity(data.len());
        vec.extend(data.iter().map(|&b| self.decode_byte(b)).skip(skip));
        vec
    }
    pub fn decode_inline(&mut self, data: &mut [u8]) {
        for b in data {
            *b = self.decode_byte(*b);
        }
    }
}

struct ExecReader<R: Read> {
    reader: R,
    decoder: Decoder
}
impl<R: Read> Read for ExecReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let len = self.reader.read(buf)?;
        for b in buf[..len].iter_mut() {
            *b = self.decoder.decode_byte(*b);
        }
        Ok(len)
    }
}
