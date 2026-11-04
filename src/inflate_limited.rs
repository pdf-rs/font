//! Bounded DEFLATE and zlib decompression.
//!
//! `inflate::inflate_bytes` and `inflate_bytes_zlib` keep growing their output for as long as
//! the compressed input asks them to: a few KiB of a hostile table can expand to gigabytes.
//! The callers here always know how large the result may be — a WOFF table's `origLength`, a
//! gzip member's `ISIZE` — and this module refuses to decode past that.

use inflate::InflateStream;

/// Decompresses a zlib stream (RFC 1950) that may not produce more than `limit` bytes.
pub fn zlib(data: &[u8], limit: usize) -> Result<Vec<u8>, String> {
    bounded(InflateStream::from_zlib(), data, limit)
}

/// Decompresses a raw DEFLATE stream (RFC 1951), the body of a gzip member for example.
pub fn deflate(data: &[u8], limit: usize) -> Result<Vec<u8>, String> {
    bounded(InflateStream::new(), data, limit)
}

/// Feeds `data` to `stream` until it stops producing output, copying each chunk into the
/// result and stopping as soon as the result would pass `limit`. `InflateStream::update`
/// decodes into its own 32 KiB window, so the bound is checked per chunk, and a stream that
/// never ends is cut off by the limit rather than by reading for ever.
fn bounded(mut stream: InflateStream, data: &[u8], limit: usize) -> Result<Vec<u8>, String> {
    let mut out = Vec::with_capacity(limit.min(64 * 1024));
    let mut rest = data;
    loop {
        let (used, decoded) = stream.update(rest)?;
        if used == 0 && decoded.is_empty() {
            return Ok(out);
        }
        if out.len().saturating_add(decoded.len()) > limit {
            return Err(format!("decompressed data is longer than the {} bytes it may have", limit));
        }
        out.extend_from_slice(decoded);
        rest = &rest[used ..];
    }
}

#[cfg(test)]
mod tests {
    use super::{deflate, zlib};

    /// A raw DEFLATE stream of one final, stored block: `len` copies of `byte`.
    fn stored_block(len: u16, byte: u8) -> Vec<u8> {
        let mut out = vec![0x01]; // BFINAL=1, BTYPE=00
        out.extend_from_slice(&len.to_le_bytes());
        out.extend_from_slice(&(!len).to_le_bytes());
        out.extend(std::iter::repeat(byte).take(usize::from(len)));
        out
    }

    fn adler32(bytes: &[u8]) -> u32 {
        let (mut a, mut b) = (1u32, 0u32);
        for &byte in bytes {
            a = (a + u32::from(byte)) % 65521;
            b = (b + a) % 65521;
        }
        (b << 16) | a
    }

    #[test]
    fn a_deflate_stream_stops_at_its_limit() {
        let stream = stored_block(1000, b'a');
        assert_eq!(deflate(&stream, 1000).expect("1000 bytes fit"), vec![b'a'; 1000]);
        assert!(deflate(&stream, 999).is_err(), "one byte over the limit must be refused");
        assert!(deflate(&stream, 0).is_err(), "a limit of zero refuses everything");
    }

    #[test]
    fn a_zlib_stream_stops_at_its_limit_too() {
        let payload = stored_block(1000, b'a');
        let mut data = vec![0x78, 0x01]; // deflate, 32 KiB window, no preset dictionary
        data.extend_from_slice(&payload);
        data.extend_from_slice(&adler32(&vec![b'a'; 1000]).to_be_bytes());

        assert_eq!(zlib(&data, 1000).expect("1000 bytes fit").len(), 1000);
        assert!(zlib(&data, 500).is_err());
    }
}
