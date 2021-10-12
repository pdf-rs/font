use nom::{
    Err,
    error::{VerboseError, VerboseErrorKind},
};


#[derive(Debug)]
pub enum FontError {
    ParseCharString,
    NoSuchSlot,
    Parse(Err<Vec<VerboseErrorKind>>),
    UnknownMagic([u8; 4]),
    Other(String),
    TypeError(&'static str),
    OutOfBounds(&'static str, u32),
    Get(&'static str),
    Require(&'static str),
}

impl<'a> From<Err<VerboseError<&'a [u8]>>> for FontError {
    fn from(e: Err<VerboseError<&'a [u8]>>) -> Self {
        FontError::Parse(e.map(|e| e.errors.into_iter().map(|(_, k)| k).collect()))
    }
}

#[macro_export]
macro_rules! require {
    ($e:expr) => {
        if !$e {
            return Err(crate::FontError::Require(stringify!($e)))
        }
    };
}
#[macro_export]
macro_rules! require_eq {
    ($a:expr, $b:expr) => {
        require!($a == $b);
    };
}
#[macro_export]
macro_rules! error {
    ($($t:tt)*) => {
        return Err(crate::FontError::Other(format!($($t)*)))
    };
}
#[macro_export]
macro_rules! expect {
    ($e:expr, $($t:tt)*) => {
        match $e {
            Some(v) => v,
            None => return Err(crate::FontError::Other(format!($($t)*)))
        }
    };
}
#[macro_export]
macro_rules! slice {
    ($s:expr, $range:expr) => {
        match $s.get($range) {
            Some(v) => v,
            None => return Err(crate::FontError::OutOfBounds(file!(), line!()))
        }
    };
}
#[macro_export]
macro_rules! offset {
    ($s:expr, $start:expr) => (slice!($s, usize::from($start) ..))
}

#[macro_export]
macro_rules! get {
    (mut $var:expr $(, $item:expr)*) => ({
        let v = &mut $var;
        $(
        let v = match v.get_mut($item) {
            Some(v) => v,
            None => return Err(crate::FontError::Get(stringify!($item)))
        }; )*
        v
    });
    ($var:expr $(, $item:expr)*) => ({
        let v = &$var;
        $(
        let v = match v.get($item) {
            Some(v) => v,
            None => return Err(crate::FontError::Get(stringify!($item)))
        }; )*
        v
    });
}
