macro_rules! parser {
    ($name:ident : $fun:ident -> $out:ty) => (
        #[allow(non_camel_case_types)]
        pub struct $name;
        impl NomParser for $name {
            type Output = $out;
            fn parse2(data: &[u8])-> crate::ParseResult<Self::Output> {
                $fun(data).map_err(FontError::from)
            }
        }
        impl FixedSize for $name {
            const SIZE: usize = std::mem::size_of::<$out>();
        }
    )
}

macro_rules! parse_field {
    ($start:expr, $input:expr, ?$ptr:ident $parser:ident, $field:expr) => ({
        let (i, offset) = <$ptr as NomParser>::parse2($input)?;
        if offset != 0 {
            let data = &$start[offset as usize ..];
            let value = <$parser as Parser>::parse(data)?;
            (i, value)
        } else {
            (i, Default::default())
        }
    });
    ($start:expr, $input:expr, @ $ptr:ident $parser:ident, $field:expr) => ({
        let (i, offset) = <$ptr as NomParser>::parse2($input)?;
        require!(offset != 0);

        let data = slice!($start, offset as usize ..);
        let value = <$parser as Parser>::parse(data)?;
        (i, value)
    });
    ($start:expr, $input:expr, $parser:ident, $field:expr) => (
        <$parser as NomParser>::parse2($input)?
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
        impl NomParser for $name {
            type Output = $name;
            fn parse2(input: &[u8]) -> crate::ParseResult<$name> {
                let i = input;
                $(
                    let (i, $field) = parse_field!(input, i, $(?$ptr_opt)* $(@$ptr)* $parser, $field);
                )*
                Ok((i, $name { $( $field, )* }))
            }
        }
        impl FixedSize for $name {
            const SIZE: usize = 0 $(+ field_size!($(@$ptr_opt)* $(@$ptr)* $parser) )*;
        }
    );
}