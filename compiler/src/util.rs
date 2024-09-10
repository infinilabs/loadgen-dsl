// MIT License
//
// Copyright (C) INFINI Labs & INFINI LIMITED.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
use crate::{
    ast::Expr,
    error::{Error, Result},
    lexer::Span,
};

macro_rules! assert_matches {
    ($left:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        match $left {
            $( $pattern )|+ $( if $guard )? => {}
            ref left => {
                panic!(
                    "assertion failed: `(left matches right)`\n left: `{:?}`\nright: `{}`",
                    left,
                    stringify!($($pattern)|+ $(if $guard)?),
                )
            }
        }
    };
}

macro_rules! debug_assert_matches {
    ($($args:tt)*) => {
        if cfg!(debug_assertions) {
            assert_matches!($($args)*);
        }
    };
}

macro_rules! yaml {
    ({$($mapping:tt)*}) => {
        yaml!(@mapping [] $($mapping)*)
    };
    ([$($array:tt)*]) => {
        yaml!(@array [] $($array)*)
    };
    ($val:expr) => {
        ::serde_yaml::Value::from($val)
    };
    // == Mapping == //
    (@mapping [$([$key:expr]: $val:expr,)*]) => {
        ::serde_yaml::Mapping::from_iter([$(
            (serde_yaml::Value::from($key), serde_yaml::Value::from($val)),
        )*])
    };
    // Nested mapping
    (@mapping [$($out:tt)*] $key:tt: {$($mapping:tt)*}, $($rest:tt)*) => {
        yaml!(@mapping [$($out)* $key: yaml!(@mapping [] $($mapping)*),] $($rest)*)
    };
    // without trailing comma
    (@mapping [$($out:tt)*] $key:tt: {$($mapping:tt)*}) => {
        yaml!(@mapping [$($out)* $key: yaml!(@mapping [] $($mapping)*),])
    };
    // Nested array
    (@mapping [$($out:tt)*] $key:tt: [$($array:tt)*], $($rest:tt)*) => {
        yaml!(@mapping [$($out)* $key: yaml!(@array [] $($array)*),] $($rest)*)
    };
    // without trailing comma
    (@mapping [$($out:tt)*] $key:tt: [$($array:tt)*]) => {
        yaml!(@mapping [$($out)* $key: yaml!(@array [] $($array)*),])
    };
    // Nested expression
    (@mapping [$($out:tt)*] $key:tt: $val:expr, $($rest:tt)*) => {
        yaml!(@mapping [$($out)* $key: $val,] $($rest)*)
    };
    // without trailing comma
    (@mapping [$($out:tt)*] $key:tt: $val:expr) => {
        yaml!(@mapping [$($out)* $key: $val,])
    };
    // == Array == //
    (@array [$($val:expr,)*]) => {
        ::serde_yaml::Sequence::from_iter([$((serde_yaml::Value::from($val)),)*])
    };
    // Nested mapping
    (@array [$($out:tt)*] {$($mapping:tt)*}, $($rest:tt)*) => {
        yaml!(@array [$($out)* yaml!(@mapping [] $($mapping)*),] $($rest)*)
    };
    // without trailing comma
    (@array [$($out:tt)*] {$($mapping:tt)*}) => {
        yaml!(@array [$($out)* yaml!(@mapping [] $($mapping)*),])
    };
    // Nested array
    (@array [$($out:tt)*] [$($array:tt)*], $($rest:tt)*) => {
        yaml!(@array [$($out)* yaml!(@array [] $($array)*),] $($rest)*)
    };
    // without trailing comma
    (@array [$($out:tt)*] [$($array:tt)*]) => {
        yaml!(@array [$($out)* yaml!(@array [] $($array)*),])
    };
    // Nested expression
    (@array [$($out:tt)*] $val:expr, $($rest:tt)*) => {
        yaml!(@array [$($out)* $val,] $($rest)*)
    };
    // without trailing comma
    (@array [$($out:tt)*] $val:expr) => {
        yaml!(@array [$($out)* $val,])
    };
}

pub(crate) trait UnpackFrom<'a>: Sized {
    fn unpack_next<I>(iter: &mut I) -> Option<Self>
    where
        I: 'a + Iterator<Item = &'a Expr>;

    fn unpack<I>(span: Span, args: I) -> Result<Self>
    where
        I: 'a + IntoIterator<Item = &'a Expr>,
    {
        let mut args = args.into_iter();
        let t =
            Self::unpack_next(&mut args).ok_or_else(|| Error::new(span, "too few arguments"))?;
        if args.next().is_some() {
            Err(Error::new(span, "too many arguments"))
        } else {
            Ok(t)
        }
    }
}

impl<'a> UnpackFrom<'a> for &'a Expr {
    fn unpack_next<I>(iter: &mut I) -> Option<Self>
    where
        I: 'a + Iterator<Item = &'a Expr>,
    {
        iter.next()
    }
}

impl<'a> UnpackFrom<'a> for Option<&'a Expr> {
    fn unpack_next<I>(iter: &mut I) -> Option<Self>
    where
        I: 'a + Iterator<Item = &'a Expr>,
    {
        Some(iter.next())
    }
}

macro_rules! impl_unpack_from_for_tuples {
    ($($T:ident)*) => {
        impl_unpack_from_for_tuples!(@impl [] $($T)*);
    };
    (@impl [$($T:ident)*]) => {};
    (@impl [$($head:tt)*] $T:ident $($rest:tt)*) => {
        impl_unpack_from_for_tuples!(@impl_for $($head)* $T);
        impl_unpack_from_for_tuples!(@impl [$($head)* $T] $($rest)*);
    };
    (@impl_for $($T:ident)*) => {
        impl<'a, $($T,)*> UnpackFrom<'a> for ($($T,)*)
        where
            $($T: UnpackFrom<'a>,)*
        {
            fn unpack_next<I>(args: &mut I) -> Option<Self>
            where
                I: 'a + Iterator<Item = &'a Expr>,
            {
                Some(($($T::unpack_next(args)?,)*))
            }
        }
    };
}

impl_unpack_from_for_tuples!(T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16);
