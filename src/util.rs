macro_rules! tryb {
    (move $exp:expr) => {
        (move || ($exp))()
    };
    ($exp:expr) => {
        (|| ($exp))()
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
        serde_yaml::Value::from($val)
    };
    // == Mapping == //
    (@mapping [$([$key:expr]: $val:expr,)*]) => {
        serde_yaml::Mapping::from_iter([$(
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
        serde_yaml::Sequence::from_iter([$((serde_yaml::Value::from($val)),)*])
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

#[cfg(test)]
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
