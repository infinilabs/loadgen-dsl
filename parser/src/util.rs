macro_rules! is_debug {
    () => {
        cfg!(debug_assertions)
    };
}

macro_rules! tryb {
    (move $exp:expr) => {
        (move || ($exp))()
    };
    ($exp:expr) => {
        (|| ($exp))()
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
