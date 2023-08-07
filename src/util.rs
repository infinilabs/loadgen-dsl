#[cfg(test)]
macro_rules! assert_matches {
        ($left:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
            match $left {
                $( $pattern )|+ $( if $guard )? => {}
                ref left => {
                    panic!(
                        "assertion failed: `(left matches right)`\n left: `{}`\nright: `{:?}`",
                        stringify!($($pattern)|+ $(if $guard)?),
                        left,
                    )
                }
            }
        };
    }
