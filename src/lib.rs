/// Set the global allocator to the WebAssembly optimized one.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[export_name = "allocate"]
pub unsafe extern "C" fn _allocate(size: u32) -> u64 {
    let size = size as usize;
    let ptr = Box::into_raw(Vec::<u8>::with_capacity(size).into_boxed_slice());
    let ptr = std::slice::from_raw_parts_mut(ptr as *mut u8, size);
    encode_ptr(Ok(Box::from_raw(ptr)))
}

/// # Safety
///
/// `ptr` must be a valid allocated memory block.
#[export_name = "deallocate"]
pub unsafe extern "C" fn _deallocate(ptr: u64) {
    drop(decode_ptr(ptr));
}

/// # Safety
///
/// `ptr` must be a valid UTF-8 string.
#[export_name = "compile"]
pub unsafe extern "C" fn _compile(ptr: u64) -> u64 {
    run_with(|| {
        let bytes = decode_ptr(ptr)?;
        let config = std::str::from_utf8_unchecked(bytes);
        compile(config)
    })
}

fn compile(input: &str) -> Result<String, String> {
    loadgen_dsl_compiler::Compiler::new()
        .compile(input)
        .map(|cfg| serde_yaml::to_string(&cfg).expect("should be a valid configuration"))
        .map_err(|e| format!("{e:?}"))
}

fn run_with<T, E>(f: impl FnOnce() -> Result<T, E>) -> u64
where
    T: Into<Vec<u8>>,
    E: Into<String>,
{
    encode_ptr(
        f().map(T::into)
            .map(Vec::into_boxed_slice)
            .map_err(E::into)
            .map_err(String::into_boxed_str),
    )
}

fn encode_ptr(bytes: Result<Box<[u8]>, Box<str>>) -> u64 {
    let errors: u64;
    let data;
    match bytes {
        Ok(t) => {
            errors = 0;
            data = t;
        }
        Err(e) => {
            errors = 1;
            data = e.into_boxed_bytes();
        }
    }
    let size = data.len() as u64;
    assert!(size < u32::MAX as u64);
    let addr = Box::into_raw(data) as *mut u8 as u64;
    (errors << 63) | (size << 32) | addr
}

unsafe fn decode_ptr(ptr: u64) -> Result<&'static mut [u8], &'static mut str> {
    const SIZE_MASK: u32 = u32::MAX >> 1;
    let addr = ptr as u32 as usize;
    let size = (((ptr >> 32) as u32) & SIZE_MASK) as usize;
    let errors = ptr >> 63;
    let bytes = std::slice::from_raw_parts_mut(addr as *mut u8, size);
    if errors == 0 {
        Ok(bytes)
    } else {
        Err(std::str::from_utf8_unchecked_mut(bytes))
    }
}
