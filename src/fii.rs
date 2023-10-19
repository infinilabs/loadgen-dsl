#[export_name = "allocate"]
extern "C" fn _allocate(size: u32) -> u64 {
    encode_ptr(Ok(vec![0; size as usize].into_boxed_slice()))
}

/// # Safety
///
/// `ptr` must be a valid allocated memory block.
#[export_name = "deallocate"]
unsafe extern "C" fn _deallocate(ptr: u64) {
    drop(decode_ptr(ptr));
}

pub fn run_with<T>(f: impl FnOnce() -> anyhow::Result<T>) -> u64
where
    T: Into<Vec<u8>>,
{
    encode_ptr(
        f().map(T::into)
            .map(Vec::into_boxed_slice)
            .map_err(|e| e.to_string())
            .map_err(String::into_boxed_str),
    )
}

pub fn encode_ptr(bytes: Result<Box<[u8]>, Box<str>>) -> u64 {
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

pub unsafe fn decode_ptr(ptr: u64) -> Result<&'static mut [u8], &'static mut str> {
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
