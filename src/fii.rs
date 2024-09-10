// MIT License
//
// Copyright (C) INFINI Labs & INFINI LIMITED.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

pub fn run_with<T>(f: impl FnOnce() -> miette::Result<T>) -> u64
where
    T: Into<Vec<u8>>,
{
    encode_ptr(
        f().map(T::into)
            .map(Vec::into_boxed_slice)
            .map_err(|e| format!("{e:?}"))
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
