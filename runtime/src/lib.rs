use std::ffi::{CStr, CString};
use std::io;
use std::mem::transmute;
use std::os::raw::c_char;

#[no_mangle]
pub extern fn getbool() -> bool {
    let mut buffer = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
    buffer.trim().parse::<bool>().expect("Unable to parse input as bool")
}

#[no_mangle]
pub extern fn getinteger() -> i64 {
    let mut buffer = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
    buffer.trim().parse::<i64>().expect("Unable to parse input as integer")
}

#[no_mangle]
pub extern fn getfloat() -> f64 {
    let mut buffer = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
    buffer.trim().parse::<f64>().expect("Unable to parse input as float")
}

#[no_mangle]
pub unsafe extern fn getstring() -> *const c_char {
    let mut buffer = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
    let buffer = String::from(buffer.trim());
    unsafe {
        let boxed = Box::new(CString::from_vec_unchecked(buffer.into_bytes()));
        let data: *const CString = transmute(boxed);
        (&*data).as_ptr()
    }
}

#[no_mangle]
pub extern fn putbool(value: bool) {
    println!("{value}");
}

#[no_mangle]
pub extern fn putinteger(value: i64) {
    println!("{value}");
}

#[no_mangle]
pub extern fn putfloat(value: f64) {
    println!("{value}");
}

#[no_mangle]
pub unsafe extern fn putstring(value: *const c_char) {
    match CStr::from_ptr(value).to_str() {
        Ok(val) => println!("{val}"),
        Err(_) => println!("Unable to display string"),
    }
}

#[no_mangle]
pub extern fn sqrt(value: i64) -> f64 {
    (value as f64).sqrt()
}

#[no_mangle]
pub unsafe extern fn strcmp(lhs: *const c_char, rhs: *const c_char) -> bool {
    let lhs = String::from(CStr::from_ptr(lhs).to_str().expect("Unable to read lhs string"));
    let rhs = String::from(CStr::from_ptr(rhs).to_str().expect("Unable to read rhs string"));
    lhs == rhs
}
