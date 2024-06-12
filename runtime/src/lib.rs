use std::io;

#[no_mangle]
pub extern fn getbool() -> bool {
    let mut buffer = String::new();
    let stdin = io::stdin(); // We get `Stdin` here.
    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
    buffer.trim().parse::<bool>().expect("Unable to parse input as bool")
}

#[no_mangle]
pub extern fn getinteger() -> i64 {
    let mut buffer = String::new();
    let stdin = io::stdin(); // We get `Stdin` here.
    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
    buffer.trim().parse::<i64>().expect("Unable to parse input as integer")
}

#[no_mangle]
pub extern fn getfloat() -> f64 {
    let mut buffer = String::new();
    let stdin = io::stdin(); // We get `Stdin` here.
    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
    buffer.trim().parse::<f64>().expect("Unable to parse input as float")
}

//#[no_mangle]
//pub extern fn getstring() -> CString {
//    let mut buffer = String::new();
//    let stdin = io::stdin(); // We get `Stdin` here.
//    stdin.read_line(&mut buffer).expect("Unable to read from stdin");
//    CString::new(buffer).expect("Unable to parse input as string")
//}

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

//#[no_mangle]
//pub extern fn putstring(value: f64) {
//    println!("{value}");
//}

#[no_mangle]
pub extern fn sqrt(value: i64) -> f64 {
    (value as f64).sqrt()
}
