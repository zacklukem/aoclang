#![feature(c_variadic)]
#![feature(duration_millis_float)]
#![allow(arithmetic_overflow)]

mod gc;
mod value;

use crate::gc::GcHeader;
use gc::{Gc, Markable};
use std::ptr::NonNull;
use std::{
    char,
    collections::HashMap,
    ffi::{c_char, CStr},
    fs,
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Add, BitAnd, BitOr, BitXor, Deref, Mul, Rem, Sub},
    panic, slice,
};
use value::{
    cons, none, some, Closure, FnPtr, HFloat, Ref, Symbol, Value, NONE_SYMBOL_PTR_TARGET,
    SOME_SYMBOL_PTR_TARGET,
};

pub struct Runtime<'a> {
    symbol_table: HashMap<String, &'static &'static str>,
    frames: Vec<&'a [Value]>,
    gc_root: Option<NonNull<GcHeader<usize>>>,
    rust_frame: Vec<Box<dyn Markable>>,
}

impl Runtime<'_> {
    pub fn guard(&mut self) {
        self.rust_frame.clear();
    }

    pub fn new() -> Self {
        let mut symbol_table = HashMap::new();
        symbol_table.insert("none".to_string(), &NONE_SYMBOL_PTR_TARGET);
        symbol_table.insert("some".to_string(), &SOME_SYMBOL_PTR_TARGET);

        Self {
            symbol_table,
            frames: Vec::new(),
            gc_root: None,
            rust_frame: Vec::new(),
        }
    }
}

fn throw(_runtime: &Runtime, msg: &str) -> ! {
    panic!("{}", msg);
}

macro_rules! throw {
    ($runtime: expr, $($arg:tt)*) => {{
        let msg = format!($($arg)*);
        throw($runtime, &msg);
    }};
}

// case PrintLine, Assert, AssertEq, HashCode
#[no_mangle]
pub unsafe extern "C" fn _al_print_line(_runtime: &mut Runtime, line: Value) -> Value {
    match line {
        Value::Str(line) => {
            println!("{}", *line);
        }
        line => {
            println!("{}", line);
        }
    }

    none()
}

#[no_mangle]
pub unsafe extern "C" fn _al_assert(runtime: &mut Runtime, val: Value) -> Value {
    if let Value::Boolean(false) = val {
        throw(runtime, "Assertion failed");
    } else {
        none()
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_assert_eq(runtime: &mut Runtime, lhs: Value, rhs: Value) -> Value {
    if lhs != rhs {
        throw!(runtime, "Assertion failed: {lhs} != {rhs}");
    } else {
        none()
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_hash_code(_runtime: &mut Runtime, val: Value) -> Value {
    let mut h = DefaultHasher::new();
    val.hash(&mut h);
    let res = (h.finish() & 0xffff_ffff) as i64;
    Value::Int(res)
}

// case Ref, Store, Load
#[no_mangle]
pub unsafe extern "C" fn _al_ref(runtime: &mut Runtime, val: Value) -> Value {
    runtime.guard();
    Value::Ref(Ref(Gc::new(runtime, val)))
}

#[no_mangle]
pub unsafe extern "C" fn _al_store(runtime: &mut Runtime, target: Value, val: Value) -> Value {
    if let Value::Ref(Ref(mut target)) = target {
        *target = val;
    } else {
        throw(runtime, "Expected reference");
    }

    none()
}

#[no_mangle]
pub unsafe extern "C" fn _al_load(runtime: &mut Runtime, source: Value) -> Value {
    if let Value::Ref(Ref(source)) = source {
        *source
    } else {
        throw(runtime, "Expected reference");
    }
}

// case Eq, Neq, Lt, Gt, Le, Ge, Add, Sub, Mul, DivInt, DivFloat, Mod, Pow, BAnd, BOr, Xor, Shl, Shr,
//   Concat, Not

#[no_mangle]
pub unsafe extern "C" fn _al_eq(_runtime: &mut Runtime, a: Value, b: Value) -> Value {
    Value::Boolean(a == b)
}

#[no_mangle]
pub unsafe extern "C" fn _al_neq(_runtime: &mut Runtime, a: Value, b: Value) -> Value {
    Value::Boolean(a != b)
}

macro_rules! comparison_op {
    ($runtime: expr, $a: expr, $b: expr, $op: tt) => {
        match ($a, $b) {
            (Value::Int(x), Value::Int(y)) => Value::Boolean(x $op y),
            (Value::Float(HFloat(x)), Value::Float(HFloat(y))) => Value::Boolean(x $op y),
            (Value::Int(x), Value::Float(HFloat(y))) => Value::Boolean((x as f64) $op y),
            (Value::Float(HFloat(x)), Value::Int(y)) => Value::Boolean(x $op y as f64),
            (Value::Str(_x), Value::Str(_y)) => unimplemented!(),
            _ => throw($runtime, "Invalid types"),
        }
    };
}

#[no_mangle]
pub unsafe extern "C" fn _al_lt(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    comparison_op!(runtime, a, b, <)
}

#[no_mangle]
pub unsafe extern "C" fn _al_gt(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    comparison_op!(runtime, a, b, >)
}

#[no_mangle]
pub unsafe extern "C" fn _al_le(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    comparison_op!(runtime, a, b, <=)
}

#[no_mangle]
pub unsafe extern "C" fn _al_ge(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    comparison_op!(runtime, a, b, >=)
}

macro_rules! math_op {
    ($runtime: expr, $a: expr, $b: expr, $iop: ident, $fop: ident) => {
        match ($a, $b) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x.$iop(y)),
            (Value::Float(HFloat(x)), Value::Float(HFloat(y))) => Value::Float(HFloat(x.$fop(y))),
            (Value::Int(x), Value::Float(HFloat(y))) => Value::Float(HFloat((x as f64).$fop(y))),
            (Value::Float(HFloat(x)), Value::Int(y)) => Value::Float(HFloat(x.$fop(y as f64))),
            _ => throw($runtime, "Invalid types"),
        }
    };
}

#[no_mangle]
pub unsafe extern "C" fn _al_add(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    math_op!(runtime, a, b, wrapping_add, add)
}

#[no_mangle]
pub unsafe extern "C" fn _al_sub(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    math_op!(runtime, a, b, wrapping_sub, sub)
}

#[no_mangle]
pub unsafe extern "C" fn _al_mul(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    math_op!(runtime, a, b, wrapping_mul, mul)
}

#[no_mangle]
pub unsafe extern "C" fn _al_div_int(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
        _ => throw(runtime, "Invalid types"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_div_float(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Float(HFloat((x as f64) / (y as f64))),
        (Value::Float(HFloat(x)), Value::Float(HFloat(y))) => Value::Float(HFloat(x / y)),
        (Value::Int(x), Value::Float(HFloat(y))) => Value::Float(HFloat((x as f64) / y)),
        (Value::Float(HFloat(x)), Value::Int(y)) => Value::Float(HFloat(x / (y as f64))),
        _ => throw(runtime, "Invalid types"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_mod(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    math_op!(runtime, a, b, wrapping_rem, rem)
}

#[no_mangle]
pub unsafe extern "C" fn _al_pow(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Float(HFloat((x as f64).powi(y as i32))),
        (Value::Float(HFloat(x)), Value::Float(HFloat(y))) => Value::Float(HFloat(x.powf(y))),
        (Value::Int(x), Value::Float(HFloat(y))) => Value::Float(HFloat((x as f64).powf(y))),
        (Value::Float(HFloat(x)), Value::Int(y)) => Value::Float(HFloat(x.powi(y as i32))),
        _ => throw(runtime, "Invalid types"),
    }
}

macro_rules! bit_op {
    ($runtime: expr, $a: expr, $b: expr, $op: tt) => {
        match ($a, $b) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x.$op(y)),
            _ => throw($runtime, "Invalid types"),
        }
    };
}

#[no_mangle]
pub unsafe extern "C" fn _al_b_and(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    bit_op!(runtime, a, b, bitand)
}

#[no_mangle]
pub unsafe extern "C" fn _al_b_or(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    bit_op!(runtime, a, b, bitor)
}

#[no_mangle]
pub unsafe extern "C" fn _al_xor(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    bit_op!(runtime, a, b, bitxor)
}

#[no_mangle]
pub unsafe extern "C" fn _al_shl(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x.wrapping_shl(y as u32)),
        _ => throw(runtime, "Invalid types"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_shr(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x.wrapping_shr(y as u32)),
        _ => throw(runtime, "Invalid types"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_concat(runtime: &mut Runtime, a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Str(a), Value::Str(b)) => {
            Value::Str(Gc::new(runtime, a.as_ref().clone().add(b.as_ref())))
        }
        (Value::Str(a), v) => Value::Str(Gc::new(runtime, format!("{}{}", a.as_ref(), v))),
        (v, Value::Str(a)) => Value::Str(Gc::new(runtime, format!("{}{}", v, a.as_ref()))),
        (a, b) => Value::Str(Gc::new(runtime, format!("{a}{b}"))),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_not(_runtime: &mut Runtime, a: Value) -> Value {
    match a {
        Value::Boolean(b) => Value::Boolean(!b),
        _ => throw(_runtime, "Expected boolean"),
    }
}

// case ListNew, ListHead, ListTail, ListIs, ListIsEmpty, ListToTuple, ListCons
#[no_mangle]
pub unsafe extern "C" fn _al_list_new(
    runtime: &mut Runtime,
    nargs: usize,
    args: *const Value,
) -> Value {
    runtime.guard();
    let mut res = None;

    let args = unsafe { slice::from_raw_parts(args, nargs) };

    for arg in args.iter().rev().copied() {
        res = cons(runtime, arg, res);
    }

    Value::List(res)
}

#[no_mangle]
pub unsafe extern "C" fn _al_list_head(runtime: &mut Runtime, list: Value) -> Value {
    runtime.guard();
    match list {
        Value::List(Some(list)) => Value::Tuple(Gc::new(runtime, vec![some(), list.0])),
        Value::List(None) => none(),
        _ => throw(runtime, "Expected list"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_list_tail(runtime: &mut Runtime, list: Value) -> Value {
    match list {
        Value::List(Some(list)) => Value::List(list.1),
        Value::List(None) => none(),
        _ => throw(runtime, "Expected list"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_list_is(_runtime: &mut Runtime, list: Value) -> Value {
    match list {
        Value::List(_) => Value::Boolean(true),
        _ => Value::Boolean(false),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_list_is_empty(_runtime: &mut Runtime, list: Value) -> Value {
    Value::Boolean(list == Value::List(None))
}

#[no_mangle]
pub unsafe extern "C" fn _al_list_to_tuple(runtime: &mut Runtime, list: Value) -> Value {
    runtime.guard();
    let Value::List(mut list) = list else {
        throw(runtime, "Expected list");
    };

    let mut res = Vec::new();

    while let Some(v) = list {
        res.push(v.0);
        list = v.1;
    }

    Value::Tuple(Gc::new(runtime, res))
}

#[no_mangle]
pub unsafe extern "C" fn _al_list_cons(runtime: &mut Runtime, head: Value, tail: Value) -> Value {
    runtime.guard();
    let Value::List(tail) = tail else {
        throw(runtime, "Expected list");
    };

    Value::List(cons(runtime, head, tail))
}

// case TupleNew, TupleGet, TupleIs, TupleSize, TuplePut
#[no_mangle]
pub unsafe extern "C" fn _al_tuple_new(
    runtime: &mut Runtime,
    nargs: usize,
    args: *const Value,
) -> Value {
    runtime.guard();
    let mut res = Vec::with_capacity(nargs);

    for i in 0..nargs {
        let arg = unsafe { args.add(i).read() };
        res.push(arg);
    }

    Value::Tuple(Gc::new(runtime, res))
}

#[no_mangle]
pub unsafe extern "C" fn _al_tuple_get(runtime: &mut Runtime, tuple: Value, index: Value) -> Value {
    let Value::Tuple(tuple) = tuple else {
        throw!(runtime, "Expected tuple: {}", tuple);
    };

    let Value::Int(index) = index else {
        throw(runtime, "Expected integer");
    };

    tuple[index as usize]
}

#[no_mangle]
pub unsafe extern "C" fn _al_tuple_is(_runtime: &mut Runtime, tuple: Value) -> Value {
    match tuple {
        Value::Tuple(_) => Value::Boolean(true),
        _ => Value::Boolean(false),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_tuple_size(runtime: &mut Runtime, tuple: Value) -> Value {
    let Value::Tuple(tuple) = tuple else {
        throw(runtime, "Expected tuple");
    };

    Value::Int(tuple.len() as i64)
}

#[no_mangle]
pub unsafe extern "C" fn _al_tuple_put(
    runtime: &mut Runtime,
    tuple: Value,
    index: Value,
    val: Value,
) -> Value {
    runtime.guard();
    let Value::Tuple(tuple) = tuple else {
        throw(runtime, "Expected tuple");
    };

    let Value::Int(index) = index else {
        throw(runtime, "Expected integer");
    };

    let mut updated = tuple.deref().clone();
    updated[index as usize] = val;

    Value::Tuple(Gc::new(runtime, updated))
}

// case StringChars, StringSize, StringFromChars, StringSplit, StringFromInt
#[no_mangle]
pub unsafe extern "C" fn _al_string_chars(runtime: &mut Runtime, string: Value) -> Value {
    runtime.guard();
    let Value::Str(string) = string else {
        throw(runtime, "Expected string");
    };

    let mut res = None;

    for ch in string.chars().rev() {
        res = cons(runtime, Value::Int(ch as i64), res);
    }

    Value::List(res)
}

#[no_mangle]
pub unsafe extern "C" fn _al_string_from_chars(runtime: &mut Runtime, mut chars: Value) -> Value {
    runtime.guard();
    let mut res = String::new();

    while let Value::List(Some(list)) = chars {
        let Value::Int(ch) = list.0 else {
            throw(runtime, "Expected integer");
        };

        let Some(ch) = char::from_u32(ch as u32) else {
            throw(runtime, "Invalid character");
        };

        res.push(ch);

        chars = Value::List(list.1);
    }

    Value::Str(Gc::new(runtime, res))
}

#[no_mangle]
pub unsafe extern "C" fn _al_string_size(runtime: &mut Runtime, string: Value) -> Value {
    let Value::Str(string) = string else {
        throw(runtime, "Expected string");
    };

    Value::Int(string.len() as i64)
}

#[no_mangle]
pub unsafe extern "C" fn _al_string_split(
    runtime: &mut Runtime,
    string: Value,
    sep: Value,
) -> Value {
    runtime.guard();
    let Value::Str(string) = string else {
        throw(runtime, "Expected string");
    };

    let Value::Str(sep) = sep else {
        throw(runtime, "Expected string");
    };

    let split = string.split(sep.as_ref()).collect::<Vec<_>>();

    let mut res = None;

    for arg in split.iter().rev() {
        let val = Value::Str(Gc::new(runtime, arg.to_string()));
        res = cons(runtime, val, res);
    }

    Value::List(res)
}

#[no_mangle]
pub unsafe extern "C" fn _al_string_from_int(runtime: &mut Runtime, int: Value) -> Value {
    runtime.guard();
    let Value::Int(int) = int else {
        throw(runtime, "Expected integer");
    };

    Value::Str(Gc::new(runtime, int.to_string()))
}

// case IntIs, IntFromString
#[no_mangle]
pub unsafe extern "C" fn _al_int_is(_runtime: &mut Runtime, int: Value) -> Value {
    match int {
        Value::Int(_) => Value::Boolean(true),
        _ => Value::Boolean(false),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_int_from_string(runtime: &mut Runtime, string: Value) -> Value {
    let Value::Str(string) = string else {
        throw(runtime, "Expected string");
    };

    match string.parse() {
        Ok(int) => Value::Int(int),
        Err(_) => throw(runtime, "Invalid integer"),
    }
}

// case FileReadString
#[no_mangle]
pub unsafe extern "C" fn _al_file_read_string(runtime: &mut Runtime, path: Value) -> Value {
    runtime.guard();
    let Value::Str(path) = path else {
        throw(runtime, "Expected string");
    };

    let Ok(out) = fs::read_to_string(path.as_ref()) else {
        throw(runtime, "Failed to read file");
    };

    Value::Str(Gc::new(runtime, out))
}

// case ClosureNew
#[no_mangle]
pub unsafe extern "C" fn _al_closure_new(
    runtime: &mut Runtime,
    fn_ptr: FnPtr,
    nargs: usize,
    args: *const Value,
) -> Value {
    runtime.guard();
    let env = _al_tuple_new(runtime, nargs, args);
    let closure = Gc::new(runtime, Closure { fn_ptr, env });
    Value::Closure(closure)
}

#[no_mangle]
pub unsafe extern "C" fn _al_closure_new_empty(_runtime: &mut Runtime, fn_ptr: FnPtr) -> Value {
    Value::Fn(fn_ptr)
}

#[no_mangle]
pub unsafe extern "C" fn _al_true_new(_runtime: &mut Runtime) -> Value {
    Value::Boolean(true)
}

#[no_mangle]
pub unsafe extern "C" fn _al_false_new(_runtime: &mut Runtime) -> Value {
    Value::Boolean(false)
}

#[no_mangle]
pub unsafe extern "C" fn _al_string_new(runtime: &mut Runtime, val: *const c_char) -> Value {
    runtime.guard();
    let val = unsafe { CStr::from_ptr(val).to_str().unwrap().to_owned() };
    Value::Str(Gc::new(runtime, val))
}

#[no_mangle]
pub unsafe extern "C" fn _al_int_new(_runtime: &mut Runtime, val: i64) -> Value {
    Value::Int(val)
}

#[no_mangle]
pub unsafe extern "C" fn _al_float_new(_runtime: &mut Runtime, val: f64) -> Value {
    Value::Float(HFloat(val))
}

#[no_mangle]
pub unsafe extern "C" fn _al_sym_new(runtime: &mut Runtime, val: *const c_char) -> Value {
    let val = unsafe { CStr::from_ptr(val).to_str().unwrap() };

    if let Some(v) = runtime.symbol_table.get(val).copied() {
        Value::Symbol(Symbol(v))
    } else {
        let v = Box::leak(Box::new(String::leak(val.to_string()) as &'static str));
        runtime.symbol_table.insert(val.to_string(), v);
        Value::Symbol(Symbol(v))
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_get_bool(runtime: &mut Runtime, val: Value) -> bool {
    match val {
        Value::Boolean(b) => b,
        _ => throw(runtime, "Expected boolean"),
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_call(
    runtime: &mut Runtime,
    fn_ptr: Value,
    nargs: usize,
    args: *const Value,
) -> Value {
    let args = unsafe { slice::from_raw_parts(args, nargs) };

    unsafe {
        match fn_ptr {
            Value::Fn(fn_ptr) => {
                macro_rules! raw_call {
                    ($f: expr, $($n: expr),*) => {
                        ($f)(runtime, $(args[$n]),*)
                    };
                }

                match nargs {
                    0 => raw_call!(fn_ptr.f0,),
                    1 => raw_call!(fn_ptr.f1, 0),
                    2 => raw_call!(fn_ptr.f2, 0, 1),
                    3 => raw_call!(fn_ptr.f3, 0, 1, 2),
                    4 => raw_call!(fn_ptr.f4, 0, 1, 2, 3),
                    5 => raw_call!(fn_ptr.f5, 0, 1, 2, 3, 4),
                    6 => raw_call!(fn_ptr.f6, 0, 1, 2, 3, 4, 5),
                    7 => raw_call!(fn_ptr.f7, 0, 1, 2, 3, 4, 5, 6),
                    8 => raw_call!(fn_ptr.f8, 0, 1, 2, 3, 4, 5, 6, 7),
                    9 => raw_call!(fn_ptr.f9, 0, 1, 2, 3, 4, 5, 6, 7, 8),
                    10 => raw_call!(fn_ptr.f10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                    11 => raw_call!(fn_ptr.f11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    12 => raw_call!(fn_ptr.f12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                    13 => raw_call!(fn_ptr.f13, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                    14 => raw_call!(fn_ptr.f14, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                    15 => raw_call!(fn_ptr.f15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                    _ => throw(runtime, "Too many arguments"),
                }
            }
            Value::Closure(fn_ptr) => {
                macro_rules! raw_call {
                    ($f: expr, $($n: expr),*) => {
                        ($f)(runtime, fn_ptr.env, $(args[$n]),*)
                    };
                }

                match nargs {
                    0 => raw_call!(fn_ptr.fn_ptr.f1,),
                    1 => raw_call!(fn_ptr.fn_ptr.f2, 0),
                    2 => raw_call!(fn_ptr.fn_ptr.f3, 0, 1),
                    3 => raw_call!(fn_ptr.fn_ptr.f4, 0, 1, 2),
                    4 => raw_call!(fn_ptr.fn_ptr.f5, 0, 1, 2, 3),
                    5 => raw_call!(fn_ptr.fn_ptr.f6, 0, 1, 2, 3, 4),
                    6 => raw_call!(fn_ptr.fn_ptr.f7, 0, 1, 2, 3, 4, 5),
                    7 => raw_call!(fn_ptr.fn_ptr.f8, 0, 1, 2, 3, 4, 5, 6),
                    8 => raw_call!(fn_ptr.fn_ptr.f9, 0, 1, 2, 3, 4, 5, 6, 7),
                    9 => raw_call!(fn_ptr.fn_ptr.f10, 0, 1, 2, 3, 4, 5, 6, 7, 8),
                    10 => raw_call!(fn_ptr.fn_ptr.f11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                    11 => raw_call!(fn_ptr.fn_ptr.f12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    12 => raw_call!(fn_ptr.fn_ptr.f13, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                    13 => raw_call!(fn_ptr.fn_ptr.f14, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                    14 => raw_call!(
                        fn_ptr.fn_ptr.f15,
                        0,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        7,
                        8,
                        9,
                        10,
                        11,
                        12,
                        13
                    ),
                    _ => throw(runtime, "Too many arguments"),
                }
            }
            _ => throw(runtime, "Expected function"),
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_raise(val: Value) -> bool {
    panic!("{}", val);
}

#[no_mangle]
pub unsafe extern "C" fn _al_runtime_new<'a>() -> *mut Runtime<'a> {
    Box::into_raw(Box::new(Runtime::new()))
}

#[no_mangle]
pub unsafe extern "C" fn _al_test_harness(f: FnPtr, name: *const c_char) {
    let name = CStr::from_ptr(name).to_str().unwrap();

    let f = f.f0;

    print!("\x1b[34mTEST {}... \x1b[0m", name);

    let time_start = std::time::Instant::now();

    match panic::catch_unwind(|| {
        let mut rt = Box::new(Runtime::new());
        f(&mut rt);
    }) {
        Ok(_) => {
            let elapsed = time_start.elapsed();
            println!("\x1b[32mPASS ({}ms)\x1b[0m", elapsed.as_millis_f64());
        }
        Err(_) => {
            println!("\x1b[31mFAILED\x1b[0m");
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn _al_enter_frame(runtime: &mut Runtime, nloc: usize, frame: *mut Value) {
    let frame = slice::from_raw_parts_mut(frame, nloc);
    frame.fill(none());
    runtime.frames.push(frame);
}

#[no_mangle]
pub unsafe extern "C" fn _al_exit_frame(runtime: &mut Runtime) {
    runtime.frames.pop();
}
