use std::cell::{Cell, UnsafeCell};
use bitflags::bitflags;
use hashbrown::HashMap;
use indexmap::IndexMap;
use num_bigint::BigInt;
use slotmap::new_key_type;

new_key_type! {
    pub struct PyObjectRef;
}

bitflags! {
    pub struct HeapFlags: u8 {
        const TRACKED   = 0x01;
        const CONTAINER = 0x02;
        const FINALIZED = 0x04;
    }
}

pub struct HeapObject {
    ref_count: Cell<u32>,
    gc_ref: Cell<i32>,
    flags: Cell<HeapFlags>,
    value: UnsafeCell<PyObject>
}


pub enum PyObject {
    I64(i64),
    F64(f64),
    BigInt(BigInt),
    Bool(bool),
    None,
    InternalInstanceValue(PyObjectInternal), // is created when object() is called, but is not the value returned, instead PyObject::ObjectRef is returned, containing a ref to this
    ObjectRef(PyObjectRef),
    List(Vec<PyObjectRef>),
    Tuple(Vec<PyObjectRef>), // only difference from list is mutability rules
    FastDict(IndexMap<PyObjectRef, PyObjectRef>), // can be used if the analyzer can ensure that no key objects could have a special __eq__/__hash__
    SlowDict(Vec<(PyObjectRef, PyObjectRef)>),

}

pub struct PyObjectInternal {
    attrs: HashMap<String, PyObjectRef>,
}


