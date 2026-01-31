use num_bigint::BigInt;
use crate::core::types::Operator;


pub enum Comp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    In,
    NotIn,
    Is,
    IsNot
}

pub enum UnaryOp {
    Positive,
    Negative,
    BooleanNot,
    BitwiseNot,
}

pub enum JumpType {
    If(bool),
    IfOrPop(bool),
    PopJumpIf(bool)
}

pub enum RaiseType {
    ReRaise,
    Raise,
    RaiseFrom,
}

pub struct CallFlags {
    pub has_varargs: bool,
    pub has_kwargs: bool,
}

pub enum FormatConversion {
    None,
    Ascii,
    Repr,
    Str
}

pub struct FormatFlags {
    pub conversion: FormatConversion,
    pub has_spec: bool,
}

pub type Label = u32;
pub type NameIndex = u32;
pub type LocalIndex = u32;
pub type CellIndex = u32;


pub enum ConstantValue {
    I64(i64),
    F64(f64),
    BigInt(BigInt),
    Bool(bool),
    String(String),
    None,

}


pub enum OpCode {

    PopTop,                     // remove top item from value stack
    DupTop,                     // duplicate top stack item
    DupTwo,                     // duplicate top 2 stack items
    Rot(u8),                    // move item at n-1 to top

    LoadGlobal(NameIndex),      // global table
    StoreGlobal(NameIndex),     // global table
    DeleteGlobal(NameIndex),    // global table
    LoadAttr(NameIndex),        // variable attributes of top
    StoreAttr(NameIndex),       // variable attributes of top
    DeleteAttr(NameIndex),      // variable attributes of top

    LoadLocal(LocalIndex),      // local table
    StoreLocal(LocalIndex),     // local table
    DeleteLocal(LocalIndex),    // local table

    LoadCell(CellIndex),        // cells for closures and stuff
    StoreCell(CellIndex),       // cells for closures and stuff
    DeleteCell(CellIndex),      // cells for closures and stuff
    LoadClassCell(CellIndex),   // cells for closures and stuff

    LoadSubscript,              // Fast version of x[4]
    StoreSubscript,             //
    DeleteSubscript,            //

    BinOp(Operator),
    AugBinOp(Operator),
    Comp(Comp),
    Contains,                   // 5 in [1, 2, 3, 4, 5]

    BuildList(u32),             // 3: [..., a, b, c]                  -> [..., [a, b, c]]
    BuildMap(u32),              // 3: [..., (k1, k2, k3), v1, v2, v3] -> [..., {k1: v1, k2: v2, k3: v3}]
    BuildTuple(u32),            // 3: [..., a, b, c]                  -> [..., (a, b, c)]
    BuildSet(u32),              // 3: [..., a, b, c]                  -> [..., {a, b, c}]
    BuildString(u32),           // 3: [..., a, b, c]                  -> [..., "{a}{b}{c}"]
    BuildSlice(u8),             // 2-3, 2: start, stop  3: start, stop, step

    ListAppend(u32),            // append top to list down x, pops value first before calculating distance
    SetAdd(u32),
    MapAdd(u32),

    ListExtend(u32),            // [..., [list], ..., [iter]] -> [..., [list, *iter], ...]
    SetUpdate(u32),
    MapUpdate(u32),
    MapMerge(u32),

    CallFunc(u32),              // argc
    CallFuncKw(u32),
    CallFuncEx(CallFlags),
    BuildTupleUnpack(u32),      // 3: [..., a, b, c, (tuple,)] -> [..., (a, b, c, tuple,)]

    LoadMethod(NameIndex),      // fast function load method, loads [method, self] or [attr, null]
    CallMethod(u32),            // argc

    Break,
    Return,
    Continue(Label),

    Jump(Label, JumpType),

    ForIter(Label),

    SetupExcept(Label),
    SetupFinally(Label),
    PopBlock,
    PopExcept,
    Raise(RaiseType),
    ReRaise,

    SetupWith(Label),
    WithExceptStart,

    YieldValue,
    YieldFrom,
    GetYieldFromIter,
    GetIter,

    ImportName(NameIndex),
    ImportFrom(NameIndex),
    ImportStar,

    LoadBuildClass, // load __build_class__ builtin
    SetupAnnotations,

    FormatValue(FormatFlags),

    MatchSequence,              // is top item a sequence-like object
    MatchMapping,               // is top item a mapping object
    MatchKeys,                  // is top item's (key tuple) keys are all in top-1's keys
    MatchClass(u32), // argc
    CopyDictFiltered, // [..., dict, keys_tuple] -> [..., new_dict] // removes keys in keys_tuple from dict
    GetLen,



}








