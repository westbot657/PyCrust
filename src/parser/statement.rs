
pub struct StatementsNode {
    statements: Vec<StatementNode>,
}

pub enum StatementNode {
    CompoundStatement(CompoundStmtNode),
    SimpleStatements(SimpleStmtsNode),
}

pub struct SingleCompoundStmtNode {
    stmt: CompoundStmtNode,
}

pub struct SimpleStmtsNode {
    statements: Vec<SimpleStmtNode>,
}

pub enum SimpleStmtNode {
    Assignment(),
    TypeAlias(),
    StarExpressions(),
    ReturnStmt(),
    ImportStmt(),
    RaiseStmt(),
    PassStmt(),
    DelStmt(),
    YieldStmt(),
    AssertStmt(),
    BreakStmt(),
    ContinueStmt(),
    GlobalStmt(),
    NonlocalStmt(),
}


pub struct CompoundStmtNode {

}
