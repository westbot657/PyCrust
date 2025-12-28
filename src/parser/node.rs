use anyhow::Result;
use crate::core::types::{Tokens, Token};

pub trait Node {
    fn parse(tokens: &mut Tokens, include_invalid_rules: bool) -> Result<Box<Self>>;
}


pub struct FileNode {
    statements: StatementsNode,
}

pub struct EvalNode(ExpressionsNode);

// GENERAL STATEMENTS
// ==================

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
    Assignment(AssignmentNode),
    TypeAlias(TypeAliasNode),
    StarExpressions(StarExpressionsNode),
    ReturnStmt(ReturnStmtNode),
    ImportStmt(ImportStmtNode),
    RaiseStmt(RaiseStmtNode),
    PassStmt(PassStmtNode),
    DelStmt(DelStmtNode),
    YieldStmt(YieldStmtNode),
    AssertStmt(AssertStmtNode),
    BreakStmt(BreakStmtNode),
    ContinueStmt(ContinueStmtNode),
    GlobalStmt(GlobalStmtNode),
    NonlocalStmt(NonlocalStmtNode),
}

pub enum CompoundStmtNode {
    FunctionDef(FunctionDefNode),
    IfStmt(IfStmtNode),
    ClassDef(ClassDefNode),
    WithStmt(WithStmtNode),
    ForStmt(ForStmtNode),
    TryStmt(TryStmtNode),
    WhileStmt(WhileStmtNode),
    MatchStmt(MatchStmtNode),
}

pub enum AssignmentNode {
    Typed { name: Token, annotation: ExpressionNode, value: Option<AnnotatedRhsNode> },
    Unpacking { target: AssignmentNodeTarget, annotation: ExpressionNode, value: Option<AnnotatedRhsNode> },
    Chained { targets: Vec<StarTargetsNode>, value: AnnotatedRhsNode},
    Augmented { target: SingleTargetNode, operator: Token, value: AnnotatedRhsNode },
}

pub enum AssignmentNodeTarget {
    SingleTarget(SingleTargetNode),
    SingleSubscriptAttributeTarget(SingleSubscriptAttributeTargetNode),
}

pub enum AnnotatedRhsNode {
    YieldExpr(YieldExprNode),
    StarExpressions(StarExpressionsNode),
}

pub struct ReturnStmtNode {
    token: Token,
    value: StarExpressionsNode,
}

pub enum RaiseStmtNode {
    RaiseNew { token: Token, value: ExpressionNode, from: Option<(Token, ExpressionNode)> },
    ReRaise(Token),
}

pub struct PassStmtNode(Token);
pub struct BreakStmtNode(Token);
pub struct ContinueStmtNode(Token);

// 'global' ','.NAME+
pub struct GlobalStmtNode {
    token: Token,
    names: Vec<Token>,
}

// 'nonlocal' ','.NAME+
pub struct NonlocalStmtNode {
    token: Token,
    names: Vec<Token>,
}

pub struct DelStmtNode {
    token: Token,
    targets: DelTargetsNode,
}

pub struct YieldStmtNode(YieldExprNode);

pub struct AssertStmtNode {
    token: Token,
    expr: ExpressionNode,
    error: Option<ExpressionNode>,
}

// Import statements
// -----------------

pub enum ImportStmtNode {
    ImportName(ImportNameNode),
    ImportFrom(ImportFromNode),
}

pub struct ImportNameNode {
    token: Token,
    names: DottedAsNamesNode,
}

pub enum ImportFromNode {
    DottedName {
        from: Token,
        dots: Vec<Token>,
        name: DottedNameNode,
        import: Token,
        targets: ImportFromTargetsNode,
    },
    Relative {
        dots: Vec<Token>,
        import: Token,
        targets: ImportFromTargetsNode,
    },
}

pub enum ImportFromTargetsNode {
    TupleCapture(ImportFromAsNamesNode),
    OpenCapture(ImportFromAsNamesNode),
    Wildcard(Token),
}

pub struct ImportFromAsNamesNode {
    names: Vec<ImportFromAsNameNode>,
}

pub struct ImportFromAsNameNode {
    name: Token,
    alias: Option<Token>,
}

pub struct DottedAsNamesNode {
    names: Vec<DottedAsNameNode>,
}

pub struct DottedAsNameNode {
    name: DottedNameNode,
    alias: Option<Token>,
}

pub enum DottedNameNode {
    Attr { parent: Box<DottedNameNode>, name: Token },
    Name(Token),
}

// COMPOUND STATEMENTS
// ===================

// Common elements
// ---------------

pub enum Block {
    Statements(StatementsNode),
    Simple(SimpleStmtsNode),
}

pub struct DecoratorsNode {
    decorators: Vec<DecoratorNode>
}

pub struct DecoratorNode {
    token: Token,
    expr: NamedExpressionNode,
}

// Class definitions
// -----------------

pub struct ClassDefNode {
    decorators: Option<DecoratorsNode>,
    class_def_raw: ClassDefRawNode,
}

pub struct ClassDefRawNode {
    token: Token,
    name: Token,
    type_params: Option<TypeParamsNode>,
    arguments: Option<ArgumentsNode>,
    block: Block,
}

// Function definitions
// --------------------

pub struct FunctionDefNode {
    decorators: Option<DecoratorsNode>,
    function_def_raw: FunctionDefRawNode,
}

pub struct FunctionDefRawNode {
    is_async: bool,
    def: Token,
    name: Token,
    type_params: Option<TypeParamsNode>,
    params: Option<ParamsNode>,
    return_type: Option<ExpressionNode>,
    block: Block,
}

// Function parameters
// -------------------

pub struct ParamsNode(ParametersNode);

pub enum ParametersNode {
    SlashNoDefault { slash_no_default: SlashNoDefaultNode, param_no_default: Vec<ParamNoDefaultNode>, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    SlashWithDefault { slash_with_default: SlashWithDefaultNode, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    ParamNoDefault { param_no_default: Vec<ParamNoDefaultNode>, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    ParamWithDefault { param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    StarEtc(StarEtcNode),
}

pub struct SlashNoDefaultNode {
    params: Vec<ParamNoDefaultNode>,
    slash: Token,
}

pub struct SlashWithDefaultNode {
    params_no_default: Vec<ParamNoDefaultNode>,
    params_with_default: Vec<ParamWithDefaultNode>,
    slash: Token,
}

pub enum StarEtcNode {
    Unannotated { param_no_default: ParamNoDefaultNode, param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    Annotated { param_no_default: ParamNoDefaultStarAnnotationNode, param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    CatchAll { param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    Kwds(KwdsNode),
}

pub struct KwdsNode {
    token: Token,
    param: ParamNoDefaultNode,
}

pub struct ParamNoDefaultNode(ParamNode);
pub struct ParamNoDefaultStarAnnotationNode(ParamStarAnnotationNode);

pub struct ParamWithDefaultNode {
    param: ParamNode,
    default: DefaultNode,
}

pub struct ParamMaybeDefaultNode {
    param: ParamNode,
    default: Option<DefaultNode>,
}

pub struct ParamNode {
    name: Token,
    annotation: AnnotationNode,
}

pub struct ParamStarAnnotationNode {
    name: Token,
    annotation: StarAnnotationNode,
}

pub struct AnnotationNode {
    expr: ExpressionNode,
}

pub struct StarAnnotationNode {
    expr: StarExpressionNode,
}

pub struct DefaultNode {
    expr: ExpressionNode,
    // | invalid_default
}

// If statement
// ------------

pub struct IfStmtNode {
    token: Token,
    expr: NamedExpressionNode,
    block: Block,
    else_branch: Option<ElseBranch>,
}

pub enum ElseBranch {
    Elif(Box<ElifStmtNode>),
    Else(Box<ElseBlockNode>),
}

pub struct ElifStmtNode {
    token: Token,
    expr: NamedExpressionNode,
    block: Block,
    else_branch: Option<ElseBranch>,
}

pub struct ElseBlockNode {
    token: Token,
    block: Block,
}

// While statement
// ---------------

pub struct WhileStmtNode {
    token: Token,
    expr: NamedExpressionNode,
    block: Block,
    else_block: Option<ElseBlockNode>,
}

// For statement
// -------------

pub struct ForStmtNode {
    is_async: bool,
    for_token: Token,
    targets: StarTargetsNode,
    in_token: Token,
    star_expr: StarExpressionsNode,
    block: Block,
    else_block: Option<ElseBlockNode>,
}

// With statement
// --------------

pub struct WithStmtNode {
    is_async: bool,
    items: Vec<WithItemNode>,
    block: Block,
}

pub struct WithItemNode {
    expr: ExpressionNode,
    alias: Option<StarTargetNode>,
}

// Try statement
// -------------

pub enum TryStmtNode {
    TryFinally { block: Block, finally: FinallyBlockNode },
    TryExcept { block: Block, excepts: Vec<ExceptBlockNode>, else_block: Option<ElseBlockNode>, finally: Option<FinallyBlockNode> },
    TryExceptStar { block: Block, excepts: Vec<ExceptStarBlockNode>, else_block: Option<ElseBlockNode>, finally: Option<FinallyBlockNode> },
}

// Except statement
// ----------------

pub enum ExceptBlockNode {
    Except { token: Token, expr: ExpressionNode, block: Block },
    Alias { token: Token, expr: ExpressionNode, alias: Token, block: Block },
    Multi { token: Token, expressions: ExpressionsNode, block: Block },
    CatchAll { token: Token, block: Block },
}

pub enum ExceptStarBlockNode {
    Except { token: Token, expr: ExpressionNode, block: Block },
    Alias { token: Token, expr: ExpressionNode, alias: Token, block: Block },
    Multi { token: Token, expressions: ExpressionsNode, block: Block },
}

pub struct FinallyBlockNode(Token, Block);

// Match statement
// ---------------

pub struct MatchStmtNode {
    token: Token,
    subject: SubjectExprNode,
    cases: Vec<CaseBlockNode>,
}

pub enum SubjectExprNode {
    StarExpr( StarNamedExpressionNode, Option<StarNamedExpressionsNode> ),
    Expr(NamedExpressionNode)
}

pub struct CaseBlockNode {
    token: Token,
    patterns: PatternsNode,
    guard: Option<GuardNode>,
    block: Block,
}

pub struct GuardNode {
    if_token: Token,
    expr: NamedExpressionNode,
}

pub enum PatternsNode {
    OpenSequence(OpenSequencePatternNode),
    Pattern(PatternNode),
}

pub struct PatternNode {
    or_pattern: OrPatternNode,
    alias: Option<PatternCaptureTargetNode>,
}

pub struct OrPatternNode {
    patterns: Vec<ClosedPatternNode>,
}

pub enum ClosedPatternNode {
    Literal(LiteralPatternNode),
    Capture(CapturePatternNode),
    Wildcard(WildcardPatternNode),
    Value(ValuePatternNode),
    Group(GroupPatternNode),
    Sequence(SequencePatternNode),
    Mapping(MappingPatternNode),
    Class(ClassPatternNode),
}

pub enum LiteralPatternNode {
    // signed_number !('+' | '-')
    SignedNumber(SignedNumberNode),
    ComplexNumber(ComplexNumberNode),
    Strings(StringsNode),
    None(Token),
    True(Token),
    False(Token),
}

type LiteralExprNode = LiteralPatternNode;

pub struct ComplexNumberNode {
    real: SignedRealNumberNode,
    sign: Token,
    imaginary: ImaginaryNumberNode,
}

pub struct SignedNumberNode {
    is_negative: bool,
    number: Token,
}

pub struct SignedRealNumberNode {
    is_negative: bool,
    real: RealNumberNode,
}

pub struct RealNumberNode(Token);

pub struct ImaginaryNumberNode(Token);

pub struct CapturePatternNode(PatternCaptureTargetNode);

pub struct PatternCaptureTargetNode {
    name: Token, // !"_" NAME !('.' | '(' | '=')
}

pub struct WildcardPatternNode(Token); // "_"

pub struct ValuePatternNode {
    attr: AttrNode, // attr !('.' | '(' | '=')
}

pub struct AttrNode {
    name_or_attr: NameOrAttrNode,
    name: Token,
}

pub enum NameOrAttrNode {
    Attr(Box<AttrNode>),
    Name(Token)
}

pub struct GroupPatternNode {
    pattern: PatternNode, // '(' pattern ')'
}

pub enum SequencePatternNode {
    Maybe(Option<MaybeSequencePatternNode>), // '[' T? ']'
    Open(Option<OpenSequencePatternNode>), // '(' T? ')'
}

pub struct OpenSequencePatternNode {
    maybe_star_pattern: MaybeStarPatternNode,
    maybe_sequence: Option<MaybeSequencePatternNode>,
}

pub struct MaybeSequencePatternNode {
    patterns: Vec<MaybeStarPatternNode>, // ','.T+ ','?
}

pub enum MaybeStarPatternNode {
    StarPattern(StarPatternNode),
    Pattern(PatternNode),
}

pub enum StarPatternNode {
    Capture(PatternCaptureTargetNode),
    Wildcard(WildcardPatternNode),
}

pub enum MappingPatternNode {
    Empty, // '{' '}'
    DoubleStar(DoubleStarPatternNode),
    ItemsDoubleStar(ItemsPatternNode, DoubleStarPatternNode),
    Items(ItemsPatternNode),
}

pub struct ItemsPatternNode {
    key_value_patterns: Vec<KeyValuePatternNode>,
}

pub struct KeyValuePatternNode {
    key: KeyExprAttr,
    pattern: PatternNode,
}

pub enum KeyExprAttr {
    LiteralExpr(LiteralExprNode),
    Attr(AttrNode),
}

pub struct DoubleStarPatternNode {
    capture_target: PatternCaptureTargetNode,
}

pub struct ClassPatternNode {
    name: NameOrAttrNode,
    positional: Option<PositionalPatternsNode>,
    keyword: Option<KeywordPatternsNode>,
}

pub struct PositionalPatternsNode {
    patterns: Vec<PatternNode>,
}

pub struct KeywordPatternsNode {
    patterns: Vec<KeywordPatternNode>,
}

pub struct KeywordPatternNode {
    name: Token,
    pattern: PatternNode,
}

// Type statement
// --------------

pub struct TypeAliasNode {
    token: Token,
    name: Token,
    type_params: Option<TypeParamsNode>,
    expr: ExpressionNode,
}

// Type parameter declaration
// --------------------------

pub struct TypeParamsNode {
    type_param_seq: TypeParamSeqNode,
}

pub struct TypeParamSeqNode(Vec<TypeParamNode>); // ','.T+ [',']

pub enum TypeParamNode {
    Named { name: Token, bound: Option<TypeParamBoundNode>, default: Option<TypeParamDefaultNode> },
    Star { name: Token, default: Option<TypeParamStarredDefaultNode> },
    DoubleStar { name: Token, default: Option<TypeParamDefaultNode> },
}

pub struct TypeParamBoundNode(ExpressionNode);
pub struct TypeParamDefaultNode(ExpressionNode);
pub struct TypeParamStarredDefaultNode(StarExpressionNode);

// EXPRESSIONS
// ===========

pub struct ExpressionsNode {
    expressions: Vec<ExpressionNode>,
}

pub enum ExpressionNode {
    Ternary { truthy: DisjunctionNode, condition: DisjunctionNode, falsey: Box<ExpressionNode> },
    Disjunction(DisjunctionNode),
    Lambda(LambdaDefNode),
}

pub enum YieldExprNode {
    From(ExpressionNode),
    Direct(Option<StarExpressionsNode>),
}

pub struct StarExpressionsNode(Vec<StarExpressionNode>);

pub enum StarExpressionNode {
    Starred(BitwiseOrNode),
    Expr(ExpressionNode),
}

pub struct StarNamedExpressionsNode(Vec<StarNamedExpressionNode>);

pub enum StarNamedExpressionNode {
    Starred(BitwiseOrNode),
    Expr(NamedExpressionNode)
}

pub struct AssignmentExpressionNode {
    name: Token,
    expr: ExpressionNode,
}

pub enum NamedExpressionNode {
    Assignment(AssignmentExpressionNode),
    Expr(ExpressionNode), // expression !':='
}

pub struct DisjunctionNode {
    first: ConjunctionNode,
    more: Vec<ConjunctionNode>,
}

pub struct ConjunctionNode {
    first: InversionNode,
    more: Vec<InversionNode>,
}

pub enum InversionNode {
    Not(Box<InversionNode>),
    Comp(ComparisonNode),
}

// Comparison operators
// --------------------

pub struct ComparisonNode {
    bitwise_or: Box<BitwiseOrNode>,
    compare_pairs: Vec<CompareOpBitwiseOrPair>,
}

pub enum CompareOpBitwiseOrPair {
    Eq(Box<BitwiseOrNode>),
    Ne(Box<BitwiseOrNode>),
    Le(Box<BitwiseOrNode>),
    Lt(Box<BitwiseOrNode>),
    Ge(Box<BitwiseOrNode>),
    Gt(Box<BitwiseOrNode>),
    NotIn(Box<BitwiseOrNode>),
    In(Box<BitwiseOrNode>),
    IsNot(Box<BitwiseOrNode>),
    Is(Box<BitwiseOrNode>),
}

pub enum BitwiseOrNode {
    Or(Box<BitwiseOrNode>, BitwiseXorNode),
    Xor(BitwiseXorNode)
}

pub enum BitwiseXorNode {
    Xor(Box<BitwiseXorNode>, BitwiseAndNode),
    And(BitwiseAndNode)
}

pub enum BitwiseAndNode {
    And(Box<BitwiseAndNode>, ShiftExprNode),
    Shift(ShiftExprNode)
}

pub enum ShiftExprNode {
    Shl(Box<ShiftExprNode>, SumNode),
    Shr(Box<ShiftExprNode>, SumNode),
    Sum(SumNode)
}

// Arithmetic operators
// --------------------

pub enum SumNode {
    Add(Box<SumNode>, TermNode),
    Sub(Box<SumNode>, TermNode),
    Term(TermNode),
}

pub enum TermNode {
    Mul(Box<TermNode>, FactorNode),
    Div(Box<TermNode>, FactorNode),
    Floor(Box<TermNode>, FactorNode),
    Mod(Box<TermNode>, FactorNode),
    MatMul(Box<TermNode>, FactorNode),
    Factor(FactorNode),
}

pub enum FactorNode {
    Pos(Box<FactorNode>),
    Neg(Box<FactorNode>),
    BitNot(Box<FactorNode>),
    Power(PowerNode),
}

pub enum PowerNode {
    Pow(AwaitPrimaryNode, Box<FactorNode>),
    Primary(AwaitPrimaryNode),
}

// Primary elements
// ----------------

pub struct AwaitPrimaryNode {
    is_await: bool,
    primary: PrimaryNode,
}

pub enum PrimaryNode {
    DotName(Box<PrimaryNode>, Token),
    Genexp(Box<PrimaryNode>, GenexpNode),
    Call(Box<PrimaryNode>, Option<ArgumentsNode>),
    Slice(Box<PrimaryNode>, SlicesNode),
    Atom(AtomNode),
}

pub enum SlicesNode {
    Slice(SliceNode),
    Multi(Vec<SliceOrStarredExpr>),
}

pub enum SliceOrStarredExpr {
    Slice(SliceNode),
    Starred(StarredExpressionNode),
}

pub enum SliceNode {
    Literal { start: Option<ExpressionNode>, end: Option<ExpressionNode>, step: Option<ExpressionNode> },
    Expr(NamedExpressionNode),
}

pub enum AtomNode {
    Name(Token),
    True(Token),
    False(Token),
    None(Token),
    Strings(StringsNode),
    Number(Token),
    Tuple(TupleNode),
    Group(GroupNode),
    Genexp(GenexpNode),
    List(ListNode),
    ListComp(ListCompNode),
    Dict(DictNode),
    Set(SetNode),
    DictComp(DictCompNode),
    SetComp(SetCompNode),
}

pub enum GroupNode {
    Yield(YieldExprNode),
    Named(NamedExpressionNode),
}

// Lambda functions
// ----------------

pub struct LambdaDefNode {
    token: Token,
    params: Option<LambdaParamsNode>,
    expr: Box<ExpressionNode>,
}

pub struct LambdaParamsNode(LambdaParametersNode);

pub enum LambdaParametersNode {
    SlashNoDefault {
        slash_no_default: LambdaSlashNoDefaultNode,
        param_no_default: Vec<LambdaParamNoDefaultNode>,
        param_with_default: Vec<LambdaParamWithDefaultNode>,
        star_etc: Option<LambdaStarEtcNode>,
    },
    SlashWithDefault {
        slash_with_default: LambdaSlashWithDefaultNode,
        param_with_default: Vec<LambdaParamWithDefaultNode>,
        star_etc: Option<LambdaStarEtcNode>,
    },
    ParamNoDefault {
        param_no_default: Vec<LambdaParamNoDefaultNode>,
        param_with_default: Vec<LambdaParamWithDefaultNode>,
        star_etc: Option<LambdaStarEtcNode>,
    },
    ParamWithDefault {
        param_with_default: Vec<LambdaParamWithDefaultNode>,
        star_etc: Option<LambdaStarEtcNode>,
    },
    StarEtc(LambdaStarEtcNode),
}

pub struct LambdaSlashNoDefaultNode {
    pub params: Vec<LambdaParamNoDefaultNode>,
    pub slash: Token,
}

pub struct LambdaSlashWithDefaultNode {
    pub params_no_default: Vec<LambdaParamNoDefaultNode>,
    pub params_with_default: Vec<LambdaParamWithDefaultNode>,
    pub slash: Token,
}

pub enum LambdaStarEtcNode {
    Star {
        param_no_default: LambdaParamNoDefaultNode,
        param_maybe_default: Vec<LambdaParamMaybeDefaultNode>,
        kwds: Option<LambdaKwdsNode>,
    },
    CatchAll {
        param_maybe_default: Vec<LambdaParamMaybeDefaultNode>,
        kwds: Option<LambdaKwdsNode>,
    },
    Kwds(LambdaKwdsNode),
}

pub struct LambdaKwdsNode {
    pub token: Token,
    pub param: LambdaParamNoDefaultNode,
}

pub struct LambdaParamNoDefaultNode(LambdaParamNode);

pub struct LambdaParamWithDefaultNode {
    pub param: LambdaParamNode,
    pub default: DefaultNode,
}

pub struct LambdaParamMaybeDefaultNode {
    pub param: LambdaParamNode,
    pub default: Option<DefaultNode>,
}

pub struct LambdaParamNode {
    pub name: Token,
}



// LITERALS
// ========
pub struct StringsNode(Token);
pub struct ListNode(Option<StarNamedExpressionsNode>);
pub struct TupleNode(Option<(Box<StarNamedExpressionNode>, Option<StarNamedExpressionsNode>)>);
pub struct SetNode(StarNamedExpressionsNode);

// Dicts
// -----

pub struct DictNode(Option<DoubleStarredKVPairsNode>);
pub struct DoubleStarredKVPairsNode(Vec<DoubleStarredKVPairNode>);

pub enum DoubleStarredKVPairNode {
    Starred(Box<BitwiseOrNode>),
    Pair(Box<KVPairNode>),
}

pub struct KVPairNode(ExpressionNode, ExpressionNode);

// Comprehensions & Generators
// ---------------------------

pub struct ForIfClausesNode(Vec<ForIfClauseNode>);

pub struct ForIfClauseNode {
    is_async: bool,
    for_token: Token,
    targets: StarTargetsNode,
    in_token: Token,
    iter: DisjunctionNode,
    guards: Vec<(Token, DisjunctionNode)>,
}

pub struct ListCompNode {
    named: NamedExpressionNode,
    clauses: ForIfClausesNode,
}

pub struct SetCompNode {
    named: NamedExpressionNode,
    clauses: ForIfClausesNode,
}

pub struct GenexpNode {
    named: NamedExpressionNode,
    clauses: ForIfClausesNode,
}

pub struct DictCompNode {
    kv_pair: KVPairNode,
    clauses: ForIfClausesNode,
}

// FUNCTION CALL ARGUMENTS
// =======================

pub struct ArgumentsNode(ArgsNode);

pub enum ArgsNode {
    Expr(Vec<StarredOrNamedExpr>, Option<KwargsNode>),
    Kwargs(KwargsNode),
}

pub enum StarredOrNamedExpr {
    Starred(StarredExpressionNode),
    Named(NamedExpressionNode),
}

pub struct KwargsNode {
    kwargs_or_starred: Vec<KwargOrStarredNode>,
    kwargs_or_double_starred: Vec<KwargOrDoubleStarredNode>,
}

pub struct StarredExpressionNode(ExpressionNode);

pub enum KwargOrStarredNode {
    Kwarg(Token, ExpressionNode),
    Starred(StarredExpressionNode),
}

pub enum KwargOrDoubleStarredNode {
    Kwarg(Token, ExpressionNode),
    Starred(ExpressionNode),
}

// ASSIGNMENT TARGETS
// ==================

// Generic targets
// ---------------

pub struct StarTargetsNode(Vec<StarTargetNode>);
pub struct StarTargetsListSeq(Vec<StarTargetNode>);
pub struct StarTargetsTupleSeq(Vec<StarTargetNode>);

pub struct StarTargetNode {
    is_starred: bool,
    target: TargetWithStarAtomNode,
}

pub enum TargetWithStarAtomNode {
    Named(TPrimaryNode, Token), // !t_lookahead
    Slices(TPrimaryNode, SliceNode), // !t_lookahead
    StarAtom(StarAtomNode),
}

pub enum StarAtomNode {
    Name(Token),
    Expr(Box<TargetWithStarAtomNode>),
    Tuple(StarTargetsTupleSeq),
    List(StarTargetsListSeq),
}

pub enum SingleTargetNode {
    Subscript(SingleSubscriptAttributeTargetNode),
    Named(Token),
    // '(' single_target ')'
}

pub enum SingleSubscriptAttributeTargetNode {
    Named(TPrimaryNode, Token), // !t_lookahead
    Slices(TPrimaryNode, SlicesNode), // !t_lookahead
}

pub enum TPrimaryNode {
    Named(Box<Self>, Token), // '.' NAME &t_lookahead
    Slices(Box<Self>, SliceNode), // '[' slices ']' &t_lookahead
    Genexp(Box<Self>, GenexpNode), // &t_lookahead
    Call(Box<Self>, Option<ArgumentsNode>), // &t_lookahead
    Atom(Box<AtomNode>), // &t_lookahead
}

// t_lookahead: '(' | '[' | '.'

// Targets for del statements
// --------------------------

pub struct DelTargetsNode(Vec<DelTargetNode>);

pub enum DelTargetNode {
    Named(TPrimaryNode, Token), // '.' NAME !t_lookahead
    Slices(TPrimaryNode, SlicesNode), // '[' slices ']' !t_lookahead
    Atom(DelTAtomNode),
}

pub enum DelTAtomNode {
    Name(Token),
    Single(Box<DelTargetNode>),
    Tuple(Option<DelTargetsNode>),
    List(Option<DelTargetsNode>),
}


