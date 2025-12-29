use crate::core::types::Token;

pub struct FileNode {
    pub statements: StatementsNode,
}

pub struct EvalNode(pub ExpressionsNode);

// GENERAL STATEMENTS
// ==================

pub struct StatementsNode {
    pub statements: Vec<StatementNode>,
}

pub enum StatementNode {
    CompoundStatement(CompoundStmtNode),
    SimpleStatements(SimpleStmtsNode),
}

pub struct SimpleStmtsNode(pub Vec<SimpleStmtNode>);

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
    pub token: Token,
    pub value: StarExpressionsNode,
}

pub enum RaiseStmtNode {
    RaiseNew { token: Token, value: ExpressionNode, from: Option<(Token, ExpressionNode)> },
    ReRaise(Token),
}

pub struct PassStmtNode(pub Token);
pub struct BreakStmtNode(pub Token);
pub struct ContinueStmtNode(pub Token);

// 'global' ','.NAME+
pub struct GlobalStmtNode {
    pub token: Token,
    pub names: Vec<Token>,
}

// 'nonlocal' ','.NAME+
pub struct NonlocalStmtNode {
    pub token: Token,
    pub names: Vec<Token>,
}

pub struct DelStmtNode {
    pub token: Token,
    pub targets: DelTargetsNode,
}

pub struct YieldStmtNode(pub YieldExprNode);

pub struct AssertStmtNode {
    pub token: Token,
    pub expr: ExpressionNode,
    pub error: Option<ExpressionNode>,
}

// Import statements
// -----------------

pub enum ImportStmtNode {
    ImportName(ImportNameNode),
    ImportFrom(ImportFromNode),
}

pub struct ImportNameNode {
    pub token: Token,
    pub names: DottedAsNamesNode,
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
    pub names: Vec<ImportFromAsNameNode>,
}

pub struct ImportFromAsNameNode {
    pub name: Token,
    pub alias: Option<Token>,
}

pub struct DottedAsNamesNode {
    pub names: Vec<DottedAsNameNode>,
}

pub struct DottedAsNameNode {
    pub name: DottedNameNode,
    pub alias: Option<Token>,
}

pub enum DottedNameNode {
    Attr { parent: Box<DottedNameNode>, name: Token },
    Name(Token),
}

// COMPOUND STATEMENTS
// ===================

// Common elements
// ---------------

pub enum BlockNode {
    Statements(StatementsNode),
    Simple(SimpleStmtsNode),
}

pub struct DecoratorsNode {
    pub decorators: Vec<DecoratorNode>
}

pub struct DecoratorNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
}

// Class definitions
// -----------------

pub struct ClassDefNode {
    pub decorators: Option<DecoratorsNode>,
    pub class_def_raw: ClassDefRawNode,
}

pub struct ClassDefRawNode {
    pub token: Token,
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    pub arguments: Option<ArgumentsNode>,
    pub block: BlockNode,
}

// Function definitions
// --------------------

pub struct FunctionDefNode {
    pub decorators: Option<DecoratorsNode>,
    pub function_def_raw: FunctionDefRawNode,
}

pub struct FunctionDefRawNode {
    pub is_async: bool,
    pub def: Token,
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    pub params: Option<ParamsNode>,
    pub return_type: Option<ExpressionNode>,
    pub block: BlockNode,
}

// Function parameters
// -------------------

pub struct ParamsNode(pub ParametersNode);

pub enum ParametersNode {
    SlashNoDefault { slash_no_default: SlashNoDefaultNode, param_no_default: Vec<ParamNoDefaultNode>, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    SlashWithDefault { slash_with_default: SlashWithDefaultNode, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    ParamNoDefault { param_no_default: Vec<ParamNoDefaultNode>, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    ParamWithDefault { param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    StarEtc(StarEtcNode),
}

pub struct SlashNoDefaultNode {
    pub params: Vec<ParamNoDefaultNode>,
    pub slash: Token,
}

pub struct SlashWithDefaultNode {
    pub params_no_default: Vec<ParamNoDefaultNode>,
    pub params_with_default: Vec<ParamWithDefaultNode>,
    pub slash: Token,
}

pub enum StarEtcNode {
    Unannotated { param_no_default: ParamNoDefaultNode, param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    Annotated { param_no_default: ParamNoDefaultStarAnnotationNode, param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    CatchAll { param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    Kwds(KwdsNode),
}

pub struct KwdsNode {
    pub token: Token,
    pub param: ParamNoDefaultNode,
}

pub struct ParamNoDefaultNode(pub ParamNode);
pub struct ParamNoDefaultStarAnnotationNode(pub ParamStarAnnotationNode);

pub struct ParamWithDefaultNode {
    pub param: ParamNode,
    pub default: DefaultNode,
}

pub struct ParamMaybeDefaultNode {
    pub param: ParamNode,
    pub default: Option<DefaultNode>,
}

pub struct ParamNode {
    pub name: Token,
    pub annotation: AnnotationNode,
}

pub struct ParamStarAnnotationNode {
    pub name: Token,
    pub annotation: StarAnnotationNode,
}

pub struct AnnotationNode {
    pub expr: ExpressionNode,
}

pub struct StarAnnotationNode {
    pub expr: StarExpressionNode,
}

pub struct DefaultNode {
    pub expr: ExpressionNode,
    // | invalid_default
}

// If statement
// ------------

pub struct IfStmtNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
    pub block: BlockNode,
    pub else_branch: Option<ElseBranch>,
}

pub enum ElseBranch {
    Elif(Box<ElifStmtNode>),
    Else(Box<ElseBlockNode>),
}

pub struct ElifStmtNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
    pub block: BlockNode,
    pub else_branch: Option<ElseBranch>,
}

pub struct ElseBlockNode {
    pub token: Token,
    pub block: BlockNode,
}

// While statement
// ---------------

pub struct WhileStmtNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
    pub block: BlockNode,
    pub else_block: Option<ElseBlockNode>,
}

// For statement
// -------------

pub struct ForStmtNode {
    pub is_async: bool,
    pub for_token: Token,
    pub targets: StarTargetsNode,
    pub in_token: Token,
    pub star_expr: StarExpressionsNode,
    pub block: BlockNode,
    pub else_block: Option<ElseBlockNode>,
}

// With statement
// --------------

pub struct WithStmtNode {
    pub is_async: bool,
    pub items: Vec<WithItemNode>,
    pub block: BlockNode,
}

pub struct WithItemNode {
    pub expr: ExpressionNode,
    pub alias: Option<StarTargetNode>,
}

// Try statement
// -------------

pub enum TryStmtNode {
    TryFinally { block: BlockNode, finally: FinallyBlockNode },
    TryExcept { block: BlockNode, excepts: Vec<ExceptBlockNode>, else_block: Option<ElseBlockNode>, finally: Option<FinallyBlockNode> },
    TryExceptStar { block: BlockNode, excepts: Vec<ExceptStarBlockNode>, else_block: Option<ElseBlockNode>, finally: Option<FinallyBlockNode> },
}

// Except statement
// ----------------

pub enum ExceptBlockNode {
    Except { token: Token, expr: ExpressionNode, block: BlockNode },
    Alias { token: Token, expr: ExpressionNode, alias: Token, block: BlockNode },
    Multi { token: Token, expressions: ExpressionsNode, block: BlockNode },
    CatchAll { token: Token, block: BlockNode },
}

pub enum ExceptStarBlockNode {
    Except { token: Token, expr: ExpressionNode, block: BlockNode },
    Alias { token: Token, expr: ExpressionNode, alias: Token, block: BlockNode },
    Multi { token: Token, expressions: ExpressionsNode, block: BlockNode },
}

pub struct FinallyBlockNode(pub Token, pub BlockNode);

// Match statement
// ---------------

pub struct MatchStmtNode {
    pub token: Token,
    pub subject: SubjectExprNode,
    pub cases: Vec<CaseBlockNode>,
}

pub enum SubjectExprNode {
    StarExpr( StarNamedExpressionNode, Option<StarNamedExpressionsNode> ),
    Expr(NamedExpressionNode)
}

pub struct CaseBlockNode {
    pub token: Token,
    pub patterns: PatternsNode,
    pub guard: Option<GuardNode>,
    pub block: BlockNode,
}

pub struct GuardNode {
    pub if_token: Token,
    pub expr: NamedExpressionNode,
}

pub enum PatternsNode {
    OpenSequence(OpenSequencePatternNode),
    Pattern(PatternNode),
}

pub struct PatternNode {
    pub or_pattern: OrPatternNode,
    pub alias: Option<PatternCaptureTargetNode>,
}

pub struct OrPatternNode {
    pub patterns: Vec<ClosedPatternNode>,
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

pub enum LiteralExprNode {
    // signed_number !('+' | '-')
    SignedNumber(SignedNumberNode),
    ComplexNumber(ComplexNumberNode),
    Strings(StringsNode),
    None(Token),
    True(Token),
    False(Token),
}

pub struct ComplexNumberNode {
    pub real: SignedRealNumberNode,
    pub sign: Token,
    pub imaginary: ImaginaryNumberNode,
}

pub struct SignedNumberNode {
    pub is_negative: bool,
    pub number: Token,
}

pub struct SignedRealNumberNode {
    pub is_negative: bool,
    pub real: RealNumberNode,
}

pub struct RealNumberNode(pub Token);

pub struct ImaginaryNumberNode(pub Token);

pub struct CapturePatternNode(pub PatternCaptureTargetNode);

pub struct PatternCaptureTargetNode {
    pub name: Token, // !"_" NAME !('.' | '(' | '=')
}

pub struct WildcardPatternNode(pub Token); // "_"

pub struct ValuePatternNode {
    pub attr: AttrNode, // attr !('.' | '(' | '=')
}

pub struct AttrNode {
    pub name_or_attr: NameOrAttrNode,
    pub name: Token,
}

pub enum NameOrAttrNode {
    Attr(Box<AttrNode>),
    Name(Token)
}

pub struct GroupPatternNode {
    pub pattern: PatternNode, // '(' pattern ')'
}

pub enum SequencePatternNode {
    Maybe(Option<MaybeSequencePatternNode>), // '[' T? ']'
    Open(Option<OpenSequencePatternNode>), // '(' T? ')'
}

pub struct OpenSequencePatternNode {
    pub maybe_star_pattern: MaybeStarPatternNode,
    pub maybe_sequence: Option<MaybeSequencePatternNode>,
}

pub struct MaybeSequencePatternNode {
    pub patterns: Vec<MaybeStarPatternNode>, // ','.T+ ','?
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
    pub key_value_patterns: Vec<KeyValuePatternNode>,
}

pub struct KeyValuePatternNode {
    pub key: KeyExprAttr,
    pub pattern: PatternNode,
}

pub enum KeyExprAttr {
    LiteralExpr(LiteralExprNode),
    Attr(AttrNode),
}

pub struct DoubleStarPatternNode {
    pub capture_target: PatternCaptureTargetNode,
}

pub struct ClassPatternNode {
    pub name: NameOrAttrNode,
    pub positional: Option<PositionalPatternsNode>,
    pub keyword: Option<KeywordPatternsNode>,
}

pub struct PositionalPatternsNode {
    pub patterns: Vec<PatternNode>,
}

pub struct KeywordPatternsNode {
    pub patterns: Vec<KeywordPatternNode>,
}

pub struct KeywordPatternNode {
    pub name: Token,
    pub pattern: PatternNode,
}

// Type statement
// --------------

pub struct TypeAliasNode {
    pub token: Token,
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    pub expr: ExpressionNode,
}

// Type parameter declaration
// --------------------------

pub struct TypeParamsNode {
    pub type_param_seq: TypeParamSeqNode,
}

pub struct TypeParamSeqNode(pub Vec<TypeParamNode>); // ','.T+ [',']

pub enum TypeParamNode {
    Named { name: Token, bound: Option<TypeParamBoundNode>, default: Option<TypeParamDefaultNode> },
    Star { name: Token, default: Option<TypeParamStarredDefaultNode> },
    DoubleStar { name: Token, default: Option<TypeParamDefaultNode> },
}

pub struct TypeParamBoundNode(pub ExpressionNode);
pub struct TypeParamDefaultNode(pub ExpressionNode);
pub struct TypeParamStarredDefaultNode(pub StarExpressionNode);

// EXPRESSIONS
// ===========

pub struct ExpressionsNode {
    pub expressions: Vec<ExpressionNode>,
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

pub struct StarExpressionsNode(pub Vec<StarExpressionNode>);

pub enum StarExpressionNode {
    Starred(BitwiseOrNode),
    Expr(ExpressionNode),
}

pub struct StarNamedExpressionsNode(pub Vec<StarNamedExpressionNode>);

pub enum StarNamedExpressionNode {
    Starred(BitwiseOrNode),
    Expr(NamedExpressionNode)
}

pub struct AssignmentExpressionNode {
    pub name: Token,
    pub expr: ExpressionNode,
}

pub enum NamedExpressionNode {
    Assignment(AssignmentExpressionNode),
    Expr(ExpressionNode), // expression !':='
}

pub struct DisjunctionNode {
    pub first: ConjunctionNode,
    pub more: Vec<ConjunctionNode>,
}

pub struct ConjunctionNode {
    pub first: InversionNode,
    pub more: Vec<InversionNode>,
}

pub enum InversionNode {
    Not(Box<InversionNode>),
    Comp(ComparisonNode),
}

// Comparison operators
// --------------------

pub struct ComparisonNode {
    pub bitwise_or: Box<BitwiseOrNode>,
    pub compare_pairs: Vec<CompareOpBitwiseOrPair>,
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
    pub is_await: bool,
    pub primary: PrimaryNode,
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
    pub token: Token,
    pub params: Option<LambdaParamsNode>,
    pub expr: Box<ExpressionNode>,
}

pub struct LambdaParamsNode(pub LambdaParametersNode);

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

pub struct LambdaParamNoDefaultNode(pub LambdaParamNode);

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
pub struct StringsNode(pub Token);
pub struct ListNode(pub Option<StarNamedExpressionsNode>);
pub struct TupleNode(pub Option<(Box<StarNamedExpressionNode>, Option<StarNamedExpressionsNode>)>);
pub struct SetNode(pub StarNamedExpressionsNode);

// Dicts
// -----

pub struct DictNode(pub Option<DoubleStarredKVPairsNode>);
pub struct DoubleStarredKVPairsNode(pub Vec<DoubleStarredKVPairNode>);

pub enum DoubleStarredKVPairNode {
    Starred(Box<BitwiseOrNode>),
    Pair(Box<KVPairNode>),
}

pub struct KVPairNode(pub ExpressionNode, pub ExpressionNode);

// Comprehensions & Generators
// ---------------------------

pub struct ForIfClausesNode(pub Vec<ForIfClauseNode>);

pub struct ForIfClauseNode {
    pub is_async: bool,
    pub for_token: Token,
    pub targets: StarTargetsNode,
    pub in_token: Token,
    pub iter: DisjunctionNode,
    pub guards: Vec<(Token, DisjunctionNode)>,
}

pub struct ListCompNode {
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
}

pub struct SetCompNode {
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
}

pub struct GenexpNode {
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
}

pub struct DictCompNode {
    pub kv_pair: KVPairNode,
    pub clauses: ForIfClausesNode,
}

// FUNCTION CALL ARGUMENTS
// =======================

pub struct ArgumentsNode(pub ArgsNode);

pub enum ArgsNode {
    Expr(Vec<StarredOrNamedExpr>, Option<KwargsNode>),
    Kwargs(KwargsNode),
}

pub enum StarredOrNamedExpr {
    Starred(StarredExpressionNode),
    Named(NamedExpressionNode),
}

pub struct KwargsNode {
    pub kwargs_or_starred: Vec<KwargOrStarredNode>,
    pub kwargs_or_double_starred: Vec<KwargOrDoubleStarredNode>,
}

pub struct StarredExpressionNode(pub ExpressionNode);

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

pub struct StarTargetsNode(pub Vec<StarTargetNode>);
pub struct StarTargetsListSeq(pub Vec<StarTargetNode>);
pub struct StarTargetsTupleSeq(pub Vec<StarTargetNode>);

pub struct StarTargetNode {
    pub is_starred: bool,
    pub target: TargetWithStarAtomNode,
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


