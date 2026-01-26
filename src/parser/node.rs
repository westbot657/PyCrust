use node_macro::node;
use crate::core::types::*;
use anyhow::{Result, anyhow, Ok};

pub trait Node
where
    Self: Sized
{
    fn parse(tokens: &mut ParseTokens) -> Result<Option<Self>>;
}

#[node]
pub struct FileNode {
    pub statements: StatementsNode,
    #[pass_if(TokenValue::EndMarker)]
    _end_marker: ()
}

#[node]
pub struct EvalNode(pub ExpressionsNode, #[token(TokenValue::Newline)] Vec<Token>, #[pass_if(TokenValue::EndMarker)] ());

// GENERAL STATEMENTS
// ==================

#[node]
pub struct StatementsNode(#[one_or_more] pub Vec<StatementNode>);

#[node]
pub enum StatementNode {
    CompoundStatement(CompoundStmtNode),
    SimpleStatements(SimpleStmtsNode),
}

#[node]
pub struct SimpleStmtsNode(#[one_or_more] #[sep(TokenValue::Symbol(Symbol::Semicolon), trailing)] pub Vec<SimpleStmtNode>);

#[node]
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

#[node]
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

#[node]
pub struct AssignmentNodeValueInner(#[token(TokenValue::Symbol(Symbol::Assign))] (), AnnotatedRhsNode );

#[node]
pub struct AssignmentNodeChainedTargetsInner(StarTargetsNode, #[token(TokenValue::Symbol(Symbol::Assign))] ());

#[node]
pub enum AssignmentNode {
    Typed {
        #[token(TokenValue::Word(_))]
        name: Token,
        #[token(TokenValue::Symbol(Symbol::Colon))]
        c: (),
        annotation: ExpressionNode,
        value: Option<AssignmentNodeValueInner>
    },
    Unpacking {
        target: AssignmentNodeTarget,
        #[token(TokenValue::Symbol(Symbol::Colon))]
        c: (),
        annotation: ExpressionNode,
        value: Option<AssignmentNodeValueInner>
    },
    Chained {
        #[one_or_more]
        targets: Vec<AssignmentNodeChainedTargetsInner>,
        value: AnnotatedRhsNode,
        #[fail_if(TokenValue::Symbol(Symbol::Assign))]
        f: (),
    },
    Augmented {
        target: SingleTargetNode,
        #[token(TokenValue::AssignOperator(_))]
        operator: Token,
        #[commit]
        value: AnnotatedRhsNode
    },
}

#[node]
pub enum AssignmentNodeTarget {
    SingleTarget(#[token(TokenValue::Symbol(Symbol::LParen))] (), SingleTargetNode, #[token(TokenValue::Symbol(Symbol::RParen))] ()),
    SingleSubscriptAttributeTarget(SingleSubscriptAttributeTargetNode),
}

#[node]
pub enum AnnotatedRhsNode {
    YieldExpr(YieldExprNode),
    StarExpressions(StarExpressionsNode),
}

#[node]
pub struct ReturnStmtNode {
    #[token(TokenValue::Keyword(Keyword::Return))]
    pub token: Token,
    pub value: StarExpressionsNode,
}

#[node]
pub enum RaiseStmtNode {
    RaiseNew {
        #[token(TokenValue::Keyword(Keyword::Raise))]
        token: Token,
        value: ExpressionNode,
        #[prefix(token(TokenValue::Keyword(Keyword::From)))]
        from: Option<ExpressionNode>
    },
    ReRaise(#[token(TokenValue::Keyword(Keyword::Raise))] Token),
}

#[node]
pub struct PassStmtNode(#[token(TokenValue::Keyword(Keyword::Pass))] pub Token);
#[node]
pub struct BreakStmtNode(#[token(TokenValue::Keyword(Keyword::Break))] pub Token);
#[node]
pub struct ContinueStmtNode(#[token(TokenValue::Keyword(Keyword::Continue))] pub Token);

// 'global' ','.NAME+
#[node]
pub struct GlobalStmtNode {
    #[token(TokenValue::Keyword(Keyword::Global))]
    pub token: Token,
    #[one_or_more]
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    #[token(TokenValue::Word(_))]
    pub names: Vec<Token>,
}

// 'nonlocal' ','.NAME+
#[node]
pub struct NonlocalStmtNode {
    #[token(TokenValue::Keyword(Keyword::Nonlocal))]
    pub token: Token,
    #[one_or_more]
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    #[token(TokenValue::Word(_))]
    pub names: Vec<Token>,
}

#[node]
pub struct DelStmtNode {
    #[token(TokenValue::Keyword(Keyword::Del))]
    pub token: Token,
    pub targets: DelTargetsNode,
}

#[node]
pub struct YieldStmtNode(pub YieldExprNode);

#[node]
pub struct AssertStmtNode {
    #[token(TokenValue::Keyword(Keyword::Assert))]
    pub token: Token,
    pub expr: ExpressionNode,
    pub error: Option<ExpressionNode>,
}

// Import statements
// -----------------

#[node]
pub enum ImportStmtNode {
    ImportName(ImportNameNode),
    ImportFrom(ImportFromNode),
}

#[node]
pub struct ImportNameNode {
    pub token: Token,
    pub names: DottedAsNamesNode,
}

#[node]
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

#[node]
pub enum ImportFromTargetsNode {
    TupleCapture(ImportFromAsNamesNode),
    OpenCapture(ImportFromAsNamesNode),
    Wildcard(Token),
}

#[node]
pub struct ImportFromAsNamesNode {
    pub names: Vec<ImportFromAsNameNode>,
}

#[node]
pub struct ImportFromAsNameNode {
    pub name: Token,
    pub alias: Option<Token>,
}

#[node]
pub struct DottedAsNamesNode {
    pub names: Vec<DottedAsNameNode>,
}

#[node]
pub struct DottedAsNameNode {
    pub name: DottedNameNode,
    pub alias: Option<Token>,
}

#[node]
pub enum DottedNameNode {
    Attr { parent: Box<DottedNameNode>, name: Token },
    Name(Token),
}

// COMPOUND STATEMENTS
// ===================

// Common elements
// ---------------

#[node]
pub enum BlockNode {
    Statements(StatementsNode),
    Simple(SimpleStmtsNode),
}

#[node]
pub struct DecoratorsNode {
    pub decorators: Vec<DecoratorNode>
}

#[node]
pub struct DecoratorNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
}

// Class definitions
// -----------------

#[node]
pub struct ClassDefNode {
    pub decorators: Option<DecoratorsNode>,
    pub class_def_raw: ClassDefRawNode,
}

#[node]
pub struct ClassDefRawNode {
    pub token: Token,
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    pub arguments: Option<ArgumentsNode>,
    pub block: BlockNode,
}

// Function definitions
// --------------------

#[node]
pub struct FunctionDefNode {
    pub decorators: Option<DecoratorsNode>,
    pub function_def_raw: FunctionDefRawNode,
}

#[node]
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

#[node]
pub struct ParamsNode(pub ParametersNode);

#[node]
pub enum ParametersNode {
    SlashNoDefault { slash_no_default: SlashNoDefaultNode, param_no_default: Vec<ParamNoDefaultNode>, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    SlashWithDefault { slash_with_default: SlashWithDefaultNode, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    ParamNoDefault { param_no_default: Vec<ParamNoDefaultNode>, param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    ParamWithDefault { param_with_default: Vec<ParamWithDefaultNode>, star_etc: Option<StarEtcNode> },
    StarEtc(StarEtcNode),
}

#[node]
pub struct SlashNoDefaultNode {
    pub params: Vec<ParamNoDefaultNode>,
    pub slash: Token,
}

#[node]
pub struct SlashWithDefaultNode {
    pub params_no_default: Vec<ParamNoDefaultNode>,
    pub params_with_default: Vec<ParamWithDefaultNode>,
    pub slash: Token,
}

#[node]
pub enum StarEtcNode {
    Unannotated { param_no_default: ParamNoDefaultNode, param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    Annotated { param_no_default: ParamNoDefaultStarAnnotationNode, param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    CatchAll { param_maybe_default: Vec<ParamMaybeDefaultNode>, kwds: Option<KwdsNode> },
    Kwds(KwdsNode),
}

#[node]
pub struct KwdsNode {
    pub token: Token,
    pub param: ParamNoDefaultNode,
}

#[node]
pub struct ParamNoDefaultNode(pub ParamNode);
#[node]
pub struct ParamNoDefaultStarAnnotationNode(pub ParamStarAnnotationNode);

#[node]
pub struct ParamWithDefaultNode {
    pub param: ParamNode,
    pub default: DefaultNode,
}

#[node]
pub struct ParamMaybeDefaultNode {
    pub param: ParamNode,
    pub default: Option<DefaultNode>,
}

#[node]
pub struct ParamNode {
    pub name: Token,
    pub annotation: AnnotationNode,
}

#[node]
pub struct ParamStarAnnotationNode {
    pub name: Token,
    pub annotation: StarAnnotationNode,
}

#[node]
pub struct AnnotationNode {
    pub expr: ExpressionNode,
}

#[node]
pub struct StarAnnotationNode {
    pub expr: StarExpressionNode,
}

#[node]
pub struct DefaultNode {
    pub expr: ExpressionNode,
    // | invalid_default
}

// If statement
// ------------

#[node]
pub struct IfStmtNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
    pub block: BlockNode,
    pub else_branch: Option<ElseBranch>,
}

#[node]
pub enum ElseBranch {
    Elif(Box<ElifStmtNode>),
    Else(Box<ElseBlockNode>),
}

#[node]
pub struct ElifStmtNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
    pub block: BlockNode,
    pub else_branch: Option<ElseBranch>,
}

#[node]
pub struct ElseBlockNode {
    pub token: Token,
    pub block: BlockNode,
}

// While statement
// ---------------

#[node]
pub struct WhileStmtNode {
    pub token: Token,
    pub expr: NamedExpressionNode,
    pub block: BlockNode,
    pub else_block: Option<ElseBlockNode>,
}

// For statement
// -------------

#[node]
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

#[node]
pub struct WithStmtNode {
    pub is_async: bool,
    pub items: Vec<WithItemNode>,
    pub block: BlockNode,
}

#[node]
pub struct WithItemNode {
    pub expr: ExpressionNode,
    pub alias: Option<StarTargetNode>,
}

// Try statement
// -------------

#[node]
pub enum TryStmtNode {
    TryFinally { block: BlockNode, finally: FinallyBlockNode },
    TryExcept { block: BlockNode, excepts: Vec<ExceptBlockNode>, else_block: Option<ElseBlockNode>, finally: Option<FinallyBlockNode> },
    TryExceptStar { block: BlockNode, excepts: Vec<ExceptStarBlockNode>, else_block: Option<ElseBlockNode>, finally: Option<FinallyBlockNode> },
}

// Except statement
// ----------------

#[node]
pub enum ExceptBlockNode {
    Except { token: Token, expr: ExpressionNode, block: BlockNode },
    Alias { token: Token, expr: ExpressionNode, alias: Token, block: BlockNode },
    Multi { token: Token, expressions: ExpressionsNode, block: BlockNode },
    CatchAll { token: Token, block: BlockNode },
}

#[node]
pub enum ExceptStarBlockNode {
    Except { token: Token, expr: ExpressionNode, block: BlockNode },
    Alias { token: Token, expr: ExpressionNode, alias: Token, block: BlockNode },
    Multi { token: Token, expressions: ExpressionsNode, block: BlockNode },
}

#[node]
pub struct FinallyBlockNode(pub Token, pub BlockNode);

// Match statement
// ---------------

#[node]
pub struct MatchStmtNode {
    pub token: Token,
    pub subject: SubjectExprNode,
    pub cases: Vec<CaseBlockNode>,
}

#[node]
pub enum SubjectExprNode {
    StarExpr( StarNamedExpressionNode, Option<StarNamedExpressionsNode> ),
    Expr(NamedExpressionNode)
}

#[node]
pub struct CaseBlockNode {
    pub token: Token,
    pub patterns: PatternsNode,
    pub guard: Option<GuardNode>,
    pub block: BlockNode,
}

#[node]
pub struct GuardNode {
    pub if_token: Token,
    pub expr: NamedExpressionNode,
}

#[node]
pub enum PatternsNode {
    OpenSequence(OpenSequencePatternNode),
    Pattern(PatternNode),
}

#[node]
pub struct PatternNode {
    pub or_pattern: OrPatternNode,
    pub alias: Option<PatternCaptureTargetNode>,
}

#[node]
pub struct OrPatternNode {
    pub patterns: Vec<ClosedPatternNode>,
}

#[node]
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

#[node]
pub enum LiteralPatternNode {
    // signed_number !('+' | '-')
    SignedNumber(SignedNumberNode),
    ComplexNumber(ComplexNumberNode),
    Strings(StringsNode),
    None(Token),
    True(Token),
    False(Token),
}

#[node]
pub enum LiteralExprNode {
    // signed_number !('+' | '-')
    SignedNumber(SignedNumberNode),
    ComplexNumber(ComplexNumberNode),
    Strings(StringsNode),
    None(Token),
    True(Token),
    False(Token),
}

#[node]
pub struct ComplexNumberNode {
    pub real: SignedRealNumberNode,
    pub sign: Token,
    pub imaginary: ImaginaryNumberNode,
}

#[node]
pub struct SignedNumberNode {
    pub is_negative: bool,
    pub number: Token,
}

#[node]
pub struct SignedRealNumberNode {
    pub is_negative: bool,
    pub real: RealNumberNode,
}

#[node]
pub struct RealNumberNode(pub Token);

#[node]
pub struct ImaginaryNumberNode(pub Token);

#[node]
pub struct CapturePatternNode(pub PatternCaptureTargetNode);

#[node]
pub struct PatternCaptureTargetNode {
    pub name: Token, // !"_" NAME !('.' | '(' | '=')
}

#[node]
pub struct WildcardPatternNode(pub Token); // "_"

#[node]
pub struct ValuePatternNode {
    pub attr: AttrNode, // attr !('.' | '(' | '=')
}

#[node]
pub struct AttrNode {
    pub name_or_attr: NameOrAttrNode,
    pub name: Token,
}

#[node]
pub enum NameOrAttrNode {
    Attr(Box<AttrNode>),
    Name(Token)
}

#[node]
pub struct GroupPatternNode {
    pub pattern: PatternNode, // '(' pattern ')'
}

#[node]
pub enum SequencePatternNode {
    Maybe(Option<MaybeSequencePatternNode>), // '[' T? ']'
    Open(Option<OpenSequencePatternNode>), // '(' T? ')'
}

#[node]
pub struct OpenSequencePatternNode {
    pub maybe_star_pattern: MaybeStarPatternNode,
    pub maybe_sequence: Option<MaybeSequencePatternNode>,
}

#[node]
pub struct MaybeSequencePatternNode {
    pub patterns: Vec<MaybeStarPatternNode>, // ','.T+ ','?
}

#[node]
pub enum MaybeStarPatternNode {
    StarPattern(StarPatternNode),
    Pattern(PatternNode),
}

#[node]
pub enum StarPatternNode {
    Capture(PatternCaptureTargetNode),
    Wildcard(WildcardPatternNode),
}

#[node]
pub enum MappingPatternNode {
    Empty(Token, Token), // '{' '}'
    DoubleStar(DoubleStarPatternNode),
    ItemsDoubleStar(ItemsPatternNode, DoubleStarPatternNode),
    Items(ItemsPatternNode),
}

#[node]
pub struct ItemsPatternNode {
    pub key_value_patterns: Vec<KeyValuePatternNode>,
}

#[node]
pub struct KeyValuePatternNode {
    pub key: KeyExprAttr,
    pub pattern: PatternNode,
}

#[node]
pub enum KeyExprAttr {
    LiteralExpr(LiteralExprNode),
    Attr(AttrNode),
}

#[node]
pub struct DoubleStarPatternNode {
    pub capture_target: PatternCaptureTargetNode,
}

#[node]
pub struct ClassPatternNode {
    pub name: NameOrAttrNode,
    pub positional: Option<PositionalPatternsNode>,
    pub keyword: Option<KeywordPatternsNode>,
}

#[node]
pub struct PositionalPatternsNode {
    pub patterns: Vec<PatternNode>,
}

#[node]
pub struct KeywordPatternsNode {
    pub patterns: Vec<KeywordPatternNode>,
}

#[node]
pub struct KeywordPatternNode {
    pub name: Token,
    pub pattern: PatternNode,
}

// Type statement
// --------------

#[node]
pub struct TypeAliasNode {
    pub token: Token,
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    pub expr: ExpressionNode,
}

// Type parameter declaration
// --------------------------

#[node]
pub struct TypeParamsNode {
    pub type_param_seq: TypeParamSeqNode,
}

#[node]
pub struct TypeParamSeqNode(pub Vec<TypeParamNode>); // ','.T+ [',']

#[node]
pub enum TypeParamNode {
    Named { name: Token, bound: Option<TypeParamBoundNode>, default: Option<TypeParamDefaultNode> },
    Star { name: Token, default: Option<TypeParamStarredDefaultNode> },
    DoubleStar { name: Token, default: Option<TypeParamDefaultNode> },
}

#[node]
pub struct TypeParamBoundNode(pub ExpressionNode);
#[node]
pub struct TypeParamDefaultNode(pub ExpressionNode);
#[node]
pub struct TypeParamStarredDefaultNode(pub StarExpressionNode);

// EXPRESSIONS
// ===========

#[node]
pub struct ExpressionsNode {
    pub expressions: Vec<ExpressionNode>,
}

#[node]
pub enum ExpressionNode {
    Ternary { truthy: DisjunctionNode, condition: DisjunctionNode, falsey: Box<ExpressionNode> },
    Disjunction(DisjunctionNode),
    Lambda(LambdaDefNode),
}

#[node]
pub enum YieldExprNode {
    From(ExpressionNode),
    Direct(Option<StarExpressionsNode>),
}

#[node]
pub struct StarExpressionsNode(pub Vec<StarExpressionNode>);

#[node]
pub enum StarExpressionNode {
    Starred(BitwiseOrNode),
    Expr(ExpressionNode),
}

#[node]
pub struct StarNamedExpressionsNode(pub Vec<StarNamedExpressionNode>);

#[node]
pub enum StarNamedExpressionNode {
    Starred(BitwiseOrNode),
    Expr(NamedExpressionNode)
}

#[node]
pub struct AssignmentExpressionNode {
    pub name: Token,
    pub expr: ExpressionNode,
}

#[node]
pub enum NamedExpressionNode {
    Assignment(AssignmentExpressionNode),
    Expr(ExpressionNode), // expression !':='
}

#[node]
pub struct DisjunctionNode {
    pub first: ConjunctionNode,
    pub more: Vec<ConjunctionNode>,
}

#[node]
pub struct ConjunctionNode {
    pub first: InversionNode,
    pub more: Vec<InversionNode>,
}

#[node]
pub enum InversionNode {
    Not(Box<InversionNode>),
    Comp(ComparisonNode),
}

// Comparison operators
// --------------------

#[node]
pub struct ComparisonNode {
    pub bitwise_or: Box<BitwiseOrNode>,
    pub compare_pairs: Vec<CompareOpBitwiseOrPair>,
}

#[node]
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

#[node] // TODO: implement manually probably
pub enum BitwiseOrNode {
    Or(Box<BitwiseOrNode>, BitwiseXorNode),
    Xor(BitwiseXorNode)
}

#[node]
pub enum BitwiseXorNode {
    Xor(Box<BitwiseXorNode>, BitwiseAndNode),
    And(BitwiseAndNode)
}

#[node]
pub enum BitwiseAndNode {
    And(Box<BitwiseAndNode>, ShiftExprNode),
    Shift(ShiftExprNode)
}

#[node]
pub enum ShiftExprNode {
    Shl(Box<ShiftExprNode>, SumNode),
    Shr(Box<ShiftExprNode>, SumNode),
    Sum(SumNode)
}

// Arithmetic operators
// --------------------

#[node]
pub enum SumNode {
    Add(Box<SumNode>, TermNode),
    Sub(Box<SumNode>, TermNode),
    Term(TermNode),
}

#[node]
pub enum TermNode {
    Mul(Box<TermNode>, FactorNode),
    Div(Box<TermNode>, FactorNode),
    Floor(Box<TermNode>, FactorNode),
    Mod(Box<TermNode>, FactorNode),
    MatMul(Box<TermNode>, FactorNode),
    Factor(FactorNode),
}

#[node]
pub enum FactorNode {
    Pos(Box<FactorNode>),
    Neg(Box<FactorNode>),
    BitNot(Box<FactorNode>),
    Power(PowerNode),
}

#[node]
pub enum PowerNode {
    Pow(AwaitPrimaryNode, Box<FactorNode>),
    Primary(AwaitPrimaryNode),
}

// Primary elements
// ----------------

#[node]
pub struct AwaitPrimaryNode {
    pub is_await: bool,
    pub primary: PrimaryNode,
}

#[node]
pub enum PrimaryNode {
    DotName(Box<PrimaryNode>, Token),
    Genexp(Box<PrimaryNode>, GenexpNode),
    Call(Box<PrimaryNode>, Option<ArgumentsNode>),
    Slice(Box<PrimaryNode>, SlicesNode),
    Atom(AtomNode),
}

#[node]
pub enum SlicesNode {
    Slice(SliceNode),
    Multi(Vec<SliceOrStarredExpr>),
}

#[node]
pub enum SliceOrStarredExpr {
    Slice(SliceNode),
    Starred(StarredExpressionNode),
}

#[node]
pub enum SliceNode {
    Literal { start: Option<ExpressionNode>, end: Option<ExpressionNode>, step: Option<ExpressionNode> },
    Expr(NamedExpressionNode),
}

#[node]
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

#[node]
pub enum GroupNode {
    Yield(YieldExprNode),
    Named(NamedExpressionNode),
}

// Lambda functions
// ----------------

#[node]
pub struct LambdaDefNode {
    pub token: Token,
    pub params: Option<LambdaParamsNode>,
    pub expr: Box<ExpressionNode>,
}

#[node]
pub struct LambdaParamsNode(pub LambdaParametersNode);

#[node]
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

#[node]
pub struct LambdaSlashNoDefaultNode {
    pub params: Vec<LambdaParamNoDefaultNode>,
    pub slash: Token,
}

#[node]
pub struct LambdaSlashWithDefaultNode {
    pub params_no_default: Vec<LambdaParamNoDefaultNode>,
    pub params_with_default: Vec<LambdaParamWithDefaultNode>,
    pub slash: Token,
}

#[node]
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

#[node]
pub struct LambdaKwdsNode {
    pub token: Token,
    pub param: LambdaParamNoDefaultNode,
}

#[node]
pub struct LambdaParamNoDefaultNode(pub LambdaParamNode);

#[node]
pub struct LambdaParamWithDefaultNode {
    pub param: LambdaParamNode,
    pub default: DefaultNode,
}

#[node]
pub struct LambdaParamMaybeDefaultNode {
    pub param: LambdaParamNode,
    pub default: Option<DefaultNode>,
}

#[node]
pub struct LambdaParamNode {
    pub name: Token,
}



// LITERALS
// ========
#[node]
pub struct StringsNode(pub Token);
#[node]
pub struct ListNode(pub Option<StarNamedExpressionsNode>);
#[node]
pub struct TupleNode(pub Option<TupleNodeInner>);
#[node]
pub struct TupleNodeInner(pub Box<StarNamedExpressionNode>, pub Option<StarNamedExpressionsNode>);

#[node]
pub struct SetNode(pub StarNamedExpressionsNode);

// Dicts
// -----

#[node]
pub struct DictNode(pub Option<DoubleStarredKVPairsNode>);
#[node]
pub struct DoubleStarredKVPairsNode(pub Vec<DoubleStarredKVPairNode>);

#[node]
pub enum DoubleStarredKVPairNode {
    Starred(Box<BitwiseOrNode>),
    Pair(Box<KVPairNode>),
}

#[node]
pub struct KVPairNode(pub ExpressionNode, pub ExpressionNode);

// Comprehensions & Generators
// ---------------------------

#[node]
pub struct ForIfClausesNode(pub Vec<ForIfClauseNode>);

#[node]
pub struct ForIfClauseNode {
    pub is_async: bool,
    pub for_token: Token,
    pub targets: StarTargetsNode,
    pub in_token: Token,
    pub iter: DisjunctionNode,
    pub guards: Vec<ForIfClauseNodeGuardsInner>,
}

#[node]
pub struct ForIfClauseNodeGuardsInner(pub Token, pub DisjunctionNode);

#[node]
pub struct ListCompNode {
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
}

#[node]
pub struct SetCompNode {
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
}

#[node]
pub struct GenexpNode {
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
}

#[node]
pub struct DictCompNode {
    pub kv_pair: KVPairNode,
    pub clauses: ForIfClausesNode,
}

// FUNCTION CALL ARGUMENTS
// =======================

#[node]
pub struct ArgumentsNode(pub ArgsNode);

#[node]
pub enum ArgsNode {
    Expr(Vec<StarredOrNamedExpr>, Option<KwargsNode>),
    Kwargs(KwargsNode),
}

#[node]
pub enum StarredOrNamedExpr {
    Starred(StarredExpressionNode),
    Named(NamedExpressionNode),
}

#[node]
pub struct KwargsNode {
    pub kwargs_or_starred: Vec<KwargOrStarredNode>,
    pub kwargs_or_double_starred: Vec<KwargOrDoubleStarredNode>,
}

#[node]
pub struct StarredExpressionNode(pub ExpressionNode);

#[node]
pub enum KwargOrStarredNode {
    Kwarg(Token, ExpressionNode),
    Starred(StarredExpressionNode),
}

#[node]
pub enum KwargOrDoubleStarredNode {
    Kwarg(Token, ExpressionNode),
    Starred(ExpressionNode),
}

// ASSIGNMENT TARGETS
// ==================

// Generic targets
// ---------------

#[node]
pub struct StarTargetsNode(pub Vec<StarTargetNode>);
#[node]
pub struct StarTargetsListSeq(pub Vec<StarTargetNode>);
#[node]
pub struct StarTargetsTupleSeq(pub Vec<StarTargetNode>);

#[node]
pub struct StarTargetNode {
    pub is_starred: bool,
    pub target: TargetWithStarAtomNode,
}

#[node]
pub enum TargetWithStarAtomNode {
    Named(TPrimaryNode, Token), // !t_lookahead
    Slices(TPrimaryNode, SliceNode), // !t_lookahead
    StarAtom(StarAtomNode),
}

#[node]
pub enum StarAtomNode {
    Name(Token),
    Expr(Box<TargetWithStarAtomNode>),
    Tuple(StarTargetsTupleSeq),
    List(StarTargetsListSeq),
}

#[node]
pub enum SingleTargetNode {
    Subscript(SingleSubscriptAttributeTargetNode),
    Named(Token),
    // '(' single_target ')'
}

#[node]
pub enum SingleSubscriptAttributeTargetNode {
    Named(TPrimaryNode, Token), // !t_lookahead
    Slices(TPrimaryNode, SlicesNode), // !t_lookahead
}

#[node] // TODO: maybe implement manually...
pub enum TPrimaryNode {
    Atom(Box<AtomNode>), // &t_lookahead
    Named(Box<Self>, Token), // '.' NAME &t_lookahead
    Slices(Box<Self>, SliceNode), // '[' slices ']' &t_lookahead
    Call(Box<Self>, Option<ArgumentsNode>), // &t_lookahead
    Genexp(Box<Self>, GenexpNode), // &t_lookahead
}

// t_lookahead: '(' | '[' | '.'

// Targets for del statements
// --------------------------

#[node]
pub struct DelTargetsNode(pub Vec<DelTargetNode>);

#[node]
pub enum DelTargetNode {
    Named(TPrimaryNode, Token), // '.' NAME !t_lookahead
    Slices(TPrimaryNode, SlicesNode), // '[' slices ']' !t_lookahead
    Atom(DelTAtomNode),
}

#[node]
pub enum DelTAtomNode {
    Name(Token),
    Single(Box<DelTargetNode>),
    Tuple(Option<DelTargetsNode>),
    List(Option<DelTargetsNode>),
}


