use std::fmt::Debug;
use node_macro::{iterative_node, node};
use crate::core::types::*;
use anyhow::{Result, anyhow, Ok};

pub trait Node
where
    Self: Sized + Debug
{
    fn parse_debug(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>> {
        let x = Self::parse(tokens, invalid_pass)?;
        let name = std::any::type_name::<Self>();
        Ok(x)
    }
    fn parse(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>>;
}

#[node]
pub struct FileNode {
    #[token(TokenValue::Newline)]
    newlines: Option<()>,
    pub statements: Option<StatementsNode>,
    #[token(TokenValue::EndMarker)]
    _end_marker: ()
}

#[node]
pub struct EvalNode(
    pub ExpressionsNode,
    #[token(TokenValue::Newline)]
    pub Vec<Token>,
    #[token(TokenValue::EndMarker)] ()
);

// GENERAL STATEMENTS
// ==================

#[node]
pub struct StatementsNode(#[one_or_more] pub Vec<StatementNode>);

#[node]
pub struct StatementNode(
    pub StatementNodeInner,
);

#[node]
pub enum StatementNodeInner {
    CompoundStatement(CompoundStmtNode),
    SimpleStatements(SimpleStmtsNode),
}

#[node]
pub struct SimpleStmtsNode(
    #[sep(TokenValue::Symbol(Symbol::Semicolon), trailing)]
    pub Vec<SimpleStmtNode>,
    #[token(TokenValue::Newline)] ()
);

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
pub struct AssignmentNodeValueInner(
    #[token(TokenValue::Symbol(Symbol::Assign))] (),
    pub AnnotatedRhsNode
);

#[node]
pub struct AssignmentNodeChainedTargetsInner(
    pub StarTargetsNode,
    #[token(TokenValue::Symbol(Symbol::Assign))] ()
);

#[node]
pub enum AssignmentNode {
    Typed {
        #[token(TokenValue::Word(_))]
        name: Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        annotation: ExpressionNode,
        value: Option<AssignmentNodeValueInner>
    },
    Unpacking {
        target: AssignmentNodeTarget,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
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
        #[errors(err("Expected expression after {operator}"))]
        value: AnnotatedRhsNode
    },
}

#[node]
pub enum AssignmentNodeTarget {
    SingleTarget(
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        SingleTargetNode,
        #[token(TokenValue::Symbol(Symbol::RParen))] ()
    ),
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
    pub value: Option<StarExpressionsNode>,
}


#[node]
pub struct RaiseStmtNodeFromInner(
    #[token(TokenValue::Keyword(Keyword::From))] (),
    pub ExpressionNode
);

#[node]
pub enum RaiseStmtNode {
    RaiseNew {
        #[token(TokenValue::Keyword(Keyword::Raise))]
        token: Token,
        value: ExpressionNode,
        from: Option<RaiseStmtNodeFromInner>
    },
    ReRaise(#[token(TokenValue::Keyword(Keyword::Raise))] Token),
}

#[node]
pub struct PassStmtNode(#[token(TokenValue::Keyword(Keyword::Pass))] pub Token);

#[node]
pub struct BreakStmtNode(#[token(TokenValue::Keyword(Keyword::Break))] pub Token);

#[node]
pub struct ContinueStmtNode(#[token(TokenValue::Keyword(Keyword::Continue))] pub Token);


#[node]
pub struct GlobalStmtNode {
    #[token(TokenValue::Keyword(Keyword::Global))]
    pub token: Token,
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    #[token(TokenValue::Word(_))]
    pub names: Vec<Token>,
}

// 'nonlocal' ','.NAME+
#[node]
pub struct NonlocalStmtNode {
    #[token(TokenValue::Keyword(Keyword::Nonlocal))]
    pub token: Token,
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    #[token(TokenValue::Word(_))]
    pub names: Vec<Token>,
}

#[node]
pub struct DelStmtNode {
    #[token(TokenValue::Keyword(Keyword::Del))]
    pub token: Token,
    pub targets: DelTargetsNode,
    #[pass_if(TokenValue::Symbol(Symbol::Semicolon) | TokenValue::Newline)]
    _lookahead: ()
}

#[node]
pub struct YieldStmtNode(pub YieldExprNode);

#[node]
pub struct AssertStmtNodeErrorInner(
    #[token(TokenValue::Symbol(Symbol::Comma))] (),
    pub ExpressionNode
);

#[node]
pub struct AssertStmtNode {
    #[token(TokenValue::Keyword(Keyword::Assert))]
    pub token: Token,
    pub expr: ExpressionNode,
    pub error: Option<AssertStmtNodeErrorInner>,
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
    #[token(TokenValue::Keyword(Keyword::Import))]
    pub token: Token,
    pub names: DottedAsNamesNode,
}

#[node]
pub enum ImportFromNode {
    DottedName {
        #[token(TokenValue::Keyword(Keyword::From))]
        from: Token,
        #[token(TokenValue::Ellipsis | TokenValue::Symbol(Symbol::Dot))]
        dots: Vec<Token>,
        name: DottedNameNode,
        #[token(TokenValue::Keyword(Keyword::Import))]
        import: Token,
        targets: ImportFromTargetsNode,
    },
    Relative {
        #[token(TokenValue::Keyword(Keyword::From))]
        from: Token,
        #[one_or_more]
        #[token(TokenValue::Ellipsis | TokenValue::Symbol(Symbol::Dot))]
        dots: Vec<Token>,
        #[token(TokenValue::Keyword(Keyword::Import))]
        import: Token,
        targets: ImportFromTargetsNode,
    },
}

#[node]
pub enum ImportFromTargetsNode {
    TupleCapture(#[token(TokenValue::Symbol(Symbol::LParen))] (), ImportFromAsNamesNode, #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>, #[token(TokenValue::Symbol(Symbol::RParen))] ()),
    OpenCapture(ImportFromAsNamesNode, #[fail_if(TokenValue::Symbol(Symbol::Comma))] ()),
    Wildcard(#[token(TokenValue::Operator(Operator::Mul))] Token),
}

#[node]
pub struct ImportFromAsNamesNode {
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    pub names: Vec<ImportFromAsNameNode>,
}

#[node]
pub struct ImportFromAsNameNodeAliasInner(#[token(TokenValue::Keyword(Keyword::As))] (), #[token(TokenValue::Word(_))] pub Token);

#[node]
pub struct ImportFromAsNameNode {
    #[token(TokenValue::Word(_))]
    pub name: Token,
    pub alias: Option<ImportFromAsNameNodeAliasInner>,
}

#[node]
pub struct DottedAsNamesNode {
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    pub names: Vec<DottedAsNameNode>,
}

#[node]
pub struct DottedAsNameNode {
    pub name: DottedNameNode,
    pub alias: Option<ImportFromAsNameNodeAliasInner>,
}

#[derive(Debug)]
pub enum DottedNameNode {
    Attr { parent: Box<DottedNameNode>, name: Token },
    Name(Token),
}
impl Node for DottedNameNode {
    fn parse(tokens: &mut ParseTokens, _: bool) -> Result<Option<Self>> {
        tokens.snapshot();
        let name_tk = if let Some(tk) = tokens.get(0) {
            if matches!(tk.value, TokenValue::Word(_)) {
                tokens.consume_next().unwrap().clone()
            } else {
                tokens.restore();
                return Ok(None)
            }
        } else {
            tokens.restore();
            return Ok(None)
        };
        let mut a = Self::Name(name_tk);
        loop {
            tokens.discard_snapshot();
            tokens.snapshot();
            if let Some(tk) = tokens.get(0) {
                if !matches!(tk.value, TokenValue::Symbol(Symbol::Dot)) {
                    break
                }
                tokens.consume_next();
                let name_tk = if let Some(tk) = tokens.get(0) {
                    if matches!(tk.value, TokenValue::Word(_)) {
                        tokens.consume_next().unwrap().clone()
                    } else {
                        tokens.restore();
                        return Ok(None)
                    }
                } else {
                    tokens.restore();
                    return Ok(None)
                };
                a = Self::Attr { parent: Box::new(a), name: name_tk }
            } else {
                break
            }
        }
        tokens.discard_snapshot();
        Ok(Some(a))
    }
}

// COMPOUND STATEMENTS
// ===================

// Common elements
// ---------------

#[node]
pub enum BlockNode {
    Statements(
        #[token(TokenValue::Newline)] (),
        #[token(TokenValue::Indent)] (),
        StatementsNode,
        #[token(TokenValue::Dedent)] ()
    ),
    Simple(SimpleStmtsNode),
}

#[node]
pub struct DecoratorsNode {
    #[one_or_more]
    pub decorators: Vec<DecoratorNode>
}

#[node]
pub struct DecoratorNode {
    #[token(TokenValue::Operator(Operator::MatMul))]
    pub token: Token,
    pub expr: NamedExpressionNode,
    #[token(TokenValue::Newline)]
    _nl: ()
}

// Class definitions
// -----------------

#[node]
pub struct ClassDefNode {
    pub decorators: Option<DecoratorsNode>,
    pub class_def_raw: ClassDefRawNode,
}

#[node]
pub struct ClassDefRawNodeArgumentsInner(
    #[token(TokenValue::Symbol(Symbol::LParen))] (),
    pub Option<ArgumentsNode>,
    #[token(TokenValue::Symbol(Symbol::RParen))] ()
);

#[node]
pub struct ClassDefRawNode {
    #[token(TokenValue::Keyword(Keyword::Class))]
    pub token: Token,
    #[token(TokenValue::Word(_))]
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    pub arguments: Option<ClassDefRawNodeArgumentsInner>,
    #[token(TokenValue::Symbol(Symbol::Colon))]
    c: (),
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
pub struct FunctionDefRawNodeReturnInner(
    #[token(TokenValue::Symbol(Symbol::Arrow))] (),
    pub ExpressionNode
);

#[node]
pub struct FunctionDefRawNode {
    #[token(TokenValue::Keyword(Keyword::Async))]
    pub is_async: bool,
    #[token(TokenValue::Keyword(Keyword::Def))]
    pub def: Token,
    #[token(TokenValue::Word(_))]
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    #[token(TokenValue::Symbol(Symbol::LParen))]
    lp: (),
    pub params: Option<ParamsNode>,
    #[token(TokenValue::Symbol(Symbol::RParen))]
    rp: (),
    pub return_type: Option<FunctionDefRawNodeReturnInner>,
    #[token(TokenValue::Symbol(Symbol::Colon))]
    c: (),
    pub block: BlockNode,
}

// Function parameters
// -------------------

#[node]
pub struct ParamsNode(pub ParametersNode);

#[node]
pub enum ParametersNode {
    SlashNoDefault {
        slash_no_default: SlashNoDefaultNode,
        param_no_default: Vec<ParamNoDefaultNode>,
        param_with_default: Vec<ParamWithDefaultNode>,
        star_etc: Option<StarEtcNode>
    },
    SlashWithDefault {
        slash_with_default: SlashWithDefaultNode,
        param_with_default: Vec<ParamWithDefaultNode>,
        star_etc: Option<StarEtcNode>
    },
    ParamNoDefault {
        #[one_or_more]
        param_no_default: Vec<ParamNoDefaultNode>,
        param_with_default: Vec<ParamWithDefaultNode>,
        star_etc: Option<StarEtcNode>
    },
    ParamWithDefault {
        #[one_or_more]
        param_with_default: Vec<ParamWithDefaultNode>,
        star_etc: Option<StarEtcNode>
    },
    StarEtc(StarEtcNode),
}

#[node]
pub enum CommaOrRParenNext {
    Comma(#[token(TokenValue::Symbol(Symbol::Comma))] ()),
    RParenNext(#[pass_if(TokenValue::Symbol(Symbol::RParen))] ()),
}

#[node]
pub struct SlashNoDefaultNode {
    #[one_or_more]
    pub param_no_default: Vec<ParamNoDefaultNode>,
    #[token(TokenValue::Operator(Operator::Div))]
    s: (),
    _c: CommaOrRParenNext,
}

#[node]
pub struct SlashWithDefaultNode {
    pub params_no_default: Vec<ParamNoDefaultNode>,
    #[one_or_more]
    pub params_with_default: Vec<ParamWithDefaultNode>,
    #[token(TokenValue::Operator(Operator::Div))]
    _t: (),
    _c: CommaOrRParenNext,
}

#[node]
pub enum StarEtcNode {
    Unannotated {
        #[prefix(token(TokenValue::Operator(Operator::Mul)))]
        param_no_default: ParamNoDefaultNode,
        param_maybe_default: Vec<ParamMaybeDefaultNode>,
        kwds: Option<KwdsNode>
    },
    Annotated {
        #[prefix(token(TokenValue::Operator(Operator::Mul)))]
        param_no_default: ParamNoDefaultStarAnnotationNode,
        param_maybe_default: Vec<ParamMaybeDefaultNode>,
        kwds: Option<KwdsNode>
    },
    CatchAll {
        #[prefix(token(TokenValue::Operator(Operator::Mul)), token(TokenValue::Symbol(Symbol::Comma)))]
        #[one_or_more]
        param_maybe_default: Vec<ParamMaybeDefaultNode>,
        kwds: Option<KwdsNode>
    },
    Kwds(KwdsNode),
}

#[node]
pub struct KwdsNode {
    #[token(TokenValue::Operator(Operator::Pow))]
    pub token: Token,
    pub param: ParamNoDefaultNode,
}

#[node]
pub struct ParamNoDefaultNode(pub ParamNode, #[skip] CommaOrRParenNext);

#[node]
pub struct ParamNoDefaultStarAnnotationNode(pub ParamStarAnnotationNode, #[skip] CommaOrRParenNext);

#[node]
pub struct ParamWithDefaultNode {
    pub param: ParamNode,
    pub default: DefaultNode,
    #[skip]
    _c: CommaOrRParenNext,
}

#[node]
pub struct ParamMaybeDefaultNode {
    pub param: ParamNode,
    pub default: Option<DefaultNode>,
    #[skip]
    _c: CommaOrRParenNext,
}

#[node]
pub struct ParamNode {
    #[token(TokenValue::Word(_))]
    pub name: Token,
    pub annotation: Option<AnnotationNode>,
}

#[node]
pub struct ParamStarAnnotationNode {
    #[token(TokenValue::Word(_))]
    pub name: Token,
    pub annotation: StarAnnotationNode,
}

#[node]
pub struct AnnotationNode {
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub expr: ExpressionNode,
}

#[node]
pub struct StarAnnotationNode {
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub expr: StarExpressionNode,
}

#[node]
pub struct DefaultNode {
    #[prefix(token(TokenValue::Symbol(Symbol::Assign)))]
    pub expr: ExpressionNode,
    // | invalid_default
}

// If statement
// ------------

#[node]
pub struct IfStmtNode {
    #[token(TokenValue::Keyword(Keyword::If))]
    pub token: Token,
    pub expr: NamedExpressionNode,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub block: BlockNode,
    pub else_branch: ElseBranch,
}

#[node]
pub enum ElseBranch {
    Elif(Box<ElifStmtNode>),
    Else(Option<ElseBlockNode>),
}

#[node]
pub struct ElifStmtNode {
    #[token(TokenValue::Keyword(Keyword::Elif))]
    pub token: Token,
    pub expr: NamedExpressionNode,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub block: BlockNode,
    pub else_branch: ElseBranch,
}

#[node]
pub struct ElseBlockNode {
    #[token(TokenValue::Keyword(Keyword::Else))]
    pub token: Token,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub block: BlockNode,
}

// While statement
// ---------------

#[node]
pub struct WhileStmtNode {
    #[token(TokenValue::Keyword(Keyword::While))]
    pub token: Token,
    pub expr: NamedExpressionNode,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub block: BlockNode,
    pub else_block: Option<ElseBlockNode>,
}

// For statement
// -------------

#[node]
pub struct ForStmtNode {
    #[token(TokenValue::Keyword(Keyword::Async))]
    pub is_async: bool,
    #[token(TokenValue::Keyword(Keyword::For))]
    pub for_token: Token,
    pub targets: StarTargetsNode,
    #[token(TokenValue::Keyword(Keyword::In))]
    pub in_token: Token,
    #[commit]
    #[errors(err("expected value to iterate over after {in_token}"))]
    pub star_expr: StarExpressionsNode,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    #[errors(
        err("Expected ':', got {}", tokens.get(0).unwrap()),
        err("Expected ':', got EOF"),
        err("Expected block after for ... in ...: @ {}", for_token.span),
    )]
    pub block: BlockNode,
    pub else_block: Option<ElseBlockNode>,
}

// With statement
// --------------

#[node]
pub enum WithItemsInner {
    Parenthesized(
        #[token(TokenValue::Symbol(Symbol::LParen))]
        (),
        #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
        Vec<WithItemNode>,
        #[token(TokenValue::Symbol(Symbol::RParen))]
        ()
    ),
    Open(
        #[sep(TokenValue::Symbol(Symbol::Comma))]
        Vec<WithItemNode>
    )
}

#[node]
pub struct WithStmtNode {
    #[token(TokenValue::Keyword(Keyword::Async))]
    pub is_async: bool,
    #[token(TokenValue::Keyword(Keyword::With))]
    pub token: Token,
    pub items: WithItemsInner,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub block: BlockNode,
}

#[node]
pub struct WithItemTargetInner(
    #[prefix(token(TokenValue::Keyword(Keyword::As)))]
    pub StarTargetNode,
    #[pass_if(TokenValue::Symbol(Symbol::Comma | Symbol::RParen | Symbol::Colon))]
    ()
);

#[node]
pub struct WithItemNode {
    pub expr: ExpressionNode,
    pub alias: Option<WithItemTargetInner>,
}

// Try statement
// -------------

#[node]
pub enum TryStmtNode {
    TryFinally {
        #[token(TokenValue::Keyword(Keyword::Try))]
        token: Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode,
        finally: FinallyBlockNode
    },
    TryExcept {
        #[token(TokenValue::Keyword(Keyword::Try))]
        token: Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode,
        #[one_or_more]
        excepts: Vec<ExceptBlockNode>,
        else_block: Option<ElseBlockNode>,
        finally: Option<FinallyBlockNode>
    },
    TryExceptStar {
        #[token(TokenValue::Keyword(Keyword::Try))]
        token: Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode,
        #[one_or_more]
        excepts: Vec<ExceptStarBlockNode>,
        else_block: Option<ElseBlockNode>,
        finally: Option<FinallyBlockNode>
    },
}

// Except statement
// ----------------

#[node]
pub enum ExceptBlockNode {
    Except {
        #[token(TokenValue::Keyword(Keyword::Except))]
        token: Token,
        expr: ExpressionNode,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode
    },
    Alias {
        #[token(TokenValue::Keyword(Keyword::Except))]
        token: Token,
        expr: ExpressionNode,
        #[token(TokenValue::Keyword(Keyword::As))]
        as_tk: (),
        #[token(TokenValue::Word(_))]
        alias: Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode
    },
    Multi {
        #[token(TokenValue::Keyword(Keyword::Except))]
        token: Token,
        expressions: ExpressionsNode,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode
    },
    CatchAll {
        #[token(TokenValue::Keyword(Keyword::Except))]
        token: Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode
    },
}

#[node]
pub enum ExceptStarBlockNode {
    Except {
        #[token(TokenValue::Keyword(Keyword::Except))]
        token: Token,
        #[prefix(token(TokenValue::Operator(Operator::Mul)))]
        expr: ExpressionNode,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode
    },
    Alias {
        #[token(TokenValue::Keyword(Keyword::Except))]
        token: Token,
        #[prefix(token(TokenValue::Operator(Operator::Mul)))]
        expr: ExpressionNode,
        #[token(TokenValue::Keyword(Keyword::As))]
        as_tk: (),
        #[token(TokenValue::Word(_))]
        alias: Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode
    },
    Multi {
        #[token(TokenValue::Keyword(Keyword::Except))]
        token: Token,
        #[prefix(token(TokenValue::Operator(Operator::Mul)))]
        expressions: ExpressionsNode,
        #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
        block: BlockNode
    },
}

#[node]
pub struct FinallyBlockNode(
    #[token(TokenValue::Keyword(Keyword::Finally))]
    pub Token,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub BlockNode
);

// Match statement
// ---------------

#[node]
pub struct MatchStmtNode {
    #[token(TokenValue::Word(w) if w == "match")]
    pub token: Token,
    pub subject: SubjectExprNode,
    #[pattern(token(TokenValue::Symbol(Symbol::Colon)), token(TokenValue::Newline), token(TokenValue::Indent))]
    skip: (),
    #[one_or_more]
    pub cases: Vec<CaseBlockNode>,
    #[token(TokenValue::Dedent)]
    de: (),
}

#[node]
pub enum SubjectExprNode {
    StarExpr(
        StarNamedExpressionNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] (),
        Option<StarNamedExpressionsNode>
    ),
    Expr(NamedExpressionNode)
}

#[node]
pub struct CaseBlockNode {
    #[token(TokenValue::Word(w) if w == "case")]
    pub token: Token,
    pub patterns: PatternsNode,
    pub guard: Option<GuardNode>,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub block: BlockNode,
}

#[node]
pub struct GuardNode {
    #[token(TokenValue::Keyword(Keyword::If))]
    pub if_token: Token,
    pub expr: NamedExpressionNode,
}

#[node]
pub enum PatternsNode {
    OpenSequence(OpenSequencePatternNode),
    Pattern(PatternNode),
}

#[node]
pub struct PatternNodeAliasInner(
    #[token(TokenValue::Keyword(Keyword::As))] (),
    pub PatternCaptureTargetNode
);

#[node]
pub struct PatternNode {
    pub or_pattern: OrPatternNode,
    pub alias: Option<PatternNodeAliasInner>,
}

#[node]
pub struct OrPatternNode {
    #[sep(TokenValue::Operator(Operator::BitOr))]
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
    SignedNumber(
        SignedNumberNode,
        #[fail_if(TokenValue::Operator(Operator::Add | Operator::Sub))] ()
    ),
    ComplexNumber(ComplexNumberNode),
    Strings(StringsNode),
    None(#[token(TokenValue::None)] Token),
    True(#[token(TokenValue::BooleanLiteral(true))] Token),
    False(#[token(TokenValue::BooleanLiteral(false))] Token),
}

#[node]
pub enum LiteralExprNode {
    SignedNumber(
        SignedNumberNode,
        #[fail_if(TokenValue::Operator(Operator::Add | Operator::Sub))] ()
    ),
    ComplexNumber(ComplexNumberNode),
    Strings(StringsNode),
    None(#[token(TokenValue::None)] Token),
    True(#[token(TokenValue::BooleanLiteral(true))] Token),
    False(#[token(TokenValue::BooleanLiteral(false))] Token),
}

#[node]
pub struct ComplexNumberNode {
    pub real: SignedRealNumberNode,
    #[token(TokenValue::Operator(Operator::Add | Operator::Sub))]
    pub sign: Token,
    pub imaginary: ImaginaryNumberNode,
}

#[node]
pub struct SignedNumberNode {
    #[token(TokenValue::Operator(Operator::Sub))]
    pub is_negative: bool,
    #[token(TokenValue::NumberLiteral(_))]
    pub number: Token,
}

#[node]
pub struct SignedRealNumberNode {
    #[token(TokenValue::Operator(Operator::Sub))]
    pub is_negative: bool,
    pub real: RealNumberNode,
}

#[node]
pub struct RealNumberNode(#[token(TokenValue::NumberLiteral(_))] pub Token);

#[node]
pub struct ImaginaryNumberNode(#[token(TokenValue::NumberLiteral(_))] pub Token);

#[node]
pub struct CapturePatternNode(pub PatternCaptureTargetNode);

#[node]
pub struct PatternCaptureTargetNode {
    #[fail_if(TokenValue::Word(w) if w == "_")]
    np: (),
    #[token(TokenValue::Word(_))]
    pub name: Token,
    #[fail_if(TokenValue::Symbol(Symbol::Dot | Symbol::LParen | Symbol::Assign))]
    na: (),
}

#[node]
pub struct WildcardPatternNode(#[token(TokenValue::Word(w) if w == "_")] pub Token);

#[node]
pub struct ValuePatternNode {
    pub attr: AttrNode,
    #[fail_if(TokenValue::Symbol(Symbol::Dot | Symbol::LParen | Symbol::Assign))]
    na: (),
}

#[derive(Debug)]
pub struct AttrNode {
    pub name_or_attr: NameOrAttrNode,
    pub name: Token,
}
impl Node for AttrNode {
    fn parse(tokens: &mut ParseTokens, _: bool) -> Result<Option<Self>> {
        tokens.snapshot();
        let tk = if let Some(tk) = tokens.get(0) {
            if matches!(tk.value, TokenValue::Word(_)) {
                tokens.consume_next().unwrap().clone()
            } else {
                tokens.restore();
                return Ok(None)
            }
        } else {
            tokens.restore();
            return Ok(None)
        };
        let mut a = NameOrAttrNode::Name(tk);

        loop {
            tokens.discard_snapshot();
            tokens.snapshot();
            if if let Some(tk) = tokens.get(0) {
                matches!(tk.value, TokenValue::Symbol(Symbol::Dot))
            } else { false } {
                tokens.consume_next();
                let tk = if let Some(tk) = tokens.get(0) {
                    if matches!(tk.value, TokenValue::Word(_)) {
                        tokens.consume_next().unwrap().clone()
                    } else {
                        tokens.restore();
                        break;
                    }
                } else {
                    tokens.restore();
                    break;
                };
                a = NameOrAttrNode::Attr(Box::new(Self { name_or_attr: a, name: tk }))
            } else {
                tokens.restore();
                break;
            }
        }

        match a {
            NameOrAttrNode::Attr(boxed) => {
                tokens.discard_snapshot();
                Ok(Some(*boxed))
            }
            NameOrAttrNode::Name(_) => {
                tokens.restore();
                Ok(None)
            }
        }

    }
}

#[node]
pub enum NameOrAttrNode {
    Attr(Box<AttrNode>),
    Name(#[token(TokenValue::Word(_))] Token)
}

#[node]
pub struct GroupPatternNode(
    #[token(TokenValue::Symbol(Symbol::LParen))] (),
    pub PatternNode,
    #[token(TokenValue::Symbol(Symbol::RParen))] ()
);

#[node]
pub enum SequencePatternNode {
    Maybe(
        #[token(TokenValue::Symbol(Symbol::LBracket))] (),
        Option<MaybeSequencePatternNode>,
        #[token(TokenValue::Symbol(Symbol::RBracket))] ()
    ),
    Open(
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        Option<OpenSequencePatternNode>,
        #[token(TokenValue::Symbol(Symbol::RParen))] ()
    ),
}

#[node]
pub struct OpenSequencePatternNode {
    pub maybe_star_pattern: MaybeStarPatternNode,
    #[token(TokenValue::Symbol(Symbol::Comma))]
    c: (),
    pub maybe_sequence: Option<MaybeSequencePatternNode>,
}

#[node]
pub struct MaybeSequencePatternNode {
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub patterns: Vec<MaybeStarPatternNode>,
}

#[node]
pub enum MaybeStarPatternNode {
    StarPattern(StarPatternNode),
    Pattern(PatternNode),
}

#[node]
pub enum StarPatternNode {
    Capture(#[prefix(token(TokenValue::Operator(Operator::Pow)))] PatternCaptureTargetNode),
    Wildcard(#[prefix(token(TokenValue::Operator(Operator::Pow)))] WildcardPatternNode),
}

#[node]
pub enum MappingPatternNode {
    Empty(
        #[token(TokenValue::Symbol(Symbol::LBrace))] (),
        #[token(TokenValue::Symbol(Symbol::RBrace))] ()
    ),
    DoubleStar(
        #[token(TokenValue::Symbol(Symbol::LBrace))] (),
        DoubleStarPatternNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>,
        #[token(TokenValue::Symbol(Symbol::RBrace))] ()
    ),
    ItemsDoubleStar(
        #[token(TokenValue::Symbol(Symbol::LBrace))] (),
        ItemsPatternNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] (),
        DoubleStarPatternNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>,
        #[token(TokenValue::Symbol(Symbol::RBrace))] ()
    ),
    Items(
        #[token(TokenValue::Symbol(Symbol::LBrace))] (),
        ItemsPatternNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>,
        #[token(TokenValue::Symbol(Symbol::RBrace))] ()
    ),
}

#[node]
pub struct ItemsPatternNode {
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    pub key_value_patterns: Vec<KeyValuePatternNode>,
}

#[node]
pub struct KeyValuePatternNode {
    pub key: KeyExprAttr,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub pattern: PatternNode,
}

#[node]
pub enum KeyExprAttr {
    LiteralExpr(LiteralExprNode),
    Attr(AttrNode),
}

#[node]
pub struct DoubleStarPatternNode {
    #[prefix(token(TokenValue::Operator(Operator::Pow)))]
    pub capture_target: PatternCaptureTargetNode,
}

#[node]
pub enum ClassPatternNode {
    Empty(
        NameOrAttrNode,
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    ),
    Positional(
        NameOrAttrNode,
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        PositionalPatternsNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    ),
    Keyword(
        NameOrAttrNode,
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        KeywordPatternsNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    ),
    Both(
        NameOrAttrNode,
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        PositionalPatternsNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] (),
        KeywordPatternsNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    )
}

#[node]
pub struct PositionalPatternsNode {
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    pub patterns: Vec<PatternNode>,
}

#[node]
pub struct KeywordPatternsNode {
    #[sep(TokenValue::Symbol(Symbol::Comma))]
    pub patterns: Vec<KeywordPatternNode>,
}

#[node]
pub struct KeywordPatternNode {
    #[token(TokenValue::Word(_))]
    pub name: Token,
    #[prefix(token(TokenValue::Symbol(Symbol::Assign)))]
    pub pattern: PatternNode,
}

// Type statement
// --------------

#[node]
pub struct TypeAliasNode {
    #[token(TokenValue::Keyword(Keyword::Type))]
    pub token: Token,
    #[token(TokenValue::Word(_))]
    pub name: Token,
    pub type_params: Option<TypeParamsNode>,
    #[prefix(token(TokenValue::Symbol(Symbol::Assign)))]
    pub expr: ExpressionNode,
}

// Type parameter declaration
// --------------------------

#[node]
pub struct TypeParamsNode(
    #[token(TokenValue::Symbol(Symbol::LBracket))] (),
    pub TypeParamSeqNode,
    #[token(TokenValue::Symbol(Symbol::RBracket))] ()
);

#[node]
pub struct TypeParamSeqNode(
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub Vec<TypeParamNode>
);

#[node]
pub enum TypeParamNode {
    Named {
        #[token(TokenValue::Word(_))]
        name: Token,
        bound: Option<TypeParamBoundNode>,
        default: Option<TypeParamDefaultNode>
    },
    Star {
        #[token(TokenValue::Operator(Operator::Mul))]
        s: (),
        #[token(TokenValue::Word(_))]
        name: Token,
        default: Option<TypeParamStarredDefaultNode>
    },
    DoubleStar {
        #[token(TokenValue::Operator(Operator::Pow))]
        s: (),
        #[token(TokenValue::Word(_))]
        name: Token,
        default: Option<TypeParamDefaultNode>
    },
}

#[node]
pub struct TypeParamBoundNode(
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub ExpressionNode
);

#[node]
pub struct TypeParamDefaultNode(
    #[prefix(token(TokenValue::Symbol(Symbol::Assign)))]
    pub ExpressionNode
);

#[node]
pub struct TypeParamStarredDefaultNode(
    #[prefix(token(TokenValue::Symbol(Symbol::Assign)))]
    pub StarExpressionNode
);

// EXPRESSIONS
// ===========

#[node]
pub struct ExpressionsNode {
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub expressions: Vec<ExpressionNode>,
}

#[node]
pub enum ExpressionNode {
    Ternary {
        truthy: DisjunctionNode,
        #[token(TokenValue::Keyword(Keyword::If))]
        i: (),
        condition: DisjunctionNode,
        #[token(TokenValue::Keyword(Keyword::Else))]
        e: (),
        falsey: Box<ExpressionNode>
    },
    Disjunction(DisjunctionNode),
    Lambda(Box<LambdaDefNode>),
}

#[node]
pub enum YieldExprNode {
    From(
        #[token(TokenValue::Keyword(Keyword::Yield))]
        Token,
        #[token(TokenValue::Keyword(Keyword::From))]
        Token,
        ExpressionNode
    ),
    Direct(
        #[token(TokenValue::Keyword(Keyword::Yield))]
        Token,
        Option<StarExpressionsNode>
    ),
}

#[node]
pub struct StarExpressionsNode(
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub Vec<StarExpressionNode>
);

#[node]
pub enum StarExpressionNode {
    Starred(#[prefix(token(TokenValue::Operator(Operator::Mul)))] BitwiseOrNode),
    Expr(ExpressionNode),
}

#[node]
pub struct StarNamedExpressionsNode(
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub Vec<StarNamedExpressionNode>
);

#[node]
pub enum StarNamedExpressionNode {
    Starred(#[prefix(token(TokenValue::Operator(Operator::Mul)))] Box<BitwiseOrNode>),
    Expr(NamedExpressionNode)
}

#[node]
pub struct AssignmentExpressionNode {
    #[token(TokenValue::Word(_))]
    pub name: Token,
    #[token(TokenValue::Symbol(Symbol::Walrus))]
    w: (),
    #[commit]
    #[errors(err("Expected ':=' after {name}"))]
    pub expr: ExpressionNode,
}

#[node]
pub enum NamedExpressionNode {
    Assignment(AssignmentExpressionNode),
    Expr(ExpressionNode, #[fail_if(TokenValue::Symbol(Symbol::Walrus))] ()),
}

#[node]
pub struct DisjunctionNode {
    #[sep(TokenValue::Keyword(Keyword::Or))]
    pub conjunctions: Vec<ConjunctionNode>,
}

#[node]
pub struct ConjunctionNode {
    #[sep(TokenValue::Keyword(Keyword::And))]
    pub inversions: Vec<InversionNode>,
}

#[node]
pub enum InversionNode {
    Not(#[prefix(token(TokenValue::Keyword(Keyword::Not)))] Box<InversionNode>),
    Comp(ComparisonNode),
}

// Comparison operators
// --------------------

#[node]
pub struct ComparisonNode {
    pub bitwise_or: BitwiseOrNode,
    pub compare_pairs: Vec<CompareOpBitwiseOrPair>,
}

#[node]
pub enum CompareOpBitwiseOrPair {
    Eq(#[prefix(token(TokenValue::Comparator(Comparator::Eq)))] BitwiseOrNode),
    Ne(#[prefix(token(TokenValue::Comparator(Comparator::Ne)))] BitwiseOrNode),
    Le(#[prefix(token(TokenValue::Comparator(Comparator::Le)))] BitwiseOrNode),
    Lt(#[prefix(token(TokenValue::Comparator(Comparator::Lt)))] BitwiseOrNode),
    Ge(#[prefix(token(TokenValue::Comparator(Comparator::Ge)))] BitwiseOrNode),
    Gt(#[prefix(token(TokenValue::Comparator(Comparator::Gt)))] BitwiseOrNode),
    NotIn(
        #[prefix(token(TokenValue::Keyword(Keyword::Not)), token(TokenValue::Keyword(Keyword::In)))]
        BitwiseOrNode
    ),
    In(
        #[prefix(token(TokenValue::Keyword(Keyword::In)))]
        BitwiseOrNode
    ),
    IsNot(
        #[prefix(token(TokenValue::Keyword(Keyword::Is)), token(TokenValue::Keyword(Keyword::Not)))]
        BitwiseOrNode
    ),
    Is(
        #[prefix(token(TokenValue::Keyword(Keyword::Is)))]
        BitwiseOrNode
    ),
}

#[iterative_node(BitwiseXorNode, Xor, Or: TokenValue::Operator(Operator::BitOr))]
pub enum BitwiseOrNode {
    Or(Box<BitwiseOrNode>, BitwiseXorNode),
    Xor(BitwiseXorNode)
}

#[iterative_node(BitwiseAndNode, And,
    Xor: TokenValue::Operator(Operator::BitXor)
)]
pub enum BitwiseXorNode {
    Xor(Box<BitwiseXorNode>, BitwiseAndNode),
    And(BitwiseAndNode)
}

#[iterative_node(ShiftExprNode, Shift,
    And: TokenValue::Operator(Operator::BitAnd)
)]
pub enum BitwiseAndNode {
    And(Box<BitwiseAndNode>, ShiftExprNode),
    Shift(ShiftExprNode)
}

#[iterative_node(SumNode, Sum,
    Shl: TokenValue::Operator(Operator::Lsh),
    Shr: TokenValue::Operator(Operator::Rsh)
)]
pub enum ShiftExprNode {
    Shl(Box<ShiftExprNode>, SumNode),
    Shr(Box<ShiftExprNode>, SumNode),
    Sum(SumNode)
}

// Arithmetic operators
// --------------------

#[iterative_node(TermNode, Term,
    Add: TokenValue::Operator(Operator::Add),
    Sub: TokenValue::Operator(Operator::Sub)
)]
pub enum SumNode {
    Add(Box<SumNode>, TermNode),
    Sub(Box<SumNode>, TermNode),
    Term(TermNode),
}

#[iterative_node(FactorNode, Factor,
    Mul: TokenValue::Operator(Operator::Mul),
    Div: TokenValue::Operator(Operator::Div),
    Floor: TokenValue::Operator(Operator::Floor),
    Mod: TokenValue::Operator(Operator::Mod),
    MatMul: TokenValue::Operator(Operator::MatMul),
)]
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
    Pos(#[prefix(token(TokenValue::Operator(Operator::Add)))] Box<FactorNode>),
    Neg(#[prefix(token(TokenValue::Operator(Operator::Sub)))] Box<FactorNode>),
    BitNot(#[prefix(token(TokenValue::Operator(Operator::BitNot)))] Box<FactorNode>),
    Power(PowerNode),
}

#[node]
pub struct PowerNodeInner(#[prefix(token(TokenValue::Operator(Operator::Pow)))] pub Box<FactorNode>);

#[node]
pub struct PowerNode {
    pub await_primary: AwaitPrimaryNode,
    pub factor: Option<PowerNodeInner>
}

// Primary elements
// ----------------

#[node]
pub struct AwaitPrimaryNode {
    #[token(TokenValue::Keyword(Keyword::Await))]
    pub is_await: bool,
    pub primary: PrimaryNode,
}

#[derive(Debug)]
pub enum PrimaryNode {
    DotName(Box<PrimaryNode>, Token),
    Genexp(Box<PrimaryNode>, GenexpNode),
    Call(Box<PrimaryNode>, Option<ArgumentsNode>),
    Slice(Box<PrimaryNode>, SlicesNode),
    Atom(AtomNode),
}
impl Node for PrimaryNode {
    fn parse(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>> {
        tokens.snapshot();

        let Some(atom) = AtomNode::parse_debug(tokens, invalid_pass)? else {
            tokens.restore();
            return Ok(None);
        };

        tokens.discard_snapshot();
        let mut p = PrimaryNode::Atom(atom);

        loop {
            let Some(tk) = tokens.get(0) else {
                break;
            };

            match tk.value {
                TokenValue::Symbol(Symbol::Dot) => {
                    tokens.snapshot();
                    tokens.consume_next();

                    let Some(name) = tokens.consume_next() else {
                        tokens.restore();
                        break;
                    };

                    if !matches!(name.value, TokenValue::Word(_)) {
                        tokens.restore();
                        break;
                    }

                    let name = name.clone();
                    tokens.discard_snapshot();
                    p = PrimaryNode::DotName(Box::new(p), name);
                }
                TokenValue::Symbol(Symbol::LBracket) => {
                    tokens.snapshot();
                    tokens.consume_next();

                    match SlicesNode::parse_debug(tokens, invalid_pass) {
                        Result::Ok(Some(slices)) => {
                            if !matches!(tokens.consume_next(), Some(Token { value: TokenValue::Symbol(Symbol::RBracket), .. })) {
                                tokens.restore();
                                break;
                            }

                            tokens.discard_snapshot();
                            p = PrimaryNode::Slice(Box::new(p), slices);
                        }
                        Result::Ok(None) => {
                            tokens.restore();
                            break;
                        }
                        Err(e) => {
                            tokens.restore();
                            return Err(e);
                        }
                    }
                }
                TokenValue::Symbol(Symbol::LParen) => {
                    tokens.snapshot();

                    match GenexpNode::parse_debug(tokens, invalid_pass) {
                        Result::Ok(Some(genexp)) => {
                            tokens.discard_snapshot();
                            p = PrimaryNode::Genexp(Box::new(p), genexp);
                        }
                        Result::Ok(None) => {
                            tokens.consume_next();

                            match ArgumentsNode::parse_debug(tokens, invalid_pass) {
                                Result::Ok(args) => {
                                    if !matches!(tokens.consume_next(), Some(Token { value: TokenValue::Symbol(Symbol::RParen), .. })) {
                                        tokens.restore();
                                        break;
                                    }

                                    tokens.discard_snapshot();
                                    p = PrimaryNode::Call(Box::new(p), args);
                                }
                                Err(e) => {
                                    tokens.restore();
                                    return Err(e);
                                }
                            }
                        }
                        Err(e) => {
                            tokens.restore();
                            return Err(e);
                        }
                    }
                }
                _ => break,
            }
        }

        Ok(Some(p))
    }
}

#[node]
pub enum SlicesNode {
    Slice(
        SliceNode,
        #[fail_if(TokenValue::Symbol(Symbol::Comma))] ()
    ),
    Multi(
        #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
        Vec<SliceOrStarredExpr>
    ),
}

#[node]
pub enum SliceOrStarredExpr {
    Slice(SliceNode),
    Starred(StarredExpressionNode),
}

#[node]
pub struct SliceStepInner(#[token(TokenValue::Symbol(Symbol::Colon))] (), pub ExpressionNode);

#[node]
pub enum SliceNode {
    Literal {
        start: Option<ExpressionNode>,
        #[token(TokenValue::Symbol(Symbol::Colon))]
        c: (),
        end: Option<ExpressionNode>,
        step: Option<SliceStepInner>
    },
    Expr(NamedExpressionNode),
}

#[node]
pub enum AtomNode {
    Name(#[token(TokenValue::Word(_))] Token),
    True(#[token(TokenValue::BooleanLiteral(true))] Token),
    False(#[token(TokenValue::BooleanLiteral(false))] Token),
    None(#[token(TokenValue::None)] Token),
    Strings(StringsNode),
    Number(#[token(TokenValue::NumberLiteral(_))] Token),
    Tuple(TupleNode),
    Group(GroupNode),
    Genexp(GenexpNode),
    List(ListNode),
    ListComp(ListCompNode),
    Dict(DictNode),
    Set(SetNode),
    DictComp(DictCompNode),
    SetComp(SetCompNode),
    Ellipsis(#[token(TokenValue::Ellipsis)] Token),
}

#[node]
pub enum GroupNodeInner {
    Yield(YieldExprNode),
    Named(NamedExpressionNode),
}

#[node]
pub struct GroupNode(
    #[token(TokenValue::Symbol(Symbol::LParen))] (),
    GroupNodeInner,
    #[token(TokenValue::Symbol(Symbol::RParen))] (),
);

// Lambda functions
// ----------------

#[node]
pub struct LambdaDefNode {
    #[token(TokenValue::Keyword(Keyword::Lambda))]
    pub token: Token,
    pub params: Option<LambdaParamsNode>,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub expr: ExpressionNode,
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
        #[one_or_more]
        param_no_default: Vec<LambdaParamNoDefaultNode>,
        param_with_default: Vec<LambdaParamWithDefaultNode>,
        star_etc: Option<LambdaStarEtcNode>,
    },
    ParamWithDefault {
        #[one_or_more]
        param_with_default: Vec<LambdaParamWithDefaultNode>,
        star_etc: Option<LambdaStarEtcNode>,
    },
    StarEtc(LambdaStarEtcNode),
}

#[node]
pub enum CommaOrColonNext {
    Comma(#[token(TokenValue::Symbol(Symbol::Comma))] ()),
    ColonNext(#[pass_if(TokenValue::Symbol(Symbol::Colon))] ()),
}

#[node]
pub struct LambdaSlashNoDefaultNode {
    #[one_or_more]
    pub params: Vec<LambdaParamNoDefaultNode>,
    #[token(TokenValue::Operator(Operator::Div))]
    s: (),
    _c: CommaOrColonNext
}

#[node]
pub struct LambdaSlashWithDefaultNode {
    pub params_no_default: Vec<LambdaParamNoDefaultNode>,
    #[one_or_more]
    pub params_with_default: Vec<LambdaParamWithDefaultNode>,
    #[token(TokenValue::Operator(Operator::Div))]
    s: (),
    _c: CommaOrColonNext
}

#[node]
pub enum LambdaStarEtcNode {
    Star {
        #[token(TokenValue::Operator(Operator::Mul))]
        s: (),
        param_no_default: LambdaParamNoDefaultNode,
        param_maybe_default: Vec<LambdaParamMaybeDefaultNode>,
        kwds: Option<LambdaKwdsNode>,
    },
    CatchAll {
        #[pattern(token(TokenValue::Operator(Operator::Mul)), token(TokenValue::Symbol(Symbol::Comma)))]
        s: (),
        #[one_or_more]
        param_maybe_default: Vec<LambdaParamMaybeDefaultNode>,
        kwds: Option<LambdaKwdsNode>,
    },
    Kwds(LambdaKwdsNode),
}

#[node]
pub struct LambdaKwdsNode(
    #[token(TokenValue::Operator(Operator::Pow))] (),
    pub LambdaParamNoDefaultNode,
);

#[node]
pub struct LambdaParamNoDefaultNode(pub LambdaParamNode, pub CommaOrColonNext);

#[node]
pub struct LambdaParamWithDefaultNode {
    pub param: LambdaParamNode,
    pub default: DefaultNode,
    _c: CommaOrColonNext
}

#[node]
pub struct LambdaParamMaybeDefaultNode {
    pub param: LambdaParamNode,
    pub default: Option<DefaultNode>,
    _c: CommaOrColonNext
}

#[node]
pub struct LambdaParamNode {
    #[token(TokenValue::Word(_))]
    pub name: Token,
}



// LITERALS
// ========
#[node]
pub enum StringsNode{
    Literal(#[token(TokenValue::StringLiteral(_))] Token),
    FString(FStringInternal),
}

#[derive(Debug)]
pub enum FStringInternalPart {
    StringContent(String),
    AnnotatedRhsNode(AnnotatedRhsNode, Vec<FStringInternalPart>)
}
impl FStringInternalPart {
    fn parse_partial(partial: &PartialFString, invalid_pass: bool) -> Result<Self> {
        match partial {
            PartialFString::StringContent(s) => {
                Ok(Self::StringContent(s.clone()))
            }
            PartialFString::TokenStream(toks, spec) => {
                let mut toks = toks.clone().into_parse_tokens();
                match AnnotatedRhsNode::parse(&mut toks, invalid_pass) {
                    Result::Ok(Some(n)) => {
                        let mut parts = Vec::new();
                        for s in spec {
                            match FStringInternalPart::parse_partial(s, invalid_pass) {
                                Result::Ok(p) => {
                                    parts.push(p)
                                }
                                Result::Err(e) => {
                                    return Err(e)
                                }
                            }
                        }
                        Ok(Self::AnnotatedRhsNode(n, parts))

                    }
                    Result::Ok(None) => {
                        Err(anyhow!("Invalid syntax in f-string expression"))
                    }
                    Err(e) => {
                        Err(e)
                    }
                }
            }
        }
    }
}


#[derive(Debug)]
pub struct FStringInternal {
    token: Token,
    parts: Vec<FStringInternalPart>
}
impl Node for FStringInternal {
    fn parse(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>> {
        tokens.snapshot();
        if let Some(t) = tokens.consume_next() {
            let t = t.clone();
            if !matches!(&t.value, TokenValue::FString(_)) {
                tokens.restore();
                return Ok(None)
            }
            let fstr = match &t.value {
                TokenValue::FString(f) => f,
                _ => unreachable!()
            };
            let mut parts = Vec::new();
            for f in fstr {
                match FStringInternalPart::parse_partial(f, invalid_pass) {
                    Result::Ok(p) => {
                        parts.push(p)
                    }
                    Result::Err(e) => {
                        tokens.restore();
                        return Err(e)
                    }
                }
            }
            tokens.discard_snapshot();
            Ok(Some(Self { token: t, parts }))
        } else {
            tokens.restore();
            Ok(None)
        }
    }
}

#[node]
pub struct ListNode(
    #[token(TokenValue::Symbol(Symbol::LBracket))] (),
    pub Option<StarNamedExpressionsNode>,
    #[token(TokenValue::Symbol(Symbol::RBracket))] (),
);

#[node]
pub struct TupleNode(
    #[token(TokenValue::Symbol(Symbol::LParen))] (),
    pub Option<TupleNodeInner>,
    #[token(TokenValue::Symbol(Symbol::RParen))] (),
);

#[node]
pub struct TupleNodeInner(
    pub StarNamedExpressionNode,
    #[token(TokenValue::Symbol(Symbol::Comma))] (),
    pub Option<StarNamedExpressionsNode>
);

#[node]
pub struct SetNode(
    #[token(TokenValue::Symbol(Symbol::LBrace))] (),
    pub StarNamedExpressionsNode,
    #[token(TokenValue::Symbol(Symbol::RBrace))] (),
);

// Dicts
// -----

#[node]
pub struct DictNode(
    #[token(TokenValue::Symbol(Symbol::LBrace))] (),
    pub Option<DoubleStarredKVPairsNode>,
    #[token(TokenValue::Symbol(Symbol::RBrace))] (),
);

#[node]
pub struct DoubleStarredKVPairsNode(
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub Vec<DoubleStarredKVPairNode>
);

#[node]
pub enum DoubleStarredKVPairNode {
    Starred(#[prefix(token(TokenValue::Operator(Operator::Pow)))] BitwiseOrNode),
    Pair(KVPairNode),
}

#[node]
pub struct KVPairNode(
    pub ExpressionNode,
    #[prefix(token(TokenValue::Symbol(Symbol::Colon)))]
    pub ExpressionNode
);

// Comprehensions & Generators
// ---------------------------

#[node]
pub struct ForIfClausesNode(#[one_or_more] pub Vec<ForIfClauseNode>);

#[node]
pub struct ForIfClauseNode {
    #[token(TokenValue::Keyword(Keyword::Async))]
    pub is_async: bool,
    #[token(TokenValue::Keyword(Keyword::For))]
    pub for_token: Token,
    pub targets: StarTargetsNode,
    #[token(TokenValue::Keyword(Keyword::In))]
    pub in_token: Token,
    #[commit]
    #[errors(err("Expected value to iterate over after {in_token}"))]
    pub iter: DisjunctionNode,
    pub guards: Vec<ForIfClauseNodeGuardsInner>,
}

#[node]
pub struct ForIfClauseNodeGuardsInner(
    #[token(TokenValue::Keyword(Keyword::If))]
    pub Token,
    pub DisjunctionNode
);

#[node]
pub struct ListCompNode {
    #[prefix(token(TokenValue::Symbol(Symbol::LBracket)))]
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
    #[token(TokenValue::Symbol(Symbol::RBracket))]
    r: (),
}

#[node]
pub struct SetCompNode {
    #[prefix(token(TokenValue::Symbol(Symbol::LBrace)))]
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
    #[token(TokenValue::Symbol(Symbol::RBrace))]
    r: (),
}

#[node]
pub struct GenexpNode {
    #[prefix(token(TokenValue::Symbol(Symbol::LParen)))]
    pub named: NamedExpressionNode,
    pub clauses: ForIfClausesNode,
    #[token(TokenValue::Symbol(Symbol::RParen))]
    r: (),
}

#[node]
pub struct DictCompNode {
    #[prefix(token(TokenValue::Symbol(Symbol::LBrace)))]
    pub kv_pair: KVPairNode,
    pub clauses: ForIfClausesNode,
    #[token(TokenValue::Symbol(Symbol::RBrace))]
    r: (),
}

// FUNCTION CALL ARGUMENTS
// =======================

#[node]
pub struct ArgumentsNode(
    pub ArgsNode,
    #[token(TokenValue::Symbol(Symbol::Comma))] Option<()>,
    #[pass_if(TokenValue::Symbol(Symbol::RParen))] (),
);

#[node]
pub struct ArgsNodeExprTargetsInner(pub StarredOrNamedExpr, #[fail_if(TokenValue::Symbol(Symbol::Assign))] ());

#[node]
pub struct ArgsNodeExprKwargsInner(#[token(TokenValue::Symbol(Symbol::Comma))] (), pub KwargsNode);

#[node]
pub enum ArgsNode {
    Expr(
        #[sep(TokenValue::Symbol(Symbol::Comma))]
        Vec<ArgsNodeExprTargetsInner>,
        Option<ArgsNodeExprKwargsInner>
    ),
    Kwargs(KwargsNode),
}

#[node]
pub enum StarredOrNamedExpr {
    Starred(StarredExpressionNode),
    Named(NamedExpressionNode),
}

#[node]
pub enum KwargsNode {
    Both(
        #[sep(TokenValue::Symbol(Symbol::Comma))]
        Vec<KwargOrStarredNode>,
        #[token(TokenValue::Symbol(Symbol::Comma))] (),
        #[sep(TokenValue::Symbol(Symbol::Comma))]
        Vec<KwargOrDoubleStarredNode>
    ),
    Starred(
        #[sep(TokenValue::Symbol(Symbol::Comma))]
        Vec<KwargOrStarredNode>,
    ),
    DoubleStarred(
        #[sep(TokenValue::Symbol(Symbol::Comma))]
        Vec<KwargOrDoubleStarredNode>
    ),
}

#[node]
pub struct StarredExpressionNode(#[prefix(token(TokenValue::Operator(Operator::Mul)))] pub ExpressionNode);

#[node]
pub enum KwargOrStarredNode {
    Kwarg(
        #[token(TokenValue::Word(_))]
        Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Assign)))]
        ExpressionNode
    ),
    Starred(StarredExpressionNode),
}

#[node]
pub enum KwargOrDoubleStarredNode {
    Kwarg(
        #[token(TokenValue::Word(_))]
        Token,
        #[prefix(token(TokenValue::Symbol(Symbol::Assign)))]
        ExpressionNode
    ),
    Starred(
        #[prefix(token(TokenValue::Operator(Operator::Pow)))]
        ExpressionNode
    ),
}

// ASSIGNMENT TARGETS
// ==================

// Generic targets
// ---------------

#[node]
pub struct StarTargetsNode(
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub Vec<StarTargetNode>
);

#[node]
pub struct StarTargetsListSeq(
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub Vec<StarTargetNode>
);

#[node]
pub enum StarTargetsTupleSeq {
    Multi(
        StarTargetNode,
        #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
        Vec<StarTargetNode>
    ),
    One(
        StarTargetNode,
        #[token(TokenValue::Symbol(Symbol::Comma))] ()
    )
}

#[node]
pub struct StarTargetNode {
    #[token(TokenValue::Operator(Operator::Mul))]
    pub is_starred: bool,
    pub target: TargetWithStarAtomNode,
}

#[node]
pub enum TargetWithStarAtomNode {
    Named(
        TPrimaryNode,
        #[token(TokenValue::Symbol(Symbol::Dot))] (),
        #[token(TokenValue::Word(_))]
        Token,
        #[fail_if(TokenValue::Symbol(Symbol::LParen | Symbol::LBracket | Symbol::Dot))] ()
    ),
    Slices(
        TPrimaryNode,
        #[token(TokenValue::Symbol(Symbol::LBracket))] (),
        SliceNode,
        #[token(TokenValue::Symbol(Symbol::RBracket))] (),
        #[fail_if(TokenValue::Symbol(Symbol::LParen | Symbol::LBracket | Symbol::Dot))] ()
    ),
    StarAtom(
        Box<StarAtomNode>
    ),
}

#[node]
pub enum StarAtomNode {
    Name(#[token(TokenValue::Word(_))] Token),
    Expr(
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        TargetWithStarAtomNode,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    ),
    Tuple(
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        Option<StarTargetsTupleSeq>,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    ),
    List(
        #[token(TokenValue::Symbol(Symbol::LBracket))] (),
        Option<StarTargetsListSeq>,
        #[token(TokenValue::Symbol(Symbol::RBracket))] (),
    ),
}

#[node]
pub enum SingleTargetNode {
    Subscript(SingleSubscriptAttributeTargetNode),
    Named(#[token(TokenValue::Word(_))] Token),
    Parenthesized(
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        Box<SingleTargetNode>,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    )
}

#[node]
pub enum SingleSubscriptAttributeTargetNode {
    Named(
        TPrimaryNode,
        #[token(TokenValue::Symbol(Symbol::Dot))] (),
        #[token(TokenValue::Word(_))]
        Token,
        #[fail_if(TokenValue::Symbol(Symbol::LParen | Symbol::LBracket | Symbol::Dot))] ()
    ),
    Slices(
        TPrimaryNode,
        #[token(TokenValue::Symbol(Symbol::LBracket))] (),
        SlicesNode,
        #[token(TokenValue::Symbol(Symbol::RBracket))] (),
        #[fail_if(TokenValue::Symbol(Symbol::LParen | Symbol::LBracket | Symbol::Dot))] ()
    ),
}

#[derive(Debug)]
pub enum TPrimaryNode {
    Named(Box<Self>, Token),
    Slices(Box<Self>, SlicesNode),
    Genexp(Box<Self>, GenexpNode),
    Call(Box<Self>, Option<ArgumentsNode>),
    Atom(Box<AtomNode>),
}
fn t_lookahead(tokens: &ParseTokens) -> bool {
    if let Some(tk) = tokens.get(0) {
        matches!(tk.value, TokenValue::Symbol(Symbol::LParen | Symbol::LBracket | Symbol::Dot))
    } else {
        false
    }
}

impl Node for TPrimaryNode {
    fn parse(tokens: &mut ParseTokens, invalid_pass: bool) -> Result<Option<Self>> {
        tokens.snapshot();

        let Some(atom) = AtomNode::parse_debug(tokens, invalid_pass)? else {
            tokens.restore();
            return Ok(None)
        };

        if !t_lookahead(tokens) {
            tokens.restore();
            return Ok(None)
        }

        tokens.discard_snapshot();
        let mut a = Self::Atom(Box::new(atom));

        loop {
            let tk = tokens.get(0).unwrap(); // unwrap cuz all branches end with t_lookahead which means this token will always exist

            match tk.value {
                TokenValue::Symbol(Symbol::Dot) => {
                    tokens.snapshot();
                    tokens.consume_next();
                    if let Some(name) = tokens.consume_next() {
                        if !matches!(name.value, TokenValue::Word(_)) {
                            tokens.restore();
                            break;
                        }
                        let name = name.clone();
                        if !t_lookahead(tokens) {
                            tokens.restore();
                            break;
                        }
                        tokens.discard_snapshot();
                        a = Self::Named(Box::new(a), name)
                    }
                }
                TokenValue::Symbol(Symbol::LBracket) => {
                    tokens.snapshot();
                    tokens.consume_next();
                    match SlicesNode::parse_debug(tokens, invalid_pass) {
                        Result::Ok(Some(slices)) => {
                            if !matches!(tokens.consume_next(), Some(Token{ value: TokenValue::Symbol(Symbol::RBracket), ..})) {
                                tokens.restore();
                                break;
                            }
                            if !t_lookahead(tokens) {
                                tokens.restore();
                                break;
                            }
                            tokens.discard_snapshot();
                            a = Self::Slices(Box::new(a), slices)
                        }
                        Result::Ok(None) => {
                            tokens.restore();
                            return Ok(None)
                        }
                        Err(e) => {
                            tokens.restore();
                            return Err(e)
                        }
                    }
                }
                TokenValue::Symbol(Symbol::LParen) => {
                    tokens.snapshot();
                    match GenexpNode::parse_debug(tokens, invalid_pass) {
                        Result::Ok(Some(genexp)) => {
                            if !t_lookahead(tokens) {
                                tokens.restore();
                                break;
                            }
                            tokens.discard_snapshot();
                            a = Self::Genexp(Box::new(a), genexp);
                        }
                        Result::Ok(None) => {
                            tokens.consume_next();
                            match ArgumentsNode::parse_debug(tokens, invalid_pass) {
                                Result::Ok(args) => {
                                    if !matches!(tokens.consume_next(), Some(Token { value: TokenValue::Symbol(Symbol::RParen), .. })) {
                                        tokens.restore();
                                        break;
                                    }
                                    if !t_lookahead(tokens) {
                                        tokens.restore();
                                        break;
                                    }
                                    tokens.discard_snapshot();
                                    a = Self::Call(Box::new(a), args)
                                }
                                Err(e) => {
                                    tokens.restore();
                                    return Err(e)
                                }
                            }
                        }
                        Err(e) => {
                            tokens.restore();
                            return Err(e)
                        }
                    }
                }
                _ => unreachable!()
            }

        }

        Ok(Some(a))

    }
}

// Targets for del statements
// --------------------------

#[node]
pub struct DelTargetsNode(
    #[sep(TokenValue::Symbol(Symbol::Comma), trailing)]
    pub Vec<DelTargetNode>
);

#[node]
pub enum DelTargetNode {
    Named(
        TPrimaryNode,
        #[token(TokenValue::Symbol(Symbol::Dot))] (),
        #[token(TokenValue::Word(_))]
        Token,
        #[fail_if(TokenValue::Symbol(Symbol::LParen | Symbol::LBracket | Symbol::Dot))] ()
    ),
    Slices(
        TPrimaryNode,
        #[token(TokenValue::Symbol(Symbol::LBracket))] (),
        SlicesNode,
        #[token(TokenValue::Symbol(Symbol::RBracket))] (),
        #[fail_if(TokenValue::Symbol(Symbol::LParen | Symbol::LBracket | Symbol::Dot))] ()
    ),
    Atom(Box<DelTAtomNode>),
}

#[node]
pub enum DelTAtomNode {
    Name(#[token(TokenValue::Word(_))] Token),
    Single(
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        DelTargetNode,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    ),
    Tuple(
        #[token(TokenValue::Symbol(Symbol::LParen))] (),
        Option<DelTargetsNode>,
        #[token(TokenValue::Symbol(Symbol::RParen))] (),
    ),
    List(
        #[token(TokenValue::Symbol(Symbol::LBracket))] (),
        Option<DelTargetsNode>,
        #[token(TokenValue::Symbol(Symbol::RBracket))] (),
    ),
}


