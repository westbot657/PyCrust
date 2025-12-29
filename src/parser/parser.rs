use crate::core::types::{ParseTokens, Symbol, Token, TokenValue};
use crate::parser::node::*;

pub trait Node: Sized {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>>;
}

macro_rules! attempt {
    ( $tokens:expr, $inv:expr, $rule:ty => $node:tt : $some:expr, $none:expr ) => {
        if let Some($node) = $rule::parse($tokens, $inv)? {
            $some
        } else {
            $none
        }
    };
    ( $tokens:expr, $inv:expr, $rule:ident => $node:tt : $some:expr) => {
        if let Some($node) = $rule::parse($tokens, $inv)? {
            $some
        }
    };
    ( $tokens:expr, $inv:expr, $( $rule:ident => $variant:expr );+ $(;)? ) => {
        $(if let Some(s) = $rule::parse($tokens, $inv)? {
            return Ok(Some($variant(s)))
        })else+
    };
}


impl Node for FileNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        Ok(Some(Self {
            statements: StatementsNode::parse(tokens, inv)?.unwrap()
        }))
    }
}

impl Node for StatementsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        let mut statements = Vec::new();
        loop {
            if let Some(s) = StatementNode::parse(tokens, inv)? {
                statements.push(s);
            } else {
                break
            }
        }
        Ok(Some(Self {
            statements
        }))
    }
}

impl Node for StatementNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        Ok(if let Some(s) = CompoundStmtNode::parse(tokens, inv)? {
            Some(Self::CompoundStatement(s))
        } else if let Some(s) = SimpleStmtsNode::parse(tokens, inv)? {
            Some(Self::SimpleStatements(s))
        } else {
            None
        })
    }
}


impl Node for SimpleStmtsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {

        tokens.snapshot();

        let mut stmts = Vec::new();
        if let Some(s) = SimpleStmtNode::parse(tokens, inv)? {
            stmts.push(s);
        } else {
            return Ok(None)
        }

        loop {
            if let Some(t) = tokens.get(0) {
                match t {
                    Token { value: TokenValue::Newline, .. } => {
                        tokens.consume_next();
                        tokens.discard_snapshot();
                        return Ok(Some(Self(stmts)))
                    }
                    Token { value: TokenValue::Symbol(Symbol::Semicolon), .. } => {
                        tokens.consume_next();
                        if let Some(s) = SimpleStmtNode::parse(tokens, inv)? {
                            stmts.push(s)
                        } else {
                            tokens.discard_snapshot();
                            return Ok(Some(Self(stmts)))
                        }
                    }
                    _ => {
                        tokens.restore();
                        return Ok(None)
                    }
                }
            }
        }
    }
}

impl Node for SimpleStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        attempt!(
            tokens, inv,
            AssignmentNode => Self::Assignment;
            TypeAliasNode => Self::TypeAlias;
            StarExpressionsNode => Self::StarExpressions;
            ReturnStmtNode => Self::ReturnStmt;
            ImportStmtNode => Self::ImportStmt;
            RaiseStmtNode => Self::RaiseStmt;
            PassStmtNode => Self::PassStmt;
            DelStmtNode => Self::DelStmt;
            YieldStmtNode => Self::YieldStmt;
            AssertStmtNode => Self::AssertStmt;
            BreakStmtNode => Self::BreakStmt;
            ContinueStmtNode => Self::ContinueStmt;
            GlobalStmtNode => Self::GlobalStmt;
            NonlocalStmtNode => Self::NonlocalStmt;
        );
        Ok(None)
    }
}


impl Node for CompoundStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        attempt!(
            tokens, inv,
            FunctionDefNode => Self::FunctionDef;
            IfStmtNode => Self::IfStmt;
            ClassDefNode => Self::ClassDef;
            WithStmtNode => Self::WithStmt;
            ForStmtNode => Self::ForStmt;
            TryStmtNode => Self::TryStmt;
            WhileStmtNode => Self::WhileStmt;
            MatchStmtNode => Self::MatchStmt;
        );
        Ok(None)
    }
}

impl Node for AssignmentNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AssignmentNodeTarget {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AnnotatedRhsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ReturnStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for RaiseStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for PassStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for BreakStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ContinueStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for GlobalStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for NonlocalStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DelStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for YieldStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AssertStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ImportStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ImportNameNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ImportFromNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ImportFromTargetsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ImportFromAsNamesNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ImportFromAsNameNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DottedAsNamesNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DottedAsNameNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DottedNameNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for BlockNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DecoratorsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DecoratorNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ClassDefNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ClassDefRawNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}


impl Node for FunctionDefNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for FunctionDefRawNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParamsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParametersNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SlashNoDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SlashWithDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarEtcNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KwdsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParamNoDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParamNoDefaultStarAnnotationNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParamWithDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParamMaybeDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParamNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ParamStarAnnotationNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AnnotationNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarAnnotationNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for IfStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ElseBranch {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ElifStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ElseBlockNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for WhileStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ForStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for WithStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for WithItemNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TryStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ExceptBlockNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ExceptStarBlockNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for FinallyBlockNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for MatchStmtNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SubjectExprNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for CaseBlockNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for GuardNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for PatternsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for PatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for OrPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ClosedPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LiteralPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LiteralExprNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ComplexNumberNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SignedNumberNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SignedRealNumberNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for RealNumberNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ImaginaryNumberNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for CapturePatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for PatternCaptureTargetNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for WildcardPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ValuePatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AttrNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for NameOrAttrNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for GroupPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SequencePatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for OpenSequencePatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for MaybeSequencePatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for MaybeStarPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for MappingPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ItemsPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KeyValuePatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KeyExprAttr {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DoubleStarPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ClassPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for PositionalPatternsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KeywordPatternsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KeywordPatternNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TypeAliasNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TypeParamsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TypeParamSeqNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TypeParamNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TypeParamBoundNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TypeParamDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TypeParamStarredDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ExpressionsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ExpressionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for YieldExprNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarExpressionsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarExpressionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarNamedExpressionsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarNamedExpressionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AssignmentExpressionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for NamedExpressionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DisjunctionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ConjunctionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for InversionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ComparisonNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for CompareOpBitwiseOrPair {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for BitwiseOrNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for BitwiseXorNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for BitwiseAndNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ShiftExprNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SumNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TermNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for FactorNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for PowerNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AwaitPrimaryNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for PrimaryNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SlicesNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SliceOrStarredExpr {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SliceNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for AtomNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for GroupNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaDefNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaParamsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaParametersNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaSlashNoDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaSlashWithDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaStarEtcNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaKwdsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaParamNoDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaParamWithDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaParamMaybeDefaultNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for LambdaParamNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StringsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ListNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TupleNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SetNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DictNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DoubleStarredKVPairsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DoubleStarredKVPairNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KVPairNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ForIfClausesNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ForIfClauseNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ListCompNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SetCompNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for GenexpNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DictCompNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ArgumentsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for ArgsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarredOrNamedExpr {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KwargsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarredExpressionNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KwargOrStarredNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for KwargOrDoubleStarredNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarTargetsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarTargetsListSeq {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarTargetsTupleSeq {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarTargetNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TargetWithStarAtomNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for StarAtomNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SingleTargetNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for SingleSubscriptAttributeTargetNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for TPrimaryNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DelTargetsNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DelTargetNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

impl Node for DelTAtomNode {
    fn parse(tokens: &mut ParseTokens, inv: bool) -> anyhow::Result<Option<Self>> {
        todo!()
    }
}

