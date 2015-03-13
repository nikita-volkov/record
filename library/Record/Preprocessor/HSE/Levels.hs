-- |
-- Aggregation of a list of associations.
module Record.Preprocessor.HSE.Levels where

import Record.Prelude hiding (exp, bracket)
import Record.Preprocessor.Model
import qualified Language.Haskell.Exts as E


type Levels =
  [Level]

module_ :: E.Module -> Levels
module_ (E.Module _ _ _ _ _ _ decls) =
  foldMap decl decls

decl :: E.Decl -> Levels
decl = 
  \case
    E.TypeDecl _ _ _ t -> type_ t
    E.TypeFamDecl {} -> mempty
    E.ClosedTypeFamDecl _ _ _ _ tl -> foldMap typeEqn tl
    E.DataDecl _ _ c _ _ q d -> level c <> foldMap qualConDecl q <> foldMap deriving_ d
    E.GDataDecl _ _ c _ _ _ g d -> level c <> foldMap gadtDecl g <> foldMap deriving_ d
    E.DataFamDecl _ c _ _ _ -> level c
    E.TypeInsDecl _ t1 t2 -> type_ t1 <> type_ t2
    E.DataInsDecl _ _ t q d -> type_ t <> foldMap qualConDecl q <> foldMap deriving_ d
    E.GDataInsDecl _ _ t _ g d -> type_ t <> foldMap gadtDecl g <> foldMap deriving_ d
    E.ClassDecl _ c _ _ _ cd -> level c <> foldMap classDecl cd
    E.InstDecl _ _ _ c _ tl idl -> level c <> foldMap type_ tl <> foldMap instDecl idl
    E.DerivDecl _ _ _ c _ tl -> level c <> foldMap type_ tl
    E.InfixDecl {} -> mempty
    E.DefaultDecl _ tl -> foldMap type_ tl
    E.SpliceDecl _ e -> exp e
    E.TypeSig _ _ t -> type_ t
    E.FunBind ml -> foldMap match ml
    E.PatBind _ p r b -> pat p <> rhs r <> binds b
    E.ForImp _ _ _ _ _ t -> type_ t
    E.ForExp _ _ _ _ t -> type_ t
    E.RulePragmaDecl _ rl -> foldMap rule rl
    E.DeprPragmaDecl {} -> mempty
    E.WarnPragmaDecl {} -> mempty
    E.InlineSig {} -> mempty
    E.InlineConlikeSig {} -> mempty
    E.SpecSig _ _ _ tl -> foldMap type_ tl
    E.SpecInlineSig _ _ _ _ tl -> foldMap type_ tl
    E.InstSig _ _ c _ tl -> level c <> foldMap type_ tl
    E.AnnPragma {} -> mempty
    E.MinimalPragma {} -> mempty

rule :: E.Rule -> Levels
rule =
  \(E.Rule _ _ rvm e1 e2) -> (foldMap . foldMap) ruleVar rvm <> exp e1 <> exp e2

ruleVar :: E.RuleVar -> Levels
ruleVar =
  \case
    E.RuleVar _ -> mempty
    E.TypedRuleVar _ t -> type_ t

match :: E.Match -> Levels
match =
  \(E.Match _ _ pl tm r b) ->
    foldMap pat pl <> foldMap type_ tm <> rhs r <> binds b

rhs :: E.Rhs -> Levels
rhs =
  \case
    E.UnGuardedRhs e -> exp e
    E.GuardedRhss gl -> foldMap guardedRhs gl

guardedRhs :: E.GuardedRhs -> Levels
guardedRhs =
  \(E.GuardedRhs _ sl e) -> foldMap stmt sl <> exp e

instDecl :: E.InstDecl -> Levels
instDecl =
  \case
    E.InsDecl d -> decl d
    E.InsType _ t1 t2 -> type_ t1 <> type_ t2
    E.InsData _ _ t qcdl dl -> type_ t <> foldMap qualConDecl qcdl <> foldMap deriving_ dl
    E.InsGData _ _ t _ gdl dl -> type_ t <> foldMap gadtDecl gdl <> foldMap deriving_ dl

classDecl :: E.ClassDecl -> Levels
classDecl =
  \case
    E.ClsDecl d -> decl d
    E.ClsDataFam _ c _ _ _ -> level c
    E.ClsTyFam _ _ _ _ -> mempty
    E.ClsTyDef _ t1 t2 -> type_ t1 <> type_ t2
    E.ClsDefSig _ _ t -> type_ t

gadtDecl :: E.GadtDecl -> Levels
gadtDecl =
  \(E.GadtDecl _ _ pl t) -> foldMap (type_ . snd) pl <> type_ t

qualConDecl :: E.QualConDecl -> Levels
qualConDecl =
  \(E.QualConDecl _ _ c cd) -> level c <> conDecl cd

conDecl :: E.ConDecl -> Levels
conDecl =
  \case
    E.ConDecl _ tl -> foldMap type_ tl
    E.InfixConDecl t1 _ t2 -> type_ t1 <> type_ t2
    E.RecDecl {} -> error "Unexpected record declaration"

typeEqn :: E.TypeEqn -> Levels
typeEqn =
  \(E.TypeEqn t1 t2) -> type_ t1 <> type_ t2

deriving_ :: E.Deriving -> Levels
deriving_ =
  \(_, tl) -> foldMap type_ tl

type_ :: E.Type -> Levels
type_ =
  \case
    E.TyForall _ c t -> level c <> type_ t
    E.TyFun t1 t2 -> type_ t1 <> type_ t2
    E.TyTuple _ tl -> foldMap type_ tl
    E.TyList t -> type_ t
    E.TyParArray t -> type_ t
    E.TyApp t1 t2 -> type_ t1 <> type_ t2
    E.TyVar _ -> mempty
    E.TyCon n -> qName Level_Type n
    E.TyParen t -> type_ t
    E.TyInfix t1 _ t2 -> type_ t1 <> type_ t2
    E.TyKind t _ -> type_ t
    E.TyPromoted _ -> mempty
    E.TyEquals t1 t2 -> type_ t1 <> type_ t2
    E.TySplice s -> splice s
    E.TyBang _ t -> type_ t

level :: E.Context -> Levels
level =
  foldMap asst

asst :: E.Asst -> Levels
asst =
  \case
    E.ClassA _ tl -> foldMap type_ tl
    E.VarA _ -> mempty
    E.InfixA t1 _ t2 -> type_ t1 <> type_ t2
    E.IParam _ t -> type_ t
    E.EqualP t1 t2 -> type_ t1 <> type_ t2
    E.ParenA a -> asst a

qName :: Level -> E.QName -> Levels
qName c =
  \case
    E.UnQual n -> name c n
    _ -> mempty

name :: Level -> E.Name -> Levels
name c =
  \case
    E.Ident "Ѣ" -> pure c
    E.Symbol "Ѣ" -> pure c
    _ -> empty

splice :: E.Splice -> Levels
splice =
  \case
    E.IdSplice _ -> mempty
    E.ParenSplice e -> exp e

exp :: E.Exp -> Levels
exp =
  \case
    E.Var _ -> mempty
    E.IPVar _ -> mempty
    E.Con q -> qName Level_Exp q
    E.Lit _ -> mempty
    E.InfixApp e1 _ e2 -> exp e1 <> exp e2
    E.App e1 e2 -> exp e1 <> exp e2
    E.NegApp e -> exp e
    E.Lambda _ pl e -> foldMap pat pl <> exp e    
    E.Let b e -> binds b <> exp e
    E.If e1 e2 e3 -> exp e1 <> exp e2 <> exp e3
    E.MultiIf gl -> foldMap guardedRhs gl
    E.Case e al -> exp e <> foldMap alt al
    E.Do sl -> foldMap stmt sl
    E.MDo sl -> foldMap stmt sl
    E.Tuple _ el -> foldMap exp el
    E.TupleSection _ em -> (foldMap . foldMap) exp em
    E.List el -> foldMap exp el
    E.ParArray el -> foldMap exp el
    E.Paren e -> exp e
    E.LeftSection e _ -> exp e
    E.RightSection _ e -> exp e
    E.RecConstr {} -> error "Unexpected Haskell98 record construction expression"
    E.RecUpdate {} -> error "Unexpected Haskell98 record update expression"
    E.EnumFrom e -> exp e
    E.EnumFromTo e1 e2 -> exp e1 <> exp e2
    E.EnumFromThen e1 e2 -> exp e1 <> exp e2
    E.EnumFromThenTo e1 e2 e3 -> exp e1 <> exp e2 <> exp e3
    E.ParArrayFromTo e1 e2 -> exp e1 <> exp e2
    E.ParArrayFromThenTo e1 e2 e3 -> exp e1 <> exp e2 <> exp e3
    E.ListComp e stl -> exp e <> foldMap qualStmt stl
    E.ParComp e stll -> exp e <> (foldMap . foldMap) qualStmt stll
    E.ParArrayComp e stll -> exp e <> (foldMap . foldMap) qualStmt stll
    E.ExpTypeSig _ e t -> exp e <> type_ t
    E.VarQuote _ -> mempty
    E.TypQuote _ -> mempty
    E.BracketExp b -> bracket b
    E.SpliceExp s -> splice s
    E.QuasiQuote {} -> mempty
    E.XTag {} -> error "XML is not supported"
    E.XETag {} -> error "XML is not supported"
    E.XPcdata {} -> error "XML is not supported"
    E.XExpTag {} -> error "XML is not supported"
    E.XChildTag {} -> error "XML is not supported"
    E.CorePragma _ e -> exp e
    E.SCCPragma _ e -> exp e
    E.GenPragma _ _ _ e -> exp e
    E.Proc _ p e -> pat p <> exp e
    E.LeftArrApp e1 e2 -> exp e1 <> exp e2
    E.RightArrApp e1 e2 -> exp e1 <> exp e2
    E.LeftArrHighApp e1 e2 -> exp e1 <> exp e2
    E.RightArrHighApp e1 e2 -> exp e1 <> exp e2
    E.LCase al -> foldMap alt al

bracket :: E.Bracket -> Levels
bracket =
  \case
    E.ExpBracket e -> exp e
    E.PatBracket p -> pat p
    E.TypeBracket t -> type_ t
    E.DeclBracket dl -> foldMap decl dl

qualStmt :: E.QualStmt -> Levels
qualStmt =
  \case
    E.QualStmt s -> stmt s
    E.ThenTrans e -> exp e
    E.ThenBy e1 e2 -> exp e1 <> exp e2
    E.GroupBy e -> exp e
    E.GroupUsing e -> exp e
    E.GroupByUsing e1 e2 -> exp e1 <> exp e2

alt :: E.Alt -> Levels
alt =
  \case
    E.Alt _ p r b -> pat p <> rhs r <> binds b

pat :: E.Pat -> Levels
pat =
  \case
    E.PVar _ -> mempty
    E.PLit _ _ -> mempty
    E.PNPlusK _ _ -> mempty
    E.PInfixApp p1 _ p2 -> pat p1 <> pat p2
    E.PApp q pl -> qName Level_Pat q <> foldMap pat pl
    E.PTuple _ pl -> foldMap pat pl
    E.PList pl -> foldMap pat pl
    E.PParen p -> pat p
    E.PRec {} -> error "Unexpected record pattern"
    E.PAsPat _ p -> pat p
    E.PWildCard -> mempty
    E.PIrrPat p -> pat p
    E.PatTypeSig _ p t -> pat p <> type_ t
    E.PViewPat e p -> exp e <> pat p
    E.PRPat rl -> foldMap rPat rl
    E.PXTag {} -> error "XML is not supported"
    E.PXETag {} -> error "XML is not supported"
    E.PXPcdata {} -> error "XML is not supported"
    E.PXPatTag {} -> error "XML is not supported"
    E.PXRPats {} -> error "XML is not supported"
    E.PQuasiQuote _ _ -> mempty
    E.PBangPat p -> pat p

rPat :: E.RPat -> Levels
rPat =
  \case
    E.RPOp r _ -> rPat r
    E.RPEither r1 r2 -> rPat r1 <> rPat r2
    E.RPSeq rl -> foldMap rPat rl
    E.RPGuard p sl -> pat p <> foldMap stmt sl
    E.RPCAs _ r -> rPat r
    E.RPAs _ r -> rPat r
    E.RPParen r -> rPat r
    E.RPPat p -> pat p

stmt :: E.Stmt -> Levels
stmt =
  \case
    E.Generator _ p e -> pat p <> exp e
    E.Qualifier e -> exp e
    E.LetStmt b -> binds b
    E.RecStmt sl -> foldMap stmt sl

binds :: E.Binds -> Levels
binds =
  \case
    E.BDecls dl -> foldMap decl dl
    E.IPBinds il -> foldMap ipBind il

ipBind :: E.IPBind -> Levels
ipBind =
  \case
    E.IPBind _ _ e -> exp e
