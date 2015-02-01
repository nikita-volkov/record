module Record.Preprocessor.HSE where

import Record.Prelude
import Record.Preprocessor.Model
import qualified Language.Haskell.Exts as E
import qualified Data.HashMap.Strict as HashMap


data Mode =
  Mode_Module

data Context =
  Context_Type |
  Context_Exp |
  Context_Pat |
  Context_Decl

-- |
-- Parses the code using "haskell-src-exts", reifying the AST types.
reifyASFTypeMap :: Mode -> String -> E.ParseResult (HashMap Label ASFType)
reifyASFTypeMap =
  \case
    Mode_Module -> 
      fmap parseModule . E.parseModule
      -- where
      --   onResult = 

parseModule :: E.Module -> ASFTypeMap
parseModule (E.Module _ _ _ _ _ _ decls) =
  undefined

parseDecl :: E.Decl -> ASFTypeMap
parseDecl = 
  \case
    E.TypeDecl _ _ _ t -> parseType t
    E.TypeFamDecl {} -> mempty
    E.ClosedTypeFamDecl _ _ _ _ tl -> foldMap parseTypeEqn tl
    E.DataDecl _ _ c _ _ q d -> parseContext c <> foldMap parseQualConDecl q <> foldMap parseDeriving d
    -- E.GDataDecl SrcLoc DataOrNew Context Name [TyVarBind] (Maybe Kind) [GadtDecl] [Deriving]  
    -- E.DataFamDecl SrcLoc Context Name [TyVarBind] (Maybe Kind)  
    -- E.TypeInsDecl SrcLoc Type Type  
    -- E.DataInsDecl SrcLoc DataOrNew Type [QualConDecl] [Deriving]  
    -- E.GDataInsDecl SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl] [Deriving] 
    -- E.ClassDecl SrcLoc Context Name [TyVarBind] [FunDep] [ClassDecl]  
    -- E.InstDecl SrcLoc (Maybe Overlap) [TyVarBind] Context QName [Type] [InstDecl] 
    -- E.DerivDecl SrcLoc (Maybe Overlap) [TyVarBind] Context QName [Type] 
    -- E.InfixDecl SrcLoc Assoc Int [Op] 
    -- E.DefaultDecl SrcLoc [Type] 
    -- E.SpliceDecl SrcLoc Exp 
    -- E.TypeSig SrcLoc [Name] Type  
    -- E.FunBind [Match] 
    -- E.PatBind SrcLoc Pat Rhs Binds  
    -- E.ForImp SrcLoc CallConv Safety String Name Type  
    -- E.ForExp SrcLoc CallConv String Name Type 
    -- E.RulePragmaDecl SrcLoc [Rule]  
    -- E.DeprPragmaDecl SrcLoc [([Name], String)]  
    -- E.WarnPragmaDecl SrcLoc [([Name], String)]  
    -- E.InlineSig SrcLoc Bool Activation QName  
    -- E.InlineConlikeSig SrcLoc Activation QName  
    -- E.SpecSig SrcLoc Activation QName [Type]  
    -- E.SpecInlineSig SrcLoc Bool Activation QName [Type] 
    -- E.InstSig SrcLoc [TyVarBind] Context QName [Type] 
    -- E.AnnPragma SrcLoc Annotation 
    -- E.MinimalPragma SrcLoc (Maybe BooleanFormula) 
    _ -> error "TODO"

parseQualConDecl :: E.QualConDecl -> ASFTypeMap
parseQualConDecl =
  \(E.QualConDecl _ _ c cd) -> parseContext c <> parseConDecl cd

parseConDecl :: E.ConDecl -> ASFTypeMap
parseConDecl =
  \case
    E.ConDecl _ tl -> foldMap parseType tl
    E.InfixConDecl t1 _ t2 -> parseType t1 <> parseType t2
    E.RecDecl {} -> error "Unexpected record declaration"

parseTypeEqn :: E.TypeEqn -> ASFTypeMap
parseTypeEqn =
  \(E.TypeEqn t1 t2) -> parseType t1 <> parseType t2

parseDeriving :: E.Deriving -> ASFTypeMap
parseDeriving =
  \(_, tl) -> foldMap parseType tl

parseType :: E.Type -> ASFTypeMap
parseType =
  \case
    E.TyForall _ c t -> parseContext c <> parseType t
    E.TyFun t1 t2 -> parseType t1 <> parseType t2
    E.TyTuple _ tl -> foldMap parseType tl
    E.TyList t -> parseType t
    E.TyParArray t -> parseType t
    E.TyApp t1 t2 -> parseType t1 <> parseType t2
    E.TyVar _ -> mempty
    E.TyCon n -> parseQName Context_Type n
    E.TyParen t -> parseType t
    E.TyInfix t1 _ t2 -> parseType t1 <> parseType t2
    E.TyKind t _ -> parseType t
    E.TyPromoted _ -> mempty
    E.TyEquals t1 t2 -> parseType t1 <> parseType t2
    E.TySplice s -> parseSplice s
    E.TyBang _ t -> parseType t

parseContext :: E.Context -> ASFTypeMap
parseContext =
  foldMap parseAsst

parseAsst :: E.Asst -> ASFTypeMap
parseAsst =
  \case
    E.ClassA _ tl -> foldMap parseType tl
    E.VarA _ -> mempty
    E.InfixA t1 _ t2 -> parseType t1 <> parseType t2
    E.IParam _ t -> parseType t
    E.EqualP t1 t2 -> parseType t1 <> parseType t2
    E.ParenA a -> parseAsst a

parseQName :: Context -> E.QName -> ASFTypeMap
parseQName c =
  \case
    E.UnQual n -> parseName c n
    _ -> mempty

parseName :: Context -> E.Name -> ASFTypeMap
parseName c =
  error . showString "TODO: parseName: " . show

parseSplice :: E.Splice -> ASFTypeMap
parseSplice =
  \case
    E.IdSplice _ -> mempty
    E.ParenSplice e -> parseExp e

parseExp :: E.Exp -> ASFTypeMap
parseExp =
  \case
    E.Var _ -> mempty
    E.IPVar _ -> mempty
    E.Con q -> parseQName Context_Exp q
    E.Lit _ -> mempty
    E.InfixApp e1 _ e2 -> parseExp e1 <> parseExp e2
    E.App e1 e2 -> parseExp e1 <> parseExp e2
    E.NegApp e -> parseExp e
    E.Lambda _ pl e -> foldMap parsePat pl <> parseExp e    
    _ -> error "TODO"

parsePat :: E.Pat -> ASFTypeMap
parsePat =
  \case
    E.PVar _ -> mempty
    E.PLit _ _ -> mempty
    E.PNPlusK _ _ -> mempty
    E.PInfixApp p1 _ p2 -> parsePat p1 <> parsePat p2
    E.PApp q pl -> parseQName Context_Pat q <> foldMap parsePat pl
    E.PTuple _ pl -> foldMap parsePat pl
    E.PList pl -> foldMap parsePat pl
    E.PParen p -> parsePat p
    E.PRec {} -> error "Unexpected record pattern"
    E.PAsPat _ p -> parsePat p
    E.PWildCard -> mempty
    E.PIrrPat p -> parsePat p
    E.PatTypeSig _ p t -> parsePat p <> parseType t
    E.PViewPat e p -> parseExp e <> parsePat p
    E.PRPat rl -> foldMap parseRPat rl
    E.PXTag {} -> mempty
    E.PXETag {} -> mempty
    E.PXPcdata {} -> mempty
    E.PXPatTag {} -> mempty
    E.PXRPats {} -> mempty
    E.PQuasiQuote _ _ -> mempty
    E.PBangPat p -> parsePat p

parseRPat :: E.RPat -> ASFTypeMap
parseRPat =
  \case
    E.RPOp r _ -> parseRPat r
    E.RPEither r1 r2 -> parseRPat r1 <> parseRPat r2
    E.RPSeq rl -> foldMap parseRPat rl
    E.RPGuard p sl -> parsePat p <> foldMap parseStmt sl
    E.RPCAs _ r -> parseRPat r
    E.RPAs _ r -> parseRPat r
    E.RPParen r -> parseRPat r
    E.RPPat p -> parsePat p

parseStmt :: E.Stmt -> ASFTypeMap
parseStmt =
  \case
    E.Generator _ p e -> parsePat p <> parseExp e
    E.Qualifier e -> parseExp e
    E.LetStmt b -> parseBinds b
    E.RecStmt sl -> foldMap parseStmt sl

parseBinds :: E.Binds -> ASFTypeMap
parseBinds =
  \case
    E.BDecls dl -> foldMap parseDecl dl
    E.IPBinds il -> foldMap parseIPBind il

parseIPBind :: E.IPBind -> ASFTypeMap
parseIPBind =
  \case
    E.IPBind _ _ e -> parseExp e
