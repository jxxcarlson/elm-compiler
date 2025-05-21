{-# LANGUAGE OverloadedStrings #-}
module AST.Pretty.Raw
    ( pretty
    , prettyJson
    , Config(..)
    , defaultConfig
    , OutputFormat(..)
    )
    where


import qualified AST.Source as Src
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Lazy.Builder.RealFloat as TBR
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Name as Name
import qualified Reporting.Annotation as A
import qualified Elm.String as ES
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO.Unsafe (unsafePerformIO)
import System.IO (appendFile)
import System.Directory (getCurrentDirectory)
import qualified Elm.Float as EF
import qualified Data.ByteString.Builder as B
import Data.List (intercalate)


data Config =
    Config
        { format :: OutputFormat
        , showPlaceholders :: Bool
        , indentSize :: Int
        , maxLineLength :: Int
        }


data OutputFormat
    = Text
    | Json
    | RagJson


defaultConfig :: Config
defaultConfig =
    Config
        { format = Text
        , showPlaceholders = True
        , indentSize = 2
        , maxLineLength = 80
        }


pretty :: Config -> Src.Module -> IO String
pretty config modul = do
    cwd <- getCurrentDirectory
    case format config of
        Text ->
            prettyText config modul
        Json ->
            return $ prettyJson config modul
        RagJson ->
            return $ prettyRagJson config modul


prettyText :: Config -> Src.Module -> IO String
prettyText config modul =
    let
        header =
            "Module: " ++ ES.toChars (Name.toElmString (Src.getName modul)) ++ "\n"
        -- debugOutput =
        --     "[DEBUG] Raw AST: " ++ show modul ++ "\n"
    in
    do
        -- writeFile "debug_output.txt" debugOutput
        return $ header ++ prettyValues config (map A.toValue (Src._values modul))
            ++ prettyUnions config (map A.toValue (Src._unions modul))
            ++ prettyAliases config (map A.toValue (Src._aliases modul))
            ++ prettyPorts config (getPorts modul)


prettyValues :: Config -> [Src.Value] -> String
prettyValues config values =
    unlines $ map (prettyValue config) values


prettyPattern :: Src.Pattern -> String
prettyPattern (A.At region pattern_) =
    let loc = prettyLocation region
    in loc ++ " " ++ case pattern_ of
        Src.PAnything -> "_"
        Src.PVar name -> ES.toChars (Name.toElmString name)
        Src.PRecord fields -> "{" ++ unwords (map (ES.toChars . Name.toElmString . A.toValue) fields) ++ "}"
        Src.PAlias pat name -> prettyPattern pat ++ " as " ++ ES.toChars (Name.toElmString (A.toValue name))
        Src.PUnit -> "()"
        Src.PTuple p1 p2 ps -> "(" ++ prettyPattern p1 ++ ", " ++ prettyPattern p2 ++ concatMap ((", " ++) . prettyPattern) ps ++ ")"
        Src.PCtor _ name patterns -> ES.toChars (Name.toElmString name) ++ concatMap ((' ':) . prettyPattern) patterns
        Src.PCtorQual _ name qual patterns -> ES.toChars (Name.toElmString name) ++ "." ++ ES.toChars (Name.toElmString qual) ++ concatMap ((' ':) . prettyPattern) patterns
        Src.PList patterns -> "[" ++ unwords (map prettyPattern patterns) ++ "]"
        Src.PCons p1 p2 -> prettyPattern p1 ++ " :: " ++ prettyPattern p2
        Src.PChr str -> show (ES.toChars str)
        Src.PStr str -> show (ES.toChars str)
        Src.PInt i -> show i


prettyExpr :: Src.Expr -> String
prettyExpr (A.At region expr_) =
    let loc = prettyLocation region
    in loc ++ " " ++ case expr_ of
        Src.Chr str -> show (ES.toChars str)
        Src.Str str -> show (ES.toChars str)
        Src.Int i -> show i
        Src.Float f -> show (BL.unpack (B.toLazyByteString (EF.toBuilder f)))
        Src.Var _ name -> ES.toChars (Name.toElmString name)
        Src.VarQual _ name qual -> ES.toChars (Name.toElmString name) ++ "." ++ ES.toChars (Name.toElmString qual)
        Src.List exprs -> "[" ++ unwords (map prettyExpr exprs) ++ "]"
        Src.Op name -> "(" ++ ES.toChars (Name.toElmString name) ++ ")"
        Src.Negate expr -> "-" ++ prettyExpr expr
        Src.Binops exprs expr -> unwords (map (\(e, op) -> prettyExpr e ++ " " ++ ES.toChars (Name.toElmString (A.toValue op))) exprs) ++ " " ++ prettyExpr expr
        Src.Lambda patterns expr -> "\\" ++ unwords (map prettyPattern patterns) ++ " -> " ++ prettyExpr expr
        Src.Call func args -> prettyExpr func ++ " " ++ unwords (map prettyExpr args)
        Src.If branches expr -> "if " ++ unwords (map (\(cond, res) -> prettyExpr cond ++ " then " ++ prettyExpr res ++ " else ") branches) ++ prettyExpr expr
        Src.Let defs expr ->
            "let " ++ intercalate "; " defStrings ++ " in " ++ prettyExpr expr
            where
                defStrings = map prettyDef defs
                prettyDef (A.At region def) = 
                    let loc = prettyLocation region
                    in loc ++ " " ++ case def of
                        Src.Define name patterns e _ ->
                            ES.toChars (Name.toElmString (A.toValue name)) ++ " " ++ unwords (map prettyPattern patterns) ++ " = " ++ prettyExpr e
                        Src.Destruct pat e ->
                            prettyPattern pat ++ " = " ++ prettyExpr e
        Src.Case expr branches -> "case " ++ prettyExpr expr ++ " of ..."
        Src.Accessor name -> "." ++ ES.toChars (Name.toElmString name)
        Src.Access expr name -> prettyExpr expr ++ "." ++ ES.toChars (Name.toElmString (A.toValue name))
        Src.Update name fields -> "{" ++ ES.toChars (Name.toElmString (A.toValue name)) ++ " | ... }"
        Src.Record fields -> "{ " ++ unwords (map (\(n, e) -> ES.toChars (Name.toElmString (A.toValue n)) ++ " = " ++ prettyExpr e) fields) ++ " }"
        Src.Unit -> "()"
        Src.Tuple e1 e2 es -> "(" ++ prettyExpr e1 ++ ", " ++ prettyExpr e2 ++ concatMap ((", " ++) . prettyExpr) es ++ ")"
        Src.Shader _ _ -> "<shader>"


prettyLocation :: A.Region -> String
prettyLocation (A.Region (A.Position startLine startCol) (A.Position endLine endCol)) =
    "[" ++ show startLine ++ ":" ++ show startCol ++ 
    "-" ++ show endLine ++ ":" ++ show endCol ++ "]"


prettyValue :: Config -> Src.Value -> String
prettyValue config (Src.Value name patterns expr _) =
    "[Function] " ++ ES.toChars (Name.toElmString (A.toValue name)) ++
    " with patterns: " ++ unwords (map prettyPattern patterns) ++
    " = " ++ prettyExpr expr ++ "\n"


prettyUnions :: Config -> [Src.Union] -> String
prettyUnions config unions =
    unlines $ map (prettyUnion config) unions


prettyUnion :: Config -> Src.Union -> String
prettyUnion config (Src.Union name vars ctors) =
    let loc = prettyLocation (A.toRegion name)
        nameStr = ES.toChars (Name.toElmString (A.toValue name))
        varsStr = if null vars then "" else " " ++ unwords (map (ES.toChars . Name.toElmString . A.toValue) vars)
        ctorsStr = unlines $ map (\(ctorName, types) -> 
            "  " ++ prettyLocation (A.toRegion ctorName) ++ " " ++ 
            ES.toChars (Name.toElmString (A.toValue ctorName)) ++
            if null types then "" else " " ++ unwords (map prettyType types)) ctors
    in "[Type] " ++ loc ++ " " ++ nameStr ++ varsStr ++ "\n" ++ ctorsStr


prettyType :: Src.Type -> String
prettyType (A.At _ type_) = case type_ of
    Src.TLambda t1 t2 -> prettyType t1 ++ " -> " ++ prettyType t2
    Src.TVar name -> ES.toChars (Name.toElmString name)
    Src.TType _ name types -> ES.toChars (Name.toElmString name) ++ if null types then "" else " " ++ unwords (map prettyType types)
    Src.TTypeQual _ name qual types -> ES.toChars (Name.toElmString name) ++ "." ++ ES.toChars (Name.toElmString qual) ++ if null types then "" else " " ++ unwords (map prettyType types)
    Src.TRecord fields _ -> "{ " ++ unwords (map (\(n, t) -> ES.toChars (Name.toElmString (A.toValue n)) ++ " : " ++ prettyType t) fields) ++ " }"
    Src.TUnit -> "()"
    Src.TTuple t1 t2 ts -> "(" ++ prettyType t1 ++ ", " ++ prettyType t2 ++ concatMap ((", " ++) . prettyType) ts ++ ")"


prettyAliases :: Config -> [Src.Alias] -> String
prettyAliases config aliases =
    unlines $ map (prettyAlias config) aliases


prettyAlias :: Config -> Src.Alias -> String
prettyAlias config (Src.Alias name _ _) =
    "[TypeAlias] " ++ ES.toChars (Name.toElmString (A.toValue name))


prettyPorts :: Config -> [Src.Port] -> String
prettyPorts config ports =
    unlines $ map (prettyPort config) ports


prettyPort :: Config -> Src.Port -> String
prettyPort config (Src.Port name _) =
    "[Port] " ++ ES.toChars (Name.toElmString (A.toValue name))


getPorts :: Src.Module -> [Src.Port]
getPorts modul =
    case Src._effects modul of
        Src.Ports ports -> ports
        _ -> []


prettyJson :: Config -> Src.Module -> String
prettyJson config modul =
    BL.unpack $ Aeson.encode $ encodeModule modul


encodeModule :: Src.Module -> Aeson.Value
encodeModule modul =
    Aeson.object
        [ "type" Aeson..= ("Module" :: String)
        , "name" Aeson..= ES.toChars (Name.toElmString (Src.getName modul))
        , "values" Aeson..= map (encodeValue . A.toValue) (Src._values modul)
        , "unions" Aeson..= map (encodeUnion . A.toValue) (Src._unions modul)
        , "aliases" Aeson..= map (encodeAlias . A.toValue) (Src._aliases modul)
        , "ports" Aeson..= map encodePort (getPorts modul)
        ]


encodeValue :: Src.Value -> Aeson.Value
encodeValue (Src.Value name patterns expr _) =
    Aeson.object
        [ "type" Aeson..= ("Function" :: String)
        , "name" Aeson..= ES.toChars (Name.toElmString (A.toValue name))
        ]


encodeUnion :: Src.Union -> Aeson.Value
encodeUnion (Src.Union name vars _) =
    Aeson.object
        [ "type" Aeson..= ("Type" :: String)
        , "name" Aeson..= ES.toChars (Name.toElmString (A.toValue name))
        ]


encodeAlias :: Src.Alias -> Aeson.Value
encodeAlias (Src.Alias name _ _) =
    Aeson.object
        [ "type" Aeson..= ("TypeAlias" :: String)
        , "name" Aeson..= ES.toChars (Name.toElmString (A.toValue name))
        ]


encodePort :: Src.Port -> Aeson.Value
encodePort (Src.Port name _) =
    Aeson.object
        [ "type" Aeson..= ("Port" :: String)
        , "name" Aeson..= ES.toChars (Name.toElmString (A.toValue name))
        ]


prettyRagJson :: Config -> Src.Module -> String
prettyRagJson config modul =
    BL.unpack $ Aeson.encode $ encodeRagModule modul


encodeRagModule :: Src.Module -> Aeson.Value
encodeRagModule modul =
    Aeson.object
        [ "type" Aeson..= ("Module" :: String)
        , "name" Aeson..= ES.toChars (Name.toElmString (Src.getName modul))
        , "values" Aeson..= map (encodeRagValue . A.toValue) (Src._values modul)
        , "unions" Aeson..= map (encodeRagUnion . A.toValue) (Src._unions modul)
        , "aliases" Aeson..= map (encodeRagAlias . A.toValue) (Src._aliases modul)
        , "ports" Aeson..= map encodeRagPort (getPorts modul)
        ]


encodeRagValue :: Src.Value -> Aeson.Value
encodeRagValue (Src.Value name patterns expr _) =
    Aeson.object
        [ "type" Aeson..= ("Function" :: String)
        , "name" Aeson..= encodeLocated name
        , "patterns" Aeson..= map encodePattern patterns
        , "expr" Aeson..= encodeExpr expr
        ]


encodeRagUnion :: Src.Union -> Aeson.Value
encodeRagUnion (Src.Union name vars ctors) =
    Aeson.object
        [ "type" Aeson..= ("Type" :: String)
        , "name" Aeson..= encodeLocated name
        , "vars" Aeson..= map encodeLocated vars
        , "constructors" Aeson..= map encodeConstructor ctors
        ]


encodeRagAlias :: Src.Alias -> Aeson.Value
encodeRagAlias (Src.Alias name vars type_) =
    Aeson.object
        [ "type" Aeson..= ("TypeAlias" :: String)
        , "name" Aeson..= encodeLocated name
        , "vars" Aeson..= map encodeLocated vars
        , "type" Aeson..= encodeType type_
        ]


encodeRagPort :: Src.Port -> Aeson.Value
encodeRagPort (Src.Port name type_) =
    Aeson.object
        [ "type" Aeson..= ("Port" :: String)
        , "name" Aeson..= encodeLocated name
        , "type" Aeson..= encodeType type_
        ]


encodeLocated :: A.Located Name.Name -> Aeson.Value
encodeLocated (A.At region value) =
    Aeson.object
        [ "region" Aeson..= encodeRegion region
        , "value" Aeson..= ES.toChars (Name.toElmString value)
        ]


encodeRegion :: A.Region -> Aeson.Value
encodeRegion (A.Region start end) =
    Aeson.object
        [ "start" Aeson..= encodePosition start
        , "end" Aeson..= encodePosition end
        ]


encodePosition :: A.Position -> Aeson.Value
encodePosition (A.Position line col) =
    Aeson.object
        [ "line" Aeson..= line
        , "column" Aeson..= col
        ]


encodeValueToString :: String -> Aeson.Value
encodeValueToString value = Aeson.String $ T.pack value


encodePattern :: Src.Pattern -> Aeson.Value
encodePattern (A.At region pattern_) =
    Aeson.object
        [ "region" Aeson..= encodeRegion region
        , "pattern" Aeson..= case pattern_ of
            Src.PAnything -> Aeson.String "Anything"
            Src.PVar name -> Aeson.object [ "type" Aeson..= ("Var" :: String), "name" Aeson..= ES.toChars (Name.toElmString name) ]
            Src.PRecord fields -> Aeson.object [ "type" Aeson..= ("Record" :: String), "fields" Aeson..= map encodeLocated fields ]
            Src.PAlias pat name -> Aeson.object [ "type" Aeson..= ("Alias" :: String), "pattern" Aeson..= encodePattern pat, "name" Aeson..= encodeLocated name ]
            Src.PUnit -> Aeson.String "Unit"
            Src.PTuple p1 p2 ps -> Aeson.object [ "type" Aeson..= ("Tuple" :: String), "patterns" Aeson..= map encodePattern (p1:p2:ps) ]
            Src.PCtor _ name patterns -> Aeson.object [ "type" Aeson..= ("Ctor" :: String), "name" Aeson..= ES.toChars (Name.toElmString name), "patterns" Aeson..= map encodePattern patterns ]
            Src.PCtorQual _ name qual patterns -> Aeson.object [ "type" Aeson..= ("CtorQual" :: String), "name" Aeson..= ES.toChars (Name.toElmString name), "qual" Aeson..= ES.toChars (Name.toElmString qual), "patterns" Aeson..= map encodePattern patterns ]
            Src.PList patterns -> Aeson.object [ "type" Aeson..= ("List" :: String), "patterns" Aeson..= map encodePattern patterns ]
            Src.PCons p1 p2 -> Aeson.object [ "type" Aeson..= ("Cons" :: String), "head" Aeson..= encodePattern p1, "tail" Aeson..= encodePattern p2 ]
            Src.PChr str -> Aeson.object [ "type" Aeson..= ("Chr" :: String), "value" Aeson..= ES.toChars str ]
            Src.PStr str -> Aeson.object [ "type" Aeson..= ("Str" :: String), "value" Aeson..= ES.toChars str ]
            Src.PInt i -> Aeson.object [ "type" Aeson..= ("Int" :: String), "value" Aeson..= i ]
        ]


encodeExpr :: Src.Expr -> Aeson.Value
encodeExpr (A.At region expr_) =
    Aeson.object
        [ "region" Aeson..= encodeRegion region
        , "expr" Aeson..= case expr_ of
            Src.Chr str -> Aeson.object [ "type" Aeson..= ("Chr" :: String), "value" Aeson..= ES.toChars str ]
            Src.Str str -> Aeson.object [ "type" Aeson..= ("Str" :: String), "value" Aeson..= ES.toChars str ]
            Src.Int i -> Aeson.object [ "type" Aeson..= ("Int" :: String), "value" Aeson..= i ]
            Src.Float f -> Aeson.object [ "type" Aeson..= ("Float" :: String), "value" Aeson..= BL.unpack (B.toLazyByteString (EF.toBuilder f)) ]
            Src.Var _ name -> Aeson.object [ "type" Aeson..= ("Var" :: String), "name" Aeson..= ES.toChars (Name.toElmString name) ]
            Src.VarQual _ name qual -> Aeson.object [ "type" Aeson..= ("VarQual" :: String), "name" Aeson..= ES.toChars (Name.toElmString name), "qual" Aeson..= ES.toChars (Name.toElmString qual) ]
            Src.List exprs -> Aeson.object [ "type" Aeson..= ("List" :: String), "exprs" Aeson..= map encodeExpr exprs ]
            Src.Op name -> Aeson.object [ "type" Aeson..= ("Op" :: String), "name" Aeson..= ES.toChars (Name.toElmString name) ]
            Src.Negate expr -> Aeson.object [ "type" Aeson..= ("Negate" :: String), "expr" Aeson..= encodeExpr expr ]
            Src.Binops exprs expr -> Aeson.object [ "type" Aeson..= ("Binops" :: String), "exprs" Aeson..= map (\(e, op) -> Aeson.object [ "expr" Aeson..= encodeExpr e, "op" Aeson..= encodeLocated op ]) exprs, "expr" Aeson..= encodeExpr expr ]
            Src.Lambda patterns expr -> Aeson.object [ "type" Aeson..= ("Lambda" :: String), "patterns" Aeson..= map encodePattern patterns, "expr" Aeson..= encodeExpr expr ]
            Src.Call func args -> Aeson.object [ "type" Aeson..= ("Call" :: String), "func" Aeson..= encodeExpr func, "args" Aeson..= map encodeExpr args ]
            Src.If branches expr -> Aeson.object [ "type" Aeson..= ("If" :: String), "branches" Aeson..= map (\(cond, res) -> Aeson.object [ "condition" Aeson..= encodeExpr cond, "result" Aeson..= encodeExpr res ]) branches, "expr" Aeson..= encodeExpr expr ]
            Src.Let defs expr -> Aeson.object [ "type" Aeson..= ("Let" :: String), "defs" Aeson..= map encodeDef defs, "expr" Aeson..= encodeExpr expr ]
            Src.Case expr branches -> Aeson.object [ "type" Aeson..= ("Case" :: String), "expr" Aeson..= encodeExpr expr, "branches" Aeson..= map (\(pat, expr) -> Aeson.object [ "pattern" Aeson..= encodePattern pat, "expr" Aeson..= encodeExpr expr ]) branches ]
            Src.Accessor name -> Aeson.object [ "type" Aeson..= ("Accessor" :: String), "name" Aeson..= ES.toChars (Name.toElmString name) ]
            Src.Access expr name -> Aeson.object [ "type" Aeson..= ("Access" :: String), "expr" Aeson..= encodeExpr expr, "name" Aeson..= encodeLocated name ]
            Src.Update name fields -> Aeson.object [ "type" Aeson..= ("Update" :: String), "name" Aeson..= encodeLocated name, "fields" Aeson..= map (\(n, e) -> Aeson.object [ "name" Aeson..= encodeLocated n, "expr" Aeson..= encodeExpr e ]) fields ]
            Src.Record fields -> Aeson.object [ "type" Aeson..= ("Record" :: String), "fields" Aeson..= map (\(n, e) -> Aeson.object [ "name" Aeson..= encodeLocated n, "expr" Aeson..= encodeExpr e ]) fields ]
            Src.Unit -> Aeson.object [ "type" Aeson..= ("Unit" :: String) ]
            Src.Tuple e1 e2 es -> Aeson.object [ "type" Aeson..= ("Tuple" :: String), "exprs" Aeson..= map encodeExpr (e1:e2:es) ]
            Src.Shader _ _ -> Aeson.object [ "type" Aeson..= ("Shader" :: String) ]
        ]


encodeDef :: A.Located Src.Def -> Aeson.Value
encodeDef (A.At region def) =
    Aeson.object
        [ "region" Aeson..= encodeRegion region
        , "def" Aeson..= case def of
            Src.Define name patterns expr _ -> Aeson.object [ "type" Aeson..= ("Define" :: String), "name" Aeson..= encodeLocated name, "patterns" Aeson..= map encodePattern patterns, "expr" Aeson..= encodeExpr expr ]
            Src.Destruct pat expr -> Aeson.object [ "type" Aeson..= ("Destruct" :: String), "pattern" Aeson..= encodePattern pat, "expr" Aeson..= encodeExpr expr ]
        ]


encodeType :: Src.Type -> Aeson.Value
encodeType (A.At region type_) =
    Aeson.object
        [ "region" Aeson..= encodeRegion region
        , "type" Aeson..= case type_ of
            Src.TLambda t1 t2 -> Aeson.object [ "type" Aeson..= ("Lambda" :: String), "arg" Aeson..= encodeType t1, "result" Aeson..= encodeType t2 ]
            Src.TVar name -> Aeson.object [ "type" Aeson..= ("Var" :: String), "name" Aeson..= ES.toChars (Name.toElmString name) ]
            Src.TType _ name types -> Aeson.object [ "type" Aeson..= ("Type" :: String), "name" Aeson..= ES.toChars (Name.toElmString name), "types" Aeson..= map encodeType types ]
            Src.TTypeQual _ name qual types -> Aeson.object [ "type" Aeson..= ("TypeQual" :: String), "name" Aeson..= ES.toChars (Name.toElmString name), "qual" Aeson..= ES.toChars (Name.toElmString qual), "types" Aeson..= map encodeType types ]
            Src.TRecord fields _ -> Aeson.object [ "type" Aeson..= ("Record" :: String), "fields" Aeson..= map (\(n, t) -> Aeson.object [ "name" Aeson..= encodeLocated n, "type" Aeson..= encodeType t ]) fields ]
            Src.TUnit -> Aeson.object [ "type" Aeson..= ("Unit" :: String) ]
            Src.TTuple t1 t2 ts -> Aeson.object [ "type" Aeson..= ("Tuple" :: String), "types" Aeson..= map encodeType (t1:t2:ts) ]
        ]


encodeConstructor :: (A.Located Name.Name, [Src.Type]) -> Aeson.Value
encodeConstructor (name, types) =
    Aeson.object
        [ "name" Aeson..= encodeLocated name
        , "types" Aeson..= map encodeType types
        ]


encodeLocatedVar :: A.Located Name.Name -> Aeson.Value
encodeLocatedVar (A.At region value) =
    Aeson.object
        [ "region" Aeson..= encodeRegion region
        , "value" Aeson..= ES.toChars (Name.toElmString value)
        ] 