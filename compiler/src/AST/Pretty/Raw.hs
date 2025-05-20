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
prettyPattern (A.At _ pattern_) =
    case pattern_ of
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
prettyExpr (A.At _ expr_) =
    case expr_ of
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
        Src.Let defs expr -> "let ... in " ++ prettyExpr expr
        Src.Case expr branches -> "case " ++ prettyExpr expr ++ " of ..."
        Src.Accessor name -> "." ++ ES.toChars (Name.toElmString name)
        Src.Access expr name -> prettyExpr expr ++ "." ++ ES.toChars (Name.toElmString (A.toValue name))
        Src.Update name fields -> "{" ++ ES.toChars (Name.toElmString (A.toValue name)) ++ " | ... }"
        Src.Record fields -> "{ " ++ unwords (map (\(n, e) -> ES.toChars (Name.toElmString (A.toValue n)) ++ " = " ++ prettyExpr e) fields) ++ " }"
        Src.Unit -> "()"
        Src.Tuple e1 e2 es -> "(" ++ prettyExpr e1 ++ ", " ++ prettyExpr e2 ++ concatMap ((", " ++) . prettyExpr) es ++ ")"
        Src.Shader _ _ -> "<shader>"


prettyValue :: Config -> Src.Value -> String
prettyValue config (Src.Value name patterns expr _) =
    "[Function] " ++ ES.toChars (Name.toElmString (A.toValue name)) ++
    " with patterns: " ++ unwords (map prettyPattern patterns) ++
    " = " ++ prettyExpr expr ++ "\n"


prettyUnions :: Config -> [Src.Union] -> String
prettyUnions config unions =
    unlines $ map (prettyUnion config) unions


prettyUnion :: Config -> Src.Union -> String
prettyUnion config (Src.Union name vars _) =
    "[Type] " ++ ES.toChars (Name.toElmString (A.toValue name))


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