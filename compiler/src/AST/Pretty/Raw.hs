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
import qualified Data.Aeson.Encode.Pretty as AesonPretty
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
import System.IO (appendFile, readFile)
import System.Directory (getCurrentDirectory)
import qualified Elm.Float as EF
import qualified Data.ByteString.Builder as B
import Data.List (intercalate, findIndex, tails, isPrefixOf)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import qualified Data.Aeson.Key as AesonKey
import Text.Regex.TDFA ((=~))


data Config =
    Config
        { format :: OutputFormat
        , showPlaceholders :: Bool
        , indentSize :: Int
        , maxLineLength :: Int
        }


data OutputFormat
    = AstRaw
    | AstJson
    | AstJsonPretty
    | Rag
    | RagJson
    | RagJsonPretty


defaultConfig :: Config
defaultConfig =
    Config
        { format = AstRaw
        , showPlaceholders = True
        , indentSize = 2
        , maxLineLength = 80
        }


pretty :: Config -> FilePath -> Src.Module -> String
pretty config filePath modul =
    case format config of
        AstRaw -> prettyRaw config modul
        AstJson -> prettyJson config modul
        AstJsonPretty -> prettyJsonPretty config modul
        Rag -> prettyRag config filePath modul
        RagJson -> prettyRagJson config filePath modul
        RagJsonPretty -> prettyRagJsonPretty config filePath modul


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


prettyJsonPretty :: Config -> Src.Module -> String
prettyJsonPretty config modul =
    BL.unpack $ AesonPretty.encodePretty $ encodeModule modul


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


prettyRagJson :: Config -> FilePath -> Src.Module -> String
prettyRagJson config filePath modul =
    BL.unpack $ Aeson.encode $ parseRagOutput (prettyRag config filePath modul)


prettyRagJsonPretty :: Config -> FilePath -> Src.Module -> String
prettyRagJsonPretty config filePath modul =
    BL.unpack $ AesonPretty.encodePretty' (AesonPretty.defConfig { AesonPretty.confIndent = AesonPretty.Spaces 2 }) $ parseRagOutput (prettyRag config filePath modul)


parseRagOutput :: String -> Aeson.Value
parseRagOutput str =
    let chunks = splitOn "\n\n" str
        parseChunk chunk =
            let lines = splitOn "\n" chunk
                parseLine line =
                    case splitOn ": " line of
                        [key, value] -> AesonKey.fromText (T.pack key) Aeson..= Aeson.String (T.pack value)
                        _ -> AesonKey.fromText (T.pack "") Aeson..= Aeson.Null
                pairs = map parseLine lines
            in Aeson.object pairs
    in Aeson.Array $ V.fromList $ map parseChunk chunks


splitOn :: String -> String -> [String]
splitOn delim str = 
    case breakOn delim str of
        (a, b) -> a : if null b then [] else splitOn delim (drop (length delim) b)


breakOn :: String -> String -> (String, String)
breakOn delim str = 
    case findIndex (isPrefixOf delim) (tails str) of
        Just i -> splitAt i str
        Nothing -> (str, "")


prettyRag :: Config -> FilePath -> Src.Module -> String
prettyRag config filePath modul =
    let values = map (prettyRagValue filePath modul) (Src._values modul)
        unions = map (prettyRagUnion filePath modul) (Src._unions modul)
        aliases = map (prettyRagAlias filePath modul) (Src._aliases modul)
        ports = map (prettyRagPort filePath modul) (getPorts modul)
    in unlines (values ++ unions ++ aliases ++ ports)

prettyRagValue :: FilePath -> Src.Module -> A.Located Src.Value -> String
prettyRagValue filePath modul (A.At region (Src.Value name patterns expr _)) =
    let lineNumbers = getLineNumbers region
        docstring = getDocstringFromRegion filePath region
        calls = getCalls expr
        imports = getImports modul
    in unlines
        [ "Type: Function"
        , "Name: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Code: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Language: elm"
        , "File: " ++ filePath
        , "StartLine: " ++ show (fst lineNumbers)
        , "EndLine: " ++ show (snd lineNumbers)
        , "Calls: " ++ show calls
        , "Imports: " ++ show imports
        , "Docstring: " ++ docstring
        ]

prettyRagUnion :: FilePath -> Src.Module -> A.Located Src.Union -> String
prettyRagUnion filePath modul (A.At region (Src.Union name vars _)) =
    let lineNumbers = getLineNumbers region
        docstring = getDocstringFromRegion filePath region
        imports = getImports modul
    in unlines
        [ "Type: Type"
        , "Name: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Code: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Language: elm"
        , "File: " ++ filePath
        , "StartLine: " ++ show (fst lineNumbers)
        , "EndLine: " ++ show (snd lineNumbers)
        , "Calls: []"
        , "Imports: " ++ show imports
        , "Docstring: " ++ docstring
        ]

prettyRagAlias :: FilePath -> Src.Module -> A.Located Src.Alias -> String
prettyRagAlias filePath modul (A.At region (Src.Alias name _ _)) =
    let lineNumbers = getLineNumbers region
        docstring = getDocstringFromRegion filePath region
        imports = getImports modul
    in unlines
        [ "Type: TypeAlias"
        , "Name: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Code: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Language: elm"
        , "File: " ++ filePath
        , "StartLine: " ++ show (fst lineNumbers)
        , "EndLine: " ++ show (snd lineNumbers)
        , "Calls: []"
        , "Imports: " ++ show imports
        , "Docstring: " ++ docstring
        ]

prettyRagPort :: FilePath -> Src.Module -> Src.Port -> String
prettyRagPort filePath modul (Src.Port name _) =
    let lineNumbers = getLineNumbers (A.toRegion name)
        docstring = getDocstringFromRegion filePath (A.toRegion name)
        imports = getImports modul
    in unlines
        [ "Type: Port"
        , "Name: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Code: " ++ ES.toChars (Name.toElmString (A.toValue name))
        , "Language: elm"
        , "File: " ++ filePath
        , "StartLine: " ++ show (fst lineNumbers)
        , "EndLine: " ++ show (snd lineNumbers)
        , "Calls: []"
        , "Imports: " ++ show imports
        , "Docstring: " ++ docstring
        ]

getFilePath :: Src.Module -> String
getFilePath modul = "test-files/program1/src/Main.elm"  -- TODO: Get actual file path from module

getDocstringFromRegion :: FilePath -> A.Region -> String
getDocstringFromRegion filePath region = unsafePerformIO $ do
    let (A.Region (A.Position startLine _) _) = region
    content <- readFile filePath
    let lines = splitOn "\n" content
    let docstring = findDocstring lines (fromIntegral startLine)
    return docstring

findDocstring :: [String] -> Int -> String
findDocstring lines targetLine =
    let beforeLines = take (targetLine - 1) lines
        -- Find the last line that starts a docstring
        startIdx = case findIndex (isDocstringStart . snd) (zip [0..] (reverse beforeLines)) of
            Just idx -> length beforeLines - idx - 1
            Nothing -> -1
    in if startIdx == -1 then ""
       else let rest = drop startIdx beforeLines
                docBlock = takeWhileInclusive (not . isDocstringEnd) rest
                docString = unlines docBlock
            in extractDocstring docString

isDocstringStart :: String -> Bool
isDocstringStart line = (dropWhile (== ' ') line) =~ ("^\\{-\\|" :: String)

isDocstringEnd :: String -> Bool
isDocstringEnd line = (dropWhile (== ' ') line) =~ ("^-}\\s*$" :: String)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

extractDocstring :: String -> String
extractDocstring str =
    case str =~ ("\\{-\\|(.*)-\\}" :: String) :: (String, String, String, [String]) of
        (_, _, _, [doc]) -> trimDocstring doc
        _ -> ""

trimDocstring :: String -> String
trimDocstring doc =
    let lines = splitOn "\n" doc
        trimmedLines = map (dropWhile (== ' ')) lines
    in unlines trimmedLines

getCalls :: Src.Expr -> [String]
getCalls (A.At _ expr_) = case expr_ of
    Src.Call (A.At _ (Src.Var _ name)) _ -> [ES.toChars (Name.toElmString name)]
    Src.Call (A.At _ (Src.VarQual _ name _)) _ -> [ES.toChars (Name.toElmString name)]
    _ -> []

getImports :: Src.Module -> [String]
getImports modul =
    map (ES.toChars . Name.toElmString . A.toValue . Src._import) (Src._imports modul)

getLineNumbers :: A.Region -> (Int, Int)
getLineNumbers (A.Region (A.Position start _) (A.Position end _)) = (fromIntegral start, fromIntegral end)

prettyRaw :: Config -> Src.Module -> String
prettyRaw _ _ = "-- raw AST pretty printing not implemented --"
