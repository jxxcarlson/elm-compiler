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


pretty :: Config -> Src.Module -> String
pretty config modul =
    case format config of
        Text ->
            prettyText config modul

        Json ->
            prettyJson config modul


prettyText :: Config -> Src.Module -> String
prettyText config modul =
    let
        header =
            "Module: " ++ ES.toChars (Name.toElmString (Src.getName modul)) ++ "\n"
    in
    header ++ prettyValues config (map A.toValue (Src._values modul))
        ++ prettyUnions config (map A.toValue (Src._unions modul))
        ++ prettyAliases config (map A.toValue (Src._aliases modul))
        ++ prettyPorts config (getPorts modul)


prettyValues :: Config -> [Src.Value] -> String
prettyValues config values =
    unlines $ map (prettyValue config) values


prettyValue :: Config -> Src.Value -> String
prettyValue config (Src.Value name patterns expr _) =
    "[Function] " ++ ES.toChars (Name.toElmString (A.toValue name))


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