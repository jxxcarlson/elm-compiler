{-# LANGUAGE OverloadedStrings #-}
module AST.Json
  ( toJson
  )
  where

import AST.Canonical (Module(..), Decls(..), Def(..), Expr_(..), Pattern_(..), Type(..), FieldType(..), Effects(..), Port(..))
import Build.Artifacts (Artifacts(..))
import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified AST.Interface as I
import Control.Concurrent.MVar (readMVar)
import Control.Monad (unsafePerformIO)

instance ToJSON Module where
  toJSON (Module name exports docs imports decls) =
    object
      [ "name" .= name
      , "exports" .= exports
      , "docs" .= docs
      , "imports" .= imports
      , "decls" .= decls
      ]

instance ToJSON Decls where
  toJSON (Decls defs unions aliases ports) =
    object
      [ "defs" .= defs
      , "unions" .= unions
      , "aliases" .= aliases
      , "ports" .= ports
      ]

instance ToJSON Def where
  toJSON (Def name args type_ expr) =
    object
      [ "name" .= name
      , "args" .= args
      , "type" .= type_
      , "expr" .= expr
      ]

instance ToJSON Expr_ where
  toJSON (Var name) = object ["tag" .= ("Var" :: String), "name" .= name]
  toJSON (Lambda pattern_ expr) = object ["tag" .= ("Lambda" :: String), "pattern" .= pattern_, "expr" .= expr]
  toJSON (Call func args) = object ["tag" .= ("Call" :: String), "func" .= func, "args" .= args]
  toJSON (If branches finally) = object ["tag" .= ("If" :: String), "branches" .= branches, "finally" .= finally]
  toJSON (Let defs expr) = object ["tag" .= ("Let" :: String), "defs" .= defs, "expr" .= expr]
  toJSON (Case expr branches) = object ["tag" .= ("Case" :: String), "expr" .= expr, "branches" .= branches]
  toJSON (Accessor field) = object ["tag" .= ("Accessor" :: String), "field" .= field]
  toJSON (Access expr field) = object ["tag" .= ("Access" :: String), "expr" .= expr, "field" .= field]
  toJSON (Update name updates) = object ["tag" .= ("Update" :: String), "name" .= name, "updates" .= updates]
  toJSON (Record fields) = object ["tag" .= ("Record" :: String), "fields" .= fields]
  toJSON (Unit) = object ["tag" .= ("Unit" :: String)]
  toJSON (Tuple a b maybe_c) = object ["tag" .= ("Tuple" :: String), "a" .= a, "b" .= b, "maybe_c" .= maybe_c]
  toJSON (List entries) = object ["tag" .= ("List" :: String), "entries" .= entries]
  toJSON (Cons head tail) = object ["tag" .= ("Cons" :: String), "head" .= head, "tail" .= tail]
  toJSON (Binop op left right) = object ["tag" .= ("Binop" :: String), "op" .= op, "left" .= left, "right" .= right]
  toJSON (LambdaCase cases) = object ["tag" .= ("LambdaCase" :: String), "cases" .= cases]
  toJSON (Literal literal) = object ["tag" .= ("Literal" :: String), "literal" .= literal]
  toJSON (GLShader src) = object ["tag" .= ("GLShader" :: String), "src" .= src]

instance ToJSON Pattern_ where
  toJSON (PVar name) = object ["tag" .= ("PVar" :: String), "name" .= name]
  toJSON (PRecord fields) = object ["tag" .= ("PRecord" :: String), "fields" .= fields]
  toJSON (PAlias pattern_ name) = object ["tag" .= ("PAlias" :: String), "pattern" .= pattern_, "name" .= name]
  toJSON (PUnit) = object ["tag" .= ("PUnit" :: String)]
  toJSON (PTuple a b maybe_c) = object ["tag" .= ("PTuple" :: String), "a" .= a, "b" .= b, "maybe_c" .= maybe_c]
  toJSON (PList entries) = object ["tag" .= ("PList" :: String), "entries" .= entries]
  toJSON (PCons head tail) = object ["tag" .= ("PCons" :: String), "head" .= head, "tail" .= tail]
  toJSON (PAnything) = object ["tag" .= ("PAnything" :: String)]
  toJSON (PBool bool) = object ["tag" .= ("PBool" :: String), "bool" .= bool]
  toJSON (PChr char) = object ["tag" .= ("PChr" :: String), "char" .= char]
  toJSON (PStr string) = object ["tag" .= ("PStr" :: String), "string" .= string]
  toJSON (PInt int) = object ["tag" .= ("PInt" :: String), "int" .= int]
  toJSON (PCtor home name args) = object ["tag" .= ("PCtor" :: String), "home" .= home, "name" .= name, "args" .= args]

instance ToJSON Type where
  toJSON (Var name) = object ["tag" .= ("Var" :: String), "name" .= name]
  toJSON (Lambda arg result) = object ["tag" .= ("Lambda" :: String), "arg" .= arg, "result" .= result]
  toJSON (Tuple a b maybe_c) = object ["tag" .= ("Tuple" :: String), "a" .= a, "b" .= b, "maybe_c" .= maybe_c]
  toJSON (Record fields) = object ["tag" .= ("Record" :: String), "fields" .= fields]
  toJSON (Unit) = object ["tag" .= ("Unit" :: String)]
  toJSON (Ctor home name args) = object ["tag" .= ("Ctor" :: String), "home" .= home, "name" .= name, "args" .= args]

instance ToJSON FieldType where
  toJSON (FieldType name type_) = object ["name" .= name, "type" .= type_]

instance ToJSON Effects where
  toJSON (NoEffects) = object ["tag" .= ("NoEffects" :: String)]
  toJSON (Send home name args) = object ["tag" .= ("Send" :: String), "home" .= home, "name" .= name, "args" .= args]

instance ToJSON Port where
  toJSON (In name type_) = object ["tag" .= ("In" :: String), "name" .= name, "type" .= type_]
  toJSON (Out name type_) = object ["tag" .= ("Out" :: String), "name" .= name, "type" .= type_]

instance ToJSON I.Interface where
  toJSON (I.Interface home values unions aliases binops) =
    object
      [ "home" .= home
      , "values" .= values
      , "unions" .= unions
      , "aliases" .= aliases
      , "binops" .= binops
      ]

instance ToJSON I.Union where
  toJSON union =
    case union of
      I.OpenUnion u -> object ["tag" .= ("OpenUnion" :: String), "union" .= u]
      I.ClosedUnion u -> object ["tag" .= ("ClosedUnion" :: String), "union" .= u]
      I.PrivateUnion u -> object ["tag" .= ("PrivateUnion" :: String), "union" .= u]

instance ToJSON I.Alias where
  toJSON alias =
    case alias of
      I.PublicAlias a -> object ["tag" .= ("PublicAlias" :: String), "alias" .= a]
      I.PrivateAlias a -> object ["tag" .= ("PrivateAlias" :: String), "alias" .= a]

instance ToJSON I.Binop where
  toJSON (I.Binop name annotation associativity precedence) =
    object
      [ "name" .= name
      , "annotation" .= annotation
      , "associativity" .= associativity
      , "precedence" .= precedence
      ]

-- Convert Build.Artifacts to JSON
toJson :: Artifacts -> LBS.ByteString
toJson artifacts =
  case _modules artifacts of
    [module_] ->
      case module_ of
        Fresh { _modul = canModule } ->
          Aeson.encode $ object
            [ "name" .= T.pack "AST"
            , "value" .= canModule
            ]
        Cached { _iface = ifaceMVar } ->
          unsafePerformIO $ do
            iface <- readMVar ifaceMVar
            return $ Aeson.encode $ object
              [ "name" .= T.pack "AST"
              , "value" .= iface
              , "note" .= T.pack "Cached module: canonical AST not available"
              ]
    _ ->
      Aeson.encode $ object
        [ "name" .= T.pack "AST"
        , "value" .= T.pack "No module found"
        ]