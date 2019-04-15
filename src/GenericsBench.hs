{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language TypeInType #-}

module GenericsBench where

import Data.Kind
import Data.List
import Data.Unique
import System.Random

data Strictness = Strict | Lazy

data OLevel = O0 | O1 | O2

data Typ
  = TypVoid -- ^ 'Data.Void.Void'
  | TypUnit -- ^ '()'
  | TypFunction Typ Typ -- ^ ('->') a b
  | TypInt -- ^ 'Data.Int.Int'
  | TypString -- ^ 'Data.String.String'
  | TypProxy Typ -- ^ 'Data.Proxy.Proxy' a
  | TypFunctor Typ -- ^ 'Data.Monoid.Ap' 'Data.Functor.Identity.Identity' a

data Namedness
  = Named
  | Unnamed

data NameValue :: Namedness -> Type where
  NamedValue :: String -> NameValue 'Named
  UnnamedValue :: NameValue 'Unnamed

data Field :: Namedness -> Type where
  Field :: ()
    => NameValue n
    -> Typ
    -> Strictness
    -> Field n

data Con :: Namedness -> Type where
  Con :: ()
    => String -- ^ name
    -> [Field n] -- ^ fields
    -> Con n

data DataTyp (n :: Namedness) where
  DataTyp :: ()
    => String
    -> [Con n]
    -> DataTyp n

-- the current encoding enforces a property i don't like:
-- that a module's 'Namedness' determines that of all its
-- datatypes.
data Module (n :: Namedness) where
  Module :: ()
    => String
    -> OLevel
    -> [DataTyp n]
    -> Module n

---------------------------------------------------------------------
-- Static Encoding
---------------------------------------------------------------------

-- | things that don't need a unique
class StaticEncode a where
  sencode :: a -> String

instance StaticEncode Strictness where
  sencode = \case
    Strict -> "!"
    Lazy -> ""

instance StaticEncode OLevel where
  sencode = \case
    O0 -> "O0"
    O1 -> "O1"
    O2 -> "O2"

instance StaticEncode Typ where
  sencode = \case
    TypVoid -> "Void"
    TypUnit -> "()"
    TypFunction ty1 ty2 -> sencode ty1 <> " -> " <> sencode ty2
    TypInt -> "Int"
    TypString -> "String"
    TypProxy ty -> "Proxy " <> sencode ty
    TypFunctor ty -> "Ap Identity " <> sencode ty

---------------------------------------------------------------------
-- Unique Encoding
---------------------------------------------------------------------

class UniqueEncode a where
  uencode :: a -> IO String

instance UniqueEncode (Field 'Named) where
  uencode (Field (NamedValue name) ty s) = do
    uid <- newUniqId
    pure $ mconcat
      [ name, uid, " :: ", sencode s, "(", sencode ty, ")" ]

instance UniqueEncode (Field 'Unnamed) where
  uencode (Field _ ty s) = pure $ mconcat
    [ sencode s, "(", sencode ty, ")" ]

instance UniqueEncode (Con 'Named) where
  uencode (Con nm fs) = do
    uid <- newUniqId
    fields <- mapM uencode fs
    let fields' = intercalate ", " fields
    pure $ mconcat
      [ nm, uid, " {", fields', "}" ]

instance UniqueEncode (Con 'Unnamed) where
  uencode (Con nm fs) = do
    uid <- newUniqId
    fields <- mapM uencode fs
    let fields' = unwords fields
    pure $ mconcat
      [ nm, uid, " ", fields' ]

instance UniqueEncode (Con n) => UniqueEncode (DataTyp n) where
  uencode (DataTyp name cons) = do
    uid <- newUniqId
    econs <- mapM uencode cons
    let econs' = intercalate "\n  | " econs
    pure $ mconcat
      [ "data ", name, uid, "\n  = ", econs' ]

instance UniqueEncode (Con n) => UniqueEncode (Module n) where
  uencode (Module name o ds) = do
    uid <- newUniqId
    eds <- mapM uencode ds
    let eds' = intercalate "\n\n" eds
    pure $ mconcat $
      [ "{-# language BangPatterns, DeriveGeneric #-}\n\n"
      , "{-# options_ghc -", sencode o, " -#}\n\n"
      , "module ", name, uid, " where\n\n"
      , "import GHC.Generics\n\n"
      , eds'
      , "\n"
      ]

newUniqId :: IO String
newUniqId = do
  u <- newUnique
  pure (show (hashUnique u))

---------------------------------------------------------------------
-- Randomisation for running
---------------------------------------------------------------------

---------------------------------------------------------------------
-- Some test values for the GHCi
---------------------------------------------------------------------
{-
fs_ :: [Field 'Named]
fs_ =
  [ Field (NamedValue "f") (TypFunctor TypVoid) Strict
  , Field (NamedValue "f") TypVoid Lazy
  , Field (NamedValue "f") (TypProxy TypString) Lazy
  ]

fs__ :: [Field 'Unnamed]
fs__ =
  [ Field UnnamedValue (TypFunctor TypVoid) Strict
  , Field UnnamedValue TypVoid Lazy
  , Field UnnamedValue (TypProxy TypString) Lazy
  ]

c_ :: Con 'Named
c_ = Con "C" fs_

c__ :: Con 'Unnamed
c__ = Con "C" fs__

d_ :: DataTyp 'Named
d_ = DataTyp "T" [c_]

d__ :: DataTyp 'Unnamed
d__ = DataTyp "T" (replicate 3 c__)

m_ :: Module 'Named
m_ = Module "M" O2 (replicate 3 d_)

m__ :: Module 'Unnamed
m__ = Module "M" O0 (replicate 5 d__)
-}


