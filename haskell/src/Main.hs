{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}

module Main where

import Data.Text (Text)
import Data.Map (Map)

import BasePrelude
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text.Strict.Lens

type MonadEval m =
  (MonadReader World m, MonadError Text m)

data Value =
  VAtom Text
    | VNumber Integer
    | VString Text
    | VBool Bool
    | VList [Value]
    deriving (Show, Eq)

newtype Func =
  Func (forall m. MonadEval m => [Value] -> m Value)

newtype World =
  World (Map Text Func)

_World :: Iso' World (Map Text Func)
_World =
  iso (\(World w) -> w) World

boil :: MonadEval m => Value -> m Value
boil v =
  case v of
    VAtom _ -> pure v
    VNumber _ -> pure v
    VBool _ -> pure v
    VList [VAtom "quote", quoted] -> pure quoted
    VList (VAtom func : args) -> mapM boil args >>= smash func
    _ -> throwError ("boil: unrecognized form: " <> view packed (show v))

smash :: MonadEval m => Text -> [Value] -> m Value
smash func args = do
  registeredMaybe <- view (_World . at func)
  case registeredMaybe of
    Nothing ->
      throwError ("smash: unable to find func: " <> func)
    Just (Func registered) ->
      registered args

test0 :: IO ()
test0 = do
  assert (unwrapped == Right (VNumber 4)) $ do
    putChar '.'
  where
    boiled = boil (VList [VAtom "+", VNumber 2, VNumber 2])
    unwrapped = runIdentity (runReaderT (runExceptT boiled) world)
    world = World [ ("+", preludeMath (+))
                  , ("-", preludeMath (-))
                  , ("*", preludeMath (*))
                  ]

preludeMath :: (Integer -> Integer -> Integer) -> Func
preludeMath bin = Func $ \args ->
  case args of
    [VNumber i, VNumber j] ->
      pure $ VNumber (bin i j)
    _ ->
      throwError ("preludeMath: bad args: " <> view packed (show args))

main :: IO ()
main = do
  test0
  putChar '\n'
