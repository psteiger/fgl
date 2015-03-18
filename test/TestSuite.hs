{- |
   Module      : TestSuite
   Description : fgl test suite
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import           Data.Graph.Inductive.Arbitrary    ()
import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.PatriciaTree as P
import qualified Data.Graph.Inductive.Tree         as T

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Word (Word8)

-- -----------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Tree Graphs"         (graphTests (Proxy :: TreeP))
  describe "PatriciaTree Graphs" (graphTests (Proxy :: PatriciaTreeP))

-- -----------------------------------------------------------------------------

graphTests :: (Graph gr) => GraphProxy gr -> Spec
graphTests p = return ()

-- -----------------------------------------------------------------------------

-- By default, we want to avoid using 'Int' to avoid clashing with the
-- 'Node' type.  Don't want to use a floating type in case of
-- potential Eq problems.
type GraphType gr = gr Char Word8

type GraphProxy gr = Proxy (GraphType gr)

type TreeP = GraphProxy T.Gr

type PatriciaTreeP = GraphProxy P.Gr

-- Not using the Data.Proxy module so this also works with older
-- versions of GHC.

data Proxy a = Proxy
  deriving (Eq, Ord, Show, Read)

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf a _ = a

aFrom :: a -> Proxy (gr a b) -> a
aFrom a _ = a
