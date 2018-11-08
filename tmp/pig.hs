import Control.Monad
import Control.Monad
import Data.Monoid
import qualified Data.Vector as V

data VM = VM { memory :: V.Vector Word
             , stack :: [Word]
             , pointer :: Int
             , prog :: V.Vector Command }
          deriving Show

data Command = POP
             | PUSH
             | STORE
             | RESTORE
             | Num Word
             deriving Show

newtype Action m a = Action { runAction :: a -> m a }

instance Monad m => Semigroup (Action m a) where
  a1 <> a2 = Action $ (runAction a1 >=> runAction a2)
instance Monad m => Monoid (Action m a) where
  mempty = Action pure
  

newtype Program a = Program { runProgram :: Action (Writer Log VM) }
