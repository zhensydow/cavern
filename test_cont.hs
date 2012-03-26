import Control.Monad.IO.Class( liftIO )
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

test :: Coroutine (Yield Int) IO String
test = do
  liftIO $ print "inicio"
  yield 2
  liftIO $ print "test"
  yield 3
  liftIO $ print "test 2"
  yield 4
  return "finished"
