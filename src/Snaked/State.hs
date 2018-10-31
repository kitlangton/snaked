module Snaked.State where

newtype MyState s a = MyState { unState :: s -> (a, s)}

stateLength :: MyState String Int
stateLength = MyState (\s -> (length s, s))

modify :: (s -> s) -> MyState s ()
modify f = MyState (\s -> ((), f s))
