import Data.List

type Commands = [(Command, Action)]
type Command  = String
type Argument = String
type Action   = [Argument] -> IO ()

theCommands = [("echo2", \args -> putStrLn (unwords args))
              ,("fib",  \args -> putStrLn (show (fib (read (head args)))))
              ]

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


doCommands :: Commands -> IO ()
doCommands commands = do 
  putStr "> "
  str <- getLine
  let x = words str
  if (length x) == 0 
  then putStrLn "GoodBye" 
  else let cmd = head x
           arg = tail x
           f = lookup cmd commands
           in
        case f of
          Nothing -> do putStrLn (cmd ++ " does not exsists")
                        doCommands theCommands
          Just x -> do x arg
                       doCommands theCommands


g1 :: (Functor f) => f (a, b) -> f (b, a)
g1 = fmap (\(a,b) -> (b,a))

g2 :: (Functor f) => f (f (a, b)) -> f (f (a, b, b, a))
g2 = fmap (\x -> fmap (\(a,b) -> (a,b,b,a)) x)

g3 :: (Functor f, Functor g) => f a -> g b -> f (g (a, b))
g3 af bg = fmap (\a -> fmap (\b -> (a,b)) bg) af 

g4 :: (Applicative f, Applicative g) => a -> f (g a)
g4 a = pure (pure a)


main :: IO()
main = do
  doCommands theCommands
