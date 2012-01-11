import Chapter08
import Practice08 hiding(eval)
import Calculate09

-- 1
readLine ::IO String
readLine = do x <- getChar
              case x of
                '\DEL' -> return "\ESC[1D"
                '\n'   -> return []
                _      -> do xs <- readLine
                             return (x:xs)


readLine2 = get ""
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              '\DEL' -> if null xs then
                          get xs
                        else
                          do putStr "\ESC[1D \ESC[1D"
                             get (init xs)
              _ -> get (xs ++ [x])

-- 2
-- 3
-- 4
-- 5
-- 6
-- 7