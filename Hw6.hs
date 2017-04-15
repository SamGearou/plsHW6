
--e ::= x | e1 e2 | lambda x. e
module Hw6 where

import Control.Applicative
import Data.Char
import Data.List

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import System.Environment
import System.Exit

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s


instance Applicative Parser where
   pure a = Parser $ \s -> Just (a,s)
   f <*> a = Parser $ \s ->
     case parse f s of
       Just (g,s') -> parse (fmap g a) s'
       Nothing -> Nothing

instance Alternative Parser where
   empty = Parser $ \s -> Nothing
   l <|> r = Parser $ \s -> parse l s <|> parse r s

type VarName = String

data LC =
   Var VarName
 | Seq LC LC
 | Lambda [VarName] LC
 | Assign VarName LC LC
 deriving (Eq)   

instance Show LC where
  show (Lambda x y)                    = "lambda " ++ (concatMap (\x -> x ++ " ") x) ++ ". " ++ show y
  show (Var x)                         = x
  show (Seq (Var x) (Var y))           = x ++ " " ++ y
  show (Seq (Var a) (Lambda x y))      = a ++ " " ++ parens' (show (Lambda x y))
  show (Seq (Lambda x y) (Var a))      = parens' (show (Lambda x y)) ++ " " ++ a
  show (Seq (Lambda x y) (Lambda a b)) = parens' (show (Lambda x y)) ++ " " ++ parens' (show (Lambda a b))
  show (Seq a (Lambda x y))            = parens' (show a) ++ " " ++ parens' (show (Lambda x y))
  show (Seq (Lambda x y) a)            = parens' (show (Lambda x y)) ++ " " ++ parens' (show a)
  show (Seq a (Seq x y))               = show a ++ " " ++ parens' (show (Seq x y))
  show (Seq x y)                       = show x ++ " " ++ show y    

parens' :: String -> String
parens' a = "(" ++ a ++ ")"

spaces' :: Parser ()
spaces' = some (satisfy isSpace) *> pure ()

char' :: Char -> Parser Char
char' c = spaces' *> satisfy (==c)

var' :: Parser String
var' = ensure (not . isKeyword) $ spaces' *> some (satisfy isAlpha)

isDot :: Char -> Bool
isDot c = c == '.'


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep = foldl (\acc (op,v) -> op acc v) <$>
                p <*> many ((\op v -> (op,v)) <$> sep <*> p)

char :: Char -> Parser Char
char c = spaces *> satisfy (==c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

spaces :: Parser ()
spaces = many (satisfy isSpace) *> pure ()

parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

ws :: Parser ()
ws = pure () <* many (satisfy isSpace)

keywords :: [String]
keywords = ["let", "in", "lambda"]

isKeyword = (`elem` keywords)

str :: String -> Parser String
str s = ws *> loop s
  where loop [] = pure []
        loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

str' :: String -> Parser String
str' s = spaces' *> loop s
  where loop [] = pure []
        loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs        

var :: Parser String
var = ensure (not . isKeyword) $ ws *> some (satisfy isAlpha)

ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a then Just (a,s') else Nothing

--str "let " *> spaces *> Assign <$> var <* (" " <* spaces <* char '.' <* " " <* spaces)
--  <|> str "lambda " *> spaces *> Lambda <$> many (satisfy noDot) <* (char '.' <* spaces)  

sequence', assign, lam, atom :: Parser LC
assign = Assign <$> (spaces *> str "let" *> var' <* char' '=') <*> lam <* str' "in" <*> assign
       <|> lam
lam = (Lambda . (explode [])) <$> (spaces *> str "lambda" *> spaces' *> some (satisfy (not . isDot)) <* char '.') <*> lam
  <|> sequence'
sequence' = atom `chainl1` (spaces' *> pure Seq)  
atom = Var <$> var <|> (char '(' *> lam <* char ')')

explode l [] = [l]
explode l (x:xs)= if (x /= ' ') then
                     explode (l ++ [x]) xs
                   else
                     l:(explode [] xs)

sub :: Map String LC -> LC -> LC
sub m (Var x) = case Map.lookup x m of
             Just v  -> v
             Nothing -> Var x
sub m (Seq x y) = Seq (sub m x) (sub m y)
sub m (Lambda x y) = Lambda x (sub m y)
sub m (Assign x y rest) = sub (Map.insert x (sub m y) m) rest

replace :: [String] -> LC -> LC -> LC
replace [x] l (Var a) = if (a == x) then
                          l
                        else
                          substitute x l (Var a)
replace [x] l a = substitute x l a
replace (x:xs) l (Var a) = if (a == x) then
                              Lambda xs l
                           else
                            if (elem x xs) then
                             Lambda xs l
                            else
                             Lambda xs (substitute x l (Var a))
replace (x:xs) l a = if (elem x xs) then
                      Lambda xs l
                     else 
                      Lambda xs (substitute x l a)

substitute :: String -> LC -> LC -> LC
substitute s (Var a) subIn = if (s == a) then
                               subIn 
                             else
                               (Var a)
substitute s (Lambda l a) subIn = if (elem s l) then
                                    Lambda l a 
                                  else 
                                    Lambda l (substitute s a subIn)
substitute s (Seq a b) subIn = Seq (substitute s a subIn) (substitute s b subIn)

interp :: LC -> LC
interp (Seq (Lambda x y) (Seq a b)) = interp (Seq (Lambda x y) (interp (Seq a b)))
interp (Seq (Lambda x y) z) = interp (replace x y z)
interp (Seq (Seq a b) z) = interp (Seq (interp (Seq a b)) z)
interp x = x

convert :: Maybe (LC, String) -> String
convert (Just (a,b)) = show (interp (sub Map.empty a))
convert Nothing    = "Not a valid Lambda"

zero :: LC
zero = Lambda ["s", "z"] (Var "z")

one :: LC
one = Lambda ["s", "z"] (Seq (Var "s") (Var "z"))

two :: LC
two = Lambda ["s", "z"] (Seq (Var "s") (Seq (Var "s") (Var "z")))

succ :: LC
succ = Lambda ["w", "y", "x"] (Seq (Var "y") (Seq (Seq (Var "w") (Var "y")) (Var "x")))

plus :: LC
plus = Lambda ["m", "n"] (Seq (Seq (Var "m") (Hw6.succ))  (Var "n"))


reduce' :: LC -> LC
reduce' (Lambda x (Seq (Var a) b)) = Lambda x  (Seq (Var a) (reduce'(interp b)))
reduce' (Seq (Var a) b)            = Seq (Var a) (reduce' (interp b)) 
reduce' x = x

validNumeral :: LC -> String 
validNumeral (Lambda (x:y:[]) b) = valid x y b 0
                          where valid x y (Seq (Var z) b) n = if (z==x) then valid x y b (n+1)
                                                               else "not valid Church numeral"
                                valid x y (Var z) n         = if (z==y) then show n
                                                               else "not valid Church numeral"
                                valid _ _ _ _               = "not valid Church numeral"
validNumeral _                   = "not valid Church numeral"

isDash :: [String] -> Bool
isDash [] = False
isDash lst = elem "-" lst 

isC :: [String] -> Bool
isC [] = False
isC lst = elem "-c" lst || elem "-cn" lst || elem "-nc" lst

isN :: [String] -> Bool
isN [] = False
isN lst = elem "-n" lst || elem "-nc" lst || elem "-cn" lst

isFile :: [String] -> Bool
isFile [] = False
isFile (x:xs) = if (not (isDash [x] || isC [x] || isN [x])) then
                  True
                else
                  isFile xs

getFile :: [String] -> IO String
getFile lst = if (isFile lst) then
                readFile (findFile lst)
              else
                getContents
          where findFile [] = []
                findFile (x:xs) = if (isFile [x]) then
                                     x
                                  else
                                     findFile xs  

getDash :: [String] -> IO String
getDash lst = if (isDash lst) then
                getContents
              else
                getFile lst

getC :: String -> [String] -> IO ()
getC str lst = if (isC lst) then
                 if (isJust (parse assign str) && (isScoped [] (interpString))) then                           
                   getN (interpString) lst
                 else
                   die "Not well scoped"
               else
                 getN (interpString) lst
            where interpString = interp (sub Map.empty (fst (fromJust(parse assign str))))

getN :: LC -> [String] -> IO ()
getN lc lst = if (isN lst) then
                 putStr (validNumeral (reduce' lc))
               else
                 putStr (show (interp lc))

main :: IO ()
main = getArgs >>= (\x -> getDash x >>= (\y -> getC y x))

-- main :: IO ()
-- main = do 
--         args <- getArgs
--         if (length args == 0) then do
--           string <- getContents
--           if (isJust (parse assign string)) then do 
--             putStr (convert (parse assign string))
--           else
--             do
--               die "Ya done messed up"
                                                             
--         else if (length args >=1) then do
--              if ((elem "-nc" args || elem "-cn" args) && elem "-" args) then do
--                 string <- getContents
--                 if (isJust (parse assign string) && (isScoped [] (interp (sub Map.empty (fst (fromJust(parse assign string))))))) then do
--                    putStr (validNumeral (reduce' (interp (sub Map.empty (fst (fromJust (parse assign string)))))))
--                 else 
--                   do
--                     die "Not well scoped"

--              else if ((elem "-" args) && (elem "-c" args)) then do
--                 string <- getContents
--                 if (isJust (parse assign string) && (isScoped [] (interp (sub Map.empty (fst (fromJust(parse assign string))))))) then do
--                    putStr (convert (parse assign string))
--                 else 
--                   do
--                     die "Not well scoped"

--              else if (elem "-" args) then do
--                string2 <- getContents
--                putStr (convert (parse assign string2))

--              else if (elem "-c" args) then do
--                file <- readFile (head (filter (\x -> x /= "-c") args))
--                if (isJust (parse assign file) && (isScoped [] (interp (sub Map.empty (fst (fromJust(parse assign file))))))) then do
--                  putStr (convert (parse assign file))
--                else
--                  do
--                   die "Not well scoped"
                  
--              else
--                 do
--                  file <- readFile (head args)
--                  if (isJust (parse assign file)) then do 
--                  putStr (convert (parse assign file))
--                  else 
--                     do
--                       die "Ya done messed up"
--         else
--            do
--             die "Ya done messed up"


isScoped :: [String] -> LC -> Bool
isScoped lst (Var x) = elem x lst
isScoped lst (Seq x y) = (isScoped lst x) && (isScoped lst y)
isScoped lst (Lambda x y) = isScoped (lst ++ x) y
