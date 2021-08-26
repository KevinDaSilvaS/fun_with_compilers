module HaskellHelpers.MonadsExample where

import Control.Monad
-- return takes any value and wraps it in a monad return 3 / (Monad m) => a -> m a

newtype MyNum a = MyNum a deriving (Show)

instance Functor MyNum where
    fmap f (MyNum x) = MyNum (f x)

instance Applicative MyNum where
    pure x = MyNum x

instance Monad MyNum where
    return x = MyNum x
    MyNum x >>= f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft n (l, r)
    | abs (l + n - r) < 4 = Just (l+n, r)
    | otherwise = Nothing

landRight n (l, r)
    | abs (l - (r + n)) < 4 = Just (l, r+n)
    | otherwise = Nothing

-- landLeft 1 (0,0) >>= landRight 3 >>= landLeft 1 

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- landLeft 1 (0,0) >>= banana >>= landLeft 1 

-- return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1 

-- list monad

-- [3,4,5] >>= (\x -> if x == 3 then [3] else []) >>= \x -> [x,-x]
-- [3, -3]

-- [3,4,5] >> [] >>= \x -> [x,-x]
-- []

-- [3,4,5] >> [8] >>= \x -> [x,-x] 
-- [8,-8,8,-8,8,-8]

-- [3,4,5] >>= \x -> [x*2] >>= (\x -> [x,-x] )
-- [6,-6,8,-8,10,-10]

-- [3,4,5] >>= \x -> [(show x) ++ "!"]                        
-- ["3!","4!","5!"]

type KnightPos = (Int, Int)

{- instance MonadPlus [] where  
    mzero = []  
    mplus = (++)  

guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero   -}

moveKnight (c, r) = do
    (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1),
        (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
        first <- moveKnight start
        second <- moveKnight first
        moveKnight second

intimes start 0 = do return start
intimes start max = do
    first <- moveKnight start
    intimes first (max-1)

-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight  

{- 
WRONG
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result 

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
            | b == 0 = do  
                tell ["Finished with " ++ show a]  
                return a  
            | otherwise = do  
                tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
                gcd' b (a `mod` b)   -}

landLeft' n (l, r)
    | abs (l + n - r) < 4 = Right (l+n, r)
    | otherwise = Left 
        ("[landLeft'] Failed with " ++ show l 
        ++ " birds on the left and " 
        ++ show r ++ " on the right side.")
            
landRight' n (l, r)
    | abs (l - (r + n)) < 4 = Right (l, r+n)
    | otherwise = Left 
        ("[landRight'] Failed with " ++ show l 
        ++ " birds on the left and " 
        ++ show r ++ " on the right side.")