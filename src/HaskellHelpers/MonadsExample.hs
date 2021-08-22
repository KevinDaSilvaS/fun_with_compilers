module HaskellHelpers.MonadsExample where

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