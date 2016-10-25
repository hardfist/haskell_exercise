{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Sum a = Sum a
    deriving (Eq,Ord,Num,Show)
getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where 
    mempty = Sum 0
    mappend = (+)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)    
