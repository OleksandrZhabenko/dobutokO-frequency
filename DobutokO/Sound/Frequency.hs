-- |
-- Module      :  DobutokO.Sound.Frequency
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"synth\" effect and frequency modulation. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Frequency where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import Data.List (intersperse)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Frequency a b c = F a b [c] | F2 a a b [c] deriving Eq

instance Show (Frequency Float Int Char) where
  show (F x n xs) = showFFloat (Just n) x xs
  show (F2 x1 x2 n xs) = showFFloat (Just n) x1 (freqChange n xs x2 x1)

freqChange :: (RealFloat a) => Int -> String -> a -> a -> String
freqChange n xs freq freq1 
 | compare freq 16 /= LT && compare freq 20000 /= GT && compare freq1 16 /= LT && compare freq1 20000 /= GT = if freq /= freq1 then
   case xs of 
    "l" -> ':':showFFloat (Just n) freq ""
    "s" -> '+':showFFloat (Just n) freq ""
    "e" -> '/':showFFloat (Just n) freq ""
    _ -> '-':showFFloat (Just n) freq "" 
     else ""
 | otherwise = error "DobutokO.Sound.Frequency.freqChange: undefined for this value of the frequencies. "  
 
data Swept a b c = SineS a b c | SquareS a b c | TriangleS a b c | SawtoothS a b c | TrapeziumS a b c | ExpS a b c
  deriving Eq

sweptC :: Swept a b c -> String
sweptC (SineS _ _ _) = "sine"
sweptC (SquareS _ _ _) = "square"
sweptC (TriangleS _ _ _) = "triangle"
sweptC (SawtoothS _ _ _) = "sawtooth"
sweptC (TrapeziumS _ _ _) = "trapezium"
sweptC (ExpS _ _ _) = "exp"

swept1 :: Swept a b c -> a
swept1 (SineS x _ _) = x
swept1 (SquareS x _ _) = x
swept1 (TriangleS x _ _) = x
swept1 (SawtoothS x _ _) = x
swept1 (TrapeziumS x _ _) = x
swept1 (ExpS x _ _) = x

swept1N :: Int -> Swept [Float] b c -> Float
swept1N n x 
 | n == 1 = head . swept1 $ x
 | compare n 1 == GT && compare n 7 == LT = 
    if null . drop (n - 1) . take n . swept1 $ x then error $ "DobutokO.Sound.Frequency.swept1N: Not defined for the arguments. " 
    else head . drop (n - 1) . take n . swept1 $ x
 | otherwise = error $ "DobutokO.Sound.Frequency.swept1N: Not defined for the first argument " ++ show n

toRange100 :: Float -> Float
toRange100 percent = abs percent - fromIntegral (truncate (abs percent / 100) * 100)

swept1N100 :: Int -> Swept [Float] b c -> Float
swept1N100 n = toRange100 . swept1N n

swept2 :: Swept a b c -> b
swept2 (SineS _ x _) = x
swept2 (SquareS _ x _) = x
swept2 (TriangleS _ x _) = x
swept2 (SawtoothS _ x _) = x
swept2 (TrapeziumS _ x _) = x
swept2 (ExpS _ x _) = x

swept3 :: Swept a b c -> c
swept3 (SineS _ _ x) = x
swept3 (SquareS _ _ x) = x
swept3 (TriangleS _ _ x) = x
swept3 (SawtoothS _ _ x) = x
swept3 (TrapeziumS _ _ x) = x
swept3 (ExpS _ _ x) = x


instance Show (Swept [Float] String Int) where
  show x 
   | compare (length . swept1 $ x) 1 == GT = mconcat [sweptC x, " ", show (F2 (swept1N 1 x) (swept1N 2 x) (swept3 x) (swept2 x)), " ",
     if compare (length . swept1 $ x) 2 == GT then mconcat . intersperse " " . map (\z -> showFFloat (Just (swept3 x)) (swept1N100 z x) "") $ [3..length (swept1 x)]
     else ""]
   | otherwise = error $"DobutokO.Sound.Frequency.show: Too less arguments for " ++ show (swept1 x) ++ " sweep the frequencies to show them. "

data Single a b = Whitenoise a b | Tpdfnoise a b | Pinknoise a b | Brownnoise a b | Pluck a b | Sine a b | Square a b | Triangle a b | Sawtooth a b | 
  Trapezium a b | Exp a b 
    deriving Eq

singleC :: Single a b -> String
singleC (Whitenoise _ _) = "whitenoise"
singleC (Tpdfnoise _ _) = "tpdfnoise"
singleC (Pinknoise _ _) = "pinknoise"
singleC (Brownnoise _ _) = "brownnoise"
singleC (Pluck _ _) = "pluck"
singleC (Sine _ _) = "sine"
singleC (Square _ _) = "square"
singleC (Triangle _ _) = "triangle"
singleC (Sawtooth _ _) = "sawtooth"
singleC (Trapezium _ _) = "trapezium"
singleC (Exp _ _) = "exp"

single1 :: Single a b -> a
single1 (Whitenoise x _) = x
single1 (Tpdfnoise x _) = x
single1 (Pinknoise x _) = x
single1 (Brownnoise x _) = x
single1 (Pluck x _) = x
single1 (Sine x _) = x
single1 (Square x _) = x
single1 (Triangle x _) = x
single1 (Sawtooth x _) = x
single1 (Trapezium x _) = x
single1 (Exp x _) = x

single1N :: Int -> Single [Float] b -> Float
single1N n x 
 | n == 1 = head . single1 $ x
 | compare n 1 == GT && compare n 6 == LT = 
    if null . drop (n - 1) . take n . single1 $ x then error $ "DobutokO.Sound.Frequency.single1N: Not defined for the arguments. " 
    else head . drop (n - 1) . take n . single1 $ x
 | otherwise = error $ "DobutokO.Sound.Frequency.single1N: Not defined for the first argument " ++ show n

single1N100 :: Int -> Single [Float] b -> Float
single1N100 n = toRange100 . single1N n

single2 :: Single a b -> b
single2 (Whitenoise _ x) = x
single2 (Tpdfnoise _ x) = x
single2 (Pinknoise _ x) = x
single2 (Brownnoise _ x) = x
single2 (Pluck _ x) = x
single2 (Sine _ x) = x
single2 (Square _ x) = x
single2 (Triangle _ x) = x
single2 (Sawtooth _ x) = x
single2 (Trapezium _ x) = x
single2 (Exp _ x) = x

instance Show (Single [Float] Int) where
  show x 
   | null . single1 $ x = error $ "DobutokO.Sound.Frequency.show: Too less arguments. "
   | otherwise = mconcat [singleC x, " ", show (F (single1N 1 x) (single2 x) ""), " ",
      if compare (length . single1 $ x) 2 == GT 
        then mconcat . intersperse " " . map (\z -> showFFloat (Just (single2 x)) (single1N100 z x) "") $ [2..length (single1 x)]
        else if (length . single1 $ x) == 2 then  showFFloat (Just (single2 x)) (single1N100 1 x) "" else ""]

 ---------------------------------------------------------------------------------------------------------------------------------------

data Di = O | T deriving Eq

data Choice a b c d = C2 (Swept a b c) (Single a c) d
    deriving Eq

choice1 :: Choice a b c d -> Swept a b c
choice1 (C2 x _ _) = x

choice2 :: Choice a b c d -> Single a c
choice2 (C2 _ y _) = y

choice3 :: Choice a b c d -> d
choice3 (C2 _ _ z) = z

choiceSet1 :: Swept a b c -> Choice a b c d -> Choice a b c d
choiceSet1 x (C2 _ y z) = C2 x y z

choiceSet2 :: Single a c -> Choice a b c d -> Choice a b c d
choiceSet2 y (C2 x _ z) = C2 x y z

choiceSet3 :: d -> Choice a b c d -> Choice a b c d
choiceSet3 z (C2 x y _) = C2 x y z

instance Show (Choice [Float] String Int Di) where
  show (C2 y _ O) = show y
  show (C2 _ z T) = show z

type Synth = Choice [Float] String Int Di

showQ :: Synth -> [String]
showQ = words . show

