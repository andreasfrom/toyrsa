module Main where

import System.IO.Unsafe
import System.Random
import Data.Char
import System.Environment

import Math.NumberTheory.Moduli (invertMod, powerModInteger')
import Math.NumberTheory.Primes.Testing (isPrime)

type N = Integer
type E = Integer
type D = Integer
data Public = Public N E
data Private = Private N D

main :: IO ()
main = do
  m    <- getContents
  args <- getArgs
  putStrLn $ eval args m

eval :: [String] -> String -> String
eval ["keys", l] _ =
  let (Public n e, Private _ d) = keys (unsafePerformIO getStdGen)(read l :: Int)
  in "Public:\nn: " ++ show n ++ "\ne: " ++ show e ++ "\nPrivate:\nd: " ++ show d
eval ["encrypt", n, e] m = encrypt (Public (read n :: N) (read e :: E)) m
eval ["decrypt", n, d] m = decrypt (Private (read n :: N) (read d :: D)) m
eval _ _ = "Usage: keys length | encrypt n e [stdin] | decrypt n d [stdin]"

keys :: StdGen -> Int -> (Public, Private)
keys sg bits =
  let lower = 2^((bits `div` 2) - 1)
      upper = lower*2 - 1
      (a,sg') = randomR (lower, upper) sg
      (b,sg'') = randomR (lower, upper) sg'
      p = nextPrime a
      q' = nextPrime b
      q = if q' == p then nextPrime . nextPrime $ nextPrime q' else q'
      n = p * q
      k = (p-1) * (q-1)
      d = randomCoprime sg'' k
      Just e = invertMod d k
  in (Public n e, Private n d)

encrypt :: Public -> String -> String
encrypt (Public n e) =
  unwords . map (show . (\m -> powerModInteger' m e n) . fromIntegral . ord)

decrypt :: Private -> String -> String
decrypt (Private n d) =
  map (chr . fromIntegral . (\m -> powerModInteger' m d n)
       . fromIntegral . (\x -> read x :: Integer)) . words

nextPrime :: Integer -> Integer
nextPrime = head . filter isPrime . enumFrom . succ -- head is safe because of Euclids theorem ;)

randomCoprime :: StdGen -> Integer -> Integer
randomCoprime sg n = let (x,sg') = randomR (3, n) sg
                     in if x `isCoprime` n then x else randomCoprime sg' n

isCoprime :: Integer -> Integer -> Bool
isCoprime a b = gcd a b == 1
