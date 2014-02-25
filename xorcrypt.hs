module Main where

import Data.Bits (xor)
import Data.Char (ord, chr)
import System.Environment (getArgs)

main :: IO ()
main = do
  m <- getContents
  args <- getArgs
  putStrLn $ eval args m

eval :: [String] -> String -> String
eval ["encrypt", ks] ms = xorEncrypt ks ms
eval ["decrypt", ks] ms = xorDecrypt ks ms
eval _ _ = "Usage: encrypt/decrypt 'key' [stdin message]"

xorEncrypt :: String -> String -> String
xorEncrypt ks ms =
  unwords $ zipWith (\k m -> show $ ord m `xor` ord k) (cycle ks) ms

xorDecrypt :: String -> String -> String
xorDecrypt ks ms =
  map chr $ zipWith (\k m -> ord k `xor` read m :: Int) (cycle ks) (words ms)
