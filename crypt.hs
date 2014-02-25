module Main where

import System.IO.Unsafe
import System.Random
import Data.Char
import Data.Bits

import Math.NumberTheory.Moduli (invertMod, powerModInteger')
import Math.NumberTheory.Primes.Testing (isPrime)

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

type N = Integer
type E = Integer
type D = Integer
data Public = Public N E
data Private = Private N D
data PQK = PQK Integer Integer Integer

main :: IO ()
main = startGUI defaultConfig { tpPort = 10000 } setup

-- GUI
setup :: Window -> UI ()
setup w = void $ do
  return w % set title "Kryptografi"

  let text300 = UI.textarea % set style [("width", "300px")]
      showOrd = unwords . map (show . ord)
  
  keysInput  <- UI.input   % set placeholder "f.eks. 8"
  keysButton <- UI.button  % set UI.text "Generer"
  keyOutput  <- UI.div

  plainText  <- text300
  cipherText <- text300
  utf8Text   <- text300 % set (attr "readonly") "true"
  neInput    <- UI.input
  eInput     <- UI.input
  ndInput    <- UI.input
  dInput     <- UI.input

  on UI.click keysButton $ \_ -> do
    kl <- keysInput % get UI.value
    let (pkq, pub@(Public n e), priv@(Private _ d))
          = keys (unsafePerformIO getStdGen) (read kl :: Int)
    out <- (formatKeys (pkq, pub, priv))
    zipWithM (\i v -> return i % set UI.value (show v))
      [neInput, ndInput, eInput, dInput] [n, n, e, d]
    return keyOutput % set UI.children [ out ]
    
  encryptButton <- UI.button % set text "Krypter"
  decryptButton <- UI.button % set text "Dekrypter"

  on UI.click encryptButton $ \_ -> do
    m <- plainText % get UI.value
    return utf8Text % set UI.value (showOrd m)

  registerRSACryptButton encryptButton neInput eInput plainText cipherText
    (\n e -> encrypt (Public (read n :: N) (read e :: E)))

  registerRSACryptButton decryptButton ndInput dInput cipherText plainText
    (\n d -> decrypt (Private (read n :: N) (read d :: D)))
 
  xorPlain   <- text300
  xorCipher  <- text300
  xorKey     <- UI.input  % set placeholder "f.eks. hund"
  xorEButton <- UI.button % set text "Krypter"
  xorDButton <- UI.button % set text "Dekrypter"

  utf8Key <- UI.span
  utf8XOR <- text300 % set (attr "readonly") "true"

  on UI.click xorEButton $ \_ -> do
    m <- xorPlain  % get UI.value
    k <- xorKey    % get UI.value
    return utf8Key % set text ("UTF-8: " ++ (showOrd k))
    return utf8XOR % set UI.value (showOrd m)

  registerXORCryptButton xorEButton xorPlain  xorCipher xorKey xorEncrypt
  registerXORCryptButton xorDButton xorCipher xorPlain  xorKey xorDecrypt

  let right10 = set style [("margin-right", "10px")]

  getBody w #+ [
    column [
       UI.h1 % set text "Kryptografi" % set style [("margin-bottom", "5px")]
       , UI.h2 % set text "RSA"       % set style [("margin-bottom", "4px")]
       , UI.h3 % set html "N&oslash;glegenerering"
       , row [ string "Antal bits:", element keysInput, element keysButton ]
       , element keyOutput
       , UI.h3 % set text "Kryptering / Dekryptering"
       , row [ framed $ column [
                  row [ string "Klartekst" % right10, UI.div % set html mMath ]
                  , element plainText
                  , UI.span % set html "UTF-8-tal-v&aelig;rdier"
                  , element utf8Text
                  , row [ (prepend "n:" neInput), (prepend "e:" eInput) ]
                  , (element encryptButton) ] % right10
             , framed $ column [
               row [ string "Ciffertekst" % right10, UI.div % set html cMath ]
               , element cipherText
               , row [ (prepend "n:" ndInput), (prepend "d:" dInput) ]
               , (element decryptButton) ] ]
         
       , UI.h2 % set text "xorCrypt"
       , row [ UI.span % set html "N&oslash;gle (n):"
             , element xorKey, element utf8Key ]
       , UI.br
       , row [ framed $ column [
                  string "Klartekst M = C `xor` n", element xorPlain
                  , UI.span % set html "UTF-8-tal-v&aelig;rdier"
                  , element utf8XOR
                  , element xorEButton ] % right10
             , framed $ column [ string "Ciffertekst C = M `xor` n"
                               , element xorCipher
                               , element xorDButton ] ] ] ]

registerRSACryptButton
  :: Element -> Element -> Element -> Element -> Element
     -> (String -> String -> String -> String) -> UI ()
registerRSACryptButton b k1 k2 f t fun =
  on UI.click b $ \_ -> do
    k1' <- k1 % get UI.value
    k2' <- k2 % get UI.value
    m <- f % get UI.value
    return f % set placeholder m % set UI.value ""
    return t % set UI.value (fun k1' k2' m)

registerXORCryptButton
  :: Element -> Element -> Element -> Element
     -> (String -> String -> String) -> UI ()
registerXORCryptButton b f t k fun =
  on UI.click b $ \_ -> do
    k' <- k  % get UI.value
    m  <- f  % get UI.value
    return f % set placeholder m % set UI.value ""
    return t % set UI.value (fun k' m)

placeholder :: WriteAttr Element String
placeholder = attr "placeholder"

prepend :: String -> Element -> UI Element
prepend s x = row [string s, element x]

framed :: UI Element -> UI Element
framed =  set style [ ("border", "1px solid black"), ("padding", "5px")]

keyGroup :: String -> [UI Element] -> UI Element -> UI Element
keyGroup s xs b = framed $ column [ string s, row xs, b ]

formatKeys :: (PQK, Public, Private) -> UI Element
formatKeys (PQK p q k, Public n e, Private _ d) = do
  column [
    UI.br
    , string "Der findes to cirka lige store primtal p og q:"
    , keyWrapper $ "p = " ++ show p ++ "\nq = " ++ show q
    , string "Produktet af disse er modulusdelen af de-/krypteringsalgoritmen:"
    , keyWrapper $ "n = p*q = " ++ show n
    , string "k findes som antallet af tal der er indbyrdes primisk med n:"
    , keyWrapper $ "k = (p-1) * (q-1) = " ++ show k
    , string $ "Vi finder den private eksponent d,\
      \som er mindre end og indbyrdes primisk med k:"
    , keyWrapper $ "d = " ++ show d
    , string "Til sidst findes den offentlige eksponent e hvor d * e mod k = 1:"
    , keyWrapper $ "e = " ++ show e
    , UI.h4 % set text "Offentlige:" % set style [("margin-bottom", "5px")]
    , keyWrapper $ "n = " ++ show n ++ "\n\ne = " ++ show e
    , UI.h4 % set text "Privat:" % set style [("margin-bottom", "5px")]
    , keyWrapper $ "d = " ++ show d ]
    where keyWrapper s = UI.pre % set UI.text s % set UI.style
                        [ ("width", "600px"),("word-wrap", "break-word")]

-- RSA
keys :: StdGen -> Int -> (PQK, Public, Private)
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
  in (PQK p q k, Public n e, Private n d)

encrypt :: Public -> String -> String
encrypt (Public n e) =
  unwords . map (show . (\m -> powerModInteger' m e n) . fromIntegral . ord)

decrypt :: Private -> String -> String
decrypt (Private n d) =
  map (chr . fromIntegral . (\m -> powerModInteger' m d n)
       . fromIntegral . (\x -> read x :: Integer)) . words

nextPrime :: Integer -> Integer
nextPrime = head . filter isPrime . enumFrom . succ

randomCoprime :: StdGen -> Integer -> Integer
randomCoprime sg n = let (x,sg') = randomR (3, n) sg
                     in if x `isCoprime` n then x else randomCoprime sg' n

isCoprime :: Integer -> Integer -> Bool
isCoprime a b = gcd a b == 1

-- xorCrypt
xorEncrypt :: String -> String -> String
xorEncrypt ks ms =
  unwords $ zipWith (\k m -> show $ ord m `xor` ord k) (cycle ks) ms

xorDecrypt :: String -> String -> String
xorDecrypt ks ms =
  map chr $ zipWith (\k m -> ord k `xor` read m :: Int) (cycle ks) (words ms)

-- Function application
(%) :: a -> (a -> b) -> b
(%) = (#)

-- MathML formatting...
cMath :: String
cMath = "<fmath alttext=\"C = M^e \\mod n\" display=\"block\"\
\class=\"ma-block\"><mrow><mi class=\"fm-mi-length-1\" mathvariant=\"italic\">\
\C</mi><mo class=\"fm-infix-loose\">=</mo><mrow class=\"ma-repel-adj\">\
\<mrow class=\"ma-repel-adj\"><msup><mi class=\"fm-mi-length-1\" \
\mathvariant=\"italic\"style=\"padding-right: 0.44ex;\">M</mi><span \
\class=\"fm-script fm-inline\" style=\"vertical-align: 0.7em;\">\
\<mi class=\"fm-mi-length-1\" mathvariant=\"italic\">e</mi></span></msup>\
\<mspace width=\".17em\" style=\"margin-right: 0.17em; padding-right: 0.001em; \
\visibility: hidden;\"></mspace><mi mathvariant=\"normal\" \
\class=\"ma-repel-adj\">mod</mi></mrow><mspace width=\".17em\" \
\style=\"margin-right: 0.17em; padding-right: 0.001em; visibility: hidden;\">\
\</mspace><mi class=\"fm-mi-length-1\" mathvariant=\"italic\">n\
\</mi></mrow></mrow></fmath>"

mMath :: String
mMath = "<fmath alttext=\"M = C^d \\mod n\" display=\"block\"\
\class=\"ma-block\"><mrow><mi class=\"fm-mi-length-1\" mathvariant=\"italic\"\
\style=\"padding-right: 0.44ex;\">M</mi><mo class=\"fm-infix-loose\">=\
\</mo><mrow class=\"ma-repel-adj\"><mrow class=\"ma-repel-adj\"><msup>\
\<mi class=\"fm-mi-length-1\" mathvariant=\"italic\">C</mi><span \
\class=\"fm-script fm-inline\" style=\"vertical-align: 0.7em;\">\
\<mi class=\"fm-mi-length-1\" mathvariant=\"italic\" style=\"padding-right: \
\0.44ex;\">d</mi></span></msup><mspace width=\".17em\" style=\"margin-right: \
\0.17em; padding-right: 0.001em; visibility: hidden;\"></mspace>\
\<mi mathvariant=\"normal\" class=\"ma-repel-adj\">mod</mi></mrow><mspace \
\width=\".17em\" style=\"margin-right: 0.17em; padding-right: 0.001em; \
\visibility: hidden;\"></mspace><mi class=\"fm-mi-length-1\" \
\mathvariant=\"italic\">n</mi></mrow></mrow></fmath>"
