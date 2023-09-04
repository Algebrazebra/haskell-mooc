module Set14a where

-- Remember to browse the docs of the Data.Text and Data.ByteString
-- libraries while working on the exercises!

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Int
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Data.Word
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Greet a person. Given the name of a person as a Text, return
-- the Text "Hello, <name>!". However, if the name is longer than 15
-- characters, output "Hello, <first 15 characters of the name>...!"
--
-- PS. the test outputs and examples print Text values as if they were
-- Strings, just like GHCi prints Texts as Strings.
--
-- Examples:
--  greetText (T.pack "Martin Freeman") ==> "Hello, Martin Freeman!"
--  greetText (T.pack "Benedict Cumberbatch") ==> "Hello, Benedict Cumber...!"

greetText :: T.Text -> T.Text
greetText name =
  let hello_txt = T.pack "Hello, "
      exclamation_mark = T.pack "!"
   in T.concat [hello_txt, abbreviate name, exclamation_mark]
  where
    abbreviate :: T.Text -> T.Text
    abbreviate txt
      | T.length txt > 15 = T.concat [T.take 15 txt, T.pack "..."]
      | otherwise = txt

------------------------------------------------------------------------------
-- Ex 2: Capitalize every second word of a Text.
--
-- Examples:
--   shout (T.pack "hello how are you")
--     ==> "HELLO how ARE you"
--   shout (T.pack "word")
--     ==> "WORD"

shout :: T.Text -> T.Text
shout txt =
  let words = T.words txt
      enumerate x = zip [0 ..] x
      indexedWords = enumerate words
      txtSpace = T.pack " "
   in T.intercalate txtSpace $ map (\(i, w) -> if (even i) then (T.toUpper w) else w) indexedWords

------------------------------------------------------------------------------
-- Ex 3: Find the longest sequence of a single character repeating in
-- a Text, and return its length.
--
-- Examples:
--   longestRepeat (T.pack "") ==> 0
--   longestRepeat (T.pack "aabbbbccc") ==> 4

longestRepeat :: T.Text -> Int
longestRepeat txt = foldr longestSubsequence 0 (T.group txt)
  where
    longestSubsequence elem acc = max (T.length elem) acc

------------------------------------------------------------------------------
-- Ex 4: Given a lazy (potentially infinite) Text, extract the first n
-- characters from it and return them as a strict Text.
--
-- The type of the n parameter is Int64, a 64-bit Int. Can you figure
-- out why this is convenient?
--
-- Example:
--   takeStrict 15 (TL.pack (cycle "asdf"))  ==>  "asdfasdfasdfasd"

takeStrict :: Int64 -> TL.Text -> T.Text
takeStrict n txt = TL.toStrict $ TL.take (fromIntegral n) txt

------------------------------------------------------------------------------
-- Ex 5: Find the difference between the largest and smallest byte
-- value in a ByteString. Return 0 for an empty ByteString
--
-- Examples:
--   byteRange (B.pack [1,11,8,3]) ==> 10
--   byteRange (B.pack []) ==> 0
--   byteRange (B.pack [3]) ==> 0

byteRange :: B.ByteString -> Word8
byteRange bstr = if (B.null bstr) then 0 else B.maximum bstr - B.minimum bstr

------------------------------------------------------------------------------
-- Ex 6: Compute the XOR checksum of a ByteString. The XOR checksum of
-- a string of bytes is computed by using the bitwise XOR operation to
-- "sum" together all the bytes.
--
-- The XOR operation is available in Haskell as Data.Bits.xor
-- (imported into this module).
--
-- Examples:
--   xorChecksum (B.pack [137]) ==> 137
--   xor 1 2 ==> 3
--   xorChecksum (B.pack [1,2]) ==> 3
--   xor 1 (xor 2 4) ==> 7
--   xorChecksum (B.pack [1,2,4]) ==> 7
--   xorChecksum (B.pack [13,197,20]) ==> 220
--   xorChecksum (B.pack [13,197,20,197,13,20]) ==> 0
--   xorChecksum (B.pack []) ==> 0

xorChecksum :: B.ByteString -> Word8
xorChecksum bstr = B.foldr xor 0 bstr

------------------------------------------------------------------------------
-- Ex 7: Given a ByteString, compute how many UTF-8 characters it
-- consists of. If the ByteString is not valid UTF-8, return Nothing.
--
-- Look at the docs of Data.Text.Encoding to find the right functions
-- for this.
--
-- Examples:
--   countUtf8Chars (encodeUtf8 (T.pack "åäö")) ==> Just 3
--   countUtf8Chars (encodeUtf8 (T.pack "wxyz")) ==> Just 4
--   countUtf8Chars (B.pack [195]) ==> Nothing
--   countUtf8Chars (B.pack [195,184]) ==> Just 1
--   countUtf8Chars (B.drop 1 (encodeUtf8 (T.pack "åäö"))) ==> Nothing

countUtf8Chars :: B.ByteString -> Maybe Int
countUtf8Chars bstr = case decodeUtf8' bstr of
  (Right decoded) -> Just (T.length decoded)
  (Left _) -> Nothing

------------------------------------------------------------------------------
-- Ex 8: Given a (nonempty) strict ByteString b, generate an infinite
-- lazy ByteString that consists of b, reversed b, b, reversed b, and
-- so on.
--
-- Example:
--   BL.unpack (BL.take 20 (pingpong (B.pack [0,1,2])))
--     ==> [0,1,2,2,1,0,0,1,2,2,1,0,0,1,2,2,1,0,0,1]

pingpong :: B.ByteString -> BL.ByteString
pingpong bstr = appendInfinitely lazyBstr
  where
    appendInfinitely lazyBstr = BL.append lazyBstr (appendInfinitely (BL.reverse lazyBstr))
    lazyBstr = BL.fromStrict bstr
