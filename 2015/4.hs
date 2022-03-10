{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}
import Data.Bits
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array

foreign import capi "openssl/md5.h MD5"
  libsslMD5 :: CString -> CInt -> Ptr CChar -> Ptr CChar

md5 :: String -> IO [Int]
md5 ss = do let len = fromIntegral $ length ss :: CInt
            css <- newCString ss
            hash <- peekArray 16 $ libsslMD5 css len nullPtr
            return $ fmap fromIntegral hash

hex_digits xs = concat $ map hex_pair xs
  where hex_pair x = [shiftR x 4, x .&. 0xF]

check_hash :: String -> Int -> Int -> IO Bool
check_hash word n k =
  do let ss = (word ++ (show n)) :: String
     digest <- md5 ss
     let letters = hex_digits $ take (div (k + 1) 2) digest
     return $ (take k letters) == (take k [0,0..])

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = do return Nothing
findM p (x:xs) = do q <- p x; if q then return (Just x) else findM p xs


word = "bgvyzdsv"
main = do a <- findM (\ n -> check_hash word n 5) [1..]
          b <- findM (\ n -> check_hash word n 6) [1..]
          print a
          print b
