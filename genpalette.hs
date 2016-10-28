import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word
import System.IO

data Pixel = Pixel { r :: Word8
    , g :: Word8
    , b :: Word8
    }

main = do
    writePpm "out.ppm" w h (genPalette w h)
    where
        w = 200
        h = 100

genPalette :: Integer -> Integer -> [Pixel]
genPalette w h =
    genPalette' w h w h
    where
        genPalette' 0 0 _ _ = []
        genPalette' w h w' h'
            | w == 0 = (genPalette' w' (h-1) w' h')
            | otherwise = p : (genPalette' (w-1) h w' h')
            where
                p = let
                    r = floor (255.99 * fromIntegral (w'-w)/fromIntegral w')
                    g = floor (255.99 * fromIntegral (h-1)/fromIntegral h')
                    b = floor (255.99 * 0.2)
                    in Pixel r g b

writePpm :: FilePath -> Integer -> Integer -> [Pixel] -> IO ()
writePpm f w h xs = do
    fh <- openFile f WriteMode
    C.hPut fh (C.pack ("P3\n" ++ (show w) ++ " " ++ (show h) ++ "\n255\n"))
    forM xs (\p -> do
        C.hPut fh (C.pack ((show (r p)) ++ " " ++ (show (g p)) ++ " " ++ (show (b p)) ++ "\n")))
    hClose fh
