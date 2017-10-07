module Main where

import Lib

main :: IO ()
main = do
    let
        dist = zip [30,30,12,11,7,5,5] "ABCDEFG"
        codeTree = huffmanTree dist
        msg = "ABABCBAADBAFBABBBEABG"
        (code, msg_left) = encodeList codeTree msg
        (bits_left, msg_received) = decodeList codeTree code
        in do
            putStrLn $ show dist
            putStrLn $ show codeTree
            putStrLn $ show msg
            putStrLn $ show code
            putStrLn $ show msg_received
