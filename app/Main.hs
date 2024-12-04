module Main where

import Text.Megaparsec as Mega
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Void (Void)
import System.IO 
import Control.Exception as Ex
import Control.Monad.Combinators
import Control.Monad (void)


-------------------------------------------------------
main :: IO ()
main = do 
 putStrLn "Haskell comments will be output to [filename]Comments.txt"
 myPutStr "Enter the file name: " 
 filename        <- getLine 
 fileContents    <- do 
                     putStrLn "Calling handle file"
                     handleFile filename 
 haskellComments <- do 
                     putStrLn "Parsing contents"
                     parseContents fileContents
 putStrLn "Writing file"
 writeFile (filename ++ "Comments.txt") (show haskellComments)

 
------------------------------------------------
parseContents :: String -> IO [Maybe HaskellComment]
parseContents s = do 
    case parse (parseHaskellComments)  "input2.txt" s of 
     Left e -> do 
        putStrLn "parsing error"
        putStrLn $ errorBundlePretty $ e 
        putStrLn "Error parsing haskell comments from file."
        putStrLn "Try again"
        error ""
     Right hc -> do 
                  putStrLn "no errors here."
                  return hc

myPutStr :: String -> IO ()
myPutStr s = (putStr s) >> (hFlush stdout)

handleFile :: FilePath -> IO String 
handleFile fp = do 
    e <- Ex.try (readFile fp) :: IO (Either SomeException String)
    case e of 
        Left e  -> do 
                    putStrLn $ show e
                    putStrLn "Try again to get file path."
                    error ""
        Right s -> do 
                    putStrLn "Returning s"
                    return s
---------------------------------
-- PARSER 
type Parser = Parsec Void String
data   SingleLineComment = SingleLineComment Symbol Comment deriving (Show, Eq)
data   MultiLineComment = MultiLineComment Symbol Comment  deriving (Show, Eq)

spaceParser :: Parser ()
spaceParser = Lexer.space hspace1 empty empty 
lexemeParser :: Parser a -> Parser a 
lexemeParser = lexeme spaceParser 

data HaskellComment = S SingleLineComment 
                    | M MultiLineComment 
                    deriving (Show, Eq)
-- symbol is for example // or -- or {- -} 
newtype Symbol  = Symbol String deriving (Show, Eq)
newtype Comment = Comment String deriving (Show, Eq)

parseHaskellComments :: Parser [Maybe HaskellComment] 
parseHaskellComments = many (Mega.try haskellComment)
    where haskellComment = do 
             spaceParser
             comment <- (Mega.try parseHaskellSingle) <|> (Mega.try parseHaskellMulti)
             return comment 
    
parseHaskellSingle :: Parser (Maybe HaskellComment)
parseHaskellSingle = do 
    spaceParser
    e <- observing (skipManyTill anySingle (string "--"))
    case e of 
        Left e   -> parseError e
        Right _  -> do 
                     st <- manyTill (anySingle) newline
                     return $ Just (S (SingleLineComment (Symbol "--") (Comment st)))

parseHaskellMulti :: Parser (Maybe HaskellComment)
parseHaskellMulti = do 
    spaceParser
    e <- observing (skipManyTill anySingle (string "{-"))
    case e of 
        Left e -> parseError e 
        Right _ -> do 
                    st <- manyTill (anySingle) (string "-}")
                    return $ Just (M (MultiLineComment (Symbol "{--}") (Comment st)))