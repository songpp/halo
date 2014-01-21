{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
module HttpRequestParser where

import           Text.ParserCombinators.Parsec


data HttpRequest

data HttpVersion = HttpVersion String deriving (Read,Show)

httpV10, httpV11 :: HttpVersion
httpV10 = HttpVersion "HTTP/1.0"
httpV11 = HttpVersion "HTTP/1.1"

data HttpMethod = GET | POST | PUT | DELETE | HEAD deriving (Read, Show, Eq)

data HttpRequestHeadLine = HeadLine {
    method  :: HttpMethod,
    url     :: String,
    version :: HttpVersion
} deriving (Show)


parseRequest input = case run of
    Left e -> error $ show e
    Right req -> req
    where 
        run = parse parseHttpRequest "headline" input

parseHttpRequest :: Parser HttpRequestHeadLine
parseHttpRequest = do
    hl <- headLine
    return hl

headLine :: Parser HttpRequestHeadLine
headLine = do
    m <- methods
    whiteSpace
    u <- parseURL
    whiteSpace
    v <- versions
    nl
    return $ HeadLine (read m :: HttpMethod) u v

whiteSpace :: Parser Char
whiteSpace = char ' '

versions :: Parser HttpVersion
versions =  try (string "HTTP/1.0" >> return httpV10)
        <|> try (string "HTTP/1.1" >> return httpV11)
        <|> fail "unkonwn http version"

methods :: Parser String
methods =   try (string "GET")
        <|> try (string "POST")
        <|> try (string "DELETE")
        <|> try (string  "HEAD")
        <|> fail "unknown http method"

parseURL :: Parser String
parseURL = many (noneOf " ")

nl :: Parser String
nl = string "\r\n"
