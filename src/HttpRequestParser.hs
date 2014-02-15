{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
module HttpRequestParser where

import           Text.ParserCombinators.Parsec
import qualified Data.IntSet as S


data HttpRequest = HttpRequest {
    request :: HttpRequestLine,
    headers :: [Header],
    body :: Maybe String  -- todo
} deriving Show


data HttpRequestLine = RequestLine {
    method  :: String,
    url     :: String,
    version :: Int
} deriving (Show)

data Header = Header {
    name :: String,
    value :: [String]
} deriving (Eq, Ord, Show)


parseRequest :: String -> HttpRequest
parseRequest input = case run of
    Left e -> error $ show e ++ " => " ++  input
    Right req -> req
    where 
        run = parse parseHttpRequest "request" input

parseHttpRequest :: Parser HttpRequest
parseHttpRequest = do
    hl <- headLine
    headers <- many requestHeader
    crlf
    return $! HttpRequest hl headers Nothing

requestHeader :: Parser Header
requestHeader = do
    header <- many token'   
    char ':' >> skipMany whiteSpace
    body <- manyTill anyChar crlf
    conts <- many $ (many1 whiteSpace) >> manyTill anyChar crlf
    return $ Header header (body:conts)

headLine :: Parser HttpRequestLine
headLine = do
    m <- methods
    whiteSpace
    u <- many1 (satisfy $ not . isWhiteSpace)
    whiteSpace
    v <- string "HTTP/1." >> versions
    crlf
    return $ RequestLine m u v

isWhiteSpace c = ' ' == c || '\t' == c

whiteSpace = satisfy isWhiteSpace

versions :: Parser Int
versions =  try (char '0' >> return 0)
        <|> (char '1' >> return 1)

methods :: Parser String
methods =   try (string "GET")
        <|> try (string "POST")
        <|> try (string "DELETE")
        <|> try (string "HEAD")
        <|> fail "unknown http method"
        

endOfLine = try (crlf >> return ()) <|> (char '\n' >> return ())

crlf = string "\r\n"

token' = satisfy $ \c -> S.notMember (fromEnum c) set
  where 
    set = S.fromList . map fromEnum $ ['\0'..'\31'] ++ "()<>@,;:\\\"/[]?={} \t" ++ ['\128'..'\255']

