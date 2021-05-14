{-# LANGUAGE BlockArguments #-}

module VerilogFiddle.Parse (
  parseVerilog
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

import VerilogFiddle.Types

-- minimal language definition and helpers
------------------------------------------
keywords = [ "module", "endmodule", "input", "output" ]
identifierStart = letter <|> oneOf "_.$\\"
tp = makeTokenParser $ emptyDef { commentLine   = "//"
                                , identStart    = identifierStart
                                , identLetter   = identifierStart <|> digit
                                , reservedNames = keywords }

ws = whiteSpace tp
stripTrailWs p = p >>= \res -> ws >> return res
reservedWs str = stripTrailWs $ reserved tp str
naturalWs = fromInteger <$> stripTrailWs (natural tp)
naturalNoWs = fromInteger <$> natural tp
identifierWs = stripTrailWs $ identifier tp
parensWs p = stripTrailWs (parens tp $ whiteSpace tp >> p)
bracesWs p = stripTrailWs (braces tp $ whiteSpace tp >> p)
bracketsWs p = stripTrailWs (brackets tp $ whiteSpace tp >> p)
charWs c = stripTrailWs $ char c
stringWs c = stripTrailWs $ string c

kw = reservedWs
nat = naturalWs
natNoWs = naturalNoWs
ident = identifierWs

commaLst :: Parser x -> Parser [x]
commaLst p = sepBy p $ charWs ','

commaLst1 :: Parser x -> Parser [x]
commaLst1 p = sepBy1 p $ charWs ','

skipTill :: Parser x -> Parser x
skipTill p = p <|> (anyChar >> skipTill p)

-- parse verilog module interfaces
----------------------------------
parseVerilogModule :: Parser VerilogModule
parseVerilogModule = do
  kw "module"
  nm <- ident
  decls <- parensWs $ commaLst1 ident
  semi tp
  ports <- harvestPorts decls []
  return $ VerilogModule nm $ reverse ports
  where harvestPorts decls acc =
              (kw "endmodule" >> return acc)
          <|> choice [
                try (parsePortDef decls) >>= \x -> harvestPorts decls $ x : acc
              , anyChar >> harvestPorts decls acc ]

parsePortDef :: [String] -> Parser VerilogPort
parsePortDef decls = do
  dir <- choice [ kw "input" >> return In
                , kw "output" >> return Out ]
  w <- option 1 parseWidthFromSlice
  nm <- ident
  semi tp
  if nm `elem` decls then return $ VerilogPort nm dir w else parserZero

parseWidthFromSlice :: Parser Integer
parseWidthFromSlice = bracketsWs do
  hi <- naturalWs
  colon tp >> ws
  lo <- naturalWs
  return if hi > lo then hi - lo + 1 else 0

parseAll :: Parser [VerilogModule]
parseAll = do
  res <- many1 $ skipTill parseVerilogModule
  skipTill eof
  return res

parseVerilog :: FilePath -> String -> [VerilogModule]
parseVerilog fp src = case parse parseAll fp src of
                     Left  e -> error $ fp ++ ": " ++ show e ++ "\n"
                     Right x -> x
