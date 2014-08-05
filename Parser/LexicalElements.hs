module Parser.LexicalElements (letterUppercase,
                               letterLowercase,
                               letterTitlecase,
                               letterModifier,
                               letterOther,
                               markNonSpacing,
                               markSpacingCombining,
                               numberDecimal,
                               numberLetter,
                               punctuationConnector,
                               otherFormat,
                               separatorSpace,
                               separatorLine,
                               separatorParagraph,
                               formatEffector,
                               otherControl,
                               otherPrivateUse,
                               otherSurrogate,
                               graphicCharacter,
                               Parser.LexicalElements.identifier,
                               reservedWord,
                               op,
                               Parser.LexicalElements.stringLiteral,
                               characterLiteral,
                               numericLiteral) where
    import Text.Parsec
    import Text.Parsec.Token
    import Data.Char

    letterUppercase :: Monad m => ParsecT [Char] u m Char
    letterUppercase = satisfy (\c -> generalCategory c == UppercaseLetter)

    letterLowercase :: Monad m => ParsecT [Char] u m Char
    letterLowercase = satisfy (\c -> generalCategory c == LowercaseLetter)

    letterTitlecase :: Monad m => ParsecT [Char] u m Char
    letterTitlecase = satisfy (\c -> generalCategory c == TitlecaseLetter)

    letterModifier :: Monad m => ParsecT [Char] u m Char
    letterModifier = satisfy (\c -> generalCategory c == ModifierLetter)

    letterOther :: Monad m => ParsecT [Char] u m Char
    letterOther = satisfy (\c -> generalCategory c == OtherLetter)

    markNonSpacing :: Monad m => ParsecT [Char] u m Char
    markNonSpacing = satisfy (\c -> generalCategory c == NonSpacingMark)

    markSpacingCombining :: Monad m => ParsecT [Char] u m Char
    markSpacingCombining = satisfy (\c -> generalCategory c == SpacingCombiningMark)

    numberDecimal :: Monad m => ParsecT [Char] u m Char
    numberDecimal = satisfy (\c -> generalCategory c == DecimalNumber)

    numberLetter :: Monad m => ParsecT [Char] u m Char
    numberLetter = satisfy (\c -> generalCategory c == LetterNumber)

    punctuationConnector :: Monad m => ParsecT [Char] u m Char
    punctuationConnector = satisfy (\c -> generalCategory c == ConnectorPunctuation)

    otherFormat :: Monad m => ParsecT [Char] u m Char
    otherFormat = satisfy (\c -> generalCategory c == Format)

    separatorSpace :: Monad m => ParsecT [Char] u m Char
    separatorSpace = satisfy (\c -> generalCategory c == Space)

    separatorLine :: Monad m => ParsecT [Char] u m Char
    separatorLine = satisfy (\c -> generalCategory c == LineSeparator)

    separatorParagraph :: Monad m => ParsecT [Char] u m Char
    separatorParagraph = satisfy (\c -> generalCategory c == ParagraphSeparator)

    formatEffector :: Monad m => ParsecT [Char] u m Char
    formatEffector = (oneOf [chr 0x09, chr 0x0A, chr 0x0B, chr 0x0C, chr 0x0D, chr 0x85])
                     <|> separatorLine <|> separatorParagraph

    otherControl :: Monad m => ParsecT [Char] u m Char
    otherControl = satisfy (\c -> generalCategory c == Control)

    otherPrivateUse :: Monad m => ParsecT [Char] u m Char
    otherPrivateUse = satisfy (\c -> generalCategory c == PrivateUse)

    otherSurrogate :: Monad m => ParsecT [Char] u m Char
    otherSurrogate = satisfy (\c -> generalCategory c == Surrogate)

    graphicCharacter :: Monad m => ParsecT [Char] u m Char
    graphicCharacter = 
        satisfy (\c -> (generalCategory c /= Control) &&
                 (generalCategory c /= PrivateUse) &&
                 (generalCategory c /= Surrogate) &&
                 (notElem c 
                  [chr 0x09, chr 0x0A, chr 0x0B, chr 0x0C, chr 0x0D, chr 0x85]) &&
                 ((mod (ord c) 0x10000) /= 0xFFFE) && ((mod (ord c) 0x10000) /= 0xFFFF))
              <?> "graphic character"
                                      
    tokenParser :: Monad m => GenTokenParser [Char] u m
    tokenParser = makeTokenParser $ LanguageDef{
                    commentStart    = "",
                    commentEnd      = "",
                    commentLine     = "--",
                    nestedComments  = False,
                    identStart      = letterUppercase <|>
                                      letterLowercase <|>
                                      letterTitlecase <|>
                                      letterModifier <|>
                                      letterOther <|>
                                      numberLetter,
                    identLetter     = letterUppercase <|>
                                      letterLowercase <|>
                                      letterTitlecase <|>
                                      letterModifier <|>
                                      letterOther <|>
                                      numberLetter <|>
                                      markNonSpacing <|>
                                      markSpacingCombining <|>
                                      numberDecimal <|>
                                      punctuationConnector,
                    opStart         = oneOf "&'()*+,-./:;<=>|",
                    opLetter        = oneOf ">.*=<",
                    reservedNames   = ["abort", "abs", "abstract", "accept",
                                       "access", "aliased", "all", "and", 
                                       "array", "at", "begin", "body", "case",
                                       "constant", "declare", "delay", "delta",
                                       "digits", "do", "else", "elsif", "end",
                                       "entry", "exception", "exit", "for",
                                       "function", "generic", "goto", "if", "in",
                                       "interface", "is", "limited", "loop",
                                       "mod", "new", "not", "null", "of", "or",
                                       "others", "out", "overriding", "package",
                                       "pragma", "private", "procedure", 
                                       "protected", "raise", "range", "record",
                                       "rem", "renames", "requeue", "return",
                                       "reverse", "select", "separate", "some",
                                       "subtype", "synchronized", "tagged",
                                       "task", "terminate", "then", "type", 
                                       "until", "use", "when", "while", "with",
                                       "xor"],
                    reservedOpNames = ["&", "'", "(", ")", "*", "+", ",", "-",
                                       ".", "/", ":", ";", "<", "=", ">", "|",
                                       "=>", "..", "**", ":=", "/=", ">=", "<=",
                                       "<<", ">>", "<>"],
                    caseSensitive   = False
                  }

    identifier :: Monad m => ParsecT [Char] u m String
    identifier = do str <- Text.Parsec.Token.identifier tokenParser
                    return $ map toLower str

    reservedWord :: Monad m => String ->  ParsecT [Char] u m ()
    reservedWord = reserved tokenParser

    op :: Monad m => String ->  ParsecT [Char] u m ()
    op = reservedOp tokenParser

    stringLiteral :: Monad m => ParsecT [Char] u m String
    stringLiteral = do char '"'
                       str1 <- manyTill graphicCharacter (char '"')
                       strs <- many $ do c <- char '"'
                                         str <- manyTill graphicCharacter (char '"')
                                         return (c:str)
                       return $ foldl (++) str1 strs

    characterLiteral :: Monad m => ParsecT [Char] u m Char
    characterLiteral = do char '\''
                          c <- graphicCharacter
                          char '\''
                          return c

    digitP :: Monad m => ParsecT [Char] u m Integer
    digitP = do d <- oneOf "0123456789"
                return $ toInteger (digitToInt d)

    extendedDigit :: Monad m => ParsecT [Char] u m Integer
    extendedDigit = do d <- oneOf "0123456789abcdefABCDEF"
                       return $ toInteger (digitToInt d)

    numeral :: Monad m => ParsecT [Char] u m Integer
    numeral = do ds <- sepBy (many1 digitP) (char '_')
                 ds' <- return $ foldl (++) [] ds
                 ds'' <- return $ zipWith (\i -> \d -> (10^i)*d) [0..] (reverse ds')
                 return $ foldl (+) 0 ds''

    basedNumeral :: Monad m => Integer -> ParsecT [Char] u m Integer
    basedNumeral b = do ds <- sepBy (many1 extendedDigit) (char '_')
                        ds' <- return $ foldl (++) [] ds
                        case (all (\d -> d < b) ds') of
                          True -> do ds'' <- return $ zipWith (\i -> \d -> (b^i)*d) [0..]  (reverse ds')
                                     return $ foldl (+) 0 ds''
                          False -> fail "digits in a based literal must be less than the base"

    numeralFrac :: (Monad m, RealFrac a) => ParsecT [Char] u m a
    numeralFrac = do ds <- sepBy (many1 digitP) (char '_')
                     ds' <- return $ foldl (++) [] ds
                     ds'' <- return $ zipWith (\i -> \d -> (10.0^^(negate i))*(fromInteger d)) [1..] ds'
                     return $ foldl (+) 0 ds''

    basedNumeralFrac :: (Monad m, RealFrac a) => Integer -> ParsecT [Char] u m a
    basedNumeralFrac b = do ds <- sepBy (many1 extendedDigit) (char '_')
                            ds' <- return $ foldl (++) [] ds
                            case (all (\d -> d < b) ds') of
                              True -> do ds'' <- return $ zipWith (\i -> \d -> ((fromInteger b)^^(negate i))*(fromInteger d)) [1..] ds'
                                         return $ foldl (+) 0 ds''
                              False -> fail "digits in a based literal must be less than the base"

    exponentParser :: Monad m => ParsecT [Char] u m Integer
    exponentParser = do oneOf "eE"
                        s <- (char '-') <|> (option '+' (char '+'))
                        n <- numeral
                        case s of
                          '+' -> return n
                          '-' -> return $ negate n
               
    numericLiteral :: (Monad m, Integral a, RealFloat b) =>
                      ParsecT [Char] u m (Either a b)
    numericLiteral = do n1 <- numeral
                        bp <- optionMaybe $ do char '#'
                                               basedNumeral n1
                        fp <- case bp of
                                Nothing -> optionMaybe $ do char '.'
                                                            numeralFrac
                                Just _ -> do r <- optionMaybe $ do char '.'
                                                                   basedNumeralFrac n1
                                             char '#'
                                             return r
                        ep <- option 0 exponentParser
                        case fp of
                          Nothing -> return $ case bp of
                                                Nothing -> Left ((fromInteger n1) * 10^ep)
                                                Just n' -> Left ((fromInteger n') * (fromInteger n1)^ep)
                          Just fp' -> 
                              return $ case bp of
                                         Nothing -> Right $ makeFloat n1 fp' ep 10
                                         Just n' -> Right $ makeFloat n' fp' ep n1

    makeFloat :: (RealFloat a) => Integer -> a -> Integer -> Integer -> a
    makeFloat m1 f2 e b = let f1 = fromInteger m1 
                              b' = fromInteger b
                              e' = fromInteger e
                          in (f1 + f2) * b'**e'
