module Parser where
    import Text.Parsec
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
                                      