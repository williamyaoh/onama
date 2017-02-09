{-# LANGUAGE OverloadedStrings #-}

{-|
  Module: Text.HTML.Onama
  Description: Parsec extended with functions to handle HTML parsing.
  Copyright: (c) William Yao, 2017
  License: BSD-3
  Maintainer: williamyaoh@gmail.com
  Stability: experimental

  Some extra primitives to parse HTMl with Parsec.

  You'll still need to import "Text.Parsec" along with this library. These
  primitives will work with all the combinators from Parsec. Note that you'll
  need to override Parsec's @satisfies@, since that one only works on
  character streams (for some reason).

  > testParser = dp
  >   tagOpen "b"
  >   bolded <- text
  >   tagClose "b"

  > testParser2 = do
  >   tagClose "div"
  >   tagOpen "p"
  >   inner <- text
  >   tagClose "p"
 -}
module Text.HTML.Onama
  ( Tag(..)
  , Position

  , parseTags

  , tag
  , satisfy

  , TagOpenSelector(..)
  , TagCloseSelector(..)
  , AttrName(..)
  , AttrValue(..)
  , AttrSelector(..)

  , (@:), (@=)

  , tagOpen_, tagOpen
  , tagClose_, tagClose
  , tagText
  , balancedTags
  , anyOpenTag, anyCloseTag, anyValue
  , innerText

  , skip
  )
where

import qualified Text.HTML.TagSoup as TS
import Text.StringLike

import qualified Text.Parsec as P
import Text.Parsec
  ( (<|>), (<?>), label, labels
  , try, unexpected
  , choice
  , count, skipMany1, many1
  , sepBy, sepBy1, endBy, endBy1, sepEndBy, sepEndBy1
  , chainl, chainl1, chainr, chainr1
  , eof
  , notFollowedBy
  , manyTill
  , lookAhead
  , anyToken
  , between
  , option, optionMaybe, optional
  , unknownError, sysUnExpectError, mergeErrorReply
  )

import qualified Data.Sequence as S
import Data.Sequence
  ( (<|), (|>), (><) )

import Data.String (IsString, fromString)

import Data.Foldable (toList)

type Position = (TS.Row, TS.Column)

data Tag str
  = TagOpen str [TS.Attribute str] Position
  | TagClose str Position
  | TagText str Position
  deriving (Eq, Show)

parseOptions :: StringLike str => TS.ParseOptions str
parseOptions = TS.parseOptions { TS.optTagPosition = True }

type CurrentPos str = (Position, [Tag str])

startPos :: CurrentPos str
startPos = ((1, 1), [])

-- | Return a list of tags parsed from some sort of string.
--   This list should then get fed into an Onama parser.
parseTags :: StringLike str => str -> [Tag str]
parseTags str =
  reverse $ snd $ foldl attachPos startPos $
    TS.canonicalizeTags $ TS.parseTagsOptions parseOptions str
    where attachPos (pos, tags) tag =
            case tag of
              TS.TagOpen name attrs  -> (pos, TagOpen name attrs pos : tags)
              TS.TagClose name       -> (pos, TagClose name pos : tags)
              TS.TagText text        -> (pos, TagText text pos : tags)
              TS.TagComment _        -> (pos, tags)
              TS.TagWarning _        -> (pos, tags)
              TS.TagPosition row col -> ((row, col), tags)

updatePos :: P.SourcePos -> Tag str -> [Tag str] -> P.SourcePos
updatePos pos tok _ =
  let (row, col) = case tok of
                     TagOpen _ _ pos -> pos
                     TagClose _ pos  -> pos
                     TagText _ pos   -> pos
  in flip P.setSourceLine row $ flip P.setSourceColumn col $ pos

tagName :: Tag str -> Maybe str
tagName (TagOpen name _ _) = Just name
tagName (TagClose name _ ) = Just name
tagName _other             = Nothing

data TagOpenSelector
  = AnyOpenTag [AttrSelector]
  | TagOpenSelector String [AttrSelector]

data TagCloseSelector
  = AnyCloseTag
  | TagCloseSelector String

instance IsString TagOpenSelector where
  fromString str = TagOpenSelector str []

instance IsString TagCloseSelector where
  fromString = TagCloseSelector

tagSelectorAttrs :: TagOpenSelector -> [AttrSelector]
tagSelectorAttrs (AnyOpenTag attrs)        = attrs
tagSelectorAttrs (TagOpenSelector _ attrs) = attrs

newtype AttrName = AttrName String

instance IsString AttrName where
  fromString = AttrName

data AttrValue
  = AnyAttr
  | AttrValue String

instance IsString AttrValue where
  fromString = AttrValue

data AttrSelector = AttrSelector AttrName AttrValue

instance IsString AttrSelector where
  fromString str = AttrSelector (AttrName str) AnyAttr

(@:) :: TagOpenSelector -> [AttrSelector] -> TagOpenSelector
(@:) tagS attrS =
  case tagS of
    AnyOpenTag _           -> AnyOpenTag attrS
    TagOpenSelector name _ -> TagOpenSelector name attrS

(@=) :: AttrName -> AttrValue -> AttrSelector
(@=) = AttrSelector

-- | Primitive. Return the next input tag.
--   All other primitive parsers should be implemented in terms of this.
tag :: (Monad m, Show str) => P.ParsecT [Tag str] u m (Tag str)
tag = P.tokenPrim show updatePos Just

-- | Create a parser which parses a single HTML tag if it passes
--   the given predicate. Return the parsed tag.
satisfy :: (Monad m, Show str) => (Tag str -> Bool) -> P.ParsecT [Tag str] u m (Tag str)
satisfy f = P.tokenPrim show updatePos $ \tag ->
              if f tag then Just tag else Nothing

matchAttrValue :: StringLike str => str -> AttrValue -> Bool
matchAttrValue val attrS = case attrS of
  AnyAttr        -> True
  AttrValue val' -> toString val == val'

tagOpen_ :: (Monad m, StringLike str, Show str)
         => TagOpenSelector
         -> P.ParsecT [Tag str] u m (Tag str)
tagOpen_ tagS =
  satisfy (\tag -> case tag of
              TagOpen name attrs _ ->
                let attrS = tagSelectorAttrs tagS in
                  case tagS of
                    AnyOpenTag _ -> matchAttrs attrS attrs
                    TagOpenSelector name' _ ->
                      toString name == name' && matchAttrs attrS attrs
              _other               -> False)
  <?> "Couldn't parse an open tag."
  where matchAttrs attrS attrs =
          all (\(AttrSelector (AttrName name) attrValS) ->
                 case lookup (fromString name) attrs of
                   Just val -> matchAttrValue val attrValS
                   Nothing  -> False)
              attrS

tagOpen :: (Monad m, StringLike str, Show str)
        => TagOpenSelector
        -> P.ParsecT [Tag str] u m (Tag str)
tagOpen tagS = try (optional tagText >> tagOpen_ tagS)

tagClose_ :: (Monad m, StringLike str, Show str)
          => TagCloseSelector
          -> P.ParsecT [Tag str] u m (Tag str)
tagClose_ tagS =
  satisfy (\tag -> case tag of
              TagClose name _ ->
                case tagS of
                  AnyCloseTag            -> True
                  TagCloseSelector name' -> toString name == name'
              _other          -> False)

tagClose :: (Monad m, StringLike str, Show str)
         => TagCloseSelector
         -> P.ParsecT [Tag str] u m (Tag str)
tagClose tagS = try (optional tagText >> tagClose_ tagS)

-- | Take a parser, return a parser which only succeeds if the given parser
--   fails. Consumes no input.
notParse :: P.Stream s m t => P.ParsecT s u m t -> P.ParsecT s u m ()
notParse parser = do
  parsed <-     try $ Just <$> parser
            <|> return Nothing
  case parsed of
    Nothing -> return ()
    Just _  -> unexpected "parser given to notParse succeeded"

tagText :: (Monad m, Show str) => P.ParsecT [Tag str] u m str
tagText = P.tokenPrim show updatePos $ \tag -> case tag of
  TagText text _ -> Just text
  _other         -> Nothing

balancedTags_ :: (Monad m, StringLike str, Show str)
              => TagOpenSelector
              -> P.ParsecT [Tag str] u m (S.Seq (Tag str))
balancedTags_ tagS = do
  openTag <- tagOpen_ tagS
  tailTags <- tagTail openTag
  return $ openTag <| tailTags

tagTail :: (Monad m, StringLike str, Show str)
        => Tag str
        -> P.ParsecT [Tag str] u m (S.Seq (Tag str))
tagTail (TagOpen name _ _) = do
  innerTags <- P.many $ try notMatchingClose
  matchingClose <- tagClose_ closeS
  return $ mconcat innerTags |> matchingClose
    where closeS = TagCloseSelector $ toString name
          notMatchingClose =   ( (balancedTags_ anyOpenTag)
                             <|> S.singleton <$> (notParse (tagClose_ closeS) >> tag)
                               )

balancedTags :: (Monad m, StringLike str, Show str)
             => TagOpenSelector
             -> P.ParsecT [Tag str] u m [Tag str]
balancedTags tagS = optional tagText >> toList <$> balancedTags_ tagS

anyOpenTag :: TagOpenSelector
anyOpenTag = AnyOpenTag []

anyCloseTag :: TagCloseSelector
anyCloseTag = AnyCloseTag

anyValue :: AttrValue
anyValue = AnyAttr

-- | @skip p@ produces a parser which will ignore the output of @p@.
skip :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m ()
skip p = p >> return ()

innerText :: StringLike str => [Tag str] -> str
innerText tags = fromString $ toList $ mconcat $ fmap tagInner tags
  where tagInner (TagOpen "br" _ _) = S.singleton '\n'
        tagInner (TagText text _)   = S.fromList $ toString text
        tagInner _other             = S.empty
