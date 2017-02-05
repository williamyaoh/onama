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
  , tagOpen, tagOpenAny
  , tagOpen_, tagOpenAny_
  , tagClose , tagCloseAny
  , tagClose_, tagCloseAny_

  , tagText , tagTextAny
  , text
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

-- | Primitive. Return the next input tag.
--   All other primitive parsers should be implemented in terms of this.
tag :: (Monad m, Show str) => P.ParsecT [Tag str] u m (Tag str)
tag = P.tokenPrim show updatePos Just

-- | Create a parser which parses a single HTML tag if it passes
--   the given predicate. Return the parsed tag.
satisfy :: (Monad m, Show str) => (Tag str -> Bool) -> P.ParsecT [Tag str] u m (Tag str)
satisfy f = P.tokenPrim show updatePos $ \tag ->
              if f tag then Just tag else Nothing

-- | Tag name should be lowercase.
--   Return the parsed tag.
tagOpen_ :: (Monad m, Show str, Eq str) => str -> P.ParsecT [Tag str] u m (Tag str)
tagOpen_ str = satisfy $ \tag ->
  case tag of
    TagOpen name _ _ -> str == name
    _                -> False

-- | Tag name should be lowercase.
--   Skips over any text nodes in the HTML before attempting to parse
--   an open tag. Usually this is what you want. If not, use 'tagOpen_'.
tagOpen :: (Monad m, Show str, Eq str) => str -> P.ParsecT [Tag str] u m (Tag str)
tagOpen str = optional text >> tagOpen_ str

tagOpenAny_ :: (Monad m, Show str) => P.ParsecT [Tag str] u m (Tag str)
tagOpenAny_ = satisfy $ \tag ->
  case tag of
    TagOpen _ _ _ -> True
    _             -> False

-- | Skips over any text nodes in the HTML before attempting to parse
--   an open tag. Usually this is what you want. If not, use 'tagOpenAny_'.
tagOpenAny :: (Monad m, Show str) => P.ParsecT [Tag str] u m (Tag str)
tagOpenAny = optional text >> tagOpenAny_

-- | Tag name should be lowercase.
--   Return the parsed tag.
tagClose_ :: (Monad m, Show str, Eq str) => str -> P.ParsecT [Tag str] u m (Tag str)
tagClose_ str = satisfy $ \tag ->
  case tag of
    TagClose name _ -> str == name
    _               -> False

-- | Tag name should be lowercase.
--   Skips over any text nodes in the HTML before attempting to parse
--   a close tag. Usually this is what you want. If not, use 'tagClose_'.
tagClose :: (Monad m, Show str, Eq str) => str -> P.ParsecT [Tag str] u m (Tag str)
tagClose str = optional text >> tagClose_ str

tagCloseAny_ :: (Monad m, Show str) => P.ParsecT [Tag str] u m (Tag str)
tagCloseAny_ = satisfy $ \tag ->
  case tag of
    TagClose _ _ -> True
    _            -> False

-- | Skips over any text nodes in the HTML before attempting to parse
--   a close tag. Usually this is what you want. If not, use 'tagCloseAny_'.
tagCloseAny :: (Monad m, Show str) => P.ParsecT [Tag str] u m (Tag str)
tagCloseAny = optional text >> tagCloseAny_

-- | Return the parsed tag.
tagText :: (Monad m, Show str, Eq str) => str -> P.ParsecT [Tag str] u m (Tag str)
tagText str = satisfy $ \tag ->
  case tag of
    TagText text _ -> str == text
    _              -> False

tagTextAny :: (Monad m, Show str) => P.ParsecT [Tag str] u m (Tag str)
tagTextAny = satisfy $ \tag ->
  case tag of
    TagText text _ -> True
    _              -> False

-- | Parse and return the text of a text tag.
text :: (Monad m, Show str) => P.ParsecT [Tag str] u m str
text = do
  tag <- tagTextAny
  case tag of
    TagText t _ -> return t
    _other      -> fail "Could not find a text tag."

-- | @skip p@ produces a parser which will ignore the output of @p@.
skip :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m ()
skip p = p >> return ()
