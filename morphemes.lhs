% Morpheme Pandoc Filter
% Eddie Antonio Santos
% 0.2.0-2015-09-20

This Pandoc filter finds specially formatted links and converts them into (the
rather verbose) morpheme notation.

Syntax
======

For example, to give the form:

[MORPHEME, .meaning](:allomorphα+, allomorphβ+)

One would type in a Markdown document:

``` markdown
[MORPHEME, .meaning](:allomorphα+, allomorphβ+)
```

Shorthand
---------

A shorthand is provided which allows for quick typing of the most common case:
when a morpheme has only one allomorph:

``` markdown
[-morpheme-, meaning](:)
```

Which renders as:

[-morpheme-, meaning](:)

Examples
========

Shorthand
---------

[+berry, berry (kind of)](:)

[-'s, .1st person possessive](:)

Longhand
--------

[S, .plural](:-z, -s, -ɨz)

HTML Representation
===================

Its plausbile HTML representation may output using [custom elements][].

`<m-morph>`
 ~  Wraps an entire morpheme

`<m-canon>`
 ~  Canonical name of the morpheme

`<m-meaning>`
 ~  A short description of the meaning of the morpheme. For lexical morphemes,
    this may simply be a single word translation; for grammatical morphemes,
    this may be a gloss.

`<x-gloss>`
 ~  A [glossing abbreviation][].

`<m-allo>`
 ~  An allomorph.

```html
<m-morph>
    {
        <m-canon>S</m-canon>,
        <m-meaning>><x-gloss>PLURAL</x-gloss></m-meaning>
    }:
    <m-allo>{-z}</m-allo>, <m-allo>{-s}</m-allo>
</m-morph>
```

[custom elements]: http://webcomponents.org/articles/introduction-to-custom-elements/
[glossing abbreviation]: https://en.wikipedia.org/wiki/List_of_glossing_abbreviations

Implementation
==============

First things first, `Text.Pandoc.JSON`: this imports `toJSONFilter`.

> import Text.Pandoc.JSON

These other standard library functions will also be used:

> import Numeric (readHex)
> import Data.Char (chr, toUpper)
> import Data.List (intercalate)

Types
-----

A morpheme contains itself, a string representing its "canoncical name", its
meaning (lexical or grammatical), and any of its allomorphs. Every morpheme
should have at least *one* "allomorph" (that is to say, its sole attested
form).

> data Morpheme = Morpheme String Meaning [Allomorph]
>   deriving (Eq, Read, Show)

Meaning is simply the lexical or grammatical, and the text to be displayed.

> data Meaning = Meaning Kind String
>   deriving (Eq, Read, Show)

An allomorph has an *attachment* (see below) and is either formatted as a
grammatical or lexical morpheme. Note that this formatting need not be related
to the meaning.

> data Allomorph = Allomorph Attachment Kind String
>   deriving (Eq, Read, Show)

> data Attachment
>   = Free
>   | Prefix
>   | Suffix
>   | Infix
>   deriving (Eq, Read, Show)

> data Kind
>   = Lexical
>   | Grammatical
>   deriving (Eq, Read, Show)

JSON Tree Matcher
-----------------

Next, the actual function itself. It is only defined for HTML, so splice in
some `RawInline` HTML formatting.

> makeMorphemes :: Maybe Format -> Inline -> Inline
> makeMorphemes (Just format) (Link content (':':allomorphs, _))
>   | format == Format "html" = RawInline format
>       $ formatMorpheme $ parseMorpheme morphText alloText
>   where morphText = inlineConcat content
>         alloText = urldecode allomorphs

For every other `Inline` form, just pass it through.

> makeMorphemes _ x = x

Parsing the mini-langauge
-------------------------

Figure out if we're in the shorthand form or the regular form. The shorthand
form is used if the collon (`:`) appears standalone; else, it's the longhand
form.

> parseMorpheme :: String -> String -> Morpheme
> parseMorpheme text ""    = parseShorthand text
> parseMorpheme pair allos = parseLonghand pair allos

Paring the shorthand means keeping the meaning as is, but parsing the form as
an allomorph.

> parseShorthand :: String -> Morpheme
> parseShorthand pair = let
>      (formText, meaningText) = cleanCleave pair
>      meaning = parseMeaning meaningText
>      form@(Allomorph _ _ text) = parseAllomorph formText
>   in Morpheme text meaning [form]

The longhand is a bunch of bullcrap.

> parseLonghand :: String -> String -> Morpheme
> parseLonghand pair alloText = let
>      (form, meaningText) = cleanCleave pair
>      meaning = parseMeaning meaningText
>      allomorphs = [parseAllomorph morph | morph <- cleanSplit alloText]
>   in Morpheme form meaning allomorphs

---

Parsing the meaning is relatively simple; if prefix

> parseMeaning :: String -> Meaning
> parseMeaning ('.':text) = Meaning Grammatical text
> parseMeaning text       = Meaning Lexical     text

This gets a little complicated. If the

> parseAllomorph :: String -> Allomorph
> parseAllomorph text@(a:rest)
>   | isFix a && isFix z = Allomorph Infix  (kind a) (init rest)
>   | isFix z            = Allomorph Prefix (kind z) (init text)
>   | isFix a            = Allomorph Suffix (kind a) rest
>   | True               = Allomorph Free   Lexical  text
>   where z = last text
> parseAllomorph "" = error "This should be parsed as a shortform"

> isFix :: Char -> Bool
> isFix '+' = True
> isFix '-' = True
> isFix _   = False

> kind :: Char -> Kind
> kind '+' = Lexical
> kind '-' = Grammatical
> kind c   = error $ "Undefined kind " ++ [c]


> formatMorpheme :: Morpheme -> String
> formatMorpheme (Morpheme canonicalName meaning allomorphs@(_:_))
>   = "{" ++ (uppercase canonicalName) ++ ", "
>         ++ "'" ++ (formatMeaning meaning) ++ "}: "
>         ++ (formatAllomorphs allomorphs)
> formatMorpheme _ = error "Must have at least one allomorph"

> formatAllomorphs :: [Allomorph] -> String
> formatAllomorphs allomorphs
>     = join ["{" ++ (formatAllomorph allo) ++ "}" | allo <- allomorphs]

> formatAllomorph :: Allomorph -> String
> formatAllomorph (Allomorph Free    _          x) = x
> formatAllomorph (Allomorph Prefix Lexical     x) = "+" ++ x
> formatAllomorph (Allomorph Infix  Lexical     x) = "+" ++ x ++ "+"
> formatAllomorph (Allomorph Suffix Lexical     x) =        x ++ "+"
> formatAllomorph (Allomorph Prefix Grammatical x) = "-" ++ x
> formatAllomorph (Allomorph Infix  Grammatical x) = "-" ++ x ++ "-"
> formatAllomorph (Allomorph Suffix Grammatical x) =        x ++ "-"

> formatMeaning :: Meaning -> String
> formatMeaning (Meaning Grammatical gloss) = tag "x-gloss" gloss
> formatMeaning (Meaning _ text) = text

Finally, `main`, which simply uses `makeMorphemes` as a JSON tree walker.

> main :: IO ()
> main = toJSONFilter makeMorphemes

Utilities
=========

General string stuff
--------------------

> split :: String -> [String]
> split str = split' (cleave str)

> split' :: (String, String) -> [String]
> split' (first, "") = [first]
> split' (first, rest) = first : (split' $ cleave rest)

> cleanSplit :: String -> [String]
> cleanSplit str = map trim $ split str

> cleave :: String -> (String, String)
> cleave str = case break (== ',') str of
>   (prefix, (',':suffix)) -> (prefix, suffix)
>   (prefix, "")           -> (prefix, "")
>   (_, _)                 -> error "Unexpected case"

> trim :: String -> String
> trim str = reverse . removeSpace . reverse . removeSpace $ str
>   where removeSpace = dropWhile (== ' ')

> cleanCleave :: String -> (String, String)
> cleanCleave str = (prefix, trim suffix)
>   where (prefix, suffix) = cleave str

> join :: [String] -> String
> join = intercalate ", "

> uppercase :: String -> String
> uppercase = map toUpper

Pandoc Stuff
------------

> inlineConcat :: [Inline] -> String
> inlineConcat inlines = concatMap toStr inlines
>   where toStr (Str text)  = text
>         toStr Space       = " "
>         toStr LineBreak   = " "
>         toStr unknown     = error $ "Unexpected form: " ++ (show unknown)

> tag :: String -> String -> String
> tag name str = "<" ++ name ++ ">" ++ str ++ "</" ++ name ++ ">"

Decodes a URL-encoded (percent-encoded) string. Useful because links are
automatically URL-encoded.

> urldecode :: [Char] -> String
> urldecode ('%':a:b:rest) = (chr $ unhex [a,b]) : (urldecode rest)
>   where unhex str = case readHex str of [(number, _)] -> number
>                                         _             -> error $ "Could not parse " ++ str
> urldecode (c:rest)       = c:(urldecode rest)
> urldecode []             = []

<style>
/* Some styles for the actual document. */
body {
    font-family: sans-serif;
    max-width: 32em;
    margin: auto;
}

.title, .author {
    text-align: center
}

.date {
    display: none
}

x-gloss {
    font-variant: small-caps;
}
</style>
