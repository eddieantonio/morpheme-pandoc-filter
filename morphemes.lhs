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

`main` simply uses `makeMorphemes` as a JSON tree walker.

> main :: IO ()
> main = toJSONFilter makeMorphemes

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

Parsing the mini-language
-------------------------

Figure out if it's the shorthand form or the regular form. The shorthand form
is used if the colon (`:`) appears standalone; else, it's the longhand form.
Note that the colon is parsed away by `makeMorpheme`.

> parseMorpheme :: String -> String -> Morpheme
> parseMorpheme text ""    = parseShorthand text
> parseMorpheme pair allos = parseLonghand pair allos

Using the shorthand means keeping the meaning as is, but parsing the canonical
form as an "allomorph." That "allomorph" is then used as the sole allomorph in
the completed morpheme, and its inner text is used as the canonical name.

> parseShorthand :: String -> Morpheme
> parseShorthand pair = let
>      (formText, meaningText) = cleanCleave pair
>      meaning = parseMeaning meaningText
>      allomorph@(Allomorph _ _ canonicalName) = parseAllomorph formText
>   in Morpheme canonicalName meaning [allomorph]

In the longhand, all parts are parsed separately.

> parseLonghand :: String -> String -> Morpheme
> parseLonghand pair alloText = let
>      (canonicalName, meaningText) = cleanCleave pair
>      meaning = parseMeaning meaningText
>      allomorphs = [parseAllomorph morph | morph <- cleanSplit alloText]
>   in Morpheme canonicalName meaning allomorphs

Parsing the meaning is relatively simple. If the meaning is prefixed by a
"`.`", then it's a gloss; otherwise it's a translation.

> parseMeaning :: String -> Meaning
> parseMeaning ('.':text) = Meaning Grammatical text
> parseMeaning text       = Meaning Lexical     text

Parsing allomorphs, however, is a bit more involved.

> parseAllomorph :: String -> Allomorph
> parseAllomorph text@(a:rest)

We have to figure out its attachment based on the first and last character
(dubbed `a` and `z`, respectively).

>   | isFix a && isFix z = Allomorph Infix  (kind a) (init rest)
>   | isFix a            = Allomorph Suffix (kind a) rest
>   | isFix z            = Allomorph Prefix (kind z) (init text)

When neither the first or last characters are "'fixes", then it must be a free
morpheme. Note that the kind (lexical or grammatical) does not really make a
difference here, so we assign it arbitrarily.

>   | True               = Allomorph Free   Lexical  text
>   where z = last text
> parseAllomorph "" = undefined

A "'fix" is either a `+` or `-` character.

> isFix :: Char -> Bool
> isFix '+' = True
> isFix '-' = True
> isFix _   = False

To determine its kind, we also judge based on the whether the character is a
`+` or `-`.

> kind :: Char -> Kind
> kind '+' = Lexical
> kind '-' = Grammatical
> kind c   = error $ "Undefined kind " ++ [c]

Another possible approach uses `Maybe to combine the last two functions into
one:

< fixKind :: Char -> Maybe Kind
< fixKind '+' = Just Lexical
< fixKind '-' = Just Grammatical
< fixKind _   = Nothing

Formatting
----------

According to the Jordan Lachler's specification:

<blockquote>
 1. For each morpheme,
     a.  an open curly bracket `{`
     b.  the "name" of the morpheme, in ALL CAPS
     c.  a comma
     d.  the gloss/translation/meaning of the morph, inside 'single quotes'

         NOTE: If the gloss is lexical, use regular lowercase type (‘dog’);
         if the gloss is grammatical, use small capitals (‘MASCULINE
         SINGULAR DEFINITE’)
     e. a close curly bracket `}`
 2. Then put a colon `:`
</blockquote>

> formatMorpheme :: Morpheme -> String
> formatMorpheme (Morpheme canonicalName meaning allomorphs@(_:_))
>   = "{" ++ (uppercase canonicalName) ++ ", "
>         ++ "'" ++ (formatMeaning meaning) ++ "}: "
>         ++ (formatAllomorphs allomorphs)
> formatMorpheme _ = error "Morpheme must contain at least one allomorph"

<blockquote>
  3. Then, for each allomorph that belongs to that morpheme, put:
      a. an open curly bracket `{`
      b. the form of the allomorph, in regular lowercase type
      c. a close curly bracket `}`
      d. if there is more than one allomorph, put a comma between allomorphs
</blockquote>

> formatAllomorphs :: [Allomorph] -> String
> formatAllomorphs allomorphs
>     = join ["{" ++ (formatAllomorph allo) ++ "}" | allo <- allomorphs]

<blockquote>
 4. If the allomorph is
     a. free, then put nothing on either side – {perro}
     b. a suffix, then put a hyphen on the left side – {-and}
     c. a prefix, then put a hyphen on the right side – {anti-} d. an infix, then
        put a hyphen on either side – {-frickin-}
     e. a bound lexical root that requires a suffix, then put a plus on the right
        side – {duerm+}
     f. a bound lexical root that requires a prefix, then put a plus on the left
        side – {+ceive}
     g. a bound lexical root that requires a suffix and a prefix, then put a plus
        on the right and left sides – {+ku+}
</blockquote>

> formatAllomorph :: Allomorph -> String
> formatAllomorph (Allomorph Free    _          x) = x
> formatAllomorph (Allomorph Suffix Grammatical x) = "-" ++ x
> formatAllomorph (Allomorph Prefix Grammatical x) =        x ++ "-"
> formatAllomorph (Allomorph Infix  Grammatical x) = "-" ++ x ++ "-"
> formatAllomorph (Allomorph Suffix Lexical     x) = "+" ++ x
> formatAllomorph (Allomorph Prefix Lexical     x) =        x ++ "+"
> formatAllomorph (Allomorph Infix  Lexical     x) = "+" ++ x ++ "+"

We'll just "gloss" over this function. 😉

> formatMeaning :: Meaning -> String
> formatMeaning (Meaning Grammatical gloss) = tag "x-gloss" gloss
> formatMeaning (Meaning _ text) = text

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
    max-width: 35em;
    margin: auto;
}

.title, .author {
    text-align: center
}

.date {
    display: none
}

blockquote {
    margin: 0;
    padding: 1px 1em;
    background-color: rgba(0,0,0,0.1);
}

x-gloss {
    font-variant: small-caps;
}
</style>
