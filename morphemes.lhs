% Morpheme Pandoc Filter
% Eddie Antonio Santos
% 0.1.1-2015-09-20

This pandoc filter finds specially formatted links and converts them into (the
rather verbose) morpheme notation.

Syntax
======

For example, to give the form:

[MORPHEME, .meaning](:allomorphα+,allomorphβ+)

One would type in a Markdown document:

``` markdown
[MORPHEME, .meaning](:allomorphα+,allomorphβ+)
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

[S, .plural](:-z,-s,-ɨz)

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

Eventually, we'll to uppercase things and join sentences:

> import Data.Char (toUpper)
> import Data.List (intercalate)

Next, the actual function itself. It is only defined for HTML, so splice in
some `RawInline` HTML formatting.

> makeMorphemes :: Maybe Format -> Inline -> Inline
> makeMorphemes (Just format) (Link content (':':allomorphs, _))
>   | format == Format "html" = RawInline format
>       $ morphemeFromText (inlineConcat content) allomorphs

For every other `Inline` form, just pass it through.

> makeMorphemes _ x = x

Now process the text. Figure out if we're in the shorthand form or the
regular form.

> morphemeFromText :: String -> String -> String
> morphemeFromText morphText "" =
>   let (canonoicalName, _) = cleanCleave morphText
>       in morphemeFromText' morphText [canonoicalName]
> morphemeFromText morphText alloText =
>   morphemeFromText' morphText (cleanSplit alloText)


> morphemeFromText' :: String -> [String] -> String
> morphemeFromText' morphText allomorphs =
>    (formatMorpheme morphText) ++ ": " ++ (formatAllomorph allomorphs)

> formatMorpheme :: String -> String
> formatMorpheme text = "{" ++ (uppercase canonicalName) ++ ", "
>       ++ "'" ++ (formatMeaning meaning) ++ "'" ++ "}"
>   where (canonicalName, meaning) = cleanCleave text

> formatAllomorph :: [String] -> String
> formatAllomorph allomorphs = join ["{" ++ allo ++ "}" | allo <- allomorphs]

> formatMeaning :: String -> String
> formatMeaning ('.':gloss) = tag "x-gloss" gloss
> formatMeaning text = text

Finally, `main`, which simply uses makeMorphemes as a JSON tree walker.

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
