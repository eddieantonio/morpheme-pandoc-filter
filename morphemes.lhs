Shorthand
=========
{morpheme+, meaning}

Longhand
========

{MORPHEME, ~1SG.PRO}: {morpheme+}

In context:

[morpheme, meaning](:morpheme+)

Examples
========

Shorthand
---------

[re-, repeat](:)

[-'s, .1sg.pro](:)

Longhand
--------

[S, .plural](:-z,-s,-ɨz)


Representation
==============

```html
<m-morph>{<m-canon>S</m-canon>,
<m-meaning>><x-gloss>PLURAL</x-gloss></m-meaning>}:
<m-allo>{-z}</m-allo>, <m-allo>{-s}</m-allow>
```

Implementation
==============


First things first, `Text.Pandoc.JSON`: this gives us `toJSONFilter`. Also
import some other things!

> import Text.Pandoc.JSON
> import Data.Char (toLower, toUpper)
> import Data.List (intercalate)


Next, the actual function itself:

> makeMorphemes :: Maybe Format -> Inline -> Inline
> makeMorphemes (Just format) (Link content (':':allomorphs,_))
>   | format == Format "html" = RawInline format
>       $ morphemeFromText (inlineConcat content) allomorphs
> makeMorphemes _ x = x

The `main` is this, lifted to work with Pandoc:

> main :: IO ()
> main = toJSONFilter makeMorphemes

Now process the text. Figure out if we're in the shorthand form or the
regular form.

> morphemeFromText :: String -> String -> String
> morphemeFromText morphText "" =
>   let (canonoicalName, _) = cleanCleave morphText
>       in morphemeFromText' morphText [lowercase canonoicalName]
> morphemeFromText morphText alloText =
>   morphemeFromText' morphText (cleanSplit alloText)

> morphemeFromText' morphText allomorphs =
>   let (canonicalName, gloss) = cleanCleave morphText
>       prefix = "{" ++ (uppercase canonicalName) ++ ", "
>                    ++ "'" ++ (tag "x-gloss" gloss) ++ "'" ++ "}"
>       suffix = join ["{" ++ allo ++ "}" | allo <- allomorphs]
>    in prefix ++ ": " ++ suffix



Utilities
=========

General string stuff
--------------------

> split :: String -> [String]
> split str = split' (cleave str)

> split' :: (String, String) -> [String]
> split' (head, "") = [head]
> split' (head, tail) = head : (split' $ cleave tail)

> cleanSplit str = map trim $ split str

> cleave :: String -> (String, String)
> cleave str = case break (== ',') str of
>   (prefix, (',':suffix)) -> (prefix, suffix)
>   (prefix, "") -> (prefix, "")

> trim :: String -> String
> trim str = reverse . removeSpace . reverse . removeSpace $ str
>   where removeSpace = dropWhile (== ' ')

> cleanCleave :: String -> (String, String)
> cleanCleave str = (prefix, trim suffix)
>   where (prefix, suffix) = cleave str

> lowercase = map toLower
> uppercase = map toUpper
> join = intercalate ", "

Pandoc Stuff
------------

> inlineConcat :: [Inline] -> String
> inlineConcat inline = concatMap toStr inline
>   where toStr (Str text) = text
>         toStr Space = " "
>         toStr LineBreak = " "

> tag name str = "<" ++ name ++ ">" ++ str ++ "</" ++ name ++ ">"
