module TStyle exposing (empty, append, concat

    , fromStyles

    , make
    , pseudoWithStyles
    , classWithStyles

    , mapSelectors
    , prefixSelector, postfixSelector
    , prefixClass, postfixClass

    , wrapInClass
    , refine, refineClass, refinePseudo

    , refineWith
    , refineClassWith
    , refinePseudoWith
    , toCss
    )
{-| Describe me please...
-}

import Html
import TStyle.Selector as Selector exposing (Selector, SelectorPath)

type S theme =
    S (Blocks theme)

toCss : t -> S t -> String
toCss theme s =
    block s
        |> List.map (blockToString theme)
        |> String.join ("\n")
--        |> toString


--------------------------------------------------------------------------------

type alias Blocks t = List (Block t)
type alias Block theme = { selectors: List SelectorPath, styles: List (Styles theme) }


--------------------------------------------------------------------------------

type alias Style = (String,String)
type alias Styles theme = theme -> List Style


block : S t -> Blocks t
block s =
    case s of
        S ss -> ss

styleToString : Style -> String
styleToString (prop,val) =
    prop ++ ": " ++ val ++ ";"



blockToString : t -> Block t -> String
blockToString theme {selectors, styles} =
    let css = List.concatMap (\s -> s theme |> List.map styleToString) styles
        sels = List.map Selector.pathToString selectors |> String.join (",\n")
    in sels ++ " { " ++ String.join " " css ++ " }"

--------------------------------------------------------------------------------

emptyBlock : Block t
emptyBlock = { selectors = [], styles = [] }


empty : S theme
empty = S []


fromStyles : Styles t -> S t
fromStyles ss =
    S [{ selectors = [[Selector.empty]],  styles = [ ss ] }]



make : List SelectorPath  -> List(Styles t) -> S t
make p ss = S [{ selectors = p, styles = ss }]


classWithStyles : String -> Styles t -> S t
classWithStyles name ss = make [[Selector.class name]] [ss]

pseudoWithStyles : String -> Styles t -> S t
pseudoWithStyles name ss = make [[Selector.pseudo name]] [ss]


{-| Concatenates two style blocks
-}
append : S t -> S t -> S t
append a b =
    case (a,b) of
        (S aa, S bb) -> S <| aa ++ bb


{-| Concatenates n style blocks
-}
concat : List (S t) -> S t
concat bs =
    List.foldl append empty bs


-- SELECTORS -----------------------------------------------

mapSelectors : (SelectorPath -> SelectorPath) -> S t -> S t
mapSelectors fn s = block s |> List.map (\b -> { b | selectors = List.map fn b.selectors}) |> S


prefixSelector : Selector -> S t -> S t
prefixSelector sel = mapSelectors (\p -> sel :: p )

postfixSelector : Selector -> S t -> S t
postfixSelector sel = mapSelectors (\p -> p ++ [sel] )

--

prefixClass : String -> S t -> S t
prefixClass = prefixSelector << Selector.class

postfixClass : String -> S t -> S t
postfixClass = postfixSelector << Selector.class

wrapInClass : String -> List (S t) -> S t
wrapInClass n = prefixClass n << concat

--------------------------------------------------------------------------------

refine : Selector -> S t -> S t
refine sel s =
    prefixSelector (Selector.refine sel) s

refineClass : String -> S t -> S t
refineClass = refine << Selector.class

refinePseudo : String -> S t -> S t
refinePseudo = refine << Selector.pseudo

--
refineWith : Selector -> Styles t -> S t
refineWith c ss = make [[Selector.refine c]] [ss]

refineClassWith : String -> Styles t -> S t
refineClassWith = refineWith << Selector.class

refinePseudoWith : String -> Styles t -> S t
refinePseudoWith = refineWith << Selector.pseudo

--------------------------------------------------------------------------------

main =
    classWithStyles ".editor"
        (\{borderWidth} ->
            [ ("border-bottom", borderWidth)

            ])


    |> toCss {borderWidth = "1px solid"}

--    |> toString
    |> Html.text
    |> List.singleton
    |> Html.pre []