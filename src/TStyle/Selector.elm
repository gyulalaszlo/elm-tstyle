module TStyle.Selector
    exposing
        ( empty
        , class
        , refine
        , id
        , pseudo
        , matchingAll
        , toString
        , pathToString
        , Selector
        , SelectorPath
        )

{-| Describe me please...
-}


type Selector
    = Class String
    | Id String
    | Pseudo String
    | Refine Selector
    | Combined (List Selector)
    | Empty


class : String -> Selector
class =
    Class


id : String -> Selector
id =
    Id


pseudo : String -> Selector
pseudo =
    Pseudo


refine : Selector -> Selector
refine =
    Refine


matchingAll : List Selector -> Selector
matchingAll =
    Combined



---


toString : Selector -> String
toString s =
    case s of
        Empty ->
            ""

        Class c ->
            "." ++ c

        Id i ->
            "#" ++ i

        Pseudo p ->
            ":" ++ p

        Refine s ->
            toString s

        Combined ss ->
            List.map toString ss |> String.join ""


empty : Selector
empty =
    Empty



-- Path


type alias SelectorPath =
    List Selector


pathFolder : Selector -> String -> String
pathFolder s ss =
    case s of
        Empty ->
            ss

        Refine s ->
            ss ++ toString s

        _ ->
            ss ++ " " ++ toString s


pathToString : SelectorPath -> String
pathToString p =
    List.foldl pathFolder "" p
