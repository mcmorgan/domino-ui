module NonEmptyList exposing
    ( Error(..)
    , NonEmptyList
    , append
    , fromList
    , head
    , map
    , singleton
    , toList
    )


type NonEmptyList a
    = NonEmptyList a (List a)


type Error
    = EmptyList


singleton : a -> NonEmptyList a
singleton a =
    NonEmptyList a []


head : NonEmptyList a -> a
head (NonEmptyList first _) =
    first


map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map f (NonEmptyList first rest) =
    List.foldl append (singleton (f first)) (List.map f rest)


toList : NonEmptyList a -> List a
toList (NonEmptyList first rest) =
    first :: rest


append : a -> NonEmptyList a -> NonEmptyList a
append a (NonEmptyList first rest) =
    NonEmptyList first (rest ++ [ a ])


fromList : List a -> Result Error (NonEmptyList a)
fromList list =
    case list of
        [] ->
            Err <| EmptyList

        first :: rest ->
            Ok <| List.foldr append (singleton first) rest
