module Data.Card exposing (Card, new)


type alias Card =
    { contentEn : String
    , contentJp : String
    , contentJpKanji : String
    }


new : Card
new =
    { contentEn = "Liquid"
    , contentJp = "えきたい"
    , contentJpKanji = "液体"
    }
