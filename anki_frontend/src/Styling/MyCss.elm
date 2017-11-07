module Styling.MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, html, input, textarea, a, p)
import Css.Namespace exposing (namespace)


type CssClasses
    = NavBar
    | InputControl
    | TextAreaControl
    | FormControl
    | SubmitControl
    | MenuOption
    | MenuContainer
    | MenuContainerCol
    | FlashCardContainer
    | FlashCardBtn
    | TimerContainer
    | RowContainer
    | ArrowContainer
    | ArrowLeft
    | ArrowRight


type CssIds
    = Page


css =
    (stylesheet << namespace "dreamwriter")
        [ body
            [ displayFlex
            , backgroundColor (hex "e9e9e9")
            , borderRadius (pc 50)
            , color (hex "FFFFFF")
            , textAlign center
            , justifyContent center
            , alignItems center

            -- , boxShadow5 (px 3) (px 3) (px 0) (px 0) (rgba 0 0 0 0.14)
            , height (pct 100)
            , width (pct 100)
            , margin (px 0)
            , fontFamilies [ "Raleway", .value sansSerif ]
            ]
        , html
            [ height (pct 100)
            , width (pct 100)
            ]
        , id Page
            [ backgroundColor (rgb 200 128 64)
            , color (hex "CCFFFF")
            , width (pct 100)
            , height (pct 100)
            , boxSizing borderBox
            , padding (px 8)
            , margin zero
            ]
        , class MenuContainer menuContainer
        , class MenuContainerCol menuContainerCol
        , class TimerContainer timerContainer
        , class FlashCardContainer flashCardContainer
        , class FlashCardBtn flashCardBtn
        , class ArrowContainer arrowContainer
        , class ArrowLeft arrowLeft
        , class ArrowRight arrowRight
        , class RowContainer rowContainer
        , class SubmitControl submitControl
        , class FormControl formControl
        , class InputControl (inputControl (hex "fff"))
        , class TextAreaControl (textAreaControl (hex "fff"))
        , class MenuOption menuOption
        , class NavBar
            [ margin zero
            , padding zero
            , children
                [ li
                    [ (display inlineBlock) |> important
                    , color primaryAccentColor
                    ]
                ]
            ]
        ]


whiteStyling attrs =
    [ displayFlex
    , textAlign center
    , justifyContent center
    , alignItems center
    , backgroundColor (hex "FFFFFF")
    , color (hex "000000")
    , borderWidth (px 4)
    , borderStyle solid
    , borderColor (hex "FFFFFF")
    , opacity (int 1)
    , boxShadow5 (px 0) (px 1) (px 2) (px 0) (rgba 0 0 0 0.1)
    ]
        ++ attrs


arrowContainer =
    [ height (px 200)
    , width (px 200)
    , displayFlex
    , justifyContent center
    , alignItems center
    ]


arrowLeft =
    [ height (em 4)
    , width (em 4)
    , border3 (px 1) solid (hex "000000")
    , borderWidth4 (px 1) (px 1) (px 0) (px 0)
    , transform (rotate (deg (-135)))
    , cursor pointer
    , after
        [ property "content" "''"
        , height (em 4)
        , width (em 4)
        , display block
        , border3 (px 1) solid (hex "000000")
        , borderWidth4 (px 1) (px 1) (px 0) (px 0)
        , transform (translate2 (px 3) (px -5))
        ]
    ]


arrowRight =
    [ height (em 4)
    , width (em 4)
    , border3 (px 1) solid (hex "000000")
    , borderWidth4 (px 1) (px 1) (px 0) (px 0)
    , transform (rotate (deg 45))
    , cursor pointer
    , after
        [ property "content" "''"
        , height (em 4)
        , width (em 4)
        , display block
        , border3 (px 1) solid (hex "000000")
        , borderWidth4 (px 1) (px 1) (px 0) (px 0)
        , transform (translate2 (px 3) (px -5))
        ]
    ]


menuContainer =
    [ displayFlex
    , flexDirection row
    , padding (px 5)
    , borderRadius (px 4)
    , children
        [ everything
            [ flex2 (int 1) (int 1)
            , margin (px 5)
            ]
        ]
    ]


rowContainer =
    [ displayFlex
    , flexDirection row
    , children
        [ everything
            [ flex2 (int 1) (int 1)
            ]
        ]
    ]


menuContainerCol =
    [ displayFlex
    , flexDirection column
    , padding (px 5)
    , borderRadius (px 8)
    , children
        [ everything
            [ flex2 (int 1) (int 1)
            , margin (px 5)
            ]
        ]
    ]


flashCardBtn =
    let
        attributes =
            [ borderRadius (px 4)
            , fontSize (em 1)
            , cursor pointer
            , children
                [ a
                    [ textDecoration none
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , width (pct 100)
                    , height (pct 100)
                    ]
                ]
            ]
    in
        whiteStyling attributes


timerContainer =
    let
        attributes =
            [ borderRadius (px 4)
            , height (em 2)
            , fontSize (em 1)
            , children
                [ p
                    [ textDecoration none
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , width (pct 100)
                    , height (pct 100)
                    ]
                ]
            ]
    in
        whiteStyling attributes


flashCardContainer =
    let
        attributes =
            [ width (pc 30)
            , height (pc 10)
            , padding (pc 5)
            , borderRadius (px 4)
            , fontSize (em 3)
            , children
                [ p
                    [ textDecoration none
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , width (pct 100)
                    , height (pct 100)
                    ]
                ]
            ]
    in
        whiteStyling attributes


menuOption =
    let
        attributes =
            [ width (px 100)
            , height (px 100)
            , padding (px 25)
            , children
                [ a
                    [ textDecoration none
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , width (pct 100)
                    , height (pct 100)
                    ]
                ]
            ]
    in
        whiteStyling attributes


formControl =
    [ displayFlex
    , width inherit
    , justifyContent center
    , flexDirection column
    , alignItems center
    ]


submitControl =
    inputControl (hex "7f8ff4")
        ++ [ cursor pointer
           , textTransform uppercase
           ]


inputControl cl =
    [ padding (px 15)
    , width (pct 20)
    , backgroundColor cl
    , boxShadow5 zero (px 6) (px 10) zero (rgba 0 0 0 0.1)
    , outline zero
    , fontSize (em 1)
    , justifyContent center
    , alignItems center
    , margin (em 1)
    , children
        [ input
            [ border zero
            , fontSize medium
            , textTransform uppercase
            , outline zero
            , fontFamily inherit
            ]
        ]
    ]


textAreaControl cl =
    [ padding (px 15)
    , width (pct 20)
    , backgroundColor cl
    , boxShadow5 zero (px 6) (px 10) zero (rgba 0 0 0 0.1)
    , outline zero
    , fontSize (em 1)
    , justifyContent center
    , alignItems center
    , margin (em 1)
    , children
        [ textarea
            [ border zero
            , fontSize medium
            , textTransform uppercase
            , fontFamily inherit
            ]
        ]
    ]


primaryAccentColor =
    hex "ccffaa"
