module Components exposing
    ( button
    , checkboxes
    , container
    , dateInput
    , defaultFieldOpts
    , field
    , fieldset
    , link
    , numberInput
    , select
    , showIf
    , textInput
    )

import Css
    exposing
        ( Style
        , alignItems
        , alignSelf
        , backgroundColor
        , block
        , border3
        , borderRadius
        , ch
        , color
        , cursor
        , display
        , displayFlex
        , flexEnd
        , flexStart
        , flexWrap
        , fontFamilies
        , fontSize
        , fontWeight
        , hex
        , hover
        , int
        , lineHeight
        , margin2
        , maxWidth
        , padding
        , pointer
        , property
        , px
        , rem
        , right
        , solid
        , textAlign
        , width
        , wrap
        )
import Date exposing (Date)
import Html.Styled as Html
    exposing
        ( Html
        , a
        , div
        , fieldset
        , input
        , label
        , legend
        , text
        )
import Html.Styled.Attributes as Attr
    exposing
        ( css
        , for
        , href
        , id
        , required
        , selected
        , target
        , type_
        , value
        )
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Types exposing (FieldOpts)


defaultFontFamily : List String
defaultFontFamily =
    [ "-apple-system"
    , "BlinkMacSystemFont"
    , "Segoe UI"
    , "Helvetica"
    , "Arial"
    , "sans-serif"
    ]


inputCss : List Style
inputCss =
    [ fontSize (rem 1)
    , padding (rem 0.375)
    , border3 (px 1) solid (hex "bbbbbb")
    , borderRadius (px 4)
    , fontFamilies defaultFontFamily
    , margin2 (rem 0.375) (rem 0)
    , width (rem 8)
    , backgroundColor (hex "ffffff")
    ]


field : String -> String -> List (Html msg) -> Html msg
field inputId labelText content =
    div [] <| label inputId labelText :: content


fieldset : String -> List (Html msg) -> Html msg
fieldset legendText content =
    Html.fieldset
        [ css
            [ displayFlex
            , alignItems flexStart
            , flexWrap wrap
            , backgroundColor (hex "fbfbfb")
            , border3 (px 1) solid (hex "eeeeee")
            , padding (rem 0.75)
            , borderRadius (px 4)
            , margin2 (rem 0.375) (rem 0)
            , property "gap" "0.375rem 0.75rem"
            ]
        ]
    <|
        [ legend
            [ css
                [ fontSize (rem 0.75)
                , lineHeight (rem 1.5)
                , fontWeight (int 400)
                ]
            ]
            [ text legendText ]
        ]
            ++ content


label : String -> String -> Html msg
label inputId labelText =
    Html.label
        [ for inputId
        , css
            [ fontWeight (int 400)
            , fontSize (rem 0.875)
            , lineHeight (rem 1)
            , display block
            ]
        ]
        [ text labelText ]


checkboxes :
    String
    -> String
    -> (String -> msg)
    -> List ( String, Bool )
    -> Html msg
checkboxes idPrefix labelText msg options =
    div
        []
    <|
        [ Html.label
            [ css
                [ fontWeight (int 400)
                , fontSize (rem 0.875)
                , lineHeight (rem 1)
                , display block
                ]
            ]
            [ text labelText ]
        , div
            [ css [ displayFlex, flexWrap wrap, property "gap" "0.5rem" ] ]
            (List.map (checkbox idPrefix msg) options)
        ]


checkbox : String -> (String -> msg) -> ( String, Bool ) -> Html msg
checkbox idPrefix msg ( option, isChecked ) =
    Html.label
        [ css
            [ fontSize (rem 0.75)
            , fontFamilies defaultFontFamily
            ]
        ]
        [ Html.input
            [ type_ "checkbox"
            , Attr.checked isChecked
            , value option
            , onInput msg
            ]
            []
        , Html.span [] [ text option ]
        ]


dateInput : String -> Maybe Date -> (String -> msg) -> FieldOpts msg -> Html msg
dateInput inputId currentValue event opts =
    let
        stringValue =
            currentValue
                |> Maybe.map Date.toIsoString
                |> Maybe.withDefault ""

        onBlurAttr =
            case opts.onBlur of
                Just msg ->
                    [ onBlur msg ]

                Nothing ->
                    []
    in
    input
        ([ id inputId
         , type_ "date"
         , value stringValue
         , onInput event
         , css inputCss
         , required opts.required
         ]
            ++ onBlurAttr
        )
        []


numberInput :
    String
    -> Int
    -> (String -> msg)
    -> FieldOpts msg
    -> Html msg
numberInput inputId currentValue event opts =
    let
        rangeAttr maybeInt =
            maybeInt |> Maybe.map String.fromInt |> Maybe.withDefault ""
    in
    input
        [ id inputId
        , type_ "number"
        , value <| String.fromInt currentValue
        , onInput event
        , Attr.min <| rangeAttr opts.min
        , Attr.max <| rangeAttr opts.max
        , required opts.required
        , css inputCss
        ]
        []


textInput : String -> String -> (String -> msg) -> FieldOpts msg -> Html msg
textInput inputId currentValue event opts =
    input
        [ id inputId
        , type_ "text"
        , value currentValue
        , onInput event
        , css inputCss
        , required opts.required
        ]
        []


defaultFieldOpts : FieldOpts msg
defaultFieldOpts =
    { onBlur = Nothing
    , min = Nothing
    , max = Nothing
    , required = False
    }


select : String -> String -> (String -> msg) -> List ( String, String ) -> Html msg
select inputId currentValue msg options =
    let
        option : ( String, String ) -> Html msg
        option ( optionValue, optionText ) =
            Html.option
                [ selected (optionValue == currentValue)
                , value optionValue
                ]
                [ text optionText ]
    in
    Html.select
        [ id inputId
        , onInput msg
        , css <|
            inputCss
                ++ [ property "-webkit-appearance" "none"
                   , property "-moz-appearance" "none"
                   , property "appearance" "none"
                   ]
        ]
        (List.map option options)


button : String -> msg -> Html msg
button buttonText msg =
    Html.button
        [ css
            [ fontSize (rem 0.75)
            , padding (rem 0.5)
            , border3 (px 1) solid (hex "bbbbbb")
            , borderRadius (px 4)
            , fontFamilies defaultFontFamily
            , margin2 (rem 0.375) (rem 0)
            , backgroundColor (hex "eeeeee")
            , cursor pointer
            , alignSelf flexEnd
            ]
        , onClick msg
        ]
        [ text buttonText ]


link : String -> String -> Html msg
link url linkText =
    Html.a
        [ href url
        , target "_blank"
        , css
            [ fontSize (rem 0.75)
            , color (hex "202c31")
            , hover [ color (hex "71819c") ]
            ]
        ]
        [ a [] []
        , text linkText
        ]


container : List (Html msg) -> Html msg
container content =
    Html.section
        [ css
            [ fontFamilies defaultFontFamily
            , padding (rem 0)
            , lineHeight (rem 1.5)
            , color (hex "202c31")
            , maxWidth (ch 80)
            ]
        ]
    <|
        content
            ++ [ footer ]


footer : Html msg
footer =
    Html.footer
        [ css [ textAlign right, fontSize (rem 0.75) ] ]
        [ link "https://www.mathiaspolligkeit.com"
            "mathiaspolligkeit.com"
        , text " | "
        , link "https://github.com/woylie/life-in-weeks"
            "view source on Github"
        ]


showIf : Bool -> Html msg -> Html msg
showIf show content =
    if show then
        content

    else
        text ""
