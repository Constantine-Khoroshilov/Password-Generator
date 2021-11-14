module Main exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Generator)
import Tuple




-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }




-- MODEL


type alias Model =
  { password : String 
  , maxLength : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { password = ""
    , maxLength = 25 
    }
  , generatePassword
  )




-- UPDATE


type Msg 
  = GotPassword String
  | SetMaxLength String
  | ResetMaxLength
  | Generate


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPassword password ->
      ( { model | password = (changePassLength model.maxLength password) }
      , Cmd.none
      )

    Generate ->
      ( model
      , generatePassword
      )

    SetMaxLength length ->
      ( { model | maxLength = (stringToInt length) }
      , Cmd.none
      )

    ResetMaxLength ->
      ( { model | maxLength = 25 }
      , Cmd.none
      )


changePassLength : Int -> String -> String
changePassLength length password =
  password
    |> String.dropRight 
      ((String.length password) - length)


stringToInt : String -> Int
stringToInt string =
  case String.toInt string of
    Just num ->
      num
    Nothing ->
      0




-- GENERATOR


generatePassword : Cmd Msg
generatePassword =
  Random.generate GotPassword generator


generator : Generator String
generator =
  listSymbols
    |> Random.map (\symList -> String.fromList symList) 


listSymbols : Generator (List Char)
listSymbols =
  Random.int 8 25
    |> Random.andThen (\len -> Random.list len symbol)


-- Choice one of the four symbols

symbol : Generator Char
symbol =
  symbols
    |> Random.andThen (\sym -> 
        Random.weighted (40, sym.lowerLetter)  
          [ (30, sym.upperLetter)
          , (20, sym.number)
          , (10, sym.mark) 
          ])


-- Generation the record of the four symbols

type alias Symbols =
  { lowerLetter : Char
  , upperLetter : Char
  , number : Char
  , mark : Char
  }


symbols : Generator Symbols
symbols =
  Random.map4 Symbols
    lowerLetter
    upperLetter
    number
    mark


-- Generation the symbol

lowerLetter : Generator Char
lowerLetter =
  Random.int 0 25
    |> Random.map (\n -> Char.fromCode (n + 97))


upperLetter : Generator Char
upperLetter =
  Random.int 0 25
    |> Random.map (\n -> Char.fromCode (n + 65))


number : Generator Char
number =
  Random.int 0 9
    |> Random.map (\n -> Char.fromCode (n + 48))


-- !"$%&'()+,-./:;<=>?@[]^_{|}~` (29)

mark : Generator Char
mark =
  Random.uniform 33 
    [ 34, 36, 37, 38, 39, 40, 41
    , 43, 44, 45, 46, 47, 58, 59
    , 60, 61, 62, 63, 64, 91, 93
    , 94, 95, 96, 123, 124, 125, 126 
    ]
      |> Random.map (\n -> Char.fromCode n)




-- VIEW
 

view : Model -> Html Msg
view model =
  main_ []
    [ h1 []
        [ text "Генератор паролей" ]

    , div [ class "password" ]
        [ text model.password ]

    , div [ class "h" ]
        [ text "Длина: "
        , text (String.fromInt (String.length model.password))
        ]
            
    , div [ class "btn-section" ]
        [ button
          [ onClick Generate ]
          [ text "Новый пароль" ]
        ]

    , div [ class "h" ]
        [ text "Максимальная длина: "
        , text (String.fromInt model.maxLength) 
        ]

    , div [ style "margin-top" "7px" ]
        [ viewRange "8" "25" (String.fromInt model.maxLength) SetMaxLength ]

    , div [ class "btn-section" ]
        [ button
          [ onClick ResetMaxLength ]
          [ text "Сброс" ]
        ]  
    ] 


viewRange : String -> String -> String -> (String -> Msg) -> Html Msg
viewRange min max v msg =
  input 
    [ type_ "range"
    , onInput msg
    , Html.Attributes.min min
    , Html.Attributes.max max
    , value v
    ] 
    []