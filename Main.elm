module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String exposing (..)

-- model
type alias Model =
  { calories : Int
  , input : Int
  , takeAway: Int
  , error : Maybe String
  }

initModel : Model
initModel =
  { calories = 0
  , input = 0
  , takeAway = 0
  , error = Nothing
  }

-- update
type Msg
  = AddCalories
  | Clear
  | Input String
  | TakeAway String
  | TakeCalories

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddCalories ->
      { model
        | calories = model.calories + model.input
        , input = 0
      }

    TakeCalories ->
      { model
        | calories = model.calories - model.takeAway
        , takeAway = 0
      }
    
    Input val ->
      case String.toInt val of
        Ok input ->
          { model
            | input = input
            , error = Nothing
          }
        
        Err err ->
          { model
            | input = 0
            , error = Just err
          }

    TakeAway val ->
      case String.toInt val of
        Ok takeAway ->
          { model
            | takeAway = takeAway
            , error = Nothing
          }

        Err err ->
          { model
            | takeAway = 0
            , error = Just err
          }

    Clear ->
      initModel

-- view
view : Model -> Html Msg
view model =
  div []
    [ h3 []
      [ text ("Total Calories: " ++ (toString model.calories )) ]
    , input
      [ type' "text"
      , placeholder "Add"
      , onInput Input 
      , value
        (if model.input == 0 then
          ""
         else
           toString model.input
        )
      ]
      []
    , input
      [ type' "text"
      , onInput TakeAway 
      , placeholder "Take Away"
      , value
        (if model.takeAway == 0 then
          ""
         else
           toString model.takeAway
        )
      ]
      []
    , div [] [ text (Maybe.withDefault "" model.error) ]
    , button
      [ type' "button"
      , onClick TakeCalories
      ]
      [ text "Take Calories" ]
    , button
      [ type' "button"
      , onClick AddCalories
      ]
      [ text "Add" ]
    , button
      [ type' "button"
      , onClick Clear
      ]
      [ text "Clear" ]
    , p [] [ text (toString model) ]
    ]


main : Program Never
main = 
  App.beginnerProgram
    { model = initModel
    , update = update
    , view = view
    }
