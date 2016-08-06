module Copy exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String exposing (..)

-- model
type alias Model =
  { calories : Int
  , input : Int
  , takeAway : Int
  , error : Maybe String
  }

initModel : Model
initModel =
  { calories = 0
  , input = 0
  , takeAway = 0,
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
        | calories = model.calories + model.inpit
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

    Clear ->
      initModel
