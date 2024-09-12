module Main where

import Prelude

import Control.Monad.State (get, modify_)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Maybe (Maybe(..), fromMaybe', isNothing)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Questions (Question, initialQuestions)
import Translation (untranslate)
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

data Query :: forall k. k -> Type
data Query a

type Input = {}

data Output

type State =
  { name :: String
  , pastQuestions :: Array (Question /\ Int)
  , maybe_currentQuestion :: Maybe (Question /\ Maybe Int)
  , futureQuestions :: Array (Question /\ Maybe Int)
  }

data Action
  = Initialize
  | SelectAnswer Int (Maybe MouseEvent)
  | NextQuestion (Maybe MouseEvent)
  | BackQuestion (Maybe MouseEvent)

type HTML = H.ComponentHTML Action Slots Aff

type Slots :: forall k. Row k
type Slots = ()

type HM = H.HalogenM State Action Slots Output Aff

component :: H.Component Query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState _input =
    { name: ""
    , pastQuestions: []
    , maybe_currentQuestion: Nothing
    , futureQuestions: []
    }

  eval = H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }

  handleAction :: Action -> HM Unit
  handleAction = case _ of
    Initialize -> do
      name <- do
        name <- getName # map untranslate # liftEffect
        if String.null name then do
          let names = [ "Albert", "Kelly", "Jo", "Bob", "Kim", "Abdul", "Ryan", "Joseph", "Jacob", "Benny" ]
          i <- randomInt 0 (Array.length names - 1) # liftEffect
          names Array.!! i # fromMaybe' (\_ -> unsafeCrashWith "impossible") # pure
        else
          pure name
      let k = 4
      let futureQuestions = initialQuestions # getRandomSubset k
      -- let futureQuestions = initialQuestions
      -- let futureQuestions = []
      modify_ _
        { name = name
        , futureQuestions = futureQuestions # map (_ /\ Nothing)
        }
    SelectAnswer i _mb_event -> do
      modify_ \state -> state { maybe_currentQuestion = state.maybe_currentQuestion # map (rmap (const (pure i))) }
    BackQuestion _mb_event -> do
      Console.log "BackQuestion"
      state <- get
      case state.maybe_currentQuestion of
        Just (q /\ mb_i) -> do
          case Array.unsnoc state.pastQuestions of
            Nothing -> do
              pure unit
            Just { init, last } -> do
              modify_ _
                { pastQuestions = init
                , maybe_currentQuestion = pure (last # rmap pure)
                , futureQuestions = Array.cons (q /\ mb_i) state.futureQuestions
                }
        _ -> pure unit
    NextQuestion _mb_event -> do
      state <- get
      Console.log "NextQuestion"
      case state.maybe_currentQuestion of
        Nothing | Array.null state.pastQuestions -> do
          Console.log "here"
          case Array.uncons state.futureQuestions of
            Nothing -> do
              pure unit
            Just { head, tail } -> do
              modify_ _
                { pastQuestions = state.pastQuestions
                , maybe_currentQuestion = pure head
                , futureQuestions = tail
                }
        Just (_ /\ Nothing) -> do
          pure unit
        Just (q /\ Just i) -> do
          case Array.uncons state.futureQuestions of
            Nothing -> do
              modify_ _
                { pastQuestions = state.pastQuestions `Array.snoc` (q /\ i)
                , maybe_currentQuestion = empty
                , futureQuestions = mempty
                }
            Just { head, tail } -> do
              modify_ _
                { pastQuestions = state.pastQuestions `Array.snoc` (q /\ i)
                , maybe_currentQuestion = pure head
                , futureQuestions = tail
                }
        _ -> pure unit

  render state = Debug.trace (show state) \_ ->
    HH.div [ HP.classes [ HH.ClassName "app" ] ]
      $ Array.fold
          [ [ HH.div [ HP.classes [ HH.ClassName "title" ] ]
                [ HH.text "name-guesser" ]
            ]
          , case state.maybe_currentQuestion of
              Nothing ->
                if Array.null state.futureQuestions then
                  [ HH.div [ HP.classes [ HH.ClassName "status" ] ]
                      [ HH.div [] [ HH.text "All done!" ]
                      , HH.div [] [ HH.text $ "Your name must be:" ]
                      , HH.div [ HP.classes [ HH.ClassName "name" ] ] [ HH.text $ "\"" <> state.name <> "\"" ]
                      ]
                  ]
                else
                  [ HH.div [ HP.classes [ HH.ClassName "status" ] ]
                      [ HH.div [] [ HH.text "I will as you a series of 4 questions in order to infer your name." ]
                      , HH.div [] [ HH.text "Click \"start\" to start" ]
                      ]
                  ]
              Just (currentQuestion /\ mb_i) ->
                [ renderQuestion mb_i currentQuestion
                ]
          , [ HH.div
                [ HP.classes [ HH.ClassName "controls" ] ]
                [ HH.div
                    [ HP.classes
                        ( Array.fold
                            [ [ HH.ClassName "control" ]
                            , if Array.null state.pastQuestions then
                                [ HH.ClassName "inactive" ]
                              else if isNothing state.maybe_currentQuestion && Array.null state.futureQuestions then
                                [ HH.ClassName "inactive" ]
                              else
                                [ HH.ClassName "active" ]
                            ]
                        )
                    , HE.onClick (BackQuestion <<< Just)
                    ]
                    [ HH.text "back" ]
                , HH.div
                    [ HP.classes
                        ( Array.fold
                            [ [ HH.ClassName "control" ]
                            , case state.maybe_currentQuestion of
                                Just (_ /\ Just _) -> [ HH.ClassName "active" ]
                                Nothing | not (Array.null state.futureQuestions) -> [ HH.ClassName "active" ]
                                _ -> [ HH.ClassName "inactive" ]
                            ]
                        )
                    , HE.onClick (NextQuestion <<< Just)
                    ]
                    if isNothing state.maybe_currentQuestion && Array.null state.pastQuestions then
                      [ HH.text "start" ]
                    else if isNothing state.maybe_currentQuestion && Array.null state.futureQuestions then
                      [ HH.text "next" ]
                    else
                      [ HH.text "next" ]
                ]
            ]
          ]

renderQuestion :: Maybe Int -> Question -> HTML
renderQuestion mb_i question =
  HH.div [ HP.classes [ HH.ClassName "status", HH.ClassName "question" ] ]
    [ HH.div [ HP.classes [ HH.ClassName "prompt" ] ]
        [ HH.text question.prompt ]
    , HH.div [ HP.classes [ HH.ClassName "answers" ] ]
        ( question.answers # Array.mapWithIndex \i' answer ->
            HH.div
              [ HP.classes
                  ( Array.fold
                      [ [ HH.ClassName "answer" ]
                      , if mb_i == Just i' then [ HH.ClassName "selected" ] else []
                      ]
                  )
              , HE.onClick (SelectAnswer i' <<< Just)
              ]
              [ HH.text answer ]
        )
    ]

--------------------------------------------------------------------------------

foreign import getName :: Effect String

foreign import getRandomSubset :: forall a. Int -> Array a -> Array a
