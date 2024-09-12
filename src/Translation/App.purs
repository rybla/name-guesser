module Translation.App where

import Prelude

import Control.Monad.State (get, modify_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Translation (translate)
import Web.UIEvent.KeyboardEvent as KE

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = H.mkComponent { initialState, eval, render }
  where
  initialState _ = { string: "" }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction ke = do
    if KE.key ke == "Enter" then do
      string <- getInput # liftEffect
      let string' = translate string
      modify_ _ { string = string' }
      pure unit
    else
      pure unit
    pure unit

  render { string } =
    HH.div []
      [ HH.div []
          [ HH.input
              [ HP.id "input"
              , HE.onKeyDown identity
              ]
          ]
      , HH.div []
          [ HH.a [ HP.href $ "https://rybla.github.io/name-guesser/?q=" <> string ]
              [ HH.text string ]
          ]
      ]

foreign import getInput :: Effect String

