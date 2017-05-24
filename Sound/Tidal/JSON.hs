-- Module by d0kt0r0

module Sound.Tidal.JSON where

import Text.JSON
import Control.Applicative
import Control.Monad
import Sound.Tidal.Context
import qualified Data.Map as Map

data WebDirtEvent = WebDirtEvent Double ParamMap

instance JSON WebDirtEvent where
    readJSON _ = Error "Reading JSON as WebDirtEvent not supported"
    showJSON (WebDirtEvent t ps) = makeObj (t':Map.elems (Map.mapWithKey (f) ps))
      where t' = ("when",showJSON t)
            f (S name _) (VS x) = (name,showJSON x)
            f (F name _) (VF x) = (name,showJSON x)
            f (I name _) (VI x) = (name,showJSON x)

instance Show WebDirtEvent where
  show x = encodeStrict x

render :: ParamPattern -> Double -> Double -> [WebDirtEvent]
render patt cps cycles = map f (arc patt (0,(approxRational cycles 0.001)))
  where f ((a,b),(c,d),params) = WebDirtEvent ((fromRational a)/cps) params
