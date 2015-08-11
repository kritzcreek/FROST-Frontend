module Openspace.Ui.Render where


import           Control.Apply
import           Control.Monad.Eff
import           DOM
import           Data.Array
import           Data.Function
import           Data.List (fromList)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Tuple
import           Openspace.Types
import           Prelude

type Grid = Array (Array (Maybe SanitizedTopic))
type SanitizedTopic = { description :: String, typ :: String }

sanitizeTopic :: Topic -> SanitizedTopic
sanitizeTopic (Topic t) = { description: t.description,
                            typ: show t.typ }

findIn :: Room -> Block -> M.Map Slot Topic -> Maybe SanitizedTopic
findIn r b timeslots = M.lookup (Slot {block:b, room:r}) timeslots
                       <#> sanitizeTopic

makeGrid :: AppState -> Grid
makeGrid as = (\r ->
                (\b -> findIn r b as.timeslots ) <$> (sort as.blocks)
              ) <$> as.rooms

foreign import renderMenu :: forall eff. Array String -> Eff( dom::DOM | eff ) Unit

foreign import renderTopicsImpl :: forall eff. Array SanitizedTopic -> Eff( dom::DOM | eff ) Unit

foreign import renderGridImpl :: forall eff. Fn3 (Array Room) (Array Block) Grid (Eff( dom::DOM | eff ) Unit)

renderTopics :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderTopics as = renderTopicsImpl (sanitizeTopic <$> filter topicNotInGrid as.topics)
  where topicNotInGrid t = elemIndex t (fromList $ M.values as.timeslots) == Nothing

renderGrid :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderGrid as = runFn3 renderGridImpl as.rooms (sort as.blocks) (makeGrid as)

renderApp :: forall eff. AppState -> Eff( dom::DOM | eff ) Unit
renderApp as = renderTopics as *> renderGrid as
