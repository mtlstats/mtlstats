{- |

mtlstats
Copyright (C) 2019 Rhéal Lamothe
<rheal.lamothe@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

module Mtlstats.Prompt.EditGoalie (goalieToEditPrompt) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~))

import Mtlstats.Prompt
import Mtlstats.Types

-- | Prompt to select a 'Goalie' for editing
goalieToEditPrompt :: Prompt
goalieToEditPrompt = selectGoaliePrompt "Goalie to edit: " $
  modify . (progMode.editGoalieStateL.egsSelectedGoalie .~)