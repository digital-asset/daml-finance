-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | This module contains specs for a number of default spinning indicators.
-- This file has been auto-generated using scripts/codegen. The specs
-- themselves have been copied from github.com/sindresorhus/cli-spinners.
module Spin.Specs
  ( dotsSpec
  , arrow3Spec
  , bouncingBarSpec
  , bouncingBallSpec
  , pongSpec
  , sharkSpec
  , grenadeSpec
  , pointSpec
  , layerSpec
  ) where

-- More Specs can be found here :
-- https://github.com/alexanderGugel/spin/blob/master/src/Spin/Specs.hs

import Spin.Types

-- | Spec for dots spinning indicator:
-- ⠋
-- ⠙
-- ⠹
-- ⠸
-- ⠼
-- ⠴
-- ⠦
-- ⠧
-- ⠇
-- ⠏
dotsSpec :: Spec
dotsSpec = Spec
  { specName = "dots"
  , specInterval = 80
  , specFrames =
    [ "⠋"
    , "⠙"
    , "⠹"
    , "⠸"
    , "⠼"
    , "⠴"
    , "⠦"
    , "⠧"
    , "⠇"
    , "⠏"
    ]
  }

-- | Spec for arrow3 spinning indicator:
-- ▹▹▹▹▹
-- ▸▹▹▹▹
-- ▹▸▹▹▹
-- ▹▹▸▹▹
-- ▹▹▹▸▹
-- ▹▹▹▹▸
arrow3Spec :: Spec
arrow3Spec = Spec
  { specName = "arrow3"
  , specInterval = 120
  , specFrames =
    [ "▹▹▹▹▹"
    , "▸▹▹▹▹"
    , "▹▸▹▹▹"
    , "▹▹▸▹▹"
    , "▹▹▹▸▹"
    , "▹▹▹▹▸"
    ]
  }

-- | Spec for bouncingBar spinning indicator:
-- [    ]
-- [=   ]
-- [==  ]
-- [=== ]
-- [ ===]
-- [  ==]
-- [   =]
-- [    ]
-- [   =]
-- [  ==]
-- [ ===]
-- [====]
-- [=== ]
-- [==  ]
-- [=   ]
bouncingBarSpec :: Spec
bouncingBarSpec = Spec
  { specName = "bouncingBar"
  , specInterval = 80
  , specFrames =
    [ "[    ]"
    , "[=   ]"
    , "[==  ]"
    , "[=== ]"
    , "[ ===]"
    , "[  ==]"
    , "[   =]"
    , "[    ]"
    , "[   =]"
    , "[  ==]"
    , "[ ===]"
    , "[====]"
    , "[=== ]"
    , "[==  ]"
    , "[=   ]"
    ]
  }

-- | Spec for bouncingBall spinning indicator:
-- ( ●    )
-- (  ●   )
-- (   ●  )
-- (    ● )
-- (     ●)
-- (    ● )
-- (   ●  )
-- (  ●   )
-- ( ●    )
-- (●     )
bouncingBallSpec :: Spec
bouncingBallSpec = Spec
  { specName = "bouncingBall"
  , specInterval = 80
  , specFrames =
    [ "( ●    )"
    , "(  ●   )"
    , "(   ●  )"
    , "(    ● )"
    , "(     ●)"
    , "(    ● )"
    , "(   ●  )"
    , "(  ●   )"
    , "( ●    )"
    , "(●     )"
    ]
  }

-- | Spec for pong spinning indicator:
-- ▐⠂       ▌
-- ▐⠈       ▌
-- ▐ ⠂      ▌
-- ▐ ⠠      ▌
-- ▐  ⡀     ▌
-- ▐  ⠠     ▌
-- ▐   ⠂    ▌
-- ▐   ⠈    ▌
-- ▐    ⠂   ▌
-- ▐    ⠠   ▌
-- ▐     ⡀  ▌
-- ▐     ⠠  ▌
-- ▐      ⠂ ▌
-- ▐      ⠈ ▌
-- ▐       ⠂▌
-- ▐       ⠠▌
-- ▐       ⡀▌
-- ▐      ⠠ ▌
-- ▐      ⠂ ▌
-- ▐     ⠈  ▌
-- ▐     ⠂  ▌
-- ▐    ⠠   ▌
-- ▐    ⡀   ▌
-- ▐   ⠠    ▌
-- ▐   ⠂    ▌
-- ▐  ⠈     ▌
-- ▐  ⠂     ▌
-- ▐ ⠠      ▌
-- ▐ ⡀      ▌
-- ▐⠠       ▌
pongSpec :: Spec
pongSpec = Spec
  { specName = "pong"
  , specInterval = 80
  , specFrames =
    [ "▐⠂       ▌"
    , "▐⠈       ▌"
    , "▐ ⠂      ▌"
    , "▐ ⠠      ▌"
    , "▐  ⡀     ▌"
    , "▐  ⠠     ▌"
    , "▐   ⠂    ▌"
    , "▐   ⠈    ▌"
    , "▐    ⠂   ▌"
    , "▐    ⠠   ▌"
    , "▐     ⡀  ▌"
    , "▐     ⠠  ▌"
    , "▐      ⠂ ▌"
    , "▐      ⠈ ▌"
    , "▐       ⠂▌"
    , "▐       ⠠▌"
    , "▐       ⡀▌"
    , "▐      ⠠ ▌"
    , "▐      ⠂ ▌"
    , "▐     ⠈  ▌"
    , "▐     ⠂  ▌"
    , "▐    ⠠   ▌"
    , "▐    ⡀   ▌"
    , "▐   ⠠    ▌"
    , "▐   ⠂    ▌"
    , "▐  ⠈     ▌"
    , "▐  ⠂     ▌"
    , "▐ ⠠      ▌"
    , "▐ ⡀      ▌"
    , "▐⠠       ▌"
    ]
  }

-- | Spec for shark spinning indicator:
-- ▐|\____________▌
-- ▐_|\___________▌
-- ▐__|\__________▌
-- ▐___|\_________▌
-- ▐____|\________▌
-- ▐_____|\_______▌
-- ▐______|\______▌
-- ▐_______|\_____▌
-- ▐________|\____▌
-- ▐_________|\___▌
-- ▐__________|\__▌
-- ▐___________|\_▌
-- ▐____________|\▌
-- ▐____________/|▌
-- ▐___________/|_▌
-- ▐__________/|__▌
-- ▐_________/|___▌
-- ▐________/|____▌
-- ▐_______/|_____▌
-- ▐______/|______▌
-- ▐_____/|_______▌
-- ▐____/|________▌
-- ▐___/|_________▌
-- ▐__/|__________▌
-- ▐_/|___________▌
-- ▐/|____________▌
sharkSpec :: Spec
sharkSpec = Spec
  { specName = "shark"
  , specInterval = 120
  , specFrames =
    [ "▐|\\____________▌"
    , "▐_|\\___________▌"
    , "▐__|\\__________▌"
    , "▐___|\\_________▌"
    , "▐____|\\________▌"
    , "▐_____|\\_______▌"
    , "▐______|\\______▌"
    , "▐_______|\\_____▌"
    , "▐________|\\____▌"
    , "▐_________|\\___▌"
    , "▐__________|\\__▌"
    , "▐___________|\\_▌"
    , "▐____________|\\▌"
    , "▐____________/|▌"
    , "▐___________/|_▌"
    , "▐__________/|__▌"
    , "▐_________/|___▌"
    , "▐________/|____▌"
    , "▐_______/|_____▌"
    , "▐______/|______▌"
    , "▐_____/|_______▌"
    , "▐____/|________▌"
    , "▐___/|_________▌"
    , "▐__/|__________▌"
    , "▐_/|___________▌"
    , "▐/|____________▌"
    ]
  }

-- | Spec for grenade spinning indicator:
-- ،
-- ′
--  ´
--  ‾
--   ⸌
--   ⸊
--   |
--   ⁎
--   ⁕
--  ෴
--   ⁓
--
--
--
grenadeSpec :: Spec
grenadeSpec = Spec
  { specName = "grenade"
  , specInterval = 80
  , specFrames =
    [ "،   "
    , "′   "
    , " ´ "
    , " ‾ "
    , "  ⸌"
    , "  ⸊"
    , "  |"
    , "  ⁎"
    , "  ⁕"
    , " ෴ "
    , "  ⁓"
    , "   "
    , "   "
    , "   "
    ]
  }

-- | Spec for point spinning indicator:
-- ∙∙∙
-- ●∙∙
-- ∙●∙
-- ∙∙●
-- ∙∙∙
pointSpec :: Spec
pointSpec = Spec
  { specName = "point"
  , specInterval = 125
  , specFrames =
    [ "∙∙∙"
    , "●∙∙"
    , "∙●∙"
    , "∙∙●"
    , "∙∙∙"
    ]
  }

-- | Spec for layer spinning indicator:
-- -
-- =
-- ≡
layerSpec :: Spec
layerSpec = Spec
  { specName = "layer"
  , specInterval = 150
  , specFrames =
    [ "-"
    , "="
    , "≡"
    ]
  }

