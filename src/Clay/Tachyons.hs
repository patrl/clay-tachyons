{-# LANGUAGE OverloadedStrings #-}

module Clay.Tachyons where

import           Clay                    hiding ( map )
import qualified Clay.Media                    as Mq
import           Prelude                 hiding ( rem
                                                , div
                                                , span
                                                )

-------------------------
-- background position --
-------------------------

bgCenter, bgTop, bgRight, bgBottom, bgLeft :: Css
[bgCenter, bgTop, bgRight, bgBottom, bgLeft] = map
  ((<>) (backgroundRepeat noRepeat) . backgroundPosition)
  [ placed sideCenter sideCenter
  , placed sideTop sideCenter
  , placed sideCenter sideRight
  , placed sideBottom sideCenter
  , placed sideCenter sideLeft
  ]

---------------------
-- background size --
---------------------

cover,contain :: Css
[cover,contain] = map (important . backgroundSize) [ Clay.cover, Clay.contain ]

------------------
-- break-points --
------------------

ns, m, l :: Css -> Css
[ns, m, l] = map
  (query Mq.screen)
  [ [Mq.minWidth $ em 30]
  , [Mq.minWidth $ em 30, Mq.maxWidth $ em 60]
  , [Mq.minWidth $ em 60]
  ]

----------------
-- type scale --
----------------

fH, fSubH, f1, f2, f3, f4, f5, f6, f7 :: Css
[fH, fSubH, f1, f2, f3, f4, f5, f6, f7] =
  map (fontSize . rem) [6.0, 5.0, 3.0, 2.25, 1.5, 1.25, 1.0, 0.875, 0.75]

----------------
-- typography --
----------------

measure, measureWide, measureNarrow :: Css
[measure, measureWide, measureNarrow] = map (maxWidth . em) [30, 34, 20]

indent :: Css
indent =
  (textIndent . Clay.indent . em) 1
    <> (marginTop . unitless) 0
    <> (marginBottom . unitless) 0

smallCaps :: Css
smallCaps = fontVariant Clay.smallCaps

truncate :: Css
truncate =
  whiteSpace nowrap <> overflow hidden <> textOverflow overflowEllipsis

lhCopy, lhTitle, lhSolid :: Css
[lhCopy, lhTitle, lhSolid] = map (lineHeight . unitless) [1.5, 1.25, 1.0]

--------------------
-- text transform --
--------------------

ttc, ttl, ttu, ttn :: Css
[ttc, ttl, ttu, ttn] =
  map textTransform [capitalize, lowercase, uppercase, none]

---------------------
-- text decoration --
---------------------

strike, underline, noUnderline :: Css
[strike, underline, noUnderline] =
  map textDecoration [lineThrough, Clay.underline, none]

----------------
-- text align --
----------------

tl, tr, tc, tj :: Css
[tl, tr, tc, tj] =
  map textAlign $ (map alignSide [sideLeft, sideRight]) <> [center, justify]

link :: Css
link = textDecoration none

------------
-- colors --
------------

black :: Color
black = map rgb [(0 0 0)]