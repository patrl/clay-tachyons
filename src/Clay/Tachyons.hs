{-# LANGUAGE OverloadedStrings #-}

module Clay.Tachyons where

import           Clay                    hiding ( map )
import qualified Clay.Media                    as Mq
import           Prelude                 hiding ( rem
                                                , div
                                                , span
                                                )
import           Control.Applicative

-------------------------
-- background position --
-------------------------

bgCenter, bgTop, bgRight, bgBottom, bgLeft :: Css
[bgCenter, bgTop, bgRight, bgBottom, bgLeft] = map
  ((<>) (backgroundRepeat noRepeat) . backgroundPosition . uncurry placed)
  [(sideCenter, sideCenter), (sideTop, sideCenter), (sideCenter, sideLeft)]

---------------------
-- background size --
---------------------

cover, contain :: Css
[cover, contain] = map (important . backgroundSize) [Clay.cover, Clay.contain]

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
  map textAlign $ (alignSide <$> [sideLeft, sideRight]) <> [center, justify]

link :: Css
link = textDecoration none

------------
-- colors --
------------

-- >>> :t uncurry
-- uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry2 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry2 f (x,y,z) = f x y z

uncurry3 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry3 f (x1, x2, x3, x4) = f x1 x2 x3 x4
--
-- uncurry rgb :: (Integer, Integer) -> Integer -> Color
-- >>> :t uncurry rgb
-- uncurry rgb :: (Integer, Integer) -> Integer -> Color

black, nearBlack, darkGray, midGray, gray, silver, lightSilver, moonGray, nearWhite, white, transparent, black90, black80, black70, black60, black50, black40, black30, black20, black10, black05, black025, black0125, white90, white80, white70, white60, white50, white40, white30, white20, white10, white05, white025, white0125
  :: Color
[black, nearBlack, darkGray, midGray, gray, silver, lightSilver, moonGray, nearWhite, white, transparent, black90, black80, black70, black60, black50, black40, black30, black20, black10, black05, black025, black0125, white90, white80, white70, white60, white50, white40, white30, white20, white10, white05, white025, white0125]
  = map
      (uncurry2 rgb)
      [ (0  , 0  , 0)
      , (1  , 1  , 1)
      , (3  , 3  , 3)
      , (5  , 5  , 5)
      , (7  , 7  , 7)
      , (9  , 9  , 9)
      , (170, 170, 170)
      , (204, 204, 204)
      , (244, 244, 244)
      , (255, 255, 255)
      ]
    ++ [Clay.transparent]
    ++ map
         (uncurry3 rgba)
         [ (0  , 0  , 0  , 0.9)
         , (0  , 0  , 0  , 0.8)
         , (0  , 0  , 0  , 0.7)
         , (0  , 0  , 0  , 0.6)
         , (0  , 0  , 0  , 0.5)
         , (0  , 0  , 0  , 0.4)
         , (0  , 0  , 0  , 0.3)
         , (0  , 0  , 0  , 0.2)
         , (0  , 0  , 0  , 0.1)
         , (0  , 0  , 0  , 0.05)
         , (0  , 0  , 0  , 0.025)
         , (0  , 0  , 0  , 0.0125)
         , (255, 255, 255, 0.9)
         , (255, 255, 255, 0.8)
         , (255, 255, 255, 0.7)
         , (255, 255, 255, 0.6)
         , (255, 255, 255, 0.5)
         , (255, 255, 255, 0.4)
         , (255, 255, 255, 0.3)
         , (255, 255, 255, 0.2)
         , (255, 255, 255, 0.1)
         , (255, 255, 255, 0.05)
         , (255, 255, 255, 0.025)
         , (255, 255, 255, 0.0125)
         ]

-------------
-- spacing --
-------------

spacingNone, spacingXS, spacingS, spacingM, spacingL, spacingXL, spacingXXL, spacingXXXL
  :: Size LengthUnit
[spacingNone, spacingXS, spacingS, spacingM, spacingL, spacingXL, spacingXXL, spacingXXXL]
  = map rem [0, 0.25, 0.5, 1.0, 2.0, 4.0, 8.0, 16.0]

-- TODO PR for clay adding an Enum instance for LengthUnit.
spacings :: [Size LengthUnit]
spacings =
  [ spacingNone
  , spacingXS
  , spacingS
  , spacingM
  , spacingL
  , spacingXL
  , spacingXXL
  , spacingXXXL
  ]

pa0, pa1, pa2, pa3, pa4, pa5, pa6, pa7 :: Css
[pa0, pa1, pa2, pa3, pa4, pa5, pa6, pa7] =
  map (uncurry3 padding . (\x -> (x, x, x, x))) spacings

pl0, pl1, pl2, pl3, pl4, pl5, pl6, pl7 :: Css
[pl0, pl1, pl2, pl3, pl4, pl5, pl6, pl7] = map paddingLeft spacings

pr0, pr1, pr2, pr3, pr4, pr5, pr6, pr7 :: Css
[pr0, pr1, pr2, pr3, pr4, pr5, pr6, pr7] = map paddingRight spacings

pb0, pb1, pb2, pb3, pb4, pb5, pb6, pb7 :: Css
[pb0, pb1, pb2, pb3, pb4, pb5, pb6, pb7] = map paddingBottom spacings

pt0, pt1, pt2, pt3, pt4, pt5, pt6, pt7 :: Css
[pt0, pt1, pt2, pt3, pt4, pt5, pt6, pt7] = map paddingTop spacings

pv0, pv1, pv2, pv3, pv4, pv5, pv6, pv7 :: Css
[pv0, pv1, pv2, pv3, pv4, pv5, pv6, pv7] =
  liftA2 mappend (map paddingTop spacings) (map paddingBottom spacings)

ph0, ph1, ph2, ph3, ph4, ph5, ph6, ph7 :: Css
[ph0, ph1, ph2, ph3, ph4, ph5, ph6, ph7] =
  liftA2 mappend (map paddingLeft spacings) (map paddingRight spacings)

ma0, ma1, ma2, ma3, ma4, ma5, ma6, ma7 :: Css
[ma0, ma1, ma2, ma3, ma4, ma5, ma6, ma7] =
  map (uncurry3 margin . (\x -> (x, x, x, x))) spacings

mr0, mr1, mr2, mr3, mr4, mr5, mr6, mr7 :: Css
[mr0, mr1, mr2, mr3, mr4, mr5, mr6, mr7] = map marginRight spacings

mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7 :: Css
[mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7] = map marginBottom spacings

mt0, mt1, mt2, mt3, mt4, mt5, mt6, mt7 :: Css
[mt0, mt1, mt2, mt3, mt4, mt5, mt6, mt7] = map marginTop spacings

mv0, mv1, mv2, mv3, mv4, mv5, mv6, mv7 :: Css
[mv0, mv1, mv2, mv3, mv4, mv5, mv6, mv7] =
  liftA2 mappend (map marginTop spacings) (map marginBottom spacings)

mh0, mh1, mh2, mh3, mh4, mh5, mh6, mh7 :: Css
[mh0, mh1, mh2, mh3, mh4, mh5, mh6, mh7] =
  liftA2 mappend (map marginLeft spacings) (map marginRight spacings)

------------
-- floats --
------------

fl, fr, fn :: Css
[fl, fr, fn] =
  map (mappend (display inline)) [float floatLeft, float floatRight]
    ++ [float none]

------------
-- clears --
------------

cl, cr, cb, cn :: Css
[cl, cr, cb, cn] = map clear [clearLeft, clearRight, both, none]

----------
-- code --
----------

pre :: Css
pre = overflow scroll <> overflowX auto <> overflowY hidden

-------------
-- display --
-------------

dn, di, db, dib, dit, dt, dtc, dtRow, dtRowGroup, dtColumn, dtColumnGroup :: Css
[dn, di, db, dib, dit, dt, dtc, dtRow, dtRowGroup, dtColumn, dtColumnGroup] =
  map
    display
    [ none
    , inline
    , block
    , inlineBlock
    , inlineTable
    , displayTable
    , tableCell
    , tableRow
    , tableRowGroup
    , tableColumn
    , tableColumnGroup
    ]

-- TODO dtFixed

-----------------
-- font-family --
-----------------

_sansSerif :: Css
_sansSerif = fontFamily
  [ "avenir next"
  , "avenir"
  , "helvetica"
  , "helvetica neue"
  , "ubuntu"
  , "roboto"
  , "noto"
  , "segoe ui"
  , "arial"
  ]
  [sansSerif]

_serif :: Css
_serif = georgia

sysSans, sysSerif :: Css
[sysSans, sysSerif] = fontFamily [] <$> [[sansSerif], [serif]]

code, courier :: Css
[code, courier] =
  (fontFamily <$> [["Consolas", "Monaco"], ["Courier Next", "courier"]])
    <*> pure [monospace]

helvetica, avenir :: Css
[helvetica, avenir] =
  (fontFamily <$> [["helvetica neue", "helvetica"], ["avenir next", "avenir"]])
    <*> pure [sansSerif]

athelas, georgia, times, bodoni, calisto, garamond, baskerville :: Css
[athelas, georgia, times, bodoni, calisto, garamond, baskerville] =
  (   fontFamily
    <$> [ ["athelas", "georgia"]
        , ["georgia"]
        , ["times"]
        , ["Bodoni MT"]
        , ["Calisto MT"]
        , ["garamond"]
        , ["baskerville"]
        ]
    )
    <*> pure [serif]

----------------
-- font-style --
----------------

_i, _fsNormal :: Css
[_i, _fsNormal] = fontStyle <$> [italic, normal]

-----------------
-- font weight --
-----------------

_normal, _b, fw1, fw2, fw3, fw4, fw5, fw6, fw7, fw8, fw9 :: Css
[_normal, _b, fw1, fw2, fw3, fw4, fw5, fw6, fw7, fw8, fw9] =
  fontWeight
    <$> (  [normal, bold]
        <> (weight <$> [100, 200, 300, 400, 500, 600, 700, 800, 900])
        )

------------
-- widths --
------------

widths :: [Size LengthUnit]
widths = rem <$> [1, 2, 4, 8, 16]

w1, w2, w3, w4, w5 :: Css
[w1, w2, w3, w4, w5] = width <$> widths

w10, w20, w25, w30, w33, w34, w40, w50, w60, w70, w75, w80, w90, w100 :: Css
[w10, w20, w25, w30, w33, w34, w40, w50, w60, w70, w75, w80, w90, w100] =
  (width :: Size Percentage -> Css)
    <$> [10, 20, 25, 30, 33, 34, 40, 50, 60, 70, 75, 80, 90, 100]

wThird :: Css
wThird = width $ (100 :: Size Percentage) @/ 3

wTwoThirds :: Css
wTwoThirds = width $ (100 :: Size Percentage) @/ 1.5

wAuto :: Css
wAuto = width auto

----------------
-- max widths --
----------------

mw100 :: Css
mw100 = maxWidth (100 :: Size Percentage)

mw1, mw2, mw3, mw4, mw5, mw6, mw7, mw8, mw9 :: Css
[mw1, mw2, mw3, mw4, mw5, mw6, mw7, mw8, mw9] =
  (maxWidth <$> widths) <> (maxWidth . rem <$> [32, 48, 64, 96])

mwNone :: Css
mwNone = maxWidth none
