{-# LANGUAGE OverloadedStrings #-}

module Clay.Tachyons where

import           Clay                    hiding ( map )
import qualified Clay.Media                    as Mq
import           Prelude                 hiding ( rem
                                                , div
                                                , span
                                                )
import           Control.Applicative

------------------
-- break-points --
------------------

-- break points can be combined with any other css via application
ns, m, l :: Css -> Css
[ns, m, l] =
  query Mq.screen
    <$> [ [Mq.minWidth $ em 30]
        , [Mq.minWidth $ em 30, Mq.maxWidth $ em 60]
        , [Mq.minWidth $ em 60]
        ]

-------------------------
-- background position --
-------------------------

bgCenter, bgTop, bgRight, bgBottom, bgLeft :: Css
[bgCenter, bgTop, bgRight, bgBottom, bgLeft] =
  ((<>) (backgroundRepeat noRepeat) . backgroundPosition . uncurry placed)
    <$> [ (sideCenter, sideCenter)
        , (sideTop   , sideCenter)
        , (sideCenter, sideLeft)
        ]

---------------------
-- background size --
---------------------

_cover, _contain :: Css
[_cover, _contain] = important . backgroundSize <$> [Clay.cover, Clay.contain]

_backgroundSize :: Css
_backgroundSize = (".cover" ? _cover) <> (".contain" ? _contain)


----------------
-- box sizing --
----------------

-- TODO add input selections

_borderBox :: Css
_borderBox =
  mconcat
      [ html
      , body
      , div
      , article
      , section
      , main_
      , footer
      , header
      , form
      , fieldset
      , legend
      , Clay.pre
      , Clay.code
      , a
      , h1
      , h2
      , h3
      , h4
      , h5
      , h6
      , p
      , ul
      , ol
      , li
      , dl
      , Clay.dt
      , dd
      , textarea
      , table
      , td
      , th
      , Clay.tr
      , ".border-box"
      ]
    ? boxSizing borderBox

-- type scale --
----------------

fH, fSubH, f1, f2, f3, f4, f5, f6, f7 :: Css
[fH, fSubH, f1, f2, f3, f4, f5, f6, f7] =
  fontSize . rem <$> [6.0, 5.0, 3.0, 2.25, 1.5, 1.25, 1.0, 0.875, 0.75]

----------------
-- typography --
----------------

measure, measureWide, measureNarrow :: Css
[measure, measureWide, measureNarrow] = maxWidth . em <$> [30, 34, 20]

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
[lhCopy, lhTitle, lhSolid] = lineHeight . unitless <$> [1.5, 1.25, 1.0]

--------------------
-- text transform --
--------------------

ttc, ttl, ttu, ttn :: Css
[ttc, ttl, ttu, ttn] =
  textTransform <$> [capitalize, lowercase, uppercase, none]

---------------------
-- text decoration --
---------------------

strike, underline, noUnderline :: Css
[strike, underline, noUnderline] =
  textDecoration <$> [lineThrough, Clay.underline, none]

----------------
-- text align --
----------------

tl, tr, tc, tj :: Css
[tl, tr, tc, tj] =
  textAlign <$> (alignSide <$> [sideLeft, sideRight]) <> [center, justify]

-----------
-- links --
-----------

_link :: Css
_link = textDecoration none

_links :: Css
_links = ".link" ? _link

------------
-- colors --
------------

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
  = rem <$> [0, 0.25, 0.5, 1.0, 2.0, 4.0, 8.0, 16.0]

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
  uncurry3 padding . (\x -> (x, x, x, x)) <$> spacings

pl0, pl1, pl2, pl3, pl4, pl5, pl6, pl7 :: Css
[pl0, pl1, pl2, pl3, pl4, pl5, pl6, pl7] = paddingLeft <$> spacings

pr0, pr1, pr2, pr3, pr4, pr5, pr6, pr7 :: Css
[pr0, pr1, pr2, pr3, pr4, pr5, pr6, pr7] = paddingRight <$> spacings

pb0, pb1, pb2, pb3, pb4, pb5, pb6, pb7 :: Css
[pb0, pb1, pb2, pb3, pb4, pb5, pb6, pb7] = paddingBottom <$> spacings

pt0, pt1, pt2, pt3, pt4, pt5, pt6, pt7 :: Css
[pt0, pt1, pt2, pt3, pt4, pt5, pt6, pt7] = paddingTop <$> spacings

pv0, pv1, pv2, pv3, pv4, pv5, pv6, pv7 :: Css
[pv0, pv1, pv2, pv3, pv4, pv5, pv6, pv7] =
  liftA2 mappend (paddingTop <$> spacings) (paddingBottom <$> spacings)

ph0, ph1, ph2, ph3, ph4, ph5, ph6, ph7 :: Css
[ph0, ph1, ph2, ph3, ph4, ph5, ph6, ph7] =
  liftA2 mappend (paddingLeft <$> spacings) (paddingRight <$> spacings)

ma0, ma1, ma2, ma3, ma4, ma5, ma6, ma7 :: Css
[ma0, ma1, ma2, ma3, ma4, ma5, ma6, ma7] =
  uncurry3 margin . (\x -> (x, x, x, x)) <$> spacings

mr0, mr1, mr2, mr3, mr4, mr5, mr6, mr7 :: Css
[mr0, mr1, mr2, mr3, mr4, mr5, mr6, mr7] = marginRight <$> spacings

mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7 :: Css
[mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7] = marginBottom <$> spacings

mt0, mt1, mt2, mt3, mt4, mt5, mt6, mt7 :: Css
[mt0, mt1, mt2, mt3, mt4, mt5, mt6, mt7] = marginTop <$> spacings

mv0, mv1, mv2, mv3, mv4, mv5, mv6, mv7 :: Css
[mv0, mv1, mv2, mv3, mv4, mv5, mv6, mv7] =
  liftA2 mappend (marginTop <$> spacings) (marginBottom <$> spacings)

mh0, mh1, mh2, mh3, mh4, mh5, mh6, mh7 :: Css
[mh0, mh1, mh2, mh3, mh4, mh5, mh6, mh7] =
  liftA2 mappend (marginLeft <$> spacings) (marginRight <$> spacings)

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

_widths :: [Size LengthUnit]
_widths = rem <$> [1, 2, 4, 8, 16]

w1, w2, w3, w4, w5 :: Css
[w1, w2, w3, w4, w5] = width <$> _widths

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

-------------
-- heights --
-------------

_heights :: [Size LengthUnit]
_heights = rem <$> [1, 2, 4, 8, 16]

_h1, _h2, _h3, _h4, _h5 :: Css
[_h1, _h2, _h3, _h4, _h5] = height <$> _heights

_h25, _h50, _h75, _h100 :: Css
[_h25, _h50, _h75, _h100] =
  height <$> ([25, 50, 75, 100] :: [Size Percentage])

_minh100 :: Css
_minh100 = minHeight (100 :: Size Percentage)

-- TODO finish this off

----------------
-- max widths --
----------------

mw100 :: Css
mw100 = maxWidth (100 :: Size Percentage)

mw1, mw2, mw3, mw4, mw5, mw6, mw7, mw8, mw9 :: Css
[mw1, mw2, mw3, mw4, mw5, mw6, mw7, mw8, mw9] =
  (maxWidth <$> _widths) <> (maxWidth . rem <$> [32, 48, 64, 96])

mwNone :: Css
mwNone = maxWidth none

-------------
-- borders --
-------------

_ba, _bt, _br, _bb, _bl, _bn :: Css
[_ba, _bt, _br, _bb, _bl, _bn] =
  [borderStyle solid <> borderWidth (px 1)]
    <> [borderTopStyle solid <> borderTopWidth (px 1)]
    <> [borderRightStyle solid <> borderRightWidth (px 1)]
    <> [borderBottomStyle solid <> borderBottomWidth (px 1)]
    <> [borderLeftStyle solid <> borderLeftWidth (px 1)]
    <> [borderStyle none <> borderWidth 0]

------------------
-- border-style --
------------------

_bDotted, _bDashed, _bSolid, _bNone :: Css
[_bDotted, _bDashed, _bSolid, _bNone] =
  borderStyle <$> [dotted, dashed, solid, none]

-------------------
-- border widths --
-------------------

_bw0, _bw1, _bw2, _bw3, _bw4, _bw5 :: Css
[_bw0, _bw1, _bw2, _bw3, _bw4, _bw5] =
  borderWidth <$> ([none] <> _borderWidths)

_borderWidths :: [Size LengthUnit]
_borderWidths = rem <$> [0.125, 0.25, 0.5, 1.0, 2.2]

-- resets
_bt0, _br0, _bb0, _bl0 :: Css
[_bt0, _br0, _bb0, _bl0] =
  [borderTopWidth, borderRightWidth, borderBottomWidth, borderLeftWidth]
    <*> [none]

-------------------
-- border radius --
-------------------

_borderRadii :: [Size LengthUnit]
_borderRadii = rem <$> [0.125, 0.25, 0.5, 1.0, 2.0]

_brad0, _brad1, _brad2, _brad3, _brad4 :: Css
[_brad0, _brad1, _brad2, _brad3, _brad4] =
  uncurry3 borderRadius
    <$> ([(\x -> (x, x, x, x)) none] <> ((\x -> (x, x, x, x)) <$> _borderRadii))

_bradCircle :: Css
_bradCircle =
  uncurry3 borderRadius ((\x -> (x, x, x, x)) (100 :: Size Percentage))

_bradPill :: Css
_bradPill = uncurry3 borderRadius ((\x -> (x, x, x, x)) (px 9999))

----------------------
-- helper functions --
----------------------

uncurry2 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry2 f (x, y, z) = f x y z

uncurry3 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry3 f (x1, x2, x3, x4) = f x1 x2 x3 x4
