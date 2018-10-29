{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clay.Tachyons
import Clay

main :: IO ()
main = putCss $ do
  fontFace $ do
    fontStyle normal
    fontWeight $ weight 400
    fontFamily ["IBM Plex Mono"] []
    fontFaceSrc [ FontFaceSrcLocal "/fonts/IBMPlexMono-Regular.woff2" ]
  fontFace $ do
    fontStyle italic
    fontWeight $ weight 400
    fontFamily ["IBM Plex Mono"] []
    fontFaceSrc [ FontFaceSrcLocal "/fonts/IBMPlexMono-Italic.woff2" ]