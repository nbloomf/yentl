module Yentl.View.PostScript (
  writePostScript
) where

import Data.List (unwords, unlines)

import Yentl.View.Cartesian
import Yentl.View.Command
import Yentl.View.Options
import Yentl.Algebra



writePostScript :: (ToCartesian t) => ViewOpts -> [Command t] -> String
writePostScript opts xs = concat
  [ "%!PS-Adobe-3.0 EPSF-3.0\n"
  , "%%BoundingBox: 0 0 " ++ show x ++ " " ++ show y ++ "\n"
  , border
  , content
  , "showpage\n"
  ]
  where
    (x,y) = optPageSize opts

    content = concatMap (toPostScript (optViewWindow opts))
                $ toViewWindow' opts xs

    border = unlines
      [ "0.75 setgray"
      , unwords [digits 3 x1, digits 3 y1, "moveto"]
      , unwords [digits 3 x2, digits 3 y1, "lineto"]
      , unwords [digits 3 x2, digits 3 y2, "lineto"]
      , unwords [digits 3 x1, digits 3 y2, "lineto"]
      , "closepath clip stroke"
      , "0 setgray"
      ]
      where
        (x1,y1) = fst $ optViewWindow opts
        (x2,y2) = snd $ optViewWindow opts



toPostScript :: ((Rational, Rational), (Rational, Rational)) -> Cartesian -> String
toPostScript (cLL, cUR) command =
  case makeBox cLL cUR of
    Nothing -> error "toPostScript (bad box)"
    Just win ->
      case command of
        CartesianPoint s (x,y) -> styleO ++ unwords
          [ digits 3 x, digits 3 y, "2 0 360 arc fill\n"
          ] ++ styleC
          where
            (styleO, styleC) = stylePostScript (s {pattern = Just Solid})


        CartesianLine s p q -> case solve2var2sol (form, win) of
          Only2of2 (hx,hy) (kx,ky) ->
            styleO ++ unlines
              [ unwords [digits 3 hx, digits 3 hy, "moveto"]
              , unwords [digits 3 kx, digits 3 ky, "lineto"]
              , "stroke"
              ] ++ styleC
          _ -> ""
          where
            Just form = lfPointPoint p q
            (styleO, styleC) = stylePostScript s


        CartesianSegment s (px,py) (qx,qy) -> styleO ++ unlines
          [ unwords [digits 3 px, digits 3 py, "moveto"]
          , unwords [digits 3 qx, digits 3 qy, "lineto"]
          , "stroke"
          ] ++ styleC
          where
            (styleO, styleC) = stylePostScript s


        CartesianRay s o@(ox,oy) a -> case solve2var2sol (ray, win) of
          Only2of2 (hx,hy) (kx,ky) ->
            styleO ++ unlines
              [ unwords [digits 3 hx, digits 3 hy, "moveto"]
              , unwords [digits 3 kx, digits 3 ky, "lineto"]
              , "stroke"
              ] ++ styleC
          Only1of2 (hx,hy) -> if isInsideBox o win
            then
              styleO ++ unlines
                [ unwords [digits 3 ox, digits 3 oy, "moveto"]
                , unwords [digits 3 hx, digits 3 hy, "lineto"]
                , "stroke"
                ] ++ styleC
            else ""
          _ -> ""
          where
            ray = reVertexPoint o a
            (styleO, styleC) = stylePostScript s


        CartesianCircle s (ox,oy) r -> styleO ++ unwords
          [ digits 3 ox, digits 3 oy, digits 3 r, "0 360 arc closepath stroke\n"
          ] ++ styleC
          where
            (styleO, styleC) = stylePostScript s


        CartesianArc s (ox,oy) r a b -> styleO ++ unwords
          [ digits 3 ox, digits 3 oy, digits 3 r, digits 3 a, digits 3 b, "arc stroke\n"
          ] ++ styleC
          where
            (styleO, styleC) = stylePostScript s



stylePostScript :: Style -> (String, String)
stylePostScript sty =
  ( colorO ++ widthO ++ patternO
  , patternC ++ widthC ++ colorC
  )
  where
    (patternO, patternC) = case pattern sty of
      Nothing    -> ("", "")
      Just Solid -> ("","")
      Just Dash  -> ("[3] 0 setdash\n","[] 0 setdash\n")

    (widthO, widthC) = case width sty of
      Nothing         -> ("","")
      Just UltraThin  -> ("0.1 setlinewidth\n","1 setlinewidth\n")
      Just VeryThin   -> ("0.3 setlinewidth\n","1 setlinewidth\n")
      Just Thin       -> ("0.6 setlinewidth\n","1 setlinewidth\n")
      Just SemiThick  -> ("","")
      Just Thick      -> ("1.5 setlinewidth\n","1 setlinewidth\n")
      Just VeryThick  -> ("2 setlinewidth\n","1 setlinewidth\n")
      Just UltraThick -> ("2.5 setlinewidth\n","1 setlinewidth\n")

    (colorO, colorC) = case color sty of
      Nothing    -> ("","")
      Just Red   -> ("1 0 0 setrgbcolor\n","0 0 0 setrgbcolor\n")
      Just Blue  -> ("0 0 1 setrgbcolor\n","0 0 0 setrgbcolor\n")
      Just Green -> ("0 1 0 setrgbcolor\n","0 0 0 setrgbcolor\n")
