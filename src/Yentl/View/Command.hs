{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Yentl.View.Command (
  Command(..), Style(..), Draw, draw, (<%), (<&), plain,

  Color(..), Pattern(..), Width(..), Opacity(..),

  ultrathin, verythin, thin, thick, verythick, ultrathick,
  dashed, red, blue, green
) where



data Command t
  = DrawPoint   Style t
  | DrawLine    Style t t
  | DrawSegment Style t t
  | DrawRay     Style t t
  | DrawCircle  Style t t
  deriving Show

class Draw t a where
  draw :: a -> [Command t]

instance (Draw t a, Draw t b) => Draw t (a,b) where
  draw (x,y) = draw x ++ draw y



data Style = Style
  { color   :: Maybe Color
  , opacity :: Maybe Opacity
  , width   :: Maybe Width
  , pattern :: Maybe Pattern
  } deriving Show

plain :: Style
plain = Style
  { color   = Nothing
  , opacity = Nothing
  , width   = Nothing
  , pattern = Nothing
  }



data Color
  = Red | Blue | Green | DarkGray | Gray | LightGray | Black
  deriving (Eq, Show)

data Opacity = Opacity deriving Show

data Width
  = UltraThin | VeryThin  | Thin       | SemiThick
  | Thick     | VeryThick | UltraThick
  deriving (Eq, Show)

data Pattern
  = Solid | Dash
  deriving (Eq, Show)



(<%) :: [Command t] -> Style -> [Command t]
xs <% sty = map foo xs
  where
    foo command = case command of
      DrawPoint s p -> DrawPoint (s <& sty) p
      DrawLine s p q -> DrawLine (s <& sty) p q
      DrawSegment s p q -> DrawSegment (s <& sty) p q
      DrawRay s p q -> DrawRay (s <& sty) p q
      DrawCircle s p q -> DrawCircle (s <& sty) p q

(<&) :: Style -> Style -> Style
s <& sty = Style
  { color   = case color sty of Nothing -> color s; Just x -> Just x
  , opacity = case opacity sty of Nothing -> opacity s; Just x -> Just x
  , width   = case width sty of Nothing -> width s; Just x -> Just x
  , pattern = case pattern sty of Nothing -> pattern s; Just x -> Just x
  }



dashed :: Style
dashed = plain { pattern = Just Dash }

ultrathin :: Style
ultrathin = plain { width = Just UltraThin }

verythin :: Style
verythin = plain { width = Just VeryThin }

thin :: Style
thin = plain { width = Just Thin }

thick :: Style
thick = plain { width = Just Thick }

verythick :: Style
verythick = plain { width = Just VeryThick }

ultrathick :: Style
ultrathick = plain { width = Just UltraThick }

red :: Style
red = plain { color = Just Red }

blue :: Style
blue = plain { color = Just Blue }

green :: Style
green = plain { color = Just Green }
