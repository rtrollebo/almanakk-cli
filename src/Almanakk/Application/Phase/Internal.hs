module Almanakk.Application.Phase.Internal (
    LunarPhaseEvent(..),
    LunarPhase (..)
) where


data LunarPhase = LunarWaxingCrescent | LunarWaxingGibbous | LunarWaningGibbous | LunarWaningCrescent deriving (Eq)
data LunarPhaseEvent = NewMoon | FirstQuarterMoon | FullMoon | LastQuarterMoon deriving (Eq)


instance Show LunarPhaseEvent where
    show NewMoon = "New"
    show FirstQuarterMoon = "First quarter"
    show FullMoon= "Full"
    show LastQuarterMoon = "Last quarter"

instance Show LunarPhase where 
    show LunarWaxingCrescent = "Waxing Crescent"
    show LunarWaxingGibbous = "Waxing Gibbous"
    show LunarWaningGibbous = "Waning Gibbous"
    show LunarWaningCrescent = "Waning Crescent"
