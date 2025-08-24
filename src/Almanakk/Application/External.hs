{-# LANGUAGE CPP #-}
module Almanakk.Application.External
(
    sunRise, 
    sunSet,
    sunRiseDelta,
    sunSetDelta,
    easter,
    solsticeNorthern,
    solsticeSouthern,
    planetPerihelion,
    planetAphelion,
    equinoxNorthward,
    equinoxSouthward,
    lunarPhaseNew,
    lunarPhaseFirstQuarter,
    lunarPhaseFull,
    lunarPhaseLastQuarter,
    lunarPhase
) where 

#ifdef USE_EXTERNAL
import Almanakk.Application.External.Lib
#else
import Almanakk.Application.External.Mock
#endif

