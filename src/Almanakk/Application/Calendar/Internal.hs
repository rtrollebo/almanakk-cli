module Almanakk.Application.Calendar.Internal (
    utcTimeToDayStr
) where

import Data.Time


utcTimeToDayStr :: UTCTime -> String
utcTimeToDayStr  (UTCTime d _) = case (toGregorian d) of
    (y, m, d) -> show y ++ "-" ++ show m ++ "-" ++ show d