module ReportIP where

import IPTypes
import LookupIP

reportIPs :: IPRangeDB -> [IP] -> String
reportIPs iprdb = unlines . map go
  where
    go ip = show ip ++ ": " ++ yesno (lookupIP iprdb ip)
    yesno True = "YES"
    yesno False = "NO"
