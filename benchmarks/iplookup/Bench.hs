import Criterion.Main

--- import BenchBuildIP
import BenchBuildIPGroups
import BenchParseIP
import BenchRanges
import BenchLookupIP

main :: IO ()
main = defaultMain benchmarks
  where
    benchmarks = bench_buildIP
                 <> bench_parseIP
                 <> bench_ranges
                 <> bench_lookupIP
