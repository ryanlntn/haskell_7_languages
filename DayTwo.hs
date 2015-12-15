module DayTwo where
    import Data.List

    sorted :: Ord a => [a] -> [a]
    sorted []    = []
    sorted (h:t) = sorted lesser ++ [h] ++ sorted greater
      where
        lesser  = filter (< h) t
        greater = filter (>= h) t

    sortedBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
    sortedBy comparator list = sortBy comparator list

    convertStringToNumber s = read (filter p s) :: Float
      where p x = x /= '$' && x /= ','
