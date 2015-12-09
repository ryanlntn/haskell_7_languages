module DayTwo where
    sorted :: Ord a => [a] -> [a]
    sorted []    = []
    sorted (h:t) = sorted lesser ++ [h] ++ sorted greater
      where
        lesser  = filter (< h) t
        greater = filter (>= h) t

