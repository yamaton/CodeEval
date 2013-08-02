hanoi n = solve n 1 2 3

solve 0 _ _ _ = []
solve n from help to = (solve (n-1) from to help) ++ [(from,to)] ++ (solve (n-1) help from to)