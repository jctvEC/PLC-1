f :: [(t->t->t)] -> [t] -> [(t->t)]
f fs xs = [(a b) | (a,b) <- zip fs xs]
