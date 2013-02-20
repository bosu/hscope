data G a where
    A :: a -> G Int
    B :: b -> G Bool

g :: Int -> Int
g proc = proc + 1
