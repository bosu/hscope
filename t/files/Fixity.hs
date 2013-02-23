module HScopeBug where
import Test.QuickCheck ((==>))

epsilon = 0.05
f = (* 1.1)
propInfixBug x = x > 0 ==> f x < epsilon
