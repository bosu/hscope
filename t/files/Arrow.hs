module Main
where

import Control.Arrow

doSth = (+ 1) . head &&& tail
