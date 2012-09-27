module Euler.List where

window _ [] = []
window n xs = take n xs : (window n (tail xs))
