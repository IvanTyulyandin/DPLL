module Main where

import Formula
import Tseitin
import Dpll
import Debug.Trace
import Criterion.Main
import Data.HashSet as Set

genEdgeColorPropVar :: Int -> Int -> Int -> Formula
genEdgeColorPropVar i j c = 
    Var $ show i ++ "_" ++ show j ++ "_" ++ show c

triangleOneColorInvariant :: Int -> Int -> Int -> Int -> Formula
triangleOneColorInvariant i j k c =
    Not (And (genEdgeColorPropVar i j c)
             (And (genEdgeColorPropVar j k c)
                  (genEdgeColorPropVar i k c)))

allTrianglesOneColored :: Int -> Int -> Formula
allTrianglesOneColored nodes colors =
    foldl1 Or [triangleOneColorInvariant i j k c 
            | i <- [1..nodes-2], j <- [i+1..nodes-1], k <- [j+1..nodes], c <- [1..colors]]

onlyOneColorEdge :: Int -> Int -> Int -> Int -> Formula
onlyOneColorEdge i j colors usedColor =
    let colored = genEdgeColorPropVar i j usedColor
        notColored = [Not $ genEdgeColorPropVar i j c | c <- [1..colors], c /= usedColor]
    in foldl1 And $ colored : notColored

allEdgesColored :: Int -> Int -> Formula
allEdgesColored nodes colors =
    let onlyOneColored = [onlyOneColorEdge i j colors usedColor 
            | i <- [1..nodes-1] 
            , j <- [i+1..nodes]
            , usedColor <- [1..colors]]
    in foldl1 Or onlyOneColored

genCliqueTest :: Int -> Int -> Formula
genCliqueTest nodes colors = And (allTrianglesOneColored nodes colors) (allEdgesColored nodes colors)

increaseNodesUntilUnsat :: Int -> Int -> Int 
increaseNodesUntilUnsat nodes colors = 
    let testFormula = genCliqueTest nodes colors
        res = runDpll $ cnfTseitin testFormula
    in case res of
        Nothing -> nodes - 1
        Just ctx -> if eval testFormula ctx
                    then trace ("SAT " ++ show nodes) $ increaseNodesUntilUnsat (nodes + 1) colors
                    else undefined

main :: IO ()
main = defaultMain [
    bgroup "fully connected graph with " 
    [bench "3 colors, 12 nodes" $ whnf (runDpll . cnfTseitin . genCliqueTest 12) 3]]
-- main = putStrLn $ show $ increaseNodesUntilUnsat 3 3
-- main = putStrLn $ show $ Set.size $ cnfTseitin $ genCliqueTest 11 3
