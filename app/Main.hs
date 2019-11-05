{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Formula
import Tseitin
import Dpll
import Debug.Trace
import Criterion.Main

genEdgeColorPropVar :: (NameRepr a) => Node -> Node -> Color -> Formula a
genEdgeColorPropVar i j c = Var $ makeRepr i j c

triangleOneColorInvariant :: (NameRepr a) => Node -> Node -> Node -> Color -> Formula a
triangleOneColorInvariant i j k c =
    Not (And [(genEdgeColorPropVar i j c),
              (genEdgeColorPropVar j k c),
              (genEdgeColorPropVar i k c)])

allTrianglesInvariant :: (NameRepr a) => NumOfNodes -> NumOfColors -> Formula a
allTrianglesInvariant nodes colors =
    And [triangleOneColorInvariant i j k c
        | i <- [1..nodes-2], j <- [i+1..nodes-1], k <- [j+1..nodes], c <- [1..colors]]

onlyOneColorEdge :: (NameRepr a) => Node -> Node -> NumOfColors -> Color -> Formula a
onlyOneColorEdge i j colors usedColor =
    let coloredWithUsedColor = genEdgeColorPropVar i j usedColor
        notColoredWithAnotherColors = [Not $ genEdgeColorPropVar i j c
                | c <- [1..colors], c /= usedColor]
    in And $ coloredWithUsedColor : notColoredWithAnotherColors

oneEdgeAllColors :: (NameRepr a) => Node -> Node -> NumOfColors -> Formula a
oneEdgeAllColors i j colors =
    Or [onlyOneColorEdge i j colors usedColor
        | usedColor <- [1..colors]]

allEdgesColored :: (NameRepr a) => NumOfNodes -> NumOfColors -> Formula a
allEdgesColored nodes colors =
    And [oneEdgeAllColors i j colors | i <- [1..nodes-1], j <- [i+1..nodes]]

genCliqueTest :: (NameRepr a) => NumOfNodes -> NumOfColors -> Formula a
genCliqueTest nodes colors = And [(allTrianglesInvariant nodes colors), (allEdgesColored nodes colors)]

increaseNodesUntilUnsat :: NumOfNodes -> NumOfColors -> NumOfNodes
increaseNodesUntilUnsat nodes colors = 
    let testFormula = (genCliqueTest nodes colors) :: Formula Int
        res = runDpll $ cnfTseitin testFormula
    in case res of
        Nothing -> nodes - 1
        Just ctx -> if eval ctx testFormula
                    then trace ("SAT " ++ show nodes) $ increaseNodesUntilUnsat (nodes + 1) colors
                    else error "SAT, but eval gave false"

main :: IO ()
main = defaultMain [
    bgroup "fully connected graph with "
    [bench "3 colors, 10 nodes" $ whnf (runDpll . cnfTseitin . (genCliqueTest 10 :: Int -> Formula Int)) 3]]
-- main = putStrLn $ show $ increaseNodesUntilUnsat 3 3
-- main =
--     let resCtx = runDpll $ cnfTseitin ((genCliqueTest 6 3) :: Formula Int)
--     in case resCtx of
--         Nothing -> print "Nothing"
--         Just ctx -> print $ eval ctx ((genCliqueTest 6 3) :: Formula Int)
-- main = print $ ((genCliqueTest 3 2) :: Formula Int)
