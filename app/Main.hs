module Main where

import Formula
import Tseitin
import Dpll
import Debug.Trace
import Criterion.Main

type Node = Int
type Color = Int
type NumOfNodes = Int
type NumOfColors = Int

genEdgeColorPropVar :: Node -> Node -> Color -> Formula
genEdgeColorPropVar i j c = 
    Var $ show i ++ "_" ++ show j ++ "_" ++ show c

triangleOneColorInvariant :: Node -> Node -> Node -> Color -> Formula
triangleOneColorInvariant i j k c =
    Not (And [(genEdgeColorPropVar i j c),
              (genEdgeColorPropVar j k c),
              (genEdgeColorPropVar i k c)])

allTrianglesOneColored :: NumOfNodes -> NumOfColors -> Formula
allTrianglesOneColored nodes colors =
    Or [triangleOneColorInvariant i j k c 
        | i <- [1..nodes-2], j <- [i+1..nodes-1], k <- [j+1..nodes], c <- [1..colors]]

onlyOneColorEdge :: Node -> Node -> NumOfColors -> Color -> Formula
onlyOneColorEdge i j colors usedColor =
    let coloredWithUsedColor = genEdgeColorPropVar i j usedColor
        notColoredWithAnotherColors = [Not $ genEdgeColorPropVar i j c
                | c <- [1..colors], c /= usedColor]
    in And $ coloredWithUsedColor : notColoredWithAnotherColors

allEdgesColored :: NumOfNodes -> NumOfColors -> Formula
allEdgesColored nodes colors =
    let onlyOneColored = [onlyOneColorEdge i j colors usedColor 
            | i <- [1..nodes-1] 
            , j <- [i+1..nodes]
            , usedColor <- [1..colors]]
    in Or onlyOneColored

genCliqueTest :: NumOfNodes -> NumOfColors -> Formula
genCliqueTest nodes colors = And [(allTrianglesOneColored nodes colors), (allEdgesColored nodes colors)]

increaseNodesUntilUnsat :: NumOfNodes -> NumOfColors -> NumOfNodes
increaseNodesUntilUnsat nodes colors = 
    let testFormula = genCliqueTest nodes colors
        res = runDpll $ cnfTseitin testFormula
    in case res of
        Nothing -> nodes - 1
        Just ctx -> if eval ctx testFormula
                    then trace ("SAT " ++ show nodes) $ increaseNodesUntilUnsat (nodes + 1) colors
                    else undefined

main :: IO ()
-- main = defaultMain [
--     bgroup "fully connected graph with " 
--     [bench "3 colors, 12 nodes" $ whnf (runDpll . cnfTseitin . genCliqueTest 12) 3]]
main = putStrLn $ show $ increaseNodesUntilUnsat 3 3
-- main = putStrLn $ show $ length $ cnfTseitin $ genCliqueTest 11 3
