module Bsp

open Model

let random = new System.Random()

let terrainRates = [
    (0.1, Mountains, StonyField)
    (0.1, StonyField, Plains)
    (0.1, Plains, Forest)
    (0.1, Wetlands, Forest)
    (0.3, Forest, DeepForest)
    (0.3, DeepForest, Forest)
]

let range = 
    let maxInt = System.Int32.MaxValue
    List.fold (fun (mix, miy, mox, moy) (x,y) -> 
        (min mix x, min miy y, max mox x, max moy y)) 
        (maxInt, maxInt, 0, 0)

let mid (mix, miy, mox, moy) =
    (mox - mix / 2, moy - miy / 2)

let rec bspDivide minSize list = 
    let minRange = minSize * 2
    let minx, miny, maxx, maxy = range list
    let cantx, canty = maxx - minx < minRange, maxy - miny < minRange
    if cantx && canty then 
        [list]
    else
        let xdivide () =
            let mid = random.Next(minx + minSize, maxx - minSize)
            fun (x,_) -> x > mid
        let ydivide () =
            let mid = random.Next(miny + minSize, maxy - minSize)
            fun (_,y) -> y > mid
        let divider = 
            if cantx then ydivide ()
            else if canty then xdivide ()
            else match random.Next(2) with 0 -> xdivide () | _ -> ydivide ()
        let left, right = List.partition divider list
        List.concat [bspDivide minSize left; bspDivide minSize right]

let bspPoints minSize list =
    bspDivide minSize list |> List.map (fun section -> section |> range |> mid)

let randomTerrain (main, off) =
    match random.Next(5) with
    | 0 -> off
    | _ -> main

let biomeTypes length = 
    let total = length |> float
    let result = 
        terrainRates 
        |> List.collect (fun (ratio, main, off) ->
            let count = ratio * total |> int |> max 1
            List.replicate count (main, off))
    let padded = List.replicate (length - List.length result) (DeepForest, Forest)
    result @ padded |> List.mapi (fun i o -> i, o)

let randomItem list =
    let length = List.length list
    let (i, item) = list.[random.Next(length)]
    let newList = List.except [(i, item)] list
    (item, newList)

let biomePoints points =
    let length = List.length points
    let biomes = biomeTypes length
    points 
        |> List.fold (fun (biomesLeft, result) (x, y) ->
            let (terrainSet, newBiomes) = randomItem biomesLeft
            newBiomes, (x, y, terrainSet)::result) (biomes, [])
        |> snd
        
let distance (x1, y1) (x2, y2) = 
    let x1, x2, y1, y2 = 
        float x1, float x2, float y1, float y2
    (x2 - x1)**2.0 - (y2 - y1)**2.0 |> sqrt

let tilesByBiome biomes tiles =
    tiles |> List.map (fun (x, y) ->
            let closest = 
                biomes 
                |> List.map (fun (ox,oy,terrain) -> distance (x,y) (ox,oy), terrain)
                |> List.sortBy (fun (dist, _) -> dist)
                |> List.take 3
            let farthest = List.last closest |> fst
            let terrainSet = 
                match random.NextDouble() * farthest with
                | n when n > fst closest.[1] -> snd closest.[2]
                | n when n > fst closest.[0] -> snd closest.[1]
                | _ -> snd closest.[0]
            x, y, randomTerrain terrainSet
        )

let getTiles worldDim =
    let startTiles = 
        [0..worldDim-1] |> List.collect (fun x -> 
        [0..worldDim-1] |> List.map (fun y -> (x, y)))
    let biomes = biomePoints <| bspPoints 3 startTiles
    tilesByBiome biomes startTiles