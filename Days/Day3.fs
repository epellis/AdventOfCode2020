module Days.Day3

module Day3 =
    type Feature =
        | Snow
        | Tree

    type Point = { X: int; Y: int }

    type Landscape =
        { Features: Feature [] []
          Dimensions: Point }

    let charToFeature = Map [ ('.', Snow); ('#', Tree) ]
    let featureToScore = Map [ (Snow, 0); (Tree, 1) ]

    let addPoints a b = { X = a.X + b.X; Y = a.Y + b.Y }

    let parseLine line =
        Seq.map (fun c -> charToFeature.[c]) line
        |> Seq.toArray

    let parseInput lines =
        let features = Seq.map parseLine lines |> Seq.toArray
        { Features = features
          Dimensions =
              { X = Seq.length features.[0]
                Y = Seq.length features } }

    let countTrees landscape startingPoint velocityVector =
        let rec countTreesInner landscape startingPoint score =
            let currentFeature =
                landscape.Features.[startingPoint.Y].[startingPoint.X]

            let updatedScore = score + featureToScore.[currentFeature]

            let updatedPoint = addPoints startingPoint velocityVector

            let updatedPointWithOverflow =
                { updatedPoint with
                      X = updatedPoint.X % landscape.Dimensions.X }

            if updatedPointWithOverflow.Y
               >= landscape.Dimensions.Y then
                updatedScore
            else
                countTreesInner landscape updatedPointWithOverflow updatedScore

        countTreesInner landscape startingPoint 0
