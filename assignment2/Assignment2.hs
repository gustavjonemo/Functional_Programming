-- Functional Programming, Assignment 2, Gustav Jönemo & Jacob Nilsson

-- Reference scores

scoreMatch :: Int
scoreMatch = 0
scoreMismatch :: Int
scoreMismatch = -1
scoreSpace :: Int
scoreSpace = -1


-- Similarity score functions (old and optimized)

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] ys = scoreSpace*length ys
similarityScore xs [] = scoreSpace*length xs
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + (if x==y then scoreMatch else scoreMismatch),
                                         similarityScore xs (y:ys) + scoreSpace,
                                         similarityScore (x:xs) ys + scoreSpace]

simScoreOpt :: Eq a => [a] -> [a] -> Int
simScoreOpt xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simScoreTable!!i!!j
    simScoreTable = [[simScoreEntry i j | j<-[0..]] | i<-[0..]]

    simScoreEntry :: Int -> Int -> Int
    simScoreEntry 0 0 = 0
    simScoreEntry i 0 = scoreSpace*i
    simScoreEntry 0 j = scoreSpace*j
    simScoreEntry i j
      | x == y    = scoreMatch + simScore (i-1) (j-1)
      | otherwise = maximum [scoreMismatch + simScore (i-1) (j-1),
                             scoreSpace + simScore i (j-1),
                             scoreSpace + simScore (i-1) j]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

-- Help functions 

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs, ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\l -> valueFcn l == maximum (map valueFcn xs)) xs


-- Alignment functions (old and optimized)

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [][] = [("","")]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs "")
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments "" ys)
optAlignments (x:xs) (y:ys) = concatMap fst . maximaBy snd $[(attachHeads x y (optAlignments xs ys), (similarityScore xs ys) + if x==y then scoreMatch else scoreMismatch),
                                                             (attachHeads x '-' (optAlignments xs (y:ys)), (similarityScore xs (y:ys)) + scoreSpace),
                                                             (attachHeads '-' y (optAlignments (x:xs) ys), (similarityScore (x:xs) ys) + scoreSpace)]

optAlignOpt :: String -> String -> [AlignmentType]
optAlignOpt xs ys = fst $optAligns (length xs) (length ys)
  where
    optAligns i j = optAlignTable!!i!!j
    optAlignTable = [[optAlignEntry i j | j<-[0..]] | i<-[0..]]

    optAlignEntry :: Int -> Int -> ([AlignmentType], Int)
    optAlignEntry 0 0 = ([("","")], 0)
    optAlignEntry i 0 = (attachTails (xs!!(i-1)) '-' (fst (optAligns (i-1) 0)), scoreSpace*i)
    optAlignEntry 0 j = (attachTails '-' (ys!!(j-1)) (fst (optAligns 0 (j-1))), scoreSpace*j)
    optAlignEntry i j = let alignList = maximaBy snd [let (aligns, points) = optAligns (i-1) (j-1) in (attachTails x y aligns, points + if x == y then scoreMatch else scoreMismatch),
                                                      let (aligns, points) = optAligns (i-1) j in (attachTails x '-' aligns, points + scoreSpace),
                                                      let (aligns, points) = optAligns i (j-1) in (attachTails '-' y aligns, points + scoreSpace)]
      in (concatMap fst alignList, snd $head alignList)
      where
          x = xs!!(i-1)
          y = ys!!(j-1)

outputOptAlignments :: String -> String -> IO()
outputOptAlignments string1 string2 = putStrLn $"There are " ++ nrAligns ++ " optimal alignments:\n\n" ++ concatMap (\l -> fst l ++ "\n" ++ snd l ++ "\n\n" ) alignments ++ "There were " ++ nrAligns ++ " optimal alignments!"
    where alignments = optAlignOpt string1 string2
          nrAligns = show $length alignments

outputNrOptAlignments :: String -> String -> IO()
outputNrOptAlignments string1 string2 = putStrLn $"There are " ++ nrAligns ++ " optimal alignments!"
    where alignments = optAlignOpt string1 string2
          nrAligns = show $length alignments