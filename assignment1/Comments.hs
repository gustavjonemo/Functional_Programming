stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b = do
  r <- randomIO :: IO Float
  return (rulesApply (map (map2 (id, pick r)) b))
--rulesApply saknar Phrase argument, används som argument i output IO (Phrase -> Phrase)
--b är lista av tupels, map utförs elementvis och tar därför in en tupel i taget
--map2 använder en tupel av funktioner, (id, pick r), och en tupel, som fås via map
--med dessa bildas från map2 (id, pick r) (phrase, [Phrase]) -> (phrase, phrase2) som är ett PhrasePair till RulesApply

----------------------------------------------------------------------------------------

--[(String, [String])] -> [([String], [[String]])] -> [(Phrase, [Phrase])]
rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (map2 (words . map toLower, map words))

-- map tar in argumentet som en lista, utför (map2 (words . map toLower, map words)) på varje element i
-- listan, dvs (String, [String]).
-- map2 tar (String, [String]) som tuple-värdes-argument med (words . map toLower, map words) som
-- funktionstuple och bildar (words.map toLower String, map words [String]) -> 
--([String], [[String]]) -> (Phrase, [Phrase]).
-- Detta utförst elementvis och listan [(Phrase, [Phrase])] returneras

----------------------------------------------------------------------------------------

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id
--reductionsApply a b = fix (\x -> try (\y -> transformationsApply "*" id x y) b) a
--reductionsApply a b = fix (try (transformationsApply "*" id a)) b
--fix . try . transformationsApply "*" id
