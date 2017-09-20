module Main where


scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
string3 = "aferociousmonadatemyhamster"
string4 = "functionalprogrammingrules"

type AlignmentType = (String,String)

main = do
 outputOptAlignment string3 string4 

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTails h1 h2 aList = [(xs++[h1], ys++[h2]) | (xs,ys) <- aList]

similarityScore :: String -> String -> Int
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys  + score x y ,
                         				similarityScore xs (y:ys) + score x '-' ,
                          				similarityScore (x:xs) ys + score '-' y]

maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = filter condition xs
						where
							value = map valueFcn xs
							maxVal = maximum(value)
							condition = (\var -> valueFcn var == maxVal)


optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy scoreAlign $ concat [pos1, pos2, pos3]
						where
							pos1 = attachHeads x y (optAlignments xs ys)
							pos2 = attachHeads x '-' (optAlignments xs (y:ys)) 
							pos3 = attachHeads '-' y (optAlignments (x:xs) ys)

score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y 
	| x == y = scoreMatch
	| otherwise = scoreMismatch

scoreAlign :: AlignmentType -> Int
scoreAlign ([], []) = 0
scoreAlign (x:xs,[]) = scoreSpace
scoreAlign ([],y:ys) = scoreSpace
scoreAlign ((x:xs),(y:ys))
	| x == '-' = scoreSpace + scoreAlign(xs,ys)
	| y == '-' = scoreSpace + scoreAlign(xs,ys)
	| y == x = scoreMatch + scoreAlign(xs,ys)
	|otherwise = scoreMismatch + scoreAlign(xs,ys)


outputOptAlignment :: String -> String -> IO()
outputOptAlignment x y = do
	let a = optAlignments2 x y 
	putStrLn ("There are " ++ (show $ length a) ++ " optimal alignments:" ++ "\n")
	printer a
	putStrLn ("There were " ++ (show $ length a) ++ " optimal alignments!")
	

printer :: [AlignmentType] -> IO()
printer [] = putStrLn ""
printer (a:as) = do
	putStrLn $ fst a
	putStrLn (snd a ++ "\n")
	printer as

similarityScore2 :: String -> String -> Int
similarityScore2 xs ys = ss (length xs) (length ys)
  where
    ss i j = sTable!!i!!j
    sTable = [[ sEntry i j | j<-[0..]] | i<-[0..] ]
       
    sEntry :: Int -> Int -> Int
    sEntry 0 0 = 0
    sEntry 0 j = j * scoreSpace
    sEntry i 0 = i * scoreSpace
    sEntry i j = maximum [score x y + ss (i-1) (j-1),
							score x '-' + ss i (j-1),
							score '-' y + ss (i-1) j]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

optAlignments2 :: String -> String -> [AlignmentType]
optAlignments2 xs ys = snd $ optAlign (length xs) (length ys)
  where
    optAlign i j = optTable!!i!!j
    optTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]
    

    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [([],[])])
    optEntry i 0 = (s + scoreSpace, attachTails x '-' a)
             where s = fst $ optAlign (i-1) 0
                   a = snd $ optAlign (i-1) 0
                   x = (xs !! (i-1))
                   
   
    optEntry 0 j = (s + scoreSpace, attachTails '-' y a)
             where s = fst $ optAlign 0 (j-1)
                   a = snd $ optAlign 0 (j-1)
                   y = (ys !! (j-1))
                   

    optEntry i j = (max, concat $ map snd maxopt)
      	where
       		x = xs!!(i-1)
         	y = ys!!(j-1)
         	s1 = fst (optAlign (i-1) (j-1))
        	p1 = snd (optAlign (i-1) (j-1))
         	s2 = fst (optAlign i (j-1))
         	p2 = snd (optAlign i (j-1))
        	p3 = snd (optAlign (i-1) j)
         	s3 = fst (optAlign (i-1) j)
        	e1 = (s1 + score x y, attachTails x y p1)
        	e2 = (s2 + score '-' y, attachTails '-' y p2)
         	e3 = (s3 + score x '-', attachTails x '-' p3)
         	entries = [e1,e2, e3]
         	maxopt = maximaBy fst entries
         	max = maximum (map fst entries)
