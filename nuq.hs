-- later i will try this with monad transformer and Parsec.

import System.IO
import System.Random
import System.Environment
import Data.List (init, last, delete, intercalate)
import Data.Maybe (fromJust)
import Control.Monad.Plus
import Text.Regex.Posix
import Text.Regex
import Control.Applicative --(getZipList, ZipList, (<*>), (<$>))

type RawQuiz = String
data Table = Table {category :: String
                   ,attributes :: [Attribute]
                   ,objects :: [Object]
                   } deriving (Show)
data GeneralOptions = GeneralOptions { howManyTimes :: HowManyTimes } deriving (Show)
data HowManyTimes = FromStart Int | FromEnd Int | TrueRandom Int deriving (Eq, Show)

-- later i will enable to set in-table options by each table
-- and if a table doesn't have some, default options from general options will fill the empty space instead.
-- but for now implementing that is no more than complicating problem.
-- so only general options are controllable now.

--data GeneralOptions = General {superTable :: SuperTableOptions
--                              ,defaultTable :: InTableOptions
--                              }
--data SuperTableOptions = SuperTable {
--                                    }
--data InTableOptions = InTable {
--                              }
type Category = String
data Attribute = Attribute { contentA :: String
                           , isKey :: Bool
                           , isActive :: Bool
                           } deriving (Eq, Ord, Show)
type Object = [(Attribute,String)]
insert :: a -> b -> [(a, b)] -> [(a, b)]
insert x y list = (x,y):list
keys :: [(a, b)] -> [a]
keys = map fst



--quiz = "<Element>\n\n--truerandom 100\n\nnumber:symbol:name:period;group;\n1 : H : Hydrogen : 1 ; 1 ;\n\n{{{2 : He : Helium : 1 ; 18 ;}}}\n\n3 : Li : Lithium : 2 ; 1 ;"
--
--main = newStdGen >>= (\g -> ask (mkStdGens $ next g) . parse . removeComment $ quiz) >>= showScore

main = getArgs >>= mapM main'

main' arg =
  withFile arg ReadMode (\handle -> do
    contents <- hGetContents handle
    main'' contents)

main'' raw = newStdGen >>= (\g -> ask (mkStdGens $ next g) . parse . removeComment $ raw) >>= showScore


mkStdGens :: (a, StdGen) -> [StdGen]
mkStdGens (_, gen) = gen:(mkStdGens $ next gen)

removeComment :: RawQuiz -> RawQuiz
--removeComment = id
removeComment = unlines . removeOneLineComment . lines . removeOverLineComment . exceptQuotes
exceptQuotes = id
removeOverLineComment :: String -> String
removeOverLineComment = concat . splitRegex (mkRegex "[{]{3}([^{]|[\n])*[}]{3}")
-- i don't know why non-greedy regex(".*?") doesn't work.
removeOneLineComment :: [String] -> [String]
removeOneLineComment = map $ (\s -> if s == "" then "" else head . splitRegex (mkRegex "##") $ s)

parse :: RawQuiz -> (GeneralOptions, [Object])
parse = parse2 . parse1

parse1 :: RawQuiz -> (GeneralOptions, [Table])
parse1 raw = (parseOptions raw, map tablize . tail . groupByCategory ("", []) . lines $ raw)
--parse1' = tail . groupByCategory ("", []) . lines $ quiz --for debug

parseOptions :: RawQuiz -> GeneralOptions
parseOptions str = GeneralOptions (mkHowManyTimes rawOptions)
  where rawOptions = map (words . tail . tail) . filter (=~ "^--(.| )*$") . lines $ str

mkHowManyTimes :: [[String]] -> HowManyTimes
mkHowManyTimes = fromJust . msum . map mkHowManyTimes'
mkHowManyTimes' :: [String] -> Maybe HowManyTimes
mkHowManyTimes' [s1, s2] =
  case s1 of
    "fromstart" -> Just . FromStart . read $ s2
    "fromend" -> Just . FromEnd . read $ s2
    "truerandom" -> Just . TrueRandom . read $ s2
    _ -> Nothing
mkHowManyTimes' _ = Nothing

parse2 :: (GeneralOptions, [Table]) -> (GeneralOptions, [Object])
parse2 (opt, tables) = (opt, concatMap objects  tables)

groupByCategory :: (Category, [String]) -> [String] -> [(Category, [String])]
groupByCategory (c, ss1) [] = (c, reverse ss1):[]
groupByCategory (c, ss1) (s:ss2)
  |s =~ "^<.*>$" = let category = tail . init $ s
                   in (c, reverse ss1):(groupByCategory (category, []) ss2)
  |s =~ "^ *$" || s =~ "^--(.| )+" = groupByCategory (c, ss1) ss2
  |otherwise   = groupByCategory (c, (s:ss1)) ss2

tablize :: (Category, [String]) -> Table
tablize (c, (s:ss)) = Table c attr $ map (insertCategory c . mkObject attr) ss
  where attr = mkAttribute s
insertCategory :: Category -> Object -> Object
insertCategory c = insert (Attribute "Category" False False) c

mkAttribute :: String -> [Attribute]
mkAttribute s = zipWith3 Attribute (map strip rawContentsA) (map mkIsKey colonListA) (map mkIsActive rawContentsA)
  where rawContentsA = splitRegex (mkRegex " *(;|:) *") s
        colonListA = filter ((||) <$> (==';') <*> (==':')) s
        mkIsKey ':' = True
        mkIsKey ';' = False
        mkIsActive (a:b:_) = not (a == '{' && a == b)
        strip str@(a:b:striped) = if not $ mkIsActive str then striped else str

mkObject :: [Attribute] -> String -> Object
mkObject attr s = zipWith (,) attr (splitRegex (mkRegex " *(;|:) *") s)

ask :: [StdGen] -> (GeneralOptions, [Object]) -> IO [Int]
ask gens o = case (howManyTimes $ fst o) of
  FromStart n -> appendFirst (n * length allObject) $ askAll n allObject firstAttr
  FromEnd n -> appendFirst (n * length allObject) $ askAll n (reverse allObject) firstAttr
  TrueRandom n -> appendFirst n $ fmap sumZipList . sequence . map askRandom . mkChain . take (n+1) $ gens
  where allObject = snd o
        keyCand = filter ((&&) <$> isKey <*> isActive) . keys
        firstAttr = head . keyCand
        askRandom (gen1, gen2) = let nowObject = pick allObject gen1
                                 in askOne nowObject (randomAttr nowObject gen2)
randomAttr object gen = pick (filter ((&&) <$> isKey <*> isActive) $ keys object) gen
askAll n a b = fmap (sumZipList . concat . replicate n) . sequence $ zipWith askOne a (map b a)
mkChain (a:b:xs) = (a,b):(mkChain (b:xs))
-- i don't know why but without mkChain it asks same objects with same attributes.
mkChain _ = []
sumZipList :: [[Int]] -> [Int]
sumZipList = foldr1 (\a b-> zipWith (+) a b)
appendFirst m lst = fmap (\l->(m:l)) lst

pick :: (RandomGen g) => [a] -> g -> a
pick items gen = items !! (fst $ randomR (0, (length items)-1) gen)

askOne :: Object -> Attribute -> IO [Int]
askOne object attr = do
  let nowCategory = fromJust $ lookup (Attribute "Category" False False) object
      nowAttribute = contentA attr
      nowKey = fromJust $ lookup attr object
      notNowAttribute = filter isActive $ attr `delete` keys object
      nowNotKey = map (fromJust . flip lookup object) notNowAttribute
      nowToAnswer = formatAnswer . map contentA $ notNowAttribute
      isCorrect = (=~ ("^"++(intercalate " *,+ *" nowNotKey)++"$"))
      correctAnswerToShow = intercalate ", " nowNotKey
  putStrLn $ "The "++nowAttribute++" of this "++nowCategory++" is "++nowKey++". What "++nowToAnswer++"?:"
  test isCorrect correctAnswerToShow 0

formatAnswer :: [String] -> String
formatAnswer [] = error "[] should not be an arguement of 'fromNotNowAttribute.'"
formatAnswer [s] = "is its " ++ s
formatAnswer ss = "are its " ++ (intercalate ", " (init ss)) ++ " and " ++ (last ss)

test :: (String -> Bool) -> String -> Int -> IO [Int]
test p correctAnswer extra = do
  answer <- getLine
  if p answer
  then putStrLn "correct!" >> return [0, extra]
  else if answer =~ "^ *$"
       then putStrLn correctAnswer >> return [1, extra]
       else putStrLn "What?" >> test p correctAnswer (extra+1)


showScore :: [Int] -> IO ()
showScore [totalN, wrong, extra] = putStrLn $ "Rights "++(show right)++", wrongs "++(show wrong)++", extra guesses "++(show extra)++", score "++(show score)
  where right = totalN - wrong
        score = (fromIntegral right / fromIntegral totalN) * 100
