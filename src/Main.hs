module Main where

import           Control.Applicative
import qualified Data.List             as List ()
import qualified Data.Map              as Map
import           System.Directory      (getCurrentDirectory,
                                        getDirectoryContents)
import           System.Environment
import           System.FilePath
import           System.IO             ()
import           System.Random         (newStdGen)
import qualified System.Random.Shuffle as Shuffler
import           Text.XML.Light

-- import Debug.Trace
--

------------------------------------------------------------------------
--  Constants
------------------------------------------------------------------------
modelTestRatio :: Double
modelTestRatio = 0.9


------------------------------------------------------------------------
--  types
------------------------------------------------------------------------
type Word = String
type Sentence = [Word]
type Bigram = (Word, Word)
type Frequencies = Map.Map (Word, Word) Integer


------------------------------------------------------------------------
--  test data
------------------------------------------------------------------------
testSentence :: Sentence
testSentence = ["Ein", "Haus", "im", "Wald"]
testSentence1 :: Sentence
testSentence1 = ["Alles", "Klar", "Ein", "Haus"]
testSentences :: [Sentence]
testSentences = [testSentence, testSentence1]
testFrequencies :: Frequencies
testFrequencies = Map.fromList [(("a", "a"), 0),(("b", "b"), 2), (("c", "c"), 1)]


------------------------------------------------------------------------
--  IO
------------------------------------------------------------------------
-- corpusPath :: FilePath
-- corpusPath = "corpus/"

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".." && (takeExtension f) == ".xml"

-- | read dir
readDir :: String -> IO [FilePath]
readDir path = do
  directory <- getCurrentDirectory
  filter isRegularFile <$> getDirectoryContents (directory ++ "/" ++ path)

parseXml :: String -> [Sentence]
parseXml source =
  let contents = parseXML source
      sentenceValues = concatMap (findElements $ simpleName "sentence") (onlyElems contents)
      sentences = map (findElements $ simpleName "tok") sentenceValues
      nestedWords = map (map strContent) sentences
      simpleName s = QName s Nothing Nothing
  in
    map ("<s>":) nestedWords


------------------------------------------------------------------------
--  perplexity calculation
------------------------------------------------------------------------

bigrams :: Sentence -> [(Word, Word)]
bigrams sentence = zip sentence $ tail sentence

frequencePerCorups :: [Sentence] -> Frequencies
frequencePerCorups = foldl (Map.unionWith (+)) Map.empty . map frequenciesPerSentence

frequenciesPerSentence :: Sentence -> Frequencies
frequenciesPerSentence sentence =
  let
    unigramFrequencies = foldl (\ f x -> Map.insertWith (+) (x, "_") 1 f) Map.empty sentence
    bigramFrequencies = foldl (\ f x -> Map.insertWith (+) x 1 f) Map.empty $ bigrams sentence
  in
    Map.unionWith (+) bigramFrequencies unigramFrequencies

lookupFrequency :: Frequencies -> Bigram -> Integer
lookupFrequency frequencies bigram =
  Map.findWithDefault 0 bigram frequencies + 1

perplexity :: Frequencies -> Sentence -> Double
perplexity frequencies sentence =
  let
    probability = fromInteger . lookupFrequency frequencies
    calc bigram@(_, w2) = probability bigram / probability (w2, "_")
    p = product $ map calc $ bigrams sentence
    size = fromIntegral $ length sentence - 1
  in
    p ** (-1/size)


splitIntoModelAndTest :: [a] -> ([a], [a])
splitIntoModelAndTest x = (take modelSize x, drop modelSize x)
   where len = fromIntegral $ length x
         modelSize = truncate $ len * modelTestRatio

shuffle :: [a] -> IO [a]
shuffle list = do
  gen <- newStdGen
  return $ Shuffler.shuffle' list (length list) gen


------------------------------------------------------------------------
--  main
------------------------------------------------------------------------
main :: IO ()
main = do
  let corpusPath = "corpus/"
  files <- readDir corpusPath
  filePaths <- shuffle $ map (corpusPath++) files
  contents <- mapM readFile filePaths
  putStrLn "Calculating..."
  let (model, test) = splitIntoModelAndTest contents
      modelSentences = concatMap parseXml model
      testModelSentences = concatMap parseXml test
      frequencies = frequencePerCorups modelSentences
      perplexities = map (perplexity frequencies) testModelSentences
  putStr "Perplexity: "
  print $ sum perplexities / (fromIntegral $ length perplexities)


