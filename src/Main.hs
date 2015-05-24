module Main where

import           Control.Applicative
import           Data.Array
import           Data.List                  (maximumBy)
import           Data.List.Extras.Argmax
import qualified Data.Map                   as M
import qualified Data.MemoCombinators       as Memo
import           Data.MemoCombinators.Class
import           Data.Ord
import           System.Directory           (getCurrentDirectory,
                                             getDirectoryContents)
import           System.Environment
import           System.FilePath
import           System.IO                  ()
import           System.Random              (newStdGen)
import qualified System.Random.Shuffle      as Shuffler
import           Text.XML.Light

import           Debug.Trace
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
type Tag = String
type TaggedWord = (Word, Tag)
type Sentence = [Word]
type TaggedSentence = [TaggedWord]
type Bigram = (Tag, Tag)
type Frequencies = M.Map (Word, Word) Integer
type TagTransitionProbs = M.Map (Tag, Tag) Double
type WordLikelihoodProbs = M.Map (Word, Tag) Double
type TagHistogram = M.Map Tag Double
type WordTagHistogram = M.Map (Word, Tag) Double


------------------------------------------------------------------------
--  Hidden Markov Model
------------------------------------------------------------------------
data HMM = HMM [Tag] TagTransitionProbs WordLikelihoodProbs deriving(Show)

------------------------------------------------------------------------
--  test data
------------------------------------------------------------------------
testSentence :: TaggedSentence
testSentence = [("Ein", "NN"), ("Haus", "VB"), ("im", "PR"), ("Wald", "NN")]
testSentence1 :: Sentence
testSentence1 = ["Alles", "Klar", "Ein", "Haus"]
-- testSentences :: [Sentence]
-- testSentences = [testSentence, testSentence1]
-- testFrequencies :: Frequencies
-- testFrequencies = Map.fromList [(("a", "a"), 0),(("b", "b"), 2), (("c", "c"), 1)]


------------------------------------------------------------------------
--  IO
------------------------------------------------------------------------
-- corpusPath :: FilePath
-- corpusPath = "corpus/"

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".." && takeExtension f == ".xml"

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
--  Train
--  Calculating parameters of Hidden Markov Model
------------------------------------------------------------------------

train :: [TaggedSentence] -> HMM
train taggedSentences = model
    where
      taggedWords = concat taggedSentences
      tagHistogram = histogram $ map snd taggedWords
      tagBigramHistogram = histogram $ concatMap (bigrams . map snd) taggedSentences
      wordTagHistogram = histogram taggedWords
      transitionProbs = M.mapWithKey (\(_, tag) v -> (v + 1) / lookupHistogram tagHistogram tag) tagBigramHistogram
      wordLikelihoodProbs = M.mapWithKey (\(_, tag) v -> (v + 1) / lookupHistogram tagHistogram tag) wordTagHistogram
      model = HMM (M.keys tagHistogram) transitionProbs wordLikelihoodProbs

histogram :: (Ord a, Fractional prob) => [a] -> M.Map a prob
histogram = foldr (flip (M.insertWith (+)) 1) M.empty

bigrams :: [Tag] -> [(Tag, Tag)]
bigrams tags = zip tags $ tail tags

lookupHistogram :: (Ord k, Num v) => M.Map k v -> k -> v
lookupHistogram hist key =
  M.findWithDefault 0 key hist + 1


------------------------------------------------------------------------
--  Viterbi
------------------------------------------------------------------------

viterbi :: HMM -> Sentence -> [Tag]
viterbi (HMM tags transitionPr wordPr) sentence =
  traceback [] (maximumBy (comparing snd) $ map (\ti -> matrix!(sentLen-1, ti)) tagRange) (sentLen-1)
    where
      sentLen = length sentence
      tagLen = length tags
      tagRange = [0..tagLen-1]
      sentRange = [0..sentLen-1]
      matrix = trace ("tags = " ++ show sentence) $ listArray ((0, 0), (sentLen-1, tagLen-1)) [prob x y | x <- sentRange, y <- tagRange]
      prob :: Int -> Int -> (Int, Double)
      prob 0 _ = (0, 1)
      prob si ti = (fst tagMax, snd tagMax * M.findWithDefault 0 (sentence!!si, tags!!ti) wordPr)
        where tagMax = tagmax si ti
      tagmax :: Int -> Int -> (Int, Double)
      tagmax si ti = argmaxWithMax (\y -> M.findWithDefault 0 (tags!!ti, tags!!y) transitionPr * snd (matrix!(si-1, y))) tagRange
      traceback :: [Tag] -> (Int, Double) -> Int -> [Tag]
      traceback resultTags _ 0 = reverse resultTags
      traceback resultTags (ti, _) index = traceback ((tags!!ti):resultTags) (matrix!(index, ti)) (index-1)

------------------------------------------------------------------------
--  Train-Test model separation
------------------------------------------------------------------------
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
  putStr "precision: "


