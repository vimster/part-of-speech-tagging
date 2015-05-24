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
type TagTransitionPr = M.Map (Tag, Tag) Double
type WordLikelihoodPr = M.Map (Word, Tag) Double
type TagHistogram = M.Map Tag Double
type WordTagHistogram = M.Map (Word, Tag) Double


------------------------------------------------------------------------
--  Hidden Markov Model
------------------------------------------------------------------------
data HMM = HMM [Tag] TagTransitionPr WordLikelihoodPr deriving(Show)

------------------------------------------------------------------------
--  test data
------------------------------------------------------------------------
testSentence :: TaggedSentence
testSentence = [("<s>", "<s>"), ("Ein", "PR"), ("Haus", "NN"), ("im", "PR"), ("Wald", "NN")]
testSentence1 :: Sentence
testSentence1 = ["Alles", "Klar", "Ein", "Haus"]


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
      transitionPr = M.mapWithKey (\(_, tag) v -> (v + 1) / lookupHistogram tagHistogram tag) tagBigramHistogram
      wordLikelihoodPr = M.mapWithKey (\(_, tag) v -> (v + 1) / lookupHistogram tagHistogram tag) wordTagHistogram
      model = HMM (M.keys tagHistogram) transitionPr wordLikelihoodPr

histogram :: (Ord a, Fractional pr) => [a] -> M.Map a pr
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
      matrix = trace ("tags = " ++ show sentence) $ listArray ((0, 0), (sentLen-1, tagLen-1)) [probability x y | x <- sentRange, y <- tagRange]

      probability :: Int -> Int -> (Int, Double)
      probability 0 _ = (0, 1)
      probability si ti = (fst tagMax, snd tagMax * findPr (sentence!!si, tags!!ti) wordPr)
        where tagMax = tagmax si ti

      tagmax :: Int -> Int -> (Int, Double)
      tagmax si ti = argmaxWithMax (\y -> findPr (tags!!ti, tags!!y) transitionPr * snd (matrix!(si-1, y))) tagRange

      traceback :: [Tag] -> (Int, Double) -> Int -> [Tag]
      traceback resultTags _ (-1) = reverse resultTags
      traceback resultTags (ti, _) index = traceback ((tags!!ti):resultTags) (matrix!(index, ti)) (index-1)

findPr :: (Num v, Ord k) => k -> M.Map k v -> v
findPr k = M.findWithDefault 0 k

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


