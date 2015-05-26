module Main where

import           Control.Applicative
import           Data.Array
import           Data.List               (maximumBy)
import           Data.List.Extras.Argmax
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Ord
import           System.Directory        (getCurrentDirectory,
                                          getDirectoryContents)
import           System.FilePath
import           System.IO               ()
import           System.Random           (newStdGen)
import qualified System.Random.Shuffle   as Shuffler
import           Text.XML.Light

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
type Pr = Double
type TaggedWord = (Word, Tag)
type Sentence = [Word]
type TaggedSentence = [TaggedWord]
type Bigram = (Tag, Tag)
type Frequencies = M.Map (Word, Word) Integer
type TagTransitionPr = M.Map (Tag, Tag) Pr
type WordLikelihoodPr = M.Map (Word, Tag) Pr
type TagHistogram = M.Map Tag Int
type WordTagHistogram = M.Map (Word, Tag) Int


------------------------------------------------------------------------
--  Hidden Markov Model
------------------------------------------------------------------------
data HMM = HMM [Tag] TagTransitionPr WordLikelihoodPr deriving(Show)

------------------------------------------------------------------------
--  IO
------------------------------------------------------------------------

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".." && takeExtension f == ".xml"

-- | read dir
readDir :: String -> IO [FilePath]
readDir path = do
  directory <- getCurrentDirectory
  filter isRegularFile <$> getDirectoryContents (directory ++ "/" ++ path)

parseXml :: String -> [TaggedSentence]
parseXml source =
  let contents = parseXML source
      sentenceValues = concatMap (findElements $ simpleName "sentence") (onlyElems contents)
      sentences = map (findElements $ simpleName "tok") sentenceValues
      nestedWords = map (map (\x -> (strContent x, fromJust $ findAttr (simpleName "cat") x))) sentences
      simpleName s = QName s Nothing Nothing
  in
    map (("<s>", "BOS"):) nestedWords


------------------------------------------------------------------------
--  Train
--  Calculating parameters of Hidden Markov Model
------------------------------------------------------------------------

train :: [TaggedSentence] -> HMM
train taggedSentences = HMM (filter (/= "BOS") (M.keys tagHistogram)) transitionPr wordLikelihoodPr
    where
      taggedWords = concat taggedSentences
      tagHistogram = histogram $ map snd taggedWords
      tagBigramHistogram = histogram $ concatMap (bigrams . map snd) taggedSentences
      wordTagHistogram = histogram taggedWords
      tagMapFunc (_, tag) v = fromIntegral v / fromIntegral (M.findWithDefault 0 tag tagHistogram)
      transitionPr = M.mapWithKey tagMapFunc tagBigramHistogram
      wordLikelihoodPr = M.mapWithKey tagMapFunc wordTagHistogram

histogram :: (Ord a) => [a] -> M.Map a Int
histogram = foldr (flip (M.insertWith (+)) 1) M.empty

bigrams :: [Tag] -> [(Tag, Tag)]
bigrams tags = zip (tail tags) tags

------------------------------------------------------------------------
--  Viterbi
------------------------------------------------------------------------

viterbi :: HMM -> Sentence -> [Tag]
viterbi (HMM tags transitionPr wordPr) sentence =
  traceback [] (maximumBy (comparing (\ti -> snd $ matrix!(sentLen-1, ti))) tagRange) (sentLen-1)
    where
      sentLen = length sentence
      tagLen = length tags
      tagRange = [0..tagLen-1]
      sentRange = [0..sentLen-1]
      matrix = listArray ((0, 0), (sentLen-1, tagLen-1)) [probability x y | x <- sentRange, y <- tagRange]

      probability :: Int -> Int -> (Int, Pr)
      probability 0 _ = (0, 1)
      probability si ti = (fst tagMax, snd tagMax * findPr (sentence!!si, tags!!ti) wordPr)
        where tagMax = tagmax si ti

      tagmax :: Int -> Int -> (Int, Pr)
      tagmax si ti = argmaxWithMax (\y -> findPr (tags!!ti, tags!!y) transitionPr * snd (matrix!(si-1, y))) tagRange

      traceback :: [Tag] -> Int -> Int -> [Tag]
      traceback resultTags _ 0 = resultTags
      traceback resultTags ti si = traceback ((tags!!ti):resultTags) (fst (matrix!(si, ti))) (si-1)

findPr :: (Fractional v, Ord k) => k -> M.Map k v -> v
findPr = M.findWithDefault 0.00001


------------------------------------------------------------------------
--  Evaluation
------------------------------------------------------------------------

precision :: [TaggedSentence] -> HMM -> Pr
precision testSentences hmm = truePositiveCount / fromIntegral (length result)
  where
    sentences = map (map fst) testSentences
    expectedTags = filter (/= "BOS") $ concatMap (map snd) testSentences
    bestTagSequences = concatMap (viterbi hmm) sentences
    result = zipWith (==) expectedTags bestTagSequences
    truePositiveCount = fromIntegral $ length $ filter (==True) result


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
      hiddenMarkovModel = train modelSentences
  putStr "Precision: "
  print $ precision testModelSentences hiddenMarkovModel


