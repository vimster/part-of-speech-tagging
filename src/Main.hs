module Main where

import           Control.Applicative
import           Data.Array
import qualified Data.List                  as List ()
import qualified Data.Map                   as M
import qualified Data.MemoCombinators       as Memo
import           Data.MemoCombinators.Class
import           System.Directory           (getCurrentDirectory,
                                             getDirectoryContents)
import           System.Environment
import           System.FilePath
import           System.IO                  ()
import           System.Random              (newStdGen)
import qualified System.Random.Shuffle      as Shuffler
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
data HMM = HMM TagTransitionProbs WordLikelihoodProbs deriving(Show)

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
      model = HMM transitionProbs wordLikelihoodProbs

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

fib :: Integer -> Integer
fib = Memo.integral fib'
    where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n-1) + fib (n-2)


-- -- | Perform a single step in the Viterbi algorithm.
-- --
-- --   Takes a list of path probabilities, and an observation, and returns the updated
-- --   list of (surviving) paths with probabilities.
-- viterbi :: HMM -> [(Double, [Tag])] -> Word -> [(Double, [Tag])]
-- viterbi (HMM a b) prev x =
--     [maximumBy (compare `on` fst)
--             [(transition_prob * prev_prob * observation_prob,
--                new_state:path)
--                     | transition_prob <- transition_probs
--                     | (prev_prob, path) <- prev
--                     | observation_prob <- observation_probs]
--         | transition_probs <- state_transitions
--         | new_state <- states]
--     where
--         observation_probs = observations x


-- -- | The initial value for the Viterbi algorithm
-- viterbi_init :: HMM -> [(Double, [Tag])]
-- viterbi_init (HMM _ _) = zip [0..] (map (:[]) [1..10])

-- -- | Calculate the most likely sequence of states for a given sequence of observations
-- --   using Viterbi's algorithm
-- bestSequence :: (Ord observation) => HMM state observation -> [observation] -> [state]
-- bestSequence hmm = (reverse . tail . snd . (maximumBy (compare `on` fst))) . (foldl (viterbi hmm) (viterbi_init hmm))


viterbi :: [Tag] -> HMM -> Sentence -> Double
viterbi tags hmm sentence = fromIntegral $ fst $ head $ map (\i -> matrix!(sentLen-1,i)) [0..tagLen]
  where
    sentLen = length sentence
    tagLen = length tags
    matrix = listArray ((0, 0), (sentLen, tagLen)) [prob x y | x <- [0..sentLen], y <- [0..tagLen]]
    prob x y = (1, "NN")
    traceback (_, tag) = 2

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


