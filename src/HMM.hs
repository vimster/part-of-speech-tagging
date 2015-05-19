module HMM
    (Prob, HMM, train, bestSequence)
    where

import           Control.Monad
import qualified Data.Foldable
import           Data.List          (groupBy, maximumBy, sort)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust, fromMaybe)
import           Debug.Trace
import           System.Environment (getArgs)
import           System.IO.Unsafe   (unsafeInterleaveIO)

type Prob = Double

-- | The type of Hidden Markov Models.
data HMM state observation = HMM [state] [Prob] [[Prob]] (observation -> [Prob])

-- | Perform a single step in the Viterbi algorithm.
--
--   Takes a list of path probabilities, and an observation, and returns the updated
--   list of (surviving) paths with probabilities.
viterbi :: Ord observation =>
               HMM state observation
            -> [(Prob, [state])]
            -> observation
            -> [(Prob, [state])]
viterbi (HMM states _ state_transitions observations) prev x =
    [maximumBy (compare `on` fst)
            [(transition_prob * prev_prob * observation_prob,
               new_state:path)
                    | transition_prob <- transition_probs
                    | (prev_prob, path) <- prev
                    | observation_prob <- observation_probs]
        | transition_probs <- state_transitions
        | new_state <- states]
    where
        observation_probs = observations x

-- | The initial value for the Viterbi algorithm
viterbi_init :: HMM state observation -> [(Prob, [state])]
viterbi_init (HMM states state_probs _ _) = zip state_probs (map (:[]) states)

learn_states :: (Ord state, Fractional prob) => [(observation, state)] -> M.Map state prob
learn_states xs = histogram $ map snd xs

learn_transitions ::  (Ord state, Fractional prob) =>
                      [(observation, state)]
                      -> M.Map (state, state) prob
learn_transitions xs = let xs' = map snd xs in
                        histogram $ zip xs' (tail xs')

learn_observations ::  (Ord state, Ord observation, Fractional prob) =>
                       M.Map state prob
                    -> [(observation, state)]
                    -> M.Map (observation, state) prob
learn_observations state_prob = M.mapWithKey (\ (observation, state) prob -> prob / (fromJust $ M.lookup state state_prob))
                            . histogram

histogram :: (Ord a, Fractional prob) => [a] -> M.Map a prob
histogram xs = let hist = foldr (flip (M.insertWith (+)) 1) M.empty xs in
                M.map (/ M.fold (+) 0 hist) hist

-- | Calculate the parameters of an HMM from a list of observations
--   and the corresponding states.
train :: (Ord observation, Ord state) => [(observation, state)] -> HMM state observation
train sample = model
    where
        states = learn_states sample
        state_list = M.keys states

        transitions = learn_transitions sample
        trans_prob_mtx = [[fromMaybe 1e-10 $ M.lookup (old_state, new_state) transitions
                                | old_state <- state_list]
                                | new_state <- state_list]

        observations = learn_observations states sample
        observation_probs = fromMaybe (fill state_list []) . (flip M.lookup $
                            M.fromList $ map (\ (e, xs) -> (e, fill state_list xs)) $
                                map (\ xs -> (fst $ head xs, map snd xs)) $
                                groupBy     ((==) `on` fst)
                                            [(observation, (state, prob))
                                                | ((observation, state), prob) <- M.toAscList observations])

        initial = map (\ state -> (fromJust $ M.lookup state states, [state])) state_list

        model = HMM state_list (fill state_list $ M.toAscList states) trans_prob_mtx observation_probs

        fill :: Eq state => [state] -> [(state, Prob)] -> [Prob]
        fill states [] = map (const 1e-10) states
        fill (s:states) xs@((s', p):xs') = if s /= s' then
                                            1e-10 : fill states xs
                                           else
                                            p : fill states xs'

-- | Test Viterbi's algorithm on an HMM by comparing the predicted states
--   against known states for the observations.
testViterbi :: (Ord observation, Ord state) =>
                   HMM state observation
                -> [(observation, state)]
                -> Rational
testViterbi hmm testData = (fromIntegral $ length $ filter id $ zipWith (==) (bestSequence hmm observations) states)
                            / (fromIntegral $ length testData)
    where
        observations = map fst testData
        states = map snd testData

-- | Calculate the most likely sequence of states for a given sequence of observations
--   using Viterbi's algorithm
bestSequence :: (Ord observation) => HMM state observation -> [observation] -> [state]
bestSequence hmm = (reverse . tail . snd . (maximumBy (compare `on` fst))) . (foldl (viterbi hmm) (viterbi_init hmm))

on f g a b = f (g a) (g b)


