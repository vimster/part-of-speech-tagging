# Part of Speech Tagging
Tool for tagging part-of-speech using the Hidden Markov Model and the Viterbi algorithm.

## Usage
Copy the xml corpus files into the root folder named corpus.
Haskell and its package and building system cabal are required.
The Haskell Plattform (https://www.haskell.org/platform/) bundles all requirements.

Run in the root directory:
$ cabal update
$ cabal install
$ cabal run

## Results on Genia corpus (Treebank)
Precision = ~ 94.78%
