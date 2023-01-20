# Entropy AI

This is a simple Entropy AI player which was developed in order to participate in the CodeCup 2023 programming contest.

# Repository Content

## File: `entropy.hs`

This is the main file containing the Entropy AI. It is written in Haskell and has been verified to work with GHC 8.6.5. Assuming a working Haskell installation, the Entropy can be tested using the command `runghc entropy.hs`. A release build with improved runtime performance can be obtained by compiling it using `ghc -O3 entropy.hs`.

The communication is done over stdin and stdout and follows the rules of the CodeCup 2023 programming context described here: https://www.codecup.nl/entropy/rules.php

* Rows are encoded as upper case characters A-G
* Columns are encoded as upper case characters a-g
* Token colors are encoded as integers ranging from 1-7

Each row and column is encoded into a 32bit unsigned integer. This allows efficient lookup of the heuristic value in an array to avoid re-calculation of the expensive heuristic function. (In theory, the lookup table could be fully precomputed by ghc during compilation using `Data.LookupTable`, however due to constraints of Template Haskell this in not possible for the CodeCup 2023 competition submission, as only a single file can be submitted.)

The main idea of the AI is to turn the game into a search tree, where each layer corresponds to a particular game state awaiting either Random token selection, Chaos player place action, or Order player move action.
To increase performance, alpha-beta pruning has been implemented for player actions. Expectiminimax is used in order to calculate the weighted average of the expected outcome for non-deterministic random token selections.

Currently, the depth of the search is limited to 4 (i.e. Order-Random-Chaos-Order for Order player and Chaos-Order-Random-Chaos for Chaos players). However, by tuning the order of the actions in the search tree, alpha beta pruning most likely could be improved enough to allow for increased depth.

## Folder: `app/`

Contains the project in order to create a simple web interface to test out and play against the developed Entropy AI.

The Entropy AI is used within the miso front-end framework and directly compiled using ghcjs to Javascript, so it can be tested in any browser offline without any server dependencies.

A copy of the generated artefacts has been hosted [here](https://kodu.ut.ee/~herman99/entropy), where one can familiarise themselves with the Entropy game and try outperforming the Entropy AI implemented here.

# References

* [CodeCup 2023 Programming Contest](https://www.codecup.nl/)
* [Abstract Games - Issue 11 Autumn 2002](https://www.abstractgames.org/uploads/1/1/6/4/116462923/abstract_games_issue_11.pdf)
* [Wikipedia: Entropy (1977 board game)](https://en.wikipedia.org/wiki/Entropy_(1977_board_game))
* [Wikipedia: Alpha–beta pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning)
* [Wikipedia: Expectiminimax](https://en.wikipedia.org/wiki/Expectiminimax)