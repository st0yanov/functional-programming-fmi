# Tic-Tac-Toe Haskell

This is an implementation of the famous game ***Tic-Tac-Toe*** in Haskell functional style.

## Overview
* [Game Description](#game-description)
* [Unbeatable Bot Strategy](#unbeatable-bot-strategy)
* [Implementation](#implementation)

## Game Description
> ***[Tic-tac-toe](https://en.wikipedia.org/wiki/Tic-tac-toe)*** (also known as ***noughts and crosses*** or ***Xs and Os***) is a paper-and-pencil game for two players, X and O, who take turns marking the spaces in a 3×3 grid. The player who succeeds in placing three of their marks in a horizontal, vertical, or diagonal row wins the game.

## Unbeatable Bot Strategy
> A player can play a perfect game of tic-tac-toe (to win or, at least, draw) if they choose the first available move from the following list, each turn, as used in Newell and Simon's 1972 tic-tac-toe program.

> 1. **Win:** If the player has two in a row, they can place a third to get three in a row.
2. **Block:** If the opponent has two in a row, the player must play the third themselves to block the opponent.
3. **Fork:** Create an opportunity where the player has two threats to win (two non-blocked lines of 2).
4. **Blocking an opponent's fork:**
   * **Option 1:** The player should create two in a row to force the opponent into defending, as long as it doesn't result in them creating a fork. For example, if "X" has a corner, "O" has the center, and "X" has the opposite corner as well, "O" must not play a corner in order to win. (Playing a corner in this scenario creates a fork for "X" to win.)
   * **Option 2:** If there is a configuration where the opponent can fork, the player should block that fork.
5. **Center:** A player marks the center. (If it is the first move of the game, playing on a corner gives "O" more opportunities to make a mistake and may therefore be the better choice; however, it makes no difference between perfect players.)
6. **Opposite corner:** If the opponent is in the corner, the player plays the opposite corner.
7. **Empty corner:** The player plays in a corner square.
8. **Empty side:** The player plays in a middle square on any of the 4 sides.

## Implementation
The board is represented in the following way:
* 1 board consists of 3 rows
* 1 row consists of 3 cells
* 1 cell contains either a Symbol or an Integer
* A Symbol is either X (crosses) or O (noughts)
* Integers in cells represent cell address

