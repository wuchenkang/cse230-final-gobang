# *22Fall CSE230 Project*: Brick Gobang Game

**Group Member**: Chenkang Wu, Xinrui Zhan, Zijian He, Xiang Yu

### Project Description

This project is a terminal interface of the gobang game implemented in Haskell using the brick library. Players not only can start local battles with challenging AI to improve their understanding of the game, but are also able to join an exciting competition with online players on the internet.

### Goal/Features

#### Rules

Gobang, also called Gomoku, is a 2-player abstract strategy board game. Gobang is played on a 9 by 9 chessboard. Two players use black and white chess pieces respectively, alternately placing a piece of their own color on an empty intersection. Black plays first. The first player to form a continuous chain of five chess pieces horizontally, vertically, or diagonally wins the game.
With a simple rule and much competitiveness, gobang is easy to play but not easy to play well, which brings players much fun. While trying to form their own continuous chain, players also need to pay attention to the opponent’s behaviors to prevent the opponent from forming the chain first.



#### Feartures

- **Game Initialization**: Users could create games with an empty 9 * 9 board. 
  - Game Mode: The users can choose one of the three options: 1. Players vs Players locally 2. Players vs AI locally 3. Players vs Players online
  - First Move: The users can choose which user to play first
  - Time limit: The users can choose the amount of time for the time limitation
  - Time limit: The users can choose the amount of time for the time limitation
  - Difficulty level (easy/hard): The users can choose the difficulty level for the AI

- **Displaying Board**: Players are able to see the current chess board at any time. This displaying features will show all placed pieces and also all empty spots.

- **Action**: In each players’ turn, players can put a piece at any empty spot. A move must be made within the time limits, otherwise, the piece will be placed at a randomly selected spot.

- **GameEnd**: After each move, the system will automatically detect whether the game is ended and display the results if so

- **Timer**: The system provide a countdown timer for each player’s move. It will automatically refresh after each move. 

- **Difficulty Level**:  Users could choose different AI difficulty levels. There are two difficulty levels in total: Easy and Hard. 

- **Online Battles**: Users can play games online with other players.


### Timeline

- 11.9 Proposal Submission
- 12.6 Finish
- 12.7 Presentation

### Reference

- [Brick](https://github.com/jtdaugherty/brick)

