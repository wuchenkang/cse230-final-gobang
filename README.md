# *22Fall CSE230 Project*: Brick Gobang Game

**Group Member**: Chenkang Wu, Xinrui Zhan, Zijian He, Lipo

### Project Description

This project is a terminal interface of the gobang game implemented in Haskell using the brick library. Players not only can start local battles with challenging AI to improve their understanding of the game, but are also able to join an exciting competition with online players on the internet.

### Goal/Features

#### Rules

Gobang, also called Gomoku, is a 2-player abstract strategy board game.Gobang is played on a 15 by 15 chess board. Two players use black and white chess pieces respectively, alternately place a piece of their own color on an empty intersection. Black plays first. The first player to form a continuous chain of five chess pieces horizontally, vertically, or diagonally wins the game.
With a simple rule and much competitiveness, gobang is easy to play but not easy to play well, which brings players much fun. While trying to form their own continuous chain, players also need to pay attention to the opponent’s behaviors to prevent the opponent from forming the chain first.


#### Feartures

- **Game Initialization**: Users could create games with an empty 15 * 15 board. They could also specify the difficulty level (local battle mode), time limit, and wether allowed to withdraw. 

- **Displaying Board**: Players are able to see the current board anytime they want to. This displaying features will show all placed pieces and also all empty spots. 

- **Action**: In each players’ turn, players could put a piece at any empty spots.

- **Withdraw**: Players can withdraw their last move before the opposite players’ move has been actioned. This is only allowed when “allow_withdraw” has been choose when initializing the game. 

- **Time Limit**: Users could set the time limit for each action. This could be set both in local battle with AI and also online PVP battles.

- **Difficulty Level**:  Users could choose different AI levels. This feature is only available in local battles. 

- **Online Battles**: Users could play games online with other players.


### Timeline

- 11.9 Proposal Submission
- TBD

### Reference

- [Brick](https://github.com/jtdaugherty/brick)

