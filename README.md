# *22Fall CSE230 Project*: Brick Gobang Game

**Group Member**: Chenkang Wu, Xinrui Zhan, Zijian He, Xiang Yu

### Project Description

This project is a terminal interface of the gobang game implemented in Haskell using the brick library. Players not only can start local battles with challenging AI to improve their understanding of the game, but are also able to join an exciting competition with online players on the internet.

### Goal/Features

#### Rules

Gobang, also called Gomoku, is a 2-player abstract strategy board game. Gobang is played on a 15 by 15 chess board. Two players use black and white chess pieces respectively, place a piece of their own color on an empty intersection in terns. The first player to form a continuous chain of five chess pieces horizontally, vertically, or diagonally wins the game.
With a simple rule and much competitiveness, gobang is easy to play but not easy to play well, which brings players much fun. While trying to form their own continuous chain, players also need to pay attention to the opponent’s behaviors to prevent the opponent from forming the chain first.


#### Feartures

- **Game Initialization**: Users could create games with an empty 15 * 15 board. They could also specify the difficulty level (local battle mode), time limit to decide the placement, and wether allowed to withdraw. If it's an online game, the side (Black/While) for each player is assigned randomly. The black one always place the first piece.

- **Displaying Board**: Players are able to see the current chess board anytime they want to. This displaying features will show all placed pieces and also all empty spots. 

- **Action**: In each players’ turn, players can put a piece at any empty spot.

- **Withdraw**: Players can withdraw their last move before the opponents taking action. This is only allowed when “allow_withdraw” is turned on as mentioned above.

- **Time Limit**: Users can set time limits for a singal move and the whole game. In the local mode against AI, this restriction will only apply to the human player. If it's a PVP battle, the `White` player will set this for the current game. 

- **Difficulty Level**:  Users could choose different AI difficulty levels, on which the search depth of a Game Tree is based. This feature is only available in local battles. And the time it takes for a search is not restricted by the `time_limit` setting. So, play easy on yourself.

- **Online Battles**: Users can play games online with other players.


### Timeline

- 11.9 Proposal Submission
- TBD

### Reference

- [Brick](https://github.com/jtdaugherty/brick)

