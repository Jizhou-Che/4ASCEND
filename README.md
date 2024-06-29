# 4ASCEND

## About

[4ASCEND](https://unityroom.com/games/4ascend) is a two-player board game developed by [橋野みずは](https://x.com/HashinoMizuha). It is also a built-in minigame in [Alice In Cradle](https://aliceincradle.com), an R18 2D action game currently under development by [ひなゆあ](https://x.com/hinayua_r18) and [橋野みずは](https://x.com/HashinoMizuha). More information about both games can be found [here](https://nanamehacha.dev).

This project implements a CPU player for the 4ASCEND game. The core algorithm involves a Minimax search tree pruned with fine-tuned heuristics and various other techniques. The aim is to provide a fundamental idea of the heuristics used in the 4ASCEND game, while examining the expressiveness of pure functional programming in Haskell.

## Performance

Under the default parameter settings, this program computes a move in roughly 7 seconds on a 2.8GHz single-core machine, and its performance greatly exceeds Primula (the highest level built-in CPU player). Adjusting the breadth and depth of the search tree can affect the computing time and power, with the default values being a fairly reasonable balance.
