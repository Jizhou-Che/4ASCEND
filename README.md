# 4ASCEND

## About

[4ASCEND](https://unityroom.com/games/4ascend) is a two-player board game developed by [橋野みずは](https://x.com/HashinoMizuha). It is also a built-in minigame in [Alice In Cradle](https://aliceincradle.com), an R18 2D action game currently under development by [ひなゆあ](https://x.com/hinayua_r18) and [橋野みずは](https://x.com/HashinoMizuha). More information about both games can be found [here](https://nanamehacha.dev).

This project implements a CPU player for the 4ASCEND game. The core algorithm involves a Minimax search tree pruned with fine-tuned heuristics and Alpha-Beta Pruning. The aim is to provide a fundamental idea of the heuristics used in the 4ASCEND game, while examining the expressiveness of pure functional programming in Haskell.

## Performance

Under the default parameter settings, the program computes a move in much less than 1 second on a 2.8GHz single-core machine, and its performance greatly exceeds Primula (the highest level built-in CPU player). A video of preliminary performance tests can be found [here](https://www.bilibili.com/video/BV1qjySYHEfv). When moving first, the program was able to defeat a 10-health Primula without taking any damage. When moving second, the program was able to defeat a 10-health Primula while taking only 1 damage. The program was also proved to perform well in both simple and complicated board situations.

Adjusting the breadth and depth of the search tree can affect the computing time and power, with the default values being a fairly reasonable balance. While increasing the depth of search generally improves computing power, increasing the breadth of search can at times be counterproductive, because it allows moves with poor heuristic but good instant effect to be considered in the search tree, and those moves tend to behave poorly in the long run.
