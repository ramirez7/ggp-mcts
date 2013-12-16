# General Game Playing Monte-Carlo Tree Search

This was my semester project for Purdue ECE 570: Artificial Intelligence. I implemented a General Game Playing, Monte-Carlo Tree Search algorithm with some enhancements from the current papers I read.

In the process, I created some easy-to-use code that can translate Game Description Language descriptions of game rules to usable Clojure functions.

## Usage

create-environment is the core of the GDL->core.logic. Just call it on a GDL description represented as a quoted list of GDL declarations and it will create an environment representing the game logic and state. This environment can then be passed to best-move and the AI will play.

To access a produced core.logic relation, call (get-relation env :RELATION-NAME). The resulting relation will take the same arguments in the same order as the GDL relation.

If you want to see the Clojure core.logic code generated, call create-code on the GDL description. A map of quoted code will be produced, with the keys being the GDL relation names as keywords.

## Progress

* Tic-Tac-Toe GDL successfuly translated to core.logic
* GDL->core.logic translator complete
* MCTS algorithm complete and should work with any GDL game
* Goal Stability Early Cutoff MCTS extension implemented

## Resources

### Papers
* [Generalized Monte-Carlo Tree Search Extensions for General Game Playing](http ://www.aaai.org/ocs/index.php/AAAI/AAAI12/paper/view/4935)
* [Monte-Carlo tree search and rapid action value estimation in computer Go](http://dl.acm.org/citation.cfm?id=1994536)
* [Understanding the Success of Perfect Information Monte Carlo Sampling in Game Tree Search](http://www.aaai.org/ocs/index.php/AAAI/AAAI10/paper/view/1876)

### General Game Playing

* [Stanford 2013 General Game Playing Competition](http://games.stanford.edu/)
* [Tic-Tac-Toe GDL example](http://games.stanford.edu/gdl.html)

### Monte-Carlo Tree Search

* [MCTS research hub](http://mcts.ai/)

## License

Copyright © 2013 Armando Ramirez

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
