# ocaml_toy_neural_networks

An OCaml implementation of automatic differentiation, some basic linear algebra, Newton's method, gradient descent and neural networks.

Trains a neural network to output an arbitrary boolean function as a demonstration. Sample output:

~~~~
$ _build/default/main.exe  '(And (Or (Var 0) (Var 1)) (Var 2))'
Data:
Input: (0 0 0). Answer: (0).
Input: (0 0 1). Answer: (0).
Input: (0 1 0). Answer: (0).
Input: (0 1 1). Answer: (1).
Input: (1 0 0). Answer: (0).
Input: (1 0 1). Answer: (1).
Input: (1 1 0). Answer: (0).
Input: (1 1 1). Answer: (1).
Creating a neural network with these numbers of nodes in its layers: (3 1).
Training neural network on the dataset...
Trained parameter values:
Parameter: ((layer_from_index 0)(node_from Bias)(node_to(Index 0))). Value: -18.693959.
Parameter: ((layer_from_index 0)(node_from(Index 0))(node_to(Index 0))). Value: 7.260924.
Parameter: ((layer_from_index 0)(node_from(Index 1))(node_to(Index 0))). Value: 7.260924.
Parameter: ((layer_from_index 0)(node_from(Index 2))(node_to(Index 0))). Value: 15.207197.
Testing the trained network on the training set:
Input: (0 0 0). Answer: (0). Output: (7.6088116140500068E-09).
Input: (0 0 1). Answer: (0). Output: (0.029691240906611373).
Input: (0 1 0). Answer: (0). Output: (1.0831570215176385E-05).
Input: (0 1 1). Answer: (1). Output: (0.97755884405683624).
Input: (1 0 0). Answer: (0). Output: (1.0831570215176366E-05).
Input: (1 0 1). Answer: (1). Output: (0.97755884405683624).
Input: (1 1 0). Answer: (0). Output: (0.015185525835664073).
Input: (1 1 1). Answer: (1). Output: (0.99998387439481018).
Success rate: 1x.
~~~~