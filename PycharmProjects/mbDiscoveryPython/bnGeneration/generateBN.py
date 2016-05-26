import functools
import random

import graphviz as gv
import numpy as np

from bnGeneration.addNodes import addNodes
from bnGeneration.addEdges import addEdges


def generateDag(numNodes, maxNumParents):
    nodesNumbers = np.arange(1, numNodes + 1)
    string = "V"
    nodes = [string + str(n) for n in nodesNumbers]

    digraph = functools.partial(gv.Digraph, format='svg')
    dag = digraph()
    edges = []

    if maxNumParents > 0:

        for i in range(1, len(nodes)):  # sample parents for each node

            upperBound = min(i, maxNumParents)  # minimum b/w the number of preceding nodes and maxNumParents
            numParents = random.randrange(0, upperBound + 1)  # randomly sample integer from [0, upperBound]
            parents = random.sample(nodes[:i], numParents)  # randomly sample indices for parents

            if len(parents) > 0:     # if the current node has parents

                for j in range(len(parents)):  # add parents to current node iteratively

                    tup = (nodes[i], parents[j])
                    edges.append(tup)

        addEdges(addNodes(dag, nodes), edges)

    return dag

# let's try creating a bayesian network
testDag = generateDag(20, 3)

print(testDag)


