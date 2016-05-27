import pygraphviz as pgv
import random
import numpy as np


def generateDag(numNodes, maxNumParents, draw=False):

    nodesNumbers = np.arange(1, numNodes + 1)
    string = "V"
    nodes = [string + str(n) for n in nodesNumbers]

    dag = pgv.AGraph(strict=True, directed=True)

    # add nodes
    dag.add_nodes_from(nodes)

    # empty edge list
    edgeList = []

    # sample parents for the current node if the maximum number of parents for the entire structure is non-zero
    if maxNumParents > 0:

        # sample parents for each node
        for i in range(1, len(nodes)):

            # take the minimum b/w the number of preceding nodes and maxNumParents as the upper bound when sample
            # the number of parents of the current node
            upperBound = min(i, maxNumParents)

            # randomly sample an integer from [0, upperBound]
            numParents = random.randrange(0, upperBound + 1)

            # randomly sample indices for parents
            parents = random.sample(nodes[:i], numParents)

            # if the current node has parents
            if len(parents) > 0:

                # add parents to current node iteratively
                for j in range(len(parents)):

                    tup = (nodes[i], parents[j])

                    edgeList.append(tup)

        dag.add_edges_from(edgeList)

    if draw:

        dag.layout(prog="dot")
        dag.draw("C:\PhDProjects\PycharmProjects\mbDiscoveryPython\img\dag.png")

    return dag



