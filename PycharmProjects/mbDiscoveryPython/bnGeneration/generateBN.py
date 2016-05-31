import pygraphviz as pgv
import random
import numpy as np
import pandas as pd

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
        for i in range(1, dag.number_of_nodes()):

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


dag = generateDag(13, 2, True)


def generateCPTs(dag, maxNumValues, concentration):

    # sample node arity
    # if maxNumValues is 2 then all nodes are binary
    # else sample arity for each node between 2 and the maxNumValues
    if maxNumValues == 2:
        arities = [2] * dag.number_of_nodes()

    else:
        arities = [random.randint(2, maxNumValues) for i in range(0, dag.number_of_nodes())]

    # empty list to store cpts values
    cpts = []

    # iteratively generate cpts values for each node
    for i in range(0, dag.number_of_nodes()):

        # get parents of the current node
        parents = dag.predecessors(dag.nodes()[i])

        # if current node has no parents then sample a cpt directly for the current node
        if len(parents) < 1:
            sampledCPT = np.random.dirichlet([concentration] * arities[i], 1)

        else:
            # count the number of total parents instantiations from arities of parents nodes
            numParentsInstants = 1
            for j in range(0, len(parents)):
                parentIndex = dag.nodes().index(parents[j])
                numParentsInstants *= arities[parentIndex]

            # sample a cpt for the current node
            sampledCPT = np.random.dirichlet([concentration] * arities[i], numParentsInstants)

        # add the sampled cpt into a final list called cpts for all nodes
        cpts.append(sampledCPT)

    # print on screen to 3 decimals
    np.set_printoptions(precision=3)

    return cpts

cpts = generateCPTs(dag, 2, 1)
print(cpts)


def generateData(dag, cpts, sampleSize):

    # empty matrix to store sampled data
    df = pd.DataFrame(index=range(0, sampleSize), columns=dag.nodes())

    alphabets = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"]

    for i in range(0, len(cpts)):
        # get parents of the current node
        parents = dag.predecessors(dag.nodes()[i])

        if len(parents) < 1:
            df.iloc[:, i] = np.random.choice(alphabets[:2], sampleSize, cpts[i][0].tolist)

        else:
            for j in range(0, sampleSize):
                df[parents].iloc[j]








    return(data)