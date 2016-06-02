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

        # sample parents for each node, except the first node
        for i in range(1, dag.number_of_nodes()):

            # take the minimum b/w the number of preceding nodes and maxNumParents as the upper bound when sample
            # the number of parents of the current node
            upperBound = min(i, maxNumParents)

            # randomly sample an integer from [0, upperBound] as the number of parents for the current node
            numParents = random.randrange(0, upperBound + 1)

            # randomly sample indices for parents
            parents = random.sample(nodes[:i], numParents)

            # if the current node has parents
            if len(parents) > 0:

                # add parents to current node iteratively
                for j in range(len(parents)):

                    # tup contains the adjacent nodes tup(from, to)
                    tup = (parents[j], nodes[i])

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

    # add arities info into cpts for the convenience when sampling data from cpts
    cpts.append(np.array(arities))

    # print on screen to 3 decimals
    np.set_printoptions(precision=3)

    return cpts

# cpts = generateCPTs(dag, 2, 1)
# print(cpts)


def generateData(dag, cpts, sampleSize):

    # empty matrix to store sampled data
    df = pd.DataFrame(index=range(0, sampleSize), columns=dag.nodes())

    # potential node values
    # alphabets = np.array(["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O"])

    # sample data from cpts node by node
    for i in range(len(cpts) - 1):

        # current node arity
        arity = cpts[len(cpts) - 1][i]

        # get parents of the current node
        parents = dag.predecessors(dag.nodes()[i])

        # if a node has no parents, sample data for it independently from the others
        if len(parents) < 1:

            df.iloc[:, i] = np.random.choice(range(arity), sampleSize, cpts[i][0].tolist)

        # # if a node has exact one parent
        # elif len(parents) == 1:
        #
        #     indices = df[parents].iloc[j]
        #
        #     df.iloc[:, i] = np.random.choice(range(arity), sampleSize, cpts[i][indices].tolist)

        # if a node has more than one parent, sample data for it based on its parents' value
        else:

            parentsIndices = [dag.nodes().index(parents[k]) for k in range(0, len(parents))]

            for j in range(0, sampleSize):

                # if a node has exact one parent, use parent value as index in cpts list
                if len(parents) == 1:

                    index = df[parentsIndices].iloc[j]

                # if a node has more than 1 parent, compute cpts list index using values2Indices function
                else:

                    cptDimension = cpts[len(cpts) - 1][parentsIndices]
                    parentsInstantIndices = df[parentsIndices].iloc[j] + 1
                    index = values2Indices(cptDimension, parentsInstantIndices) - 1

                # sample data from associated cpt
                # j is row, i is column
                df.iloc[j, i] = int(np.random.choice(range(arity), 1, cpts[i][index].tolist))

    return df


# convert parent nodes values to indices
def values2Indices(cptDimension, indices):

    index = 0

    for i in range(len(cptDimension) - 1, 0, -1):

        index = (index + (indices[i] - 1)) * cptDimension[i - 1]

    index += indices[0]

    return index


dag = generateDag(7, 2, False)
cpts = generateCPTs(dag, 3, 5)
data = generateData(dag, cpts, 5000)




