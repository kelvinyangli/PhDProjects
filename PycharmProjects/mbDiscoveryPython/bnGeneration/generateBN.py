import pygraphviz as pgv
import random
import numpy as np
import pandas as pd


def generateDag(numNodes, maxNumParents):

    nodes = ["V" + str(n) for n in range(1, numNodes + 1)]

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

            # randomly sample parents for the current node from preceding indices
            parents = random.sample(nodes[:i], numParents)

            # if the current node has parents
            if len(parents) > 0:

                # add parents to current node iteratively
                for j in range(len(parents)):

                    # tup contains the adjacent nodes tup(from, to)
                    tup = (parents[j], nodes[i])

                    edgeList.append(tup)

        dag.add_edges_from(edgeList)

    return dag


def generateCPTs(dag, maxNumValues, concentration):

    # sample node arity
    # if maxNumValues is 2 then all nodes are binary
    # else sample arity for each node between 2 and the maxNumValues
    if maxNumValues == 2:
        arities = [2] * dag.number_of_nodes()

    else:
        arities = [random.randint(2, maxNumValues) for i in range(dag.number_of_nodes())]

    # empty list to store cpts values
    cpts = []

    # iteratively generate cpts values for each node
    for i in range(dag.number_of_nodes()):

        # get parents of the current node
        parents = dag.predecessors(dag.nodes()[i])

        # if current node has no parents then sample a cpt directly for the current node
        if len(parents) < 1:
            sampledCPT = np.random.dirichlet([concentration] * arities[i], 1)

        else:
            # count the number of total parents instantiations from arities of parents nodes
            numParentsInstants = 1
            for j in range(len(parents)):
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


# the sampled data are integers containing 0, 1, 2, ... instead of strings A, B, C, ...
# since this can save lots of efforts to convert strings back to integers sometimes
# this code has a problem, since dag returns nodes not the correct ordering
def generateData(dag, cpts, sampleSize):

    # empty matrix to store sampled data
    df = pd.DataFrame(index=range(sampleSize), columns=dag.nodes())

    # sample data from cpts node by node
    for i in range(len(cpts) - 1):

        # current node arity
        arity = cpts[len(cpts) - 1][i]

        # get parents of the current node
        parents = dag.predecessors(dag.nodes()[i])

        # if a node has no parents, sample data for it independently from the others
        if len(parents) < 1:

            df.iloc[:, i] = np.random.choice(range(arity), sampleSize, cpts[i][0].tolist)

        # if a node has more than one parent, sample data for it based on its parents' value
        else:

            parentsIndices = [dag.nodes().index(parents[k]) for k in range(len(parents))]

            cptDimension = cpts[len(cpts) - 1][parentsIndices]

            # for each row j of df
            for j in range(sampleSize):

                # if a node has exact one parent, use parent value as index in cpts list
                if len(parents) == 1:

                    index = df.iloc[j, parentsIndices[0]]

                # if a node has more than 1 parent, compute cpts list index using values2Indices function
                else:

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







