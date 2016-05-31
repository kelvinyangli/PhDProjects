import random
import numpy as np


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




