import math
import numpy as np


# compute log factorial
def logFactorial(x, base=2):

    cumulativeSum = 0

    for i in range(1, x + 1):

        cumulativeSum += math.log(i, base)

    return cumulativeSum


# arities needs to be numpy array
def mmlCPT(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base=2):

    arityChild = arities[nodeIndex]

    numParents = len(parentsIndices)

    numParentsInstantiations = np.prod(arities[parentsIndices])

    fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * math.log((math.pi * math.e / 6), base)

    nonFixedTerm = 0

    # log((|x| - 1)!)
    logConstant = math.log(math.factorial(arityChild - 1), base)

    for i in range(0, numParentsInstantiations):

        if numParents == 1:
            # if single parent then just use index i

            commonParentsIndices = indexListPerNodePerValue[[parentsIndices]][[i]]

            N_pa_i = len(commonParentsIndices)

            # sum_i^arityChild log(N(pa_i, x_i))!
            cumSum = singleParentComputation(nodeIndex, commonParentsIndices, arityChild, indexListPerNodePerValue, base=base)

        else:
            # if more than 1 parent, use function to get potential combination

            potentialCombination = getParentsInstantiationIndices(arities, numParents, parentsIndices, numParentsInstantiations, i)

            commonParentsIndices = intersectIndices(numParents, parentsIndices, indexListPerNodePerValue, potentialCombination)

            N_pa_i = len(commonParentsIndices)

            cumSum = multiParentsComputation(nodeIndex, arityChild, indexListPerNodePerValue, commonParentsIndices, base=base)

            # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
            logNumerator = logFactorial(N_pa_i + arityChild - 1, base=base)

            nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum

    return (fixedTerm + nonFixedTerm)
