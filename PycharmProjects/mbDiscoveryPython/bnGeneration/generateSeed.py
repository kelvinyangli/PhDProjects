# get a random seed from then function random in module random, the default core generatator is Mersenne Twister
import random


def generateSeed():
    seed = round(random.random(), 8)
    seed *= (10 ** 8)
    return int(seed)

# pseudoSeed = generateSeed()
# print(pseudoSeed)

