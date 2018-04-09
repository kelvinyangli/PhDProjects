import networkx as nx
import random as rnd
import matplotlib.pyplot as plt

def generate_random_edges(n_nodes, max_n_pas):
    for node in n_nodes:
        upperBound = min(node, max_n_pas)
        nPas = rnd.randrange(0, upperBound + 1)
        if nPas > 0:
            pas = rnd.sample(range(node), nPas)

def generate_random_dag(n_nodes):
    #nodes = ['V' + str(n) for n in range(1, n_nodes + 1)]
    dag = nx.DiGraph()
    dag.add_nodes_from(range(n_nodes))
    return dag


G = generate_random_dag(5)
M = nx.adjacency_matrix(G)
print(M.todense())


