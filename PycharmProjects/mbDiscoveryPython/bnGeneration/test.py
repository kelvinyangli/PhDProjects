from generateBN import *


dag = generateDag(15, 3)
cpts = generateCPTs(dag, 2, 1)
data = generateData(dag, cpts, 1000)

print(dag.nodes())

tempDag = dag
tempDag.layout(prog="dot")
tempDag.draw("C:\PhDProjects\PycharmProjects\mbDiscoveryPython\img\dag.png")

print(tempDag.nodes())
# print(cpts)
print(data.iloc[0:10,])
