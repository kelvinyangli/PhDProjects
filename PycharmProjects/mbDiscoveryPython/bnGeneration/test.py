import profile
import pprint

profiler = cProfile.Profile()
profiler.runcall(generateData, dag, cpts, 100)
pprint.pprint(profiler.getstats())


