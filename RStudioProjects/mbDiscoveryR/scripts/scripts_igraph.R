g1 = graph(edges = c("A", "T", "T", "E", "S", "L", "L", "E", "S", "B", "B", "D", "E", "D", "E", "X"), directed = T)
plot(g1)
plot(g1, edge.arrow.size = .3)

net.sym <- as.undirected(g1, mode= "collapse", edge.attr.comb=list(weight="sum", "ignore"))
# color
vcol = rep("white", vcount(g1))
vcol[3] = "green"
vcol[c(2, 8, 5)] = "gold"
plot(g1, edge.arrow.size = 0.3, vertex.color=vcol)
plot(g1, edge.arrow.size = 0.3, vertex.color=vcol, mark.groups = c(2, 8, 5, 6))

# rainbow color
pal1 <- heat.colors(5, alpha=1)   #  5 colors from the heat palette, opaque
pal2 <- rainbow(5, alpha=.5)      #  5 colors from the heat palette, transparent

# define a bn
net = graph_from_literal("V1" -+ "V3":"V14":"V5",
                         "V9",
                         "V19",
                         "V14" -+ "V16":"V4":"V18",
                         "V3" -+ "V5",
                         "V5" -+ "V6":"V20":"V17",
                         "V2" -+ "V14":"V18":"V10":"V7":"V5":"V20", 
                         "V7" -+ "V11":"V6",  
                         "V16" -+ "V15":"V12":"V4", 
                         "V11" -+ "V12",
                         "V6" -+ "V20", 
                         "V12" -+ "V13",
                         "V15" -+ "V8":"V12")

layers = list(c("V1", "V9", "V19"), 
              c("V2", "V3"), 
              c("V14", "V10", "V7", "V5"), 
              c("V16", "V18", "V6", "V17"), 
              c("V15", "V4", "V11", "V20"), 
              c("V8", "V12"), 
              c("V13"))

all(sort(unlist(layers)) == sort(V(net)$name))

# plot
vcol = rep("grey80", vcount(net))
vcol[4] = "green"
vcol[c(2, 6)] = pal2[4]
plot(net, vertex.label = V(net)$name, edge.arrow.size = 0.1)
layout_with_lgl(net)


V(net)$color <- "white"
V(net)$size <- 10
V(net)$size2 <- 5
V(net)$label <- lapply(V(net)$name, function(x) paste(strwrap(x, 12), collapse="\n"))
E(net)$arrow.size <- 0.5

## Create a similar layout using the predefined layers
lay = layout_with_sugiyama(net, layers=apply(sapply(layers, function(x) V(net)$name %in% x), 1, which))
plot(lay$extd_graph)

#lay = layout_with_sugiyama(net, attributes = "all")
#lay = layout_with_sugiyama(net, layers=apply(sapply(layers, function(x) V(net)$name %in% x), 1, which))
#vcol = rep("grey80", vcount(net))
node = "V16"
index = which(V(net)$name == node)
vcol[index] = "green"
indices = which(V(net)$name %in% mbList[[index]])
vcol[indices] = pal2[4]

plot(lay$extd_graph, vertex.color=vcol, mark.group = which(V(net)$name %in% bnlearn::mb(cpts, node)))

net = graph_from_literal("V1" -+ "V14":"V3":"V5", "V9", "V19", "V2" -+ "V4":"V8":"V10":"V7":"V5":"V20", "V3" -+ "V5", "V14" -+ "V16":"V4":"V18",
                         "V10", "V7" -+ "V11":"V6", "V5" -+ "V6":"V20":"V17", "V16" -+ "V5":"V12":"V4", "V18", "V6" -+ "V20", "V17", 
                         "V15" -+ "V8":"V12", "V4", "V11" -+ "V12", "V20", "V8", "V12" -+ "V13", "V13")

edges = c()
for (i in 1:20) edges = c(edges, dagTrue$arcs[i,])
names(edges) = c()
net = graph(edges, isolates = names(cpts)[!names(cpts) %in% V(net)$name])
