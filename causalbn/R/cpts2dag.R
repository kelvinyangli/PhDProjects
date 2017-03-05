# this function get the dag from bnlearn cpts
cpts2dag = function(cpts) {
  
  dag = model2network(modelstring(cpts))
  
  return(dag)
  
}