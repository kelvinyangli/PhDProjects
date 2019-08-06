directing_tree = function(tree, root) {
  nbrs = bnlearn::nbr(tree, root)
  pa = bnlearn::parents(tree, root)
  undirectedNbrs = nbrs[!nbrs %in% pa]
  if (length(undirectedNbrs) > 0) {
    for (y in undirectedNbrs) {
      tree = bnlearn::set.arc(tree, root, y)
      tree = directing_tree(tree, y)
    }
  }

  return(tree)
}

