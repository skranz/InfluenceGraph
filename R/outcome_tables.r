create_oct = function(var, ig) {  
  pocts = ig$oct[ ig$parents[[var]] ]

  if (length(pocts)==0) return(make_depth_0_table(var,ig))
  
  restore.point("create_oct")
  tab = join_tables(pocts)
  vals = eval(ig$endo[[var]], tab)
  tab[[var]] = vals
  tab
}



#' A table that describes for all elements y[i] of y the feasible elements of ancestor x
#' that can possible lead to y[i].
#' 
#' The dimension is at most |x|*|y|
influence_graph_make_oct = function(var,ig) {

  restore.point("influence_graph_make_oct")
  
  parents = ig$parents[[var]]
  for (p in parents) {
    if (is.null(ig$oct[[p]]))
      influence_graph_make_oct(p,ig)
  }
  ig$oct[[var]] = create_oct(var,ig)
  invisible(ig$oct[[var]])
} 
