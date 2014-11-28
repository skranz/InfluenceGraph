
#' Returns all variable names in an R expression
var.in.expr = function(expr,expr.str, envir=baseenv()) {
  library(codetools)
  if (!missing(expr.str)) {
    if (length(expr.str)==0)
      return(NULL)
    expr = parse(text=expr.str,srcfile=NULL)
  }
  f <- function() {} # a dummy function
  body(f) <- expr       # containing the expression as its body
  codetools::findGlobals(f,merge=FALSE)$variables
}


example.reduce.cartesian = function() {
  A = 1:3
  B = quote(A)
  C = quote(A)

  ig = InfluenceGraph$new(exo = list(A=A), endo=list(B=B,C=C))
  self = ig
  ig$build_graph()
  ig$get_ancestors("B")
  ig$get_ancestors("A")
  ig$get_descendants("A")
  
  ig$g
  it.add(it,A)
}

library(R6)
InfluenceGraph <- R6Class("InfluenceGraph",
  public = list(
    exo = NULL,
    endo = NULL,
    exo.vars = NULL,
    endo.vars = NULL,
    vars = NULL,
    dependsOn = NULL,
    depth = NULL, 
    g = NULL,

    initialize = function(exo=NULL,endo=NULL) {
      self$exo <- exo
      self$endo <- endo
      self$exo.vars <- names(exo)
      self$endo.vars <- names(endo)
      self$vars = c(names(exo), names(endo))
    },
    build_graph = function() {  
      influence_graph_build_graph(self)
    },
    make_tables = function() {
      make.influence.graph.tables(ig)
    },
    get_ancestors = function(var,order=.Machine$integer.max) {
      influence_graph_get_ancestors(var, self, order=order)
    },
    get_descendants = function(var,order=.Machine$integer.max) {
      influence_graph_get_descendants(var, self, order=order)
    }
    
  )
)

influence_graph_build_graph = function(self) {
  restore.point("influence_graph_build_graph")
  endo = self$endo; exo = self$exo
  self$dependsOn = lapply(endo,var.in.expr)
  self$vars = c(names(exo),names(endo))
  
  g <- graph.empty(directed=TRUE) + vertices(self$vars)
  i = 1
  for (i in seq_along(endo)) {
    pnames =  self$dependsOn[[i]]
    name = names(endo)[[i]]
    if (length(pnames)>0) {
      g[from=pnames,to=rep(name,length(pnames))] <- TRUE
    }
  }
  if (!is.dag(g))
    stop("Influence graph is not acyclic. This means the game structure or behavior structure has invalid cycles in variable's influences on each other.")
  self$g = g
  
  # Compute depth of each endogenous variable
  #influence_graph_make_depth(self)  
  
}


influence_graph_get_ancestors = function(var,self, order=.Machine$integer.max) {
  res = setdiff(self$vars[neighborhood(g,order=order,nodes=var,mode="in")[[1]]], var)
  if (length(res)==0)
    res = NULL
  res
}

influence_graph_get_descendants = function(var,self, order=.Machine$integer.max) {
  res = setdiff(self$vars[neighborhood(g,order=order,nodes=var,mode="out")[[1]]], var)
  if (length(res)==0)
    res = NULL
  res
}


influence_graph_make_depth = function(self) {
  g = self$g
  #neighborhood(g,order=.Machine$integer.max,nodes="B",mode="in")
  influencedBy = lapply(self$endo.vars, self$get_ancestors)


}

make.influence.graph.tables = function(self) {
  restore.point("make.influence.table")
  
}

influence.tables.add = function(...) {
  
}

conditional.cross = function(B,C,A) {
  
}