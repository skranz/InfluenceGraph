
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


examples.InfluenceGraph = function() {
  A = 1:3
  B = quote(A)
  C = quote(A)
  D = quote(B+C)
  E = 25
  ig = InfluenceGraph$new(exo = list(A=A), endo=list(B=B,C=C,D=D,E=E))
  self = ig
  ig$build_graph()
  plot(ig$g)
  ig$get_ancestors("B")
  ig$get_ancestors("A")
  ig$get_descendants("A")
  ig$make_depth()
  ig$endo.depth
  
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
    influencedBy = NULL,
    depth = NULL,
    endo.depth = NULL,
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
    make_depth = function() {
      influence_graph_make_depth(self)
    },

    make_tables = function() {
      influence_graph_make_tables(self)
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
  self$make_depth()  
  
}


influence_graph_get_ancestors = function(var,self, order=.Machine$integer.max) {
  res = setdiff(self$vars[neighborhood(g,order=order,nodes=var,mode="in")[[1]]], var)
  #if (length(res)==0)
  #  res = NULL
  res
}

influence_graph_get_descendants = function(var,self, order=.Machine$integer.max) {
  res = setdiff(self$vars[neighborhood(g,order=order,nodes=var,mode="out")[[1]]], var)
  #if (length(res)==0)
  #  res = NULL
  res
}


influence_graph_make_depth = function(self) {
  restore.point("influence_graph_make_depth")
  endo.vars = self$endo.vars
  g = self$g
  #neighborhood(g,order=.Machine$integer.max,nodes="B",mode="in")
  influencedBy = lapply(self$endo.vars, self$get_ancestors)
  names(influencedBy) = self$endo.vars

  endo.depth = rep(NA, length(endo.vars))
  independent = sapply(influencedBy, function(el) length(el)==0)
  endo.depth[independent] = 0
  
  depth = c(rep(0, length(self$exo.vars)),endo.depth)
  names(depth) = self$vars  
  while(TRUE) {
    check.vars = self$vars[which(is.na(depth))]
    if (length(check.vars)==0)
      break
    var = check.vars[1]
    for (var in check.vars) {
      depth[var] = max(depth[influencedBy[[var]] ])+1
    }
  }
  self$depth = depth
  self$endo.depth = depth[self$endo.vars]
}

influence_graph_make_tables = function(self) {
  restore.point("make.influence.table")

  self$tables = vector("list", length(self$vars))
  
  self$depth
}

make_depth_zero_table = function(self, var) {
  if (var %in% self$exo.vars) {
    df = data.frame(var = self$exo[[var]])    
  } else {
    df = data.frame(var = eval(self$endo[[var]]))        
  }
  colnames(df) = var
  df
}
