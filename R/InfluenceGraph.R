

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
  library(sktools)
  
  C = 0:4
  D = quote(C+1)
  E = 1:2
  F = quote((D <= 3)*10) 
  G = quote((D <= 2)*10)
  H = quote(D %% 2 +1 )
  I = quote(H)
  J = 0:1 
  W = quote(H+J)
  X = quote(E+F)
  Y = quote(G+I)
  Z = quote(X+Y+W)

  
  ig = InfluenceGraph$new(exo = nlist(C,E,J), endo=nlist(D,F,G,H,I,W,X,Y,Z))
  self = ig
  ig$build_graph()
  ig$plot()
  oct = ig$make_oct("Z")
  oct
  
  #ig$make_tables(cartesian=TRUE)
  ig$make_tables(cartesian=FALSE)
  ig$tables
  tab = ig$tables[["Z"]]
  tab = ig$outcomes_table("Z")
  tab = tab[,c("Z",ig$exo.vars),drop=FALSE]
  
  tab = reduce_irrelevant(tab,"C")
  tab
  tab = reduce_irrelevant(tab,"E")
  tab
  tab = reduce_irrelevant(tab,"J")
  tab
  

  N=c("left","right")
  L=1:3
  R=4:6
  payoff=quote(case_distinction(
    L,N=="left",
    R
  ))
  
  ig = InfluenceGraph$new(exo = nlist(N,L,R), endo=nlist(payoff))
  self = ig
  ig$build_graph()
  ig$plot()
  ig$make_tables(cartesian=FALSE)
  ig$tables
  tab = ig$outcomes_table("payoff")
  tab
  
  # reduce irrelevant

  
  reduce_irrelevant = function(tab, var) {
    vals = unique(tab[[var]])
    d = group_by_(tab, .dots=setdiff(colnames(tab),var))     
    fun = function(df) {
      df.vals = unique(df[[var]])
      if (setequal(vals, df.vals)) {
        df = df[1,,drop=FALSE]
        df[[var]] = NA
      }
      return(df)
    }

    do(d, fun(.))
  }
  var = "R"
  tab = ig$outcomes_table("payoff")
  tab = reduce_irrelevant(tab,"R")
  tab
  tab = reduce_irrelevant(tab,"L")
  tab
  tab = reduce_irrelevant(tab,"N")
  tab

}

library(R6)
InfluenceGraph <- R6Class("InfluenceGraph",
  public = list(
    exo = NULL,
    endo = NULL,
    exo.vars = NULL,
    endo.vars = NULL,
    exo.inds = NULL,
    endo.inds = NULL,
    vars = NULL,
    var.inds = NULL,
    
    # defined for endo.vars only
    parents = NULL,
    influencedBy = NULL,
    
    # defined for all vars
    influences = NULL,
    children = NULL,
    
    depth = NULL,
    endo.depth = NULL,
    g = NULL,
    tables = NULL,
    oct = NULL,

    initialize = function(exo=NULL,endo=NULL) {
      self$exo <- exo
      self$endo <- endo
      self$exo.vars <- names(exo)
      self$endo.vars <- names(endo)
      self$exo.inds = seq_along(self$exo.vars)
      self$endo.inds = seq_along(self$endo.vars)+length(self$exo.vars)
      self$vars = c(names(exo), names(endo))
      
      vars = self$vars
      var.inds = seq_along(vars)
      names(var.inds) = vars
      self$var.inds = var.inds
    },
    build_graph = function() {  
      influence_graph_build_graph(self)
    },
    make_depth = function() {
      influence_graph_make_depth(self)
    },

    make_tables = function(cartesian=TRUE) {
      influence_graph_make_tables(ig=self, cartesian=cartesian)
    },
    make_oct = function(var) {
      influence_graph_make_oct(var,ig=self)
    },
    get_ancestors = function(var,order=.Machine$integer.max) {
      influence_graph_get_ancestors(var, self, order=order)
    },
    get_descendants = function(var,order=.Machine$integer.max) {
      influence_graph_get_descendants(var, self, order=order)
    },
    all_paths_children = function(from, to) {
      influence_graph_all_paths_children(from, to, self)
    },
    common_ancestors = function(vars,direct.line=TRUE, only.exo=FALE) {
      influence_graph_common_ancestors(vars, self, direct.line=direct.line,only.exo=only.exo)
    },
    outcomes_table = function(var) {
      influence_graph_outcomes_table(var,ig=self)
    },
    get_x.of.y = function(x,y) {
      influence_graph_get_x.of.y(x,y,ig)
    },
    plot = function(col.exo = "blue", col.endo="black",
                    edge.arrow.size = 0.3, vertex.size=20,...) {
      restore.point("plot.influence.graph")
      g = self$g
      if (is.null(g))  {
        cat("Build_graph() was not yet called. Cannot plot.")
        return()
      }

      color =  c(rep(col.exo,   length(self$exo)),
                rep(col.endo,  length(self$endo)))
      no.influence = sapply(self$children, function(c) length(c)==0)
      color[no.influence] <- "brown"
      V(g)$label.color <-color

#       lay1 <- layout.sugiyama(g)$layout
#       plot.igraph(g, layout=lay1,edge.arrow.size = edge.arrow.size,
#                   vertex.size = vertex.size, vertex.color="white",
#                   vertex.label.font = 12, vertex.frame.color="white")

      plot.igraph(g, edge.arrow.size = edge.arrow.size,
                  vertex.size = vertex.size, vertex.color="white",
                  vertex.label.font = 12, vertex.frame.color="white")
      
      #plot.igraph(self$g, mark.group=mark.group, edge.arrow.size = edge.arrow.size,...)   
      
    }
    
  )
)

influence_graph_build_graph = function(ig) {
  restore.point("influence_graph_build_graph")
  endo = ig$endo; exo = ig$exo
  ig$parents = lapply(endo,var.in.expr)
  ig$vars = c(names(exo),names(endo))
  
  g <- graph.empty(directed=TRUE) + vertices(ig$vars)
  i = 1
  for (i in seq_along(endo)) {
    pnames =  ig$parents[[i]]
    name = names(endo)[[i]]
    if (length(pnames)>0) {
      g[from=pnames,to=rep(name,length(pnames))] <- TRUE
    }
  }
  if (!is.dag(g))
    stop("Influence graph is not acyclic. This means the game structure or behavior structure has invalid cycles in variable's influences on each other.")
  ig$g = g
  
  # Compute depth of each endogenous variable
  ig$make_depth()  

}


influence_graph_get_ancestors = function(var,ig, order=.Machine$integer.max) {
  g = ig$g
  res = setdiff(ig$vars[neighborhood(g,order=order,nodes=var,mode="in")[[1]]], var)
  #if (length(res)==0)
  #  res = NULL
  res
}

influence_graph_get_descendants = function(var,ig, order=.Machine$integer.max) {
  g = ig$g
  res = setdiff(ig$vars[neighborhood(g,order=order,nodes=var,mode="out")[[1]]], var)
  #if (length(res)==0)
  #  res = NULL
  res
}


influence_graph_make_depth = function(ig) {
  #restore.point("influence_graph_make_depth")
  #browser()
  endo.vars = ig$endo.vars
  g = ig$g
  #neighborhood(g,order=.Machine$integer.max,nodes="B",mode="in")
  influencedBy = lapply(ig$endo.vars, ig$get_ancestors)
  names(influencedBy) = ig$endo.vars

  influences = lapply(ig$vars, ig$get_descendants)
  children = lapply(ig$vars, ig$get_descendants, order=1)
  names(influences) = names(children) = ig$vars
  ig$influences = influences
  ig$children = children
  
  endo.depth = rep(NA, length(endo.vars))
  independent = sapply(influencedBy, function(el) length(el)==0)
  endo.depth[independent] = 0
  
  depth = c(rep(0, length(ig$exo.vars)),endo.depth)
  names(depth) = ig$vars  
  while(TRUE) {
    check.vars = ig$vars[which(is.na(depth))]
    if (length(check.vars)==0)
      break
    var = check.vars[1]
    for (var in check.vars) {
      depth[var] = max(depth[influencedBy[[var]] ])+1
    }
  }
  ig$depth = depth
  ig$endo.depth = depth[ig$endo.vars]
  ig$influencedBy = influencedBy
}

#' Get the common ancestors of variables vars
#' 
#' @param direct.line if TRUE remove common ancestors that only influence all variables
#'        via another common ancestors
influence_graph_common_ancestors = function(vars, ig, direct.line=TRUE, only.exo=FALSE) {
  #x = "X"; y="Y"
  #vars = c("W","X","Y")
  restore.point("influence_graph_common_ancestors")
  influencedBy = ig$influencedBy
  common.anc = intersect.list(influencedBy[vars])

  if (only.exo) return(setdiff(common.anc,ig$exo.vars))
  if (!direct.line) return(common.anc)
  if (length(common.anc)==0) return(NULL)
  
  anc = common.anc[1]
  is.direct = sapply(common.anc, function(anc) {
    for (var in vars) {
      apc = ig$all_paths_children(from=anc, to=var)
      if (!all(apc[[anc]] %in% common.anc))
        return(TRUE)
    }
    return(FALSE)
  })
  return(common.anc[is.direct])
}

influence_graph_make_tables = function(ig, cartesian = FALSE) {
  restore.point("make.influence.table")
  
  depth = ig$depth
  vars = ig$vars
  tables = vector("list", length(vars))
  names(tables) = vars
  ig$tables = tables
  
  max.depth = max(depth)
  for (d in 0:max.depth) {
    avars = vars[depth==d]
    for (var in avars) {
      ig$tables[[var]] = make_depth_d_table(var,d=d, ig, cartesian=cartesian)
    }      
  }  
}


make_depth_d_table = function(var,d, ig, cartesian=FALSE) {  
  if (d==0) return(make_depth_0_table(var,ig))
  if (d==1 | cartesian) return(make_cartesian_table(var,ig))

  restore.point("make_depth_d_table")

  # Try to get a smaller parents table than just the cartesian product
  # if parents have common ancestors that restrict value combinations
  tab = make_reduced_parents_table(var, ig)

  vals = eval(ig$endo[[var]], tab)
  tab[[var]] = vals
  tab
}


make_depth_0_table = function(var, ig) {
  restore.point("make_depth0_table")
  if (var %in% ig$exo.vars) {
    df = data_frame(var = ig$exo[[var]])    
  } else {
    df = data_frame(var = eval(ig$endo[[var]]))        
  }
  colnames(df) = var
  rownames(df) = NULL

  df
}

make_cartesian_table = function(var,ig) {
  tab = make_cartesian_parents_table(var,ig)
  vals = eval(ig$endo[[var]], tab)
  tab[[var]] = vals
  rownames(tab) = NULL

  tab
}

make_cartesian_parents_table = function(var,ig) {
  restore.point("make_cartesian_parents_table")

  pvars = ig$parents[[var]]
  tables = ig$tables
  pvars = ig$parents[[var]]

  # generate grid of all parent value combinations  
  pvals = lapply(pvars, function(pvar) {
    tab = tables[[pvar]]
    sort(unique(tab[[pvar]]))
  })
  names(pvals)=pvars

  tab = expand.grid(pvals, stringsAsFactors = FALSE)
  rownames(tab) = NULL

  tab
}

#' Table of all relevant parents' value combinations
#' 
#' Tries to reduce number of combinations of the cartesian product
#' if parent variables have common ancestors whose influence
#' may render some combinantions infeasible
make_reduced_parents_table = function(var,ig) {
  restore.point("make_reduced_parents_table")

  pvars = ig$parents[[var]]
  if (length(pvars)==1) return(make_cartesian_parents_table(var,ig))

  tab = make_cartesian_parents_table(var,ig)
  
  # Go through all combinations of at least 2 parent variables
  n = 2
  max.n = length(pvars)
  for (n in 2:max.n) {
    combs = combn(pvars,n, simplify=FALSE)
    comb = combs[[1]]
    for (comb in combs) {
      avars = ig$common_ancestors(comb, only.exo=TRUE)
      if (length(avars)==0) next
      avar = avars[[1]]
      for (avar in avars) {
        rtab = reduced.parents.table(comb,avar,ig)
        # Continue here: intersect tab and rtab
        old.nrow = NROW(tab)
        tab = filter.with.table(tab, rtab)
        cat(paste0("\n",paste0(comb, collapse="_"),".",avar, ": ",
            old.nrow, " -> ", NROW(tab), ""))
      }
    }

  }
  rownames(tab) = NULL

  return(tab)
}

reduced.parents.table = function(vars,x,ig) {
  restore.point("reduced.parents.table")
  
  #if (vars[1]=="W") stop()
  
  x.of.vars = lapply(vars, function(var) {
    ig$get_x.of.y(x,var)
  })

  x.of.res = reduce.by.x(x.of.vars[[1]], x.of.vars[[2]], x=x, keep.x = TRUE)
  inds = setdiff(seq_along(x.of.vars),1:2)
  ind = inds[1]
  for (ind in inds) {
    x.of.res = reduce.by.x(x.of.res, x.of.vars[[ind]], x=x, keep.x = TRUE)
  }  
  # Get rid of x col and duplicates
  cols = setdiff(colnames(x.of.res),x)
  x.of.res = unique(x.of.res[,cols])
  x.of.res
}


#' This helper function needs to be made more memory efficient using C++ code
reduce.by.x = function(x.of.left, x.of.right,x = colnames(tab1)[1], keep.x = TRUE) {
  restore.point("reduce.by.x")
  l_r.x =inner_join(x.of.left, x.of.right, by=x)
  if (!keep.x) {
    cols = setdiff(colnames(l_r.x),x)
    l_r.x = l_r.x[,cols]
  }
  l_r.x = unique(l_r.x)
  l_r.x
}

examples.get.element.set = function() {
  Gt = data_frame(D=1:5, G=c(0,0,0,10,10))
  Ft = data_frame(D=1:5, F=c(0,0,0,10,10))
  Yt = data_frame(G=c(0,0,10,10),H=c(1,2,1,2),Y=c(1,2,11,12))
  Xt = data_frame(F=c(0,0,10,10),E=c(1,2,1,2),X=c(1,2,11,12))
 
  library(dplyr)
  
  Y.df = inner_join(select(Gt,G,D), select(Yt,G,Y), by="G")
  Y.df = select(Y.df, D,Y)

  
  
  # For every possible value of X store the possible values of D
  D.of.F = select(Ft,F,D)
  F.of.X = select(Xt,F,X)
  
  D.of.X = inner_join(D.of.F,F.of.X, by="F")
  D.of.X = select(D.of.X, D,X)
  D.of.X

  # For every possible value of Y store the possible values of D
  D.of.G = select(Gt,G,D)
  G.of.Y = select(Yt,G,Y)
  
  D.of.Y = inner_join(D.of.G,G.of.Y, by="G")
  D.of.Y = select(D.of.Y, D,Y)
  D.of.Y

  X_Y.D  
  # Possibly generates a very big table before duplicates are removed
  X_Y.D = inner_join(D.of.X, D.of.Y, by="D") %>%
          select(X,Y)
  X_Y.D = X_Y.D[!duplicated(X_Y.D),]
  #X_Y.cond.D
  
  
  df.merge = merge(Gt,Yt,by = "G")
  select(df.merge, D,Y)
}

#' Get all paths from variable from to variable to in form of a children list

#' @value Returns a list that contains for all nodes that are on a path their children
#'        that are on a path.
influence_graph_all_paths_children = function(from,to,ig) {
  restore.point("influence_graph_all_parents_paths")
  influences = ig$influences; children = ig$children
  vars = ig$vars
  if (!to %in% influences[[from]])
    return(NULL)
  
  on.paths = sapply(vars, function(var) {
    to %in% influences[[var]]
  })
  pvars = c(vars[on.paths],to)
  
  path.children = lapply(pvars, function(var) {
    intersect(children[[var]],pvars)
  }) 
  names(path.children) = pvars
  path.children
} 

#' A table that describes for all elements y[i] of y the feasible elements of ancestor x
#' that can possible lead to y[i].
#' 
#' The dimension is at most |x|*|y|
influence_graph_get_x.of.y = function(x,y,ig) {
  restore.point("influence_graph_get_x.of.y")
  #x = "D"; y="Y" 
  
  influences = ig$influences; children = ig$children
  vars = ig$vars
  if (!y %in% influences[[x]])
    return(NULL)
  
  apc = influence_graph_all_paths_children(x,y,ig=ig)
  env = new.env()
  env$x.of.y = NULL

  
  children = apc[[x]]
  for (cvar in children) {
    x.of.child = ig$tables[[cvar]][,c(x,cvar)]
    recursive.x.of.y(x=x,y=y,var=cvar, x.of.var = x.of.child, apc=apc,env=env)    
  }
  env$x.of.y
} 

recursive.x.of.y = function(x,y,var,x.of.var, apc, env) {
  restore.point("recursive.x.of.y")
  
  #cat("\n\nx=",x," y=",y, " var = ",var,"\n")
  #print(x.of.var)
  # have reached destination
  if (var == y) {
    if (is.null(env$x.of.y)) {
      env$x.of.y = x.of.var
    } else {
      env$x.of.y = intersect.tables(env$x.of.y, x.of.var)
    }
    return()
  }
  
  children = apc[[var]]
  for (cvar in children) {
    var.of.child = ig$tables[[cvar]][, c(var,cvar)]
    x.of.child = inner_join(x.of.var,var.of.child, by=var)
    x.of.child = x.of.child[,c(x,cvar)]
    recursive.x.of.y(x=x,y=y,var=cvar, x.of.var = x.of.child, apc=apc,env=env)    
  }
}
