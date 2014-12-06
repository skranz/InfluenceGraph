examples.join_or_expand = function() {
  x = data_frame(D=1:5, G=c(0,0,0,10,10))
  y = data_frame(F=c(0,0,0,10,10))
  inner_join(x,y)
  join_or_expand(Ft,Yt)
  
}

seq_along_rows = function(x) {
  if (NROW(x)>0) return(1:NROW(x))
  numeric(0)
}

seq_along_cols = function(x) {
  if (NCOL(x)>0) return(1:NCOL(x))
  numeric(0)
}

join_tables = function(tables, unique=TRUE) {
  restore.point("join_tables")
  if (length(tables)==0) return(NULL)
  if (length(tables)==1)
    return(unique(tables[[1]]))
  
  tab = join_or_expand(x=tables[[1]],y=tables[[2]], unique=unique)
  
  for (i in setdiff(seq_along(tables),1:2)) {
    tab = join_or_expand(x=tab,y=tables[[i]], unique=unique)
  }
  tab  
} 

join_or_expand = function(x,y,by=intersect(colnames(x),colnames(y)), unique=TRUE) {
  if (length(by)>0) {
    ret = inner_join(x,y,by=by)
  } else {
    ind = expand.grid(list(y=seq_along_rows(y),x=seq_along_rows(x)))
    ret = cbind(x[ind$x,],y[ind$y,])
  }
  if (unique) {
    ret= unique(ret)
    rownames(ret) = NULL
  }
  ret
}

examples.intersect.list = function() {
  intersect.list(list(1:10,c(3,6,8,10),7:17))
}

intersect.list = function(li) {
   args <- li
   nargs <- length(args)
   if(nargs == 1) {
     return(args[[1]])
   } else if (nargs==0) {
     return(NULL)
   } else if(nargs == 2) {
     intersect(args[[1]], args[[2]])
   } else {
     intersect(args[[1]], intersect.list(args[-1]))
   }
}

#' Return common rows of tab1 and tab2
intersect.tables = function(tab1, tab2) {
  restore.point("intersect.tables")
  tab = rbind(tab1, tab2)
  tab[duplicated(tab),]
}

#' Filter those rows of df which are in filter.df
#'
#' Uses only the columns of filter.df for the filter operation
#' but returning all column of df
filter.with.table = function(df, filter.df) {
  restore.point("filter.with.table")
  if (NROW(filter.df)==0)
    return(df[integer(0),])
  
  tab = rbind(filter.df, df[,colnames(filter.df)])
  dupl = duplicated(tab)[-(1:NROW(filter.df))]
  df[dupl,]
}

#' case_distinction
case_distinction <- function(...) {
  args = list(...)
  
  restore.point("case_distinction")
  n = length(args)
  n = 3
  val.ind = seq(1,n,by=2)
  cond.ind = seq(2,n,by=2)

  vals = args[val.ind]
  cond = args[cond.ind]
  nv = length(vals) 
  
  len = max(sapply(cond,length),sapply(vals,length))
  v = rep(vals[[nv]], length.out=len)
  if (nv >1) {
    for (i in (nv-1):1) {
      rows = which(rep(cond[[i]], length.out=len))
      v[rows] = rep(vals[[i]], length.out=len)[rows]
    }
  }
  v
}