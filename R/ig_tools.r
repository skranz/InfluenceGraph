examples.intersect.list = function() {
  intersect.list(list(1:10,c(3,6,8,10),7:17))
}

intersect.list = function(li) {
   args <- li
   nargs <- length(args)
   if(nargs <= 1) {
     stop("cannot evaluate intersection fewer than 2 arguments")
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
