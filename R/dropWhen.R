#' Remove columns with specific values
#'
#' @param values Character vector containing all the values besides NA for which a column will be removed
#' @param df Data frame to column filter
#' @param unique Boolean for whether to return only unique rows after column filtering
#' @param requireAll Boolean as to whether you want to require all rows in a column to equal a value in `values` before filtering the column out.
#' @return Returns a columnn filtered data frame
#' @author Amy Paguirigan
#' @examples
#' df <- data.frame(this = c(NA, seq(1, 10, 1)), that = rep(0, 11), thisotherThing = rep(NA))
#' cleaner <- dropWhen(df, unique = FALSE, values = c("0"), requireAll = FALSE)
#' @export
dropWhen <- function(df, unique = FALSE, values, requireAll = TRUE) {

  if(requireAll == FALSE){
    for(i in values){
      df[df == values] <- NA
    }
  }
  for(j in 1:ncol(df)){
    for(i in values){
     df$j <- ifelse(all(df$j==i)==T, NA, df$j)
    }
  }

  filtered <- Filter(function(x)!all(is.na(x)), df)
  if(unique == TRUE){
    filtered <- unique(filtered)
  }

  return(filtered)
}

