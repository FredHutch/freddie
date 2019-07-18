#' Remove columns with specific trash
#'
#' @param trash Character vector containing all the trash values, besides NA, for which a column can be removed
#' @param df Data frame to column filter
#' @param unique Boolean for whether to return only unique rows after column filtering
#' @param requireAll Boolean as to whether you want to require all rows in a column to equal a value in `trash` before filtering the column out (TRUE).
#' @return Returns a columnn filtered data frame
#' @author Amy Paguirigan
#' @examples
#' df <- data.frame(this = c(NA, seq(1, 5, 1), seq(1, 5, 1)), that = rep(0, 11),
#' thisotherThing = rep(NA), ohAndThis = rep(c("trash", "0"), 11))
#' cleanerdf <- dropWhen(df, unique = FALSE, trash = c("0", "trash"), requireAll = FALSE)
#' @export
dropWhen <- function(df, unique = FALSE, trash=NULL, requireAll = TRUE) {

  if(requireAll == FALSE){
    # If all the trash in a column do not have to be the same value in `trash` to be removed
    for(i in trash){
      # For each of the entries in the parameter `trash`
      df[df == trash] <- NA
        # Turn any of the values in `trash` to NA regardless of whether that column might get filtered out or not!!!
        # This could impact the values in portions of the data frame beyond columns that would be filtered out
    }
  }

  if(is.null(trash)==F){
    # if trash is specified, then filter for other column content types too
    for(j in 1:ncol(df)){
      # For all the columns in df
      for(i in trash){
          # for all the values of `trash`
       if(all(df[j,] == i, na.rm = T)) {df[j,] <- NA}
          # set the values of the entire column to NA IF the entire column is equal to that value in `trash`
    }
   }
  }

  filtered <- Filter(function(x)!all(is.na(x)), df)
  # filter the data frame to retain only columns that are not all NA

  if(unique == TRUE) {
    # If unique rows are desired
    filtered <- unique(filtered)
      # only return rows that are unique
  }

  return(filtered)
}

