#' Remove columns with specific trash
#'
#' @param trash Character vector containing all the trash values, besides NA, for which a column can be removed
#' @param df Data frame to column filter
#' @param unique Boolean for whether to return only unique rows after column filtering
#' @param requireAll Boolean as to whether you want to require all rows in a column to equal a value in `trash` before filtering the column out (TRUE).
#' @param except Character vector containing all column names to ignore when dropping columns (these will be retained regardless of the other parameter choices).
#' @return Returns a columnn filtered data frame.  Note, does not take into account numeric, integer or character value differences (it will treat them all the same).
#' @author Amy Paguirigan
#' @examples
#' df <- data.frame(this = c(NA, seq(1, 5, 1), seq(1, 5, 1)), that = rep(0, 11),
#' thisotherThing = rep(NA), ohAndThis = rep(c("trash", "0"), 11))
#' cleanerdf <- dropWhen(df, unique = FALSE, trash = c("0", "trash"), requireAll = FALSE)
#' @export
dropWhen <- function(df, unique = FALSE, trash=NULL, requireAll = TRUE, except = NULL) {
  # Add atemporary index column so in case there are non-unique rows in the original df, the join doesn't break.
  df$dropIndex <- seq(1:nrow(df))
  # save a copy of the index and the extra columns for future re-application, but not the entire thing for memory purposes
  extraColumns <- df[colnames(df) %in% c("dropIndex", except)]
  if(is.null(except)==F){
    df <- df[!colnames(df) %in% except] # Remove the columns we don't to apply dropWhen to.
  }

  if (requireAll == FALSE){
    # If all the values in a column do not have to be the same value in `trash` to be removed, just have to all be ONE of the values,
    for (j in 1:ncol(df)){ # for each column in df
      if (all(df[,j] %in% trash, na.rm = T)) {df[,j] <- NA} # if all the values in the column are in `trash` then set the values to NA so they will be removed later
    }
  }

  if (is.null(trash) == FALSE){
    # if trash is specified, then filter for other column content types too
    for (j in 1:ncol(df)){
      # For all the columns in df
      for (i in trash){
        # for all the values of `trash`
        if (all(df[,j] == i, na.rm = T)) {df[,j] <- NA}
        # set the values of the entire column to NA IF the entire column is equal to that value in `trash`
      }
    }
  }

  filtered <- Filter(function(x)!all(is.na(x)), df)
  # filter the data frame to retain only columns that are not all NA

  fullframe <- suppressMessages(dplyr::left_join(filtered, extraColumns))

  if(unique == TRUE) {
    # If unique rows are desired
    fullframe <- unique(fullframe)
    # only return rows that are unique
  }
  fullframe$dropIndex <- NULL

  return(fullframe)
}

