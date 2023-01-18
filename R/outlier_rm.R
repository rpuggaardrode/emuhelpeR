#' Automated removal of outliers by group
#'
#' Recodes values as `NA` if they fall outside of three standard deviations of
#' the mean value within the same grouping variable.
#'
#' @param df A data frame.
#' @param var A string corresponding to a column name in `df` where outliers
#' should be removed.
#' @param group_var One or more strings containing the names of columns in `df`
#' to be used as grouping variables.
#' Optional; default is `NULL`, in which case automatic outlier removal will
#' be based on means and standard deviation in the data at large.
#' @param report A Boolean. If `TRUE` (default), prints a message stating
#' how many values were recoded as `NA`.
#'
#' @return A data frame identical to `df` with outliers recoded as `NA`.
#' @export
#'
#' @examples
#' sum(is.na(ssff_data$praatF0))
#' x <- outlier_rm(df=ssff_data, var='praatF0', group_var=c('speaker', 'vowel'))
#' sum(is.na(x$praatF0))
outlier_rm <- function(df,
                       var,
                       group_var=NULL,
                       report=TRUE) {

  if (!is.null(group_var)) {
    df <- df %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
      dplyr::mutate('upp{var}' := mean(!!as.name(var), na.rm=T) + 3*stats::sd(!!as.name(var), na.rm=T),
                    'low{var}' := mean(!!as.name(var), na.rm=T) - 3*stats::sd(!!as.name(var), na.rm=T))
  } else {
    df <- df %>%
      dplyr::mutate('upp{var}' := mean(!!as.name(var), na.rm=T) + 3*stats::sd(!!as.name(var), na.rm=T),
                    'low{var}' := mean(!!as.name(var), na.rm=T) - 3*stats::sd(!!as.name(var), na.rm=T))
  }

  df <- df %>%
    dplyr::mutate(
      '{var}' := ifelse(!!as.name(var) > !!as.name(paste0('upp', var)) |
                          !!as.name(var) < !!as.name(paste0('low', var)),
                        NA, !!as.name(var))
    )

  if(report) {
    print(paste0('The number of NAs removed during automated outlier removal: ',
                 sum(is.na(df[[var]]))))
  }

  return(df)

}
