#' Normalize by speaker and rescale
#'
#' Performs by-speaker z-score normalization on a given variable and stores it
#' in a separate column, then rescales the z-scores based on the overall mean
#' and SD values in the data.
#'
#' @param df A data frame containing a column with the variable to be normalized
#' and rescaled, and optionally a column containing a unique id for each speaker.
#' @param var A string giving the name of the column in `df` containing the
#' continuous variable to be normalized.
#' @param speaker An optional string giving the name of the column in `df`
#' containing speakers id's. Default is `NULL`, in which case z-score
#' normalization will be done on the basis of the data at large and no rescaled
#' values will be returned.
#'
#' @return A data frame identical to `df` with the added columns `z{var}` and
#' `norm{var}`.
#' @seealso More information about the example data can be found in [seg_list] and
#' [ssff_data].
#' @export
#'
#' @examples
#' colnames(ssff_data)
#' unique(ssff_data$speaker)
#' head(ssff_data$eggF0, 20)
#' x <- normz(df=ssff_data, var='eggF0', speaker='speaker')
#' colnames(x)
#' head(x$zeggF0, 20)
#' head(x$normeggF0, 20)
normz <- function(df, var, speaker=NULL) {

  if(!is.null(speaker)) {
    df <- df %>% dplyr::group_by(!!as.name(speaker)) %>%
      dplyr::mutate('z{var}' := as.vector(scale(!!as.name(var)))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate('norm{var}' := mean(!!as.name(var), na.rm=T) +
                      !!as.name(paste0('z', var)) * stats::sd(!!as.name(var), na.rm=T))
  } else {
    df <- df %>%
      dplyr::mutate('z{var}' := as.vector(scale(!!as.name(var))))
  }

  return(df)

}
