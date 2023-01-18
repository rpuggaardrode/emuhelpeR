#' Processing fundamental frequency and dependencies
#'
#' `f0_proc` performs automatic outlier removal of fundamental frequency
#' values and dependent values, normalizes the resulting values, and rescales to
#' the original frequency scale.
#'
#' Zero values are recoded as `NA`. Values are
#' recoded as `NA` if they fall outside of three standard deviations of the mean
#' value within the same group. If the data contain other measures that are
#' dependent on F0, e.g. spectral measures, then the corresponding values
#' of these are also coded as `NA`.
#'
#' `f0_proc` also calculates by-speaker z-score normalization and stores it
#' in a separate column, then rescales the z-scores based on the overall mean
#' and SD values.
#'
#' Optionally, values that are measured too far away from a boundary specified
#' by the user are coded as `NA`. This option assumes that the data frame
#' was generated using the [emuR::get_trackdata()] function to import data
#' from an EMU-SDMS database.
#'
#' @param df A data frame containing a column with fundamental frequency
#' measurements and columns with any other information to be accounted for.
#' @param f0col A string containing the name of the column in `df` that contains
#' fundamental frequency values. Default is `F0`.
#' @param dep One or more strings containing the name of columns in `df`
#' containing measures that are directly dependent on fundamental frequency.
#' Optional; default is `NULL`.
#' @param speaker An optional string giving the name of the column in `df`
#' containing speakers id's. Default is `NULL`, in which case z-score
#' normalization will be done on the basis of the data at large and no rescaled
#' values will be returned.
#' @param group_var One or more strings containing the names of columns in `df`
#' to be used as grouping variables for automatic outlier removal.
#' Optional; default is `NULL`, in which case automatic outlier removal will
#' be based on means and standard deviation in the data at large.
#' @param timing_rm An optional list with two arguments:
#' * A string containing the label associated with a boundary in the data.
#' Values that are measured sufficiently far from this boundary are recoded as
#' `NA`. `f0_proc` assumes that this label is stored in the column `labels` in
#' a data frame generated with [emuR::get_trackdata()].
#' * A number indicating the distance from this boundary at which measurements
#' should be ignored. See example below.
#'
#' @return A data frame identical to `df` with outliers coded as `NA` and
#' with the added columns `zF0` and `normF0`, and corresponding column for
#' values that depend on F0. The column with F0 values are renamed `F0`.
#' @seealso Some of the functionality in `f0_proc` assumes that the user has
#' generated the raw data using [emuR] and EMU-SDMS.
#'
#' `f0_proc` is incorporated in the function [import_ssfftracks()] which
#' automatically imports F0 values and other available measures from an EMU
#' database, performs outlier removal, normalizes values, and rescales.
#' @export
#'
#' @examples
#' colnames(ssff_data)
#' head(ssff_data$praatF0, 20)
#' head(ssff_data$H1H2c, 20)
#' sum(is.na(ssff_data$praatF0))
#' sum(is.na(ssff_data$H1H2c))
#' #Uses both speaker and vowel variables for outlier removal; F0 values and
#' #dependents are recoded as NA if they are more than 250 ms from boundaries
#' #labeled 'cl'.
#' x <- f0_proc(df=ssff_data, f0col='praatF0', dep='H1H2c', speaker='speaker',
#' group_var=c('speaker', 'vowel'), timing_rm=list('cl', 250))
#' colnames(x)
#' head(x$F0, 20)
#' head(x$zF0, 20)
#' head(x$normF0, 20)
#' head(x$zH1H2c, 20)
#' head(x$normH1H2c, 20)
#' sum(is.na(x$F0))
#' sum(is.na(x$H1H2c))
f0_proc <- function(df,
                    f0col='F0',
                    dep=NULL,
                    speaker=NULL,
                    group_var=NULL,
                    timing_rm=NULL) {

  spec_cols <- c(f0col, dep, speaker, group_var)
  avail_cols <- colnames(df)
  if (any(!(spec_cols %in% avail_cols))){
    msng <- spec_cols[which(!(spec_cols %in% avail_cols))]
    stop('The following arguments are not available in the data frame: \n', msng)
  }

  rowna <- which(df[,f0col]==0)
  df[rowna,f0col] <- NA

  df$F0 <- df[[f0col]]
  if (f0col != 'F0') {
    df <- df[,-which(names(df)==f0col)]
  }

  if (!is.null(timing_rm)) {
    if(length(timing_rm) != 2) {
      stop('timing_rm must take two arguments, one indicating the transcription label
           used for automated time-based removal, and one indicating the
           duration limit above which measures must be removed.')
    } else {
      df <- df %>%
        dplyr::group_by(.data$session, .data$sl_rowIdx) %>%
        dplyr::mutate(times_rel = ifelse(labels==timing_rm[[1]], -rev(.data$times_rel), .data$times_rel)) %>%
        dplyr::mutate(F0 = ifelse(.data$times_rel < -timing_rm[[2]], NA, .data$F0))
    }
  }

  if(!is.null(group_var)){
    df <- df %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
      dplyr::mutate(uppF0 = mean(.data$F0, na.rm=T) + 3*stats::sd(.data$F0, na.rm=T),
                    lowF0 = mean(.data$F0, na.rm=T) - 3*stats::sd(.data$F0, na.rm=T))
  } else {
    df <- df %>% dplyr::mutate(uppF0 = mean(.data$F0, na.rm=T) + 3*stats::sd(.data$F0, na.rm=T),
                               lowF0 = mean(.data$F0, na.rm=T) - 3*stats::sd(.data$F0, na.rm=T))
  }

  df <- df %>% dplyr::mutate(F0 = ifelse((.data$F0 > .data$uppF0 | .data$F0 < .data$lowF0), NA, .data$F0))
  df <- normz(df, 'F0', speaker)

  if (!is.null(dep)) {
    for(d in dep) {
      df[[d]] <- ifelse(is.na(df[['F0']]), NA, df[[d]])
      df <- normz(df, d, speaker)
    }
  }

  return(df)

}
