#' Processing formants and dependencies
#'
#' `fn_proc` performs automatic outlier removal of formant
#' values and dependent values, normalizes the resulting values, and rescales to
#' the original frequency scale.
#'
#' Zero values are recoded as `NA`. Values are
#' recoded as `NA` if they fall outside of three standard deviations of the mean
#' value within the same group. If the data contain other measures that are
#' dependent on specific formant values, e.g. spectral measures, then the
#' corresponding values of these are also coded as `NA`. This assumes that
#' these spectral measures are also dependent on F0, and that F0 has already
#' been processed using [f0_proc()]; this will also ensure that variables are
#' labeled correctly. Spectral measures which depend on e.g. F1 will be recoded
#' as `NA` if either the corresponding F0 measurement or the corresponding F1
#' measurement are labeled `NA`.
#'
#' `fn_proc` also calculates by-speaker z-score normalization and stores it
#' in a separate column, then rescales the z-scores based on the overall mean
#' and SD values.
#'
#' `fn_proc` is intended for formants, but values other than formants can
#' also be passed to the function if the user wishes to perform automated
#' outlier removal and/or normalization and rescaling on those.
#'
#' @param df #' @param df A data frame containing one or more columns with
#' formant measurements and columns with any other information to be accounted
#' for.
#' @param fncol One or more strings containing the names of columns in `df` that
#' contain formant values. Default is `c('F1', 'F2')`.
#' @param f0col A string containing the name of the column in `df` that contains
#' fundamental frequency values. Default is `F0`; this column will automatically
#' be renamed `F0` if the data has already been processed using [f0_proc()].
#' Only used if formant dependencies are also being processed; see `fndep`.
#' @param fndep A list where each element contains two strings:
#' * The name of a measure that is directly dependent on a specific formant.
#' * Which formant the measure is dependent on.
#'
#' The structure is as follows:
#' `fndep=list(c('F1dep', 'F1'), c('F2dep', 'F2')`
#'
#' Optional; default is `NULL`.
#' @param speaker An optional string giving the name of the column in `df`
#' containing speakers id's. Default is `NULL`, in which case z-score
#' normalization will be done on the basis of the data at large and no rescaled
#' values will be returned.
#' @param group_var One or more strings containing the names of columns in `df`
#' to be used as grouping variables for automatic outlier removal.
#' Optional; default is `NULL`, in which case automatic outlier removal will
#' be based on means and standard deviation in the data at large.
#' @param report A Boolean. If `TRUE` (default), prints a message stating
#' how many values in each track were
#' recoded as `NA` during automated outlier removal.
#'
#' @return A data frame identical to `df` with formant outliers coded as `NA` and
#' with the added columns `zF{n}` and `normF{n}`, and corresponding columns for
#' values that depend on formants. The columsn with formant values are renamed
#' `F1`, `F2`, etc.
#' @seealso `fn_proc` should be functional regardless of how the raw data is
#' calculated, but the function is intended for raw data generated using [emuR]
#' and EMU-SDMS.
#'
#' Some of the functionality in `fn_proc` assumes that the data has already
#' been processed using [f0_proc()].
#'
#' `fn_proc` is incorporated in the function [import_ssfftracks()] which
#' automatically imports formant values and other available measures from an EMU
#' database, performs outlier removal, normalizes values, and rescales.
#'
#' More information about the example data can be found in [seg_list] and
#' [ssff_data].
#'
#' This general-purpose function was adapted from the data processing used in
#' the following paper:
#'
#' Kirby, James, Marc Brunelle & Pittayawat Pittayaporn (forthc.)
#' Transphonologization of onset voicing: Revisiting Northern and Eastern
#' Kmhmu. To be published in Phonetica.
#'
#' <https://doi.org/10.17605/OSF.IO/WV6QZ>
#' @export
#'
#' @examples
#' colnames(ssff_data)
#' head(ssff_data$praatF3, 20)
#' head(ssff_data$H1A3c, 20)
#' sum(is.na(ssff_data$praatF0))
#' sum(is.na(ssff_data$praatF3))
#' sum(is.na(ssff_data$H1A3c))
#' #see ?f0_proc
#' y <- f0_proc(df=ssff_data, f0col='praatF0', dep='H1H2c', speaker='speaker',
#' group_var=c('speaker', 'vowel'), timing_rm=list('cl', 250))
#' x <- fn_proc(df=y, fncol=c('praatF1', 'praatF2', 'praatF3'),
#' fndep=list(c('H1A1c', 'F1'), c('H1A3c', 'F3')),
#' speaker='speaker', group_var=c('speaker', 'vowel'))
#' head(x$F3, 20)
#' head(x$H1A3c, 20)
#' head(x$zF3, 20)
#' head(x$normF3, 20)
#' sum(is.na(x$F0))
#' sum(is.na(x$F3))
#' sum(is.na(x$H1A3c))
fn_proc <- function(df,
                    fncol=c('F1', 'F2'),
                    f0col='F0',
                    fndep=NULL,
                    speaker=NULL,
                    group_var=NULL,
                    report=TRUE){

  deps <- unlist(fndep)[which(1:length(unlist(fndep)) %% 2 != 0)]
  spec_cols <- c(fncol, deps, speaker, group_var)
  avail_cols <- colnames(df)
  if (any(!(spec_cols %in% avail_cols))){
    msng <- spec_cols[which(!(spec_cols %in% avail_cols))]
    stop('The following arguments are not available in the data frame: \n', msng)
  }
  if(!is.null(fndep) && !(f0col %in% avail_cols)) {
    stop('Please provide a valid column with F0 measurements.')
  }

  for (f in 1:length(fncol)){

    if (fncol[1] != 'F1'){
      df[[paste0('F', f)]] <- df[[fncol[f]]]
      fn <- colnames(df)[ncol(df)]
    } else {
      fn <- fncol[f]
    }

    df <- outlier_rm(df, paste0('F', f), group_var, report=F)
    if (report) {
      print(paste0('Number of NAs removed from F', f,
                   ' track during automated outlier removal: ',
                   sum(is.na(df[[paste0('F', f)]]))))
    }
    df <- normz(df, paste0('F', f), speaker)

  }

  if (!is.null(fndep)) {
    for (d in fndep) {
      dep <- d[1]
      depfn <- d[2]
      df[[dep]] <- ifelse(is.na(df[[f0col]]) | is.na(df[[depfn]]), NA, df[[dep]])
      if (report) {
        print(paste0('Number of NAs removed from ',
                     dep, ' track during automated outlier removal: ',
                     sum(is.na(df[[dep]]))))
      }
      df <- normz(df, dep, speaker)
    }
  }

  if (fncol[1] != 'F1'){
    df <- df[,-which(names(df) %in% fncol)]
  }

  return(df)

}
