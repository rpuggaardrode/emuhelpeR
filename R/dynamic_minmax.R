#' Helper function for running PraatSauce with dynamic pitch floor and ceiling
#'
#' Shell wrapper calling PraatSauce to measure F0 in order to determine
#' suitable minimum and maximum limits, then reruns PraatSauce with supplied
#' arguments and the new F0 limits. Usually called from
#' [run_ps_dynamic_minmax].
#'
#' @param fm Boolean; should formants be measured in the second pass?
#' @param sm Boolean; should spectral measures be taken in the second pass?
#' @param min_multiplier Numeric; what should the first quartile be
#' multiplied with in the second pass? Default is `0.75`.
#' @param max_multiplier Numeric; what should the third quartile be
#' multiplied with in the second pass? Default is `1.5`.
#' @param ... Further arguments passed to [run_praatsauce].
#'
#' @return A data frame with columns giving information about the
#' sound file imported from the name of the sound file, information about
#' the time at which measures were estimated, and the estimated measures
#' themselves.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Don't run directly, see `help(run_ps_dynamic_minmax)`
#' }
dynamic_minmax <- function(fm=TRUE, sm=TRUE, min_multiplier=0.75,
                           max_multiplier=1.5, ...) {
  tmp <- run_praatsauce(formantMeasures=F, spectralMeasures=F,
                        f0min=60, f0max=700, ...)
  tmp$f0[which(tmp$f0 == 0)] <- NA
  q <- stats::quantile(tmp$f0, probs=c(0.25, 0.75), na.rm=T, names=F)
  tmp_dyn <- run_praatsauce(f0min=min_multiplier*q[1],
                            f0max=max_multiplier*q[2],
                            formantMeasures=fm, spectralMeasures=sm, ...)
  return(tmp_dyn)
}

