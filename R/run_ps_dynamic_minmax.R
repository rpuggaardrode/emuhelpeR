#' Run PraatSauce with dynamically estimated pitch floor and ceiling
#'
#' Shell wrapper to run PraatSauce for multiple speakers with dynamically
#' estimated F0 limits, following De Looze and Hirst (2021).
#' The function will first call PraatSauce to estimate F0 only with pitch
#' floor at 60 Hz and pitch ceiling at 700 Hz. The first and third quantiles
#' are calculated for each speaker, and PraatSauce is rerun with supplied
#' arguments and pitch floor estimated as 3/4 of the first quantile of the
#' first pass, and pitch ceiling as 1½ of the third quantile of the first pass.
#'
#' @param directory Root directory containing subdirectories for individual
#' speakers to be measured. Must be the full name, there's no
#' path extension.
#' @param formantMeasures Boolean; should formants be measured in the second
#' pass?
#' @param spectralMeasures Boolean; should other spectral mesures be calculated
#' in the second pass?
#' @param ... Further arguments passed to [run_praatsauce].
#'
#' @return A data frame with columns giving information about the
#' sound file imported from the name of the sound file, information about
#' the time at which measures were estimated, and the estimated measures
#' themselves.
#' @seealso As mentioned above, this function is a wrapper for a suite of
#' Praat scripts. More information about the scripts can be found
#' on GitHub: <https://github.com/kirbyj/praatsauce>. Information about the
#' VoiceSauce scripts which PraatSauce is based on can be found here:
#' <http://www.phonetics.ucla.edu/voicesauce/>.
#'
#' The function [bulk_run_praatsauce()] will run PraatSauce in bulk
#' for multiple speakers with arguments supplied by the user.
#'
#' The function [praatsauce2ssff()] will convert measures from PraatSauce to
#' SSFF files and add them to EMU database.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming the working directory meets the requirements mentioned above
#' ps_out <- run_ps_dynamic_minmax(getwd())
#' }
run_ps_dynamic_minmax <- function(directory, formantMeasures=TRUE,
                                  spectralMeasures=TRUE,
                                  min_multiplier=0.75,
                                  max_multiplier=1.5, ...) {
  fm <- formantMeasures
  sm <- spectralMeasures
  speakers <- list.dirs(directory, full.names=T, recursive=F)
  ps_dyn <- dynamic_minmax(fm=fm, sm=sm,
                           min_multiplier=min_multiplier,
                           max_multiplier=max_multiplier,
                           wav_loc=speakers[1], ...)
  for (s in speakers[-1]) {
    tmp <- dynamic_minmax(fm=fm, sm=sm,
                          min_multiplier=min_multiplier,
                          max_multiplier=max_multiplier,
                          wav_loc=s, ...)
    ps_dyn <- rbind(ps_dyn, tmp)
  }
  return(ps_dyn)
}
