#' Run PraatSauce in bulk with different arguments for different speakers
#'
#' Shell wrapper to run PraatSauce in bulk with different arguments for
#' different speakers, fx with varying pitch floor and ceiling or varying
#' formant references.
#'
#' @param directory Root directory containing subdirectories for individual
#' speakers to be measured. Must be the full name, there's no
#' path extension.
#' @param dyn_args A named list or data frame containing dynamic arguments. If a
#' list is provided, the first element should be a vector called `speakers`, and
#' other elements should be vectors with arguments passed to [run_praatsauce()].
#' If a data frame is provided, the first column should be called `speakers`,
#' and the other columns should have named that can be passed to
#' [run_praatsauce()].
#' @param ... Other arguments passed to [run_praatsauce()], which are the
#' same for all speakers.
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
#' The function [run_ps_dynamic_minmax()] will dynamically estimate suitable
#' pitch floors and ceilings for a number of speakers.
#'
#' The function [praatsauce2ssff()] will convert measures from PraatSauce to
#' SSFF files and add them to EMU database.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' da <- list(speakers=c('a', 'b', 'c'), f0min=c(120, 150, 80),
#' f0max=c(300, 400, 250))
#' bulk_run_praatsauce(directory='my_directory', dyn_args=da)
#' }
bulk_run_praatsauce <- function(directory, dyn_args, ...) {
  extra_args <- list(...)
  if (is.data.frame(dyn_args)) dyn_args <- as.list(dyn_args)
  if (any(!names(dyn_args)[-1] %in% names(as.list(args(run_praatsauce))))) {
    stop('Some of the provided dynamic arguments are not known PraatSauce arguments')
  }

  dirs <- list.dirs(directory, full.names=FALSE, recursive=FALSE)
  d <- which(dirs==dyn_args$speakers[1])
  args <- sapply(dyn_args, '[[', d)[-1]
  speaker_dir <- paste0(directory, '/', dyn_args$speakers[1])
  sd <- list(wav_loc=speaker_dir)
  all_args <- c(sd, args, extra_args)
  psout <- do.call(run_praatsauce, all_args)

  for (s in dyn_args$speakers[-1]) {
    i <- which(dirs==s)
    args <- sapply(dyn_args, '[[', d)[-1]
    speaker_dir <- paste0(directory, '/', s)
    sd <- list(wav_loc=speaker_dir)
    all_args <- c(sd, args, extra_args)
    tmp <- do.call(run_praatsauce, all_args)
    psout <- rbind(psout, tmp)
  }
  return(psout)
}
