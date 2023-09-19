#' Run PraatSauce from R
#'
#' Shell wrapper to run the PraatSauce suite of scripts written by James Kirby
#' for the open source signal processing software Praat. The results should be
#' very similar to the suite of MATLAB scripts called VoiceSauce. Computes fundamental
#' frequency, three formants, formant bandwidths, and a range of measures of
#' spectral slope and harmonics-to-noise ratio.
#'
#' @param wav_loc String giving the name of a directory where the .wav files
#' to be processed are stored. Must be the full name, there's no
#' path extension.
#' @param tg_loc String giving the name of a directory where the .TextGrid files
#' to be processed are stored. Default is `NULL`; if no directory is provided,
#' `run_praatsauce` assumes that .TextGrid files are stored in the same directory
#' as `wav_loc`.
#' @param out_loc String giving the name of a directory where the PraatSauce
#' output should be stored. Default is `NULL`; if no directory is provided,
#' the output is stored in a temporary folder.
#' @param out_file String giving the desired name of the PraatSauce output
#' file. Default is `tmp.txt` (assuming that the file is only stored in a
#' temporary directory anyway).
#' @param praat_loc String giving the location of the Praat executable on
#' the user's machine. Default is `praat`, which should be sufficient if
#' `praat.exe` is stored in the default location for executables on the user's
#' machine, such as the `system32` folder on Windows.
#' @param channel Number of the channel to be analyzed. Default is `1`;
#' use `0` for stereo.
#' @param interval_tier Number of the interval tier to query. Default is `1`.
#' @param skip_these_labels String containing a well-formed regex giving
#' interval labels on `interval_tier` to ignore. Default is `^$`, in which case
#' empty labels are ignored.
#' @param point_tier Number of a point tier to query. Default is `NULL` (no
#' point tier is queried).
#' @param point_tier_labels A single string containing labels of interest in
#' `point_tier` separated by spaces. Default is `NULL`.
#' @param separator String giving a character that separates variables in
#' filenames. Default is `_`.
#' @param measure String giving the desired unit of distance between measures.
#' Default is `ms`, in which case measures are taken at every n ms, set by
#' `points`; another option is `equidist`, in which case measures are taken at
#' n equidistant points (also set by `points`).
#' @param points Distance between measures in the units given in `measure`.
#' Default is `5`.
#' @param resample_to_16k Boolean; should recordings be resampled to 16 kHz
#' prior to analysis? Default is `TRUE`.
#' @param pitchTracking Boolean; should pitch be estimated? Default is `TRUE`.
#' @param formantMeasures Boolean; should formants be estimated? Default is
#' `TRUE`.
#' @param spectralMeasures Boolean; should other spectral measures be estimated?
#' Default is `TRUE`.
#' @param windowLength Length of the analysis window. Default is `0.025`.
#' @param windowPosition Position of the analysis window. Default is `0.5`.
#' @param maxFormantHz Maximum frequency in Hz to be considered in formant
#' estimation. Default is `5000`.
#' @param f0min Lower limit to estimated fundamental frequency in Hz. Default is
#' `50`.
#' @param f0max Upper limit to estimated fundamental frequency in Hz. Default is
#' `300`.
#' @param timeStep Time step used to determine how close the analysis frame are
#' for formant measurement. Default is `NULL`, in which case 1/4 of the window
#' is used.
#' @param maxNumFormants Maximum number of formants to be estimated. Default is
#' `5`.
#' @param preEmphFrom Pre-emphasis begins at this frequency in Hz; default is
#' `50`.
#' @param formantTracking Boolean; should formant tracks be smoothed? Default
#' is `TRUE`.
#' @param F1ref Reference value for first formant used for formant smoothing.
#' Default is `500`.
#' @param F2ref Reference value for second formant used for formant smoothing.
#' Default is `1500`.
#' @param F3ref Reference value for third formant used for formant smoothing.
#' Default is `2500`.
#' @param useBandwidthFormula Boolean; should formant bandwidths be estimated
#' by the Hawks and Miller formula? Default is `FALSE`, in which case
#' Praat's estimates are used. Note that the Hawks and Miller formula can only
#' be used if pitch is also measured.
#'
#' @return A data frame containing with columns giving information about the
#' sound file imported from the name of the sound file, information about
#' the time at which measures were estimated, and the estimated measures
#' themselves.
#' @seealso As mentioned above, this function is a wrapper for a suite of
#' Praat scripts. More information about the scripts can be found
#' on GitHub: <https://github.com/kirbyj/praatsauce>. Information about the
#' VoiceSauce scripts which PraatSauce is based on can be found here:
#' <http://www.phonetics.ucla.edu/voicesauce/>.
#'
#' The function [praatsauce2ssff()] will convert measures from PraatSauce to
#' SSFF files and add them to EMU database.
#' @export
#'
#' @examples
#' # Not right now
#' 2+2
run_praatsauce <- function(wav_loc,
                           tg_loc=NULL,
                           out_loc=NULL,
                           out_file='tmp.txt',
                           praat_loc='praat',
                           channel=1,
                           run_whole_file=TRUE,
                           interval_tier=1,
                           skip_these_labels='^$',
                           point_tier=NULL,
                           point_tier_labels=NULL,
                           separator='_',
                           measure='ms',
                           points=5,
                           resample_to_16k=TRUE,
                           pitchTracking=TRUE,
                           formantMeasures=TRUE,
                           spectralMeasures=TRUE,
                           windowLength=0.025,
                           windowPosition=0.5,
                           maxFormantHz=5000,
                           f0min=50,
                           f0max=300,
                           timeStep=NULL,
                           maxNumFormants=5,
                           preEmphFrom=50,
                           formantTracking=TRUE,
                           F1ref=500,
                           F2ref=1500,
                           F3ref=2500,
                           useBandwidthFormula=FALSE) {

  if (!(measure %in% c('ms', 'equidist'))) {
    stop('The value of measure should be either ms or equidist')
  } else if (measure == 'ms') {
    measure <- '"every n milliseconds"'
  } else if (measure == 'equidist') {
    measure <- '"n equidistant points"'
  }

  if (run_whole_file) {
    fls <- list.files(wav_loc, pattern='*.wav') %>%
      stringr::str_remove_all('.wav')

    for (f in fls) {
      snd <- rPraat::snd.read(paste0(wav_loc, '/', f, '.wav'))
      rPraat::tg.createNewTextGrid(0, snd$duration) %>%
        rPraat::tg.insertNewIntervalTier(newTierName='dummy') %>%
        rPraat::tg.setLabel(1, 1, f) %>%
        rPraat::tg.write(paste0(wav_loc, '/', f, '.TextGrid'), format='text')
    }
  }

  point_tier <- as.numeric(point_tier)
  resample_to_16k <- as.numeric(resample_to_16k)
  pitchTracking <- as.numeric(pitchTracking)
  formantMeasures <- as.numeric(formantMeasures)
  spectralMeasures <- as.numeric(spectralMeasures)
  formantTracking <- as.numeric(formantTracking)
  useBandwidthFormula <- as.numeric(useBandwidthFormula)

  for (bool in c(point_tier, resample_to_16k, pitchTracking,
                 formantMeasures, spectralMeasures,
                 formantTracking, useBandwidthFormula)) {
    bool <- as.numeric(bool)
  }

  if (length(point_tier_labels) == 0) {
    point_tier_labels <- 0
  }

  if (length(timeStep) == 0) {
    timeStep <- 0
  }

  if (length(tg_loc) == 0) {
    tg_loc <- wav_loc
  }

  if (length(out_loc) == 0) {
    out_loc <- tempdir()
  }

  if (length(point_tier) == 0) {
    point_tier <- 0
  }

  if (stringr::str_sub(out_loc, start=-1) != '/') {
    out_loc <- paste0(out_loc, '/')
  }

  ps <- system.file('extdata/ps-script/shellSauce.praat', package='emuhelpeR')
  call <- stringr::str_glue('{praat_loc} {ps} {wav_loc} {tg_loc} {out_loc} {out_file} ',
                            '1 {channel} {interval_tier} "{skip_these_labels}" {point_tier} ',
                            '{point_tier_labels} "{separator}" 0 {measure} {points} ',
                            '{resample_to_16k} {pitchTracking} {formantMeasures} ',
                            '{spectralMeasures} {windowLength} {windowPosition} ',
                            '{maxFormantHz} 0.005 0 {f0min} {f0max} 0 {timeStep} ',
                            '{maxNumFormants} {preEmphFrom} {formantTracking} {F1ref} ',
                            '{F2ref} {F3ref} 0 0 {useBandwidthFormula}')

  system(call)
  out <- utils::read.csv(paste0(out_loc, out_file))
  return(out)

  if (run_whole_file){
    list.files(wav_loc, pattern='*.TextGrid', full.names=T) %>% file.remove()
  }

}

dynamic_minmax <- function(...) {
  tmp <- run_praatsauce(formantMeasures=F, spectralMeasures=F, ...)
  tmp$f0[which(tmp$f0 == 0)] <- NA
  q <- quantile(tmp$f0, probs=c(0.25, 0.75), na.rm=T, names=F)
  tmp_dyn <- run_praatsauce(f0min=q[1], f0max=q[2], ...)
  return(tmp_dyn)
}

run_ps_dynamic_minmax <- function(directory, ...) {
  speakers <- list.dirs(directory, full.names=T, recursive=F)
  ps_dyn <- dynamic_minmax(wav_loc=speakers[1])
  for (s in speakers[-1]) {
    tmp <- dynamic_minmax(wav_loc=s)
    ps_dyn <- rbind(ps_dyn, tmp)
  }
  return(ps_dyn)
}
