#' Calculate first four DCT coefficients as time series
#'
#' Given a vector of numbers corresponding to a sound file, will generate
#' DFT spectra over equidistant time steps and calculate the first four
#' DCT coefficients of each spectrum.
#'
#' @param snd_vector A vector corresponding to a sound file.
#' @param sr Sample rate of `snd_vector`.
#' @param freq_ceiling Maximum frequency to be included in the spectra.
#' Default is `8000`, in which case the `snd_vector` is resampled to 16 kHz.
#' @param step Duration of time steps over which to generate spectra in ms.
#' Default is `5`
#' @param plot Logical; whether or not to plot the resulting time series.
#' Default is `TRUE`.
#'
#' @return A data frame with one column `t` indicating time, and one column
#' for each DCT coefficient `k0`, `k1`, `k2`, `k3`.
#' @seealso This function is called by [dct2ssff()] which generates these
#' time series for each sound file in an EMU-SDMS data base.
#'
#' Spectra are generated using [phonTools::spectralslice()] and coefficients are
#' calculated using [emuR::dct()].
#'
#' [moments_ts()] is a sibling function for spectral moments.
#' @export
#'
#' @examples
#' # Load sound file
#' wav_file <- paste0('extdata/ps/f1_ses/F1-0002-car-rep1-buu-106_bndl/',
#' 'F1-0002-car-rep1-buu-106.wav')
#' datapath <- system.file(wav_file, package='emuhelpeR')
#' x <- rPraat::snd.read(datapath)
#'
#' # Extract signal vector and sample rate
#' sig <- x$sig[,1]
#' sr <- x$fs
#'
#' # Get coefficients
#' coef <- dct_ts(snd_vector=sig, sr=sr)
dct_ts <- function(snd_vector,
                   sr,
                   freq_ceiling=8000,
                   step=5,
                   plot=TRUE) {

  ln <- round(length(snd_vector) / sr / (step/1000))
  step_samp <- freq_ceiling*2*(step/1000)

  if (sr != freq_ceiling / 2) {
    snd_vector <- soundgen::resample(snd_vector, samplingRate=sr, samplingRate_new=freq_ceiling*2)
  }

  tmp <- data.frame(t = seq(step, ln*step, by=step),
                    k0 = rep(NA),
                    k1 = rep(NA),
                    k2 = rep(NA),
                    k3 = rep(NA))

  for (i in 1:ln) {
    slice <- snd_vector[(((i-1)*step_samp)+1):(i*step_samp)]
    spec <- phonTools::spectralslice(slice, fs=freq_ceiling*2, show=F)
    coef <- emuR::dct(spec[,2], m=3, fit=F)
    tmp$k0[i] <- coef[1]
    tmp$k1[i] <- coef[2]
    tmp$k2[i] <- coef[3]
    tmp$k3[i] <- coef[4]
  }

  if (plot) {
    graphics::par(mfrow=c(2,2))
    plot(tmp$t, tmp$k0, type='l', ylab='k0', xlab='Time (ms)')
    plot(tmp$t, tmp$k1, type='l', ylab='k1', xlab='Time (ms)')
    plot(tmp$t, tmp$k2, type='l', ylab='k2', xlab='Time (ms)')
    plot(tmp$t, tmp$k3, type='l', ylab='k3', xlab='Time (ms)')
  }

  return(tmp)

}
