#' Calculate first four spectral moments as time series
#'
#' Given a vector of numbers corresponding to a sound file, will generate
#' DFT spectra over equidistant time steps and calculate the first four
#' spectral moments of each spectrum.
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
#' for each spectral moment `cog`, `sd`, `skew`, `kurtosis`.
#' @seealso This function is called by [moments2ssff()] which generates these
#' time series for each sound file in an EMU-SDMS data base.
#'
#' Spectra are generated using [phonTools::spectralslice()] and moments are
#' calculated using [emuR::moments()].
#'
#' [dct_ts()] is a sibling function for DCT coefficients.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/ps/f1_ses/F1-0002-car-rep1-buu-106_bndl/F1-0002-car-rep1-buu-106.wav', package='emuhelpeR')
#' x <- rPraat::snd.read(datapath)
#' mom <- moments_ts(snd_vector=x$sig[,1], sr=x$fs)
moments_ts <- function(snd_vector,
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
                    cog = rep(NA),
                    sd = rep(NA),
                    skew = rep(NA),
                    kurtosis = rep(NA))

  for (i in 1:ln) {
    slice <- snd_vector[(((i-1)*step_samp)+1):(i*step_samp)]
    spec <- phonTools::spectralslice(slice, fs=freq_ceiling*2, show=F)
    mom <- emuR::moments(spec[,2], spec[,1], minval=T)
    tmp$cog[i] <- mom[1]
    tmp$sd[i] <- sqrt(mom[2])
    tmp$skew[i] <- mom[3]
    tmp$kurtosis[i] <- mom[4]
  }

  if (plot) {
    graphics::par(mfrow=c(2,2))
    plot(tmp$t, tmp$cog, type='l', ylab='Centre of gravity (Hz)', xlab='Time (ms)')
    plot(tmp$t, tmp$sd, type='l', ylab='Standard deviation (Hz)', xlab='Time (ms)')
    plot(tmp$t, tmp$skew, type='l', ylab='Skewness', xlab='Time (ms)')
    plot(tmp$t, tmp$kurtosis, type='l', ylab='Kurtosis', xlab='Time (ms)')
  }

  return(tmp)

}
