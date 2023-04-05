#' Add first four spectral moments as time series to EMU database
#'
#' Given a loaded EMU database, for each sound file will calculate the first
#' four spectral moments from DFT spectra over equidistant time steps and add
#' them as SSFF tracks to the database.
#'
#' @param db_handle The handle of an EMU database which is already loaded in R.
#' @param freq_ceiling Maximum frequency to be included in the spectra.
#' Default is `8000`, in which case the `snd_vector` is resampled to 16 kHz.
#' @param step Duration of time steps over which to generate spectra in ms.
#' Default is `5`
#'
#' @export
#' @seealso This function calls [moments_ts()] which can be used to calculate
#' spectral moments as time series for individual sound files.
#' This function partially wraps functionality from the `emuR` package, in
#' particular the [emuR::add_files], [emuR::add_ssffTrackDefinition],
#' and [emuR::moments] functions.
#'
#' [dct2ssff()] is a sibling function for DCT coefficients.
#'
#' @examples
#' datapath <- system.file('extdata/ps', package='emuhelpeR')
#' raw <- emuR::load_emuDB(datapath)
#' moments2ssff(raw)
#' #To have a look at an SSFF track in EMU-SDMS, run e.g.
#' #add_signal_canvas(raw, 'cog')
#' #emuR::serve(raw)
moments2ssff <- function(db_handle,
                         freq_ceiling=8000,
                         step=5) {

  path <- db_handle[['basePath']]
  wavs <- list.files(path, pattern='*.wav', recursive=T, full.names=T)

  dir.create(paste0(getwd(), '/ssff/'))

  sessions <- list.dirs(path, full.names=F, recursive=F)

  for (s in sessions) {

    dir.create(paste0(getwd(), '/ssff/', s))
    ssff_path <- paste0(getwd(), '/ssff/', s)

    s_wavs <- wavs[which(grepl(s, wavs))]

    for (w in s_wavs) {
      spl <- strsplit(w, '/')[[1]]
      filename <- strsplit(spl[length(spl)], '.wav')[[1]]
      ses <- strsplit(spl[length(spl)-2], '.wav')[[1]]

      snd <- rPraat::snd.read(w)
      snd_vector <- snd$sig[,1]
      sr <- snd$fs
      mom <- moments_ts(snd_vector, sr, freq_ceiling, step, plot=F)

      for (m in c('cog', 'sd', 'skew', 'kurtosis')) {
        vals <- as.matrix(mom[[m]])

        ado <- list()
        attr(ado, 'trackFormats') <- c('REAL32')
        attr(ado, 'sampleRate') <- (1/step)*1000
        attr(ado, 'origFreq') <- 0
        attr(ado, 'startTime') <- step / 1000
        attr(ado, 'endRecord') <- snd$duration
        class(ado) <- 'AsspDataObj'
        wrassp::AsspFileFormat(ado) <- 'SSFF'
        wrassp::AsspDataFormat(ado) <- as.integer(2)
        ado <- wrassp::addTrack(ado, m, vals, 'REAL32')

        new_path <- paste0(ssff_path, '/', filename, '.', m)
        wrassp::write.AsspDataObj(ado, file = new_path)
      }

    }

    for (m in c('cog', 'sd', 'skew', 'kurtosis')) {
      emuR::add_files(db_handle, ssff_path, paste0('\\.', m), strsplit(s, '_ses')[[1]])
    }

  }

  for (m in c('cog', 'sd', 'skew', 'kurtosis')) {
    emuR::add_ssffTrackDefinition(db_handle, m, m, m)
  }

  unlink('ssff', recursive=T)

}
