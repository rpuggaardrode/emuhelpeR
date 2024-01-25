#' Reset signal canvas order in EMU database
#'
#' Sets the default signal canvas order in an EMU database to the standard
#' setting of waveform and spectrogram.
#'
#' @param db_handle The handle of an EMU database which is already loaded in R.
#'
#' @seealso Wraps the function [emuR::set_signalCanvasesOrder].
#' @export
#'
#' @examples
#' \dontrun{
#' datapath <- system.file('extdata/db', package='emuhelpeR')
#' raw <- emuR::load_emuDB(datapath)
#' emuR::get_signalCanvasesOrder(raw, 'default')
#' add_signal_canvas(db_handle=raw, add='eggF0')
#' emuR::get_signalCanvasesOrder(raw, 'default')
#' reset_signal_canvas(db_handle=raw)
#' emuR::get_signalCanvasesOrder(raw, 'default')
#' }
reset_signal_canvas <- function(db_handle) {
  sco <- c('OSCI', 'SPEC')
  emuR::set_signalCanvasesOrder(db_handle, 'default', sco)
}

