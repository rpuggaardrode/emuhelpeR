#' Add signal canvases to EMU database
#'
#' Wrapper to add one or more signal canvases to the default view of an EMU
#' database.
#'
#' @param db_handle The handle of an EMU database which is already loaded in R.
#' @param add One or more strings containing the names of canvases to add to
#' the EMU database.
#'
#' @seealso This function wraps the functions [emuR::get_signalCanvasesOrder]
#' and [emuR::set_signalCanvasesOrder].
#' @export
#'
#' @examples
#' \dontrun{
#' datapath <- system.file('extdata/db', package='emuhelpeR')
#' raw <- emuR::load_emuDB(datapath)
#' emuR::get_signalCanvasesOrder(raw, 'default')
#' add_signal_canvas(db_handle=raw, add='eggF0')
#' emuR::get_signalCanvasesOrder(raw, 'default')
#' #To have a look at the new order in EMU-SDMS, run
#' emuR::serve(raw)
#' }

add_signal_canvas <- function(db_handle, add) {
  sco <- emuR::get_signalCanvasesOrder(db_handle, 'default')
  emuR::set_signalCanvasesOrder(db_handle, 'default', c(sco, add))
}

