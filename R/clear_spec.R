#' No longer display SSFF track on top of spectrogram when viewing EMU database
#'
#' Change the configuration of an EMU database perspective to no longer
#' display signal from an SSFF track on top of the spectrogram.
#'
#' @param db The handle of an EMU database which is loaded into R.
#' @param perspective A string giving the name of the perspective for which the
#' configuration should be changed. Default is `default`.
#'
#' @seealso `signal_on_spec()` is a function for adding SSFF tracks to a
#' spectrogram when viewing an EMU database
#' @export
#'
#' @examples
#' #not now
clear_spec <- function(db, perspective='default') {
  json_fn <- list.files(db$basePath, pattern='*.json')
  fp <- file.path(db$basePath, json_fn)
  pn <- emuR::list_perspectives(db)$name
  ind <- which(pn == perspective)

  d <- rjson::fromJSON(file=fp)
  d$EMUwebAppConfig$perspectives[[ind]]$signalCanvases$assign[[1]] <- NULL
  j <- rjson::toJSON(d, 1)
  conn <- file(fp)
  writeLines(j, conn)
  close(conn)
}
