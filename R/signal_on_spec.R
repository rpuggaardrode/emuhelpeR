#' Display SSFF track on top of spectrogram when viewing EMU database
#'
#' Change the configuration of an EMU database perspective to display signal
#' from an SSFF track on top of the spectrogram.
#'
#' @param db The handle of an EMU database which is loaded into R.
#' @param track A string giving the name of the SSFF track name to display on
#' top of the spectrogram. See `emuR::list_ssffTrackDefinitions()`.
#' @param perspective A string giving the name of the perspective for which the
#' configuration should be changed. Default is `default`.
#'
#' @seealso `clear_spec()` resets the perspective to no longer display an SSFF
#' track on top of the spectrogram.
#' @export
#'
#' @examples
#' #not now
signal_on_spec <- function(db, track, perspective='default') {
  json_fn <- list.files(db$basePath, pattern='*.json')
  fp <- file.path(db$basePath, json_fn)
  pn <- emuR::list_perspectives(db)$name
  ind <- which(pn == perspective)

  tn <- emuR::list_ssffTrackDefinitions(db)$name
  if (!track %in% tn) stop(paste('The specified track does not match any of the',
                                 'available SSFF tracks in the database. See',
                                 'emuR::list_ssffTrackDefinitions()'))

  d <- rjson::fromJSON(file=fp)
  d$EMUwebAppConfig$perspectives[[ind]]$signalCanvases$assign[[1]] <-
    list(signalCanvasName = 'SPEC', ssffTrackName = track)
  j <- rjson::toJSON(d, 1)
  conn <- file(fp)
  writeLines(j, conn)
  close(conn)
}
