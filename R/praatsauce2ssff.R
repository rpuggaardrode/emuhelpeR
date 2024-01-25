#' Convert PraatSauce output to SSFF tracks and add to EMU database
#'
#' All measurement tracks in PraatSauce output file are converted into SSFF
#' tracks and added to an existing EMU database.
#'
#' @param ps_output Data frame with named columns containing PraatSauce output.
#' @param db_handle The handle of an EMU database which is already loaded in R.
#' @param session_col String containing the name of a column in `ps_output`
#' which contains the names of emuDB sessions. Optional; default is NULL,
#' in case all sessions are named `0000_ses`, which is the default name of
#' an EMU-SDMS session in case there is only one.
#'
#' @export
#' @seealso
#' This function partially wraps functionality from the `emuR` package, in
#' particular the [emuR::add_files] and [emuR::add_ssffTrackDefinition]
#' functions.
#'
#' The function is built to process output from the PraatSauce suite of Praat
#' scripts, which can be downloaded here:
#' <https://github.com/kirbyj/praatsauce>
#'
#' Since PraatSauce aims to be as similar as possible to the VoiceSauce suite of
#' MATLAB scripts, the function should also work for VoiceSauce output,
#' although this has not been tested.
#'
#' @examples
#' \dontrun{
#' datapath <- system.file('extdata/ps', package='emuhelpeR')
#' raw <- emuR::load_emuDB(datapath)
#' dplyr::glimpse(ps)
#' praatsauce2ssff(ps_output=ps, db_handle=raw, session_col='session')
#' # To have a look at an SSFF track in EMU-SDMS, run
#' add_signal_canvas(raw, 'f0')
#' emuR::serve(raw)
#' }

praatsauce2ssff <- function(ps_output,
                            db_handle,
                            session_col=NULL) {

  ps <- as.data.frame(ps_output)
  sr <- round(1 / (ps[[2,'t_ms']] - ps[1,'t_ms']), 0)
  col1 <- which(colnames(ps) == 'f0')
  cols <- col1:ncol(ps)
  cn <- colnames(ps)[cols]
  dir.create(paste0(getwd(), '/ssff/'))

  if (is.null(session_col)) {
    sessions <- '0000'
  } else {
    sessions <- unique(ps[[session_col]])
  }

  for (s in sessions) {
    dir.create(paste0(getwd(), '/ssff/', s))
    ssff_path <- paste0(getwd(), '/ssff/', s)

    if (is.null(session_col)) {
      tmp_s <- ps
    } else {
      tmp_s <- ps[which(ps[[session_col]]==s),]
    }
    fls <- unique(tmp_s[['Filename']])

    for (f in fls) {
      tmp <- tmp_s[which(tmp_s$Filename==f),]
      start <- tmp$seg_Start[1]

      for (c in cols) {
        colname <- colnames(tmp)[c]
        vals <- as.matrix(tmp[[c]])

        ado <- list()
        attr(ado, 'trackFormats') <- c('REAL32')
        attr(ado, 'sampleRate') <- sr
        attr(ado, 'origFreq') <- 0
        attr(ado, 'startTime') <- start
        attr(ado, 'endRecord') <- nrow(tmp)
        class(ado) <- 'AsspDataObj'
        wrassp::AsspFileFormat(ado) <- 'SSFF'
        wrassp::AsspDataFormat(ado) <- as.integer(2)
        ado <- wrassp::addTrack(ado, colname, vals, 'REAL32')

        new_path <- paste0(ssff_path, '/', f, '.', colname)
        wrassp::write.AsspDataObj(ado, file = new_path)

      }

    }

    for (c in cn) {emuR::add_files(db_handle, ssff_path, c, s)}

  }

  for (c in cn) {emuR::add_ssffTrackDefinition(db_handle, c, c, c)}

  unlink('ssff', recursive=T)

}
