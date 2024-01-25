#' Load and serve EMU-SDMS database object
#'
#' Convenience function for loading and serving EMU-SDMS database in a single
#' command without specifying
#' its name whenever there is only one EMU-SDMS database in the working
#' directory and its subdirectories.
#'
#' @param recursive Logical; default is `TRUE`. Whether or not to recursively
#' search subdirectories of `dir` for
#' EMU databases.
#' @param serve Logical; whether or not to serve the database in a browser window.
#'
#' @return A list containing an EMU-SDMS database object.
#' @seealso This is a wrapper for the functions [emuR::load_emuDB()] and
#' [emuR::serve()].
#' @export
#'
#' @examples
#' \dontrun{
#' x <- load_db()
#' }

load_db <- function(recursive=TRUE, serve=TRUE) {

  dirs <- list.dirs(full.names=F, recursive=recursive)
  emuDB <- which(stringr::str_sub(dirs, start=-5) == 'emuDB')

  if (length(emuDB) != 1) {
    stop(paste0('It looks like more than one emuDB is found in the directory.',
                'Please specify a directory with only one emuDB.'))
  }

  if (serve) {
    emuR::serve(emuR::load_emuDB(dirs[emuDB]))
  } else {
    emuR::load_emuDB(dirs[emuDB])
  }

}

