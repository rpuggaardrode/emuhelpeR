#' Load EMU-SDMS database object
#'
#' Convenience function for loading an EMU-SDMS database without specifying
#' its name whenever there is only one EMU-SDMS database in the working
#' directory and its subdirectories.
#'
#' @param recursive Logical; default is `TRUE`. Whether or not to recursively
#' search subdirectories of `dir` for
#' EMU databases.
#'
#' @return A list containing an EMU-SDMS database object.
#' @seealso This is a wrapper for the function [emuR::load_emuDB()].
#' @export
#'
#' @examples
#' # don't run
#' # x <- load_db()
#' # serve()
load_db <- function(recursive=TRUE) {

  dirs <- list.dirs(full.names=F, recursive=recursive)
  emuDB <- which(stringr::str_sub(dirs, start=-5) == 'emuDB')

  if (length(emuDB) != 1) {
    stop(paste0('It looks like more than one emuDB is found in the directory.',
                'Please specify a directory with only one emuDB.'))
  }

  emuR::load_emuDB(dirs[emuDB])

}
