#' Import all available SSFF tracks from EMU database and process the raw data
#'
#' For a given EMU database and segment list, `import_ssfftracks` will import
#' measurements from all available SSFF tracks and store them in a data frame
#' with the same structure and information as the supplied segment list.
#' Note that importing SSFF track measurements is fairly time demanding,
#' so this function may take a while to run even for small datasets.
#'
#' `import_ssfftracks` also optionally performs automated outlier removal
#' of fundamental frequency and formants by recoding
#' zero values as `NA` and recoding values as `NA` if they fall outside of
#' three standard deviations from the mean value within the same group.
#' Corresponding easures which directly depend on fundamental frequency and
#' formants are also recoded as `NA`. Additionally, by-speaker z-score
#' normalization is performed of all measures is performed and stored in a
#' separate column, and these are subsequently rescaled based on the overall
#' mean and SD values.
#'
#' Optionally, F0 values that are measured too far away from a boundary specified
#' by the user are coded as `NA`.
#'
#' @param db_handle The handle of an EMU database which is already loaded in R.
#' @param seg_list A data frame containing a list of segments from which to
#' import SSFF track measurements. Should be generated with [emuR::query].
#' @param f0col A string containing the name of the SSFF track that contains
#' fundamental frequency values. Can be checked using
#' [emuR::list_ssffTrackDefinitions()].
#' Optional; default is `F0`. Only used if `proc=TRUE`.
#' @param f0dep One or more strings containing the name of SSFF tracks
#' containing measures that are directly dependent on fundamental frequency.
#' Optional; default is `NULL`. Only used if `proc=TRUE`.
#' @param fncol One or more strings containing the names of SSFF tracks that
#' contain formant measurements
#' Optional; default is `NULL`. Only used if `proc=TRUE`.
#' @param fndep A list where each element contains two strings:
#' * The name of an SSFF track that is directly dependent on a specific formant.
#' * Which formant the measure is dependent on.
#'
#' The structure is as follows:
#' `fndep=list(c('F1dep', 'F1'), c('F2dep', 'F2')`
#'
#' Optional; default is `NULL`. Only used if `proc=TRUE`.
#' @param speaker An optional string giving the name of the column in `seg_list`
#' containing speakers id's.
#' Default is `NULL`. If `speaker=NULL` and `proc=TRUE`, z-score
#' normalization will be done on the basis of the data at large and no rescaled
#' values will be returned.
#' @param group_var One or more strings containing the names of columns in
#' `seg_list` to be used as grouping variables for automatic outlier removal.
#' Optional; default is `NULL`. If `group_var=NULL` and `proc=TRUE`,
#' automatic outlier removal will
#' be based on means and standard deviation in the data at large.
#' @param timing_rm An optional list with two arguments:
#' * A string containing the label associated with a boundary in the data.
#' Values that are measured sufficiently far from this boundary are recoded as
#' `NA`. This label should be stored in the column `labels` in `seg_list`.
#' * A number indicating the distance from this boundary at which measurements
#' should be ignored. See example below.
#'
#' #' Optional; default is `NULL`.
#' @param proc A Boolean. Default is `TRUE`; if `FALSE`, the function only
#' returns raw measurement values and does not perform any preprocessing.
#'
#' @return A data frame with the same structure as `seg_list` containing columns
#' with all measurements available in the SSFF database. Optionally also contains
#' columns with normalized and rescaled values for all measurements, labeled
#' `z{variable}` and `norm{variable}` respectively.
#' @seealso This function assumes that the user has data stored in EMU database
#' (see [emuR]), that the database has already been loaded into R using
#' `emuR::load_emuDB()`, and has generated a list with relevant portions of that
#' database using [emuR::query()].
#'
#' The data processing used in `import_ssfftracks` makes use of other `emuhelpeR`
#' functions [f0_proc()], [fn_proc()], and [normz()], which can all be used
#' independently.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/db', package='emuhelpeR')
#' raw <- emuR::load_emuDB(datapath)
#' dplyr::glimpse(seg_list)
#' x <- import_ssfftracks(db_handle=raw, seg_list=seg_list, f0col='praatF0',
#' f0dep='H1H2c', fncol=c('praatF1', 'praatF2', 'praatF3'),
#' fndep=list(c('H1A1c', 'F1'), c('H1A3c', 'F3')), speaker='speaker',
#' group_var=c('speaker', 'vowel'), timing_rm=list('cl', 250))
#' dplyr::glimpse(x)
#' y <- import_ssfftracks(db_handle=raw, seg_list=seg_list, proc=FALSE)
#' dplyr::glimpse(y)
import_ssfftracks <- function(db_handle,
                            seg_list,
                            f0col='F0',
                            f0dep=NULL,
                            fncol=c('F1', 'F2'),
                            fndep=NULL,
                            speaker=NULL,
                            group_var=NULL,
                            timing_rm=NULL,
                            proc=TRUE
) {

  trax <- emuR::list_ssffTrackDefinitions(db_handle)$name

  init <- trax[1]
  tmp <- emuR::get_trackdata(db_handle, seg_list, ssffTrackName=init)
  tmp[[init]] <- tmp[['T1']]
  tmp <- tmp[,-which(names(tmp)=='T1')]
  for (tr in trax[-1]){
    tmp[[tr]] <- emuR::get_trackdata(db_handle, seg_list, ssffTrackName=tr)$T1
  }

  if (proc) {

    fndeps <- unlist(fndep)[which(1:length(unlist(fndep)) %% 2 != 0)]
    fixed <- c(f0col, f0dep, fncol, fndeps)

    if (any(!(fixed %in% trax))) {
      stop (paste0('One or more of the specified ssff tracks \n',
                   fixed,
                   'are not available in the EMU database. Available tracks are \n',
                   trax))
    }

    if (!is.null(f0col)) {
      tmp <- f0_proc(tmp, f0col, f0dep, speaker, group_var, timing_rm)
    }
    if (!is.null(fncol)) {
      tmp <- fn_proc(tmp, fncol, 'F0', fndep, speaker, group_var)
    }

    missing <- trax[which(!(trax %in% fixed))]

    for(v in missing){
      tmp <- normz(tmp, v, speaker)
    }

  }

  return(tmp)

}
