import_ssfftracks <- function(db_handle,
                            seg_list,
                            f0col='F0',
                            f0dep=NULL,
                            fncol=NULL,
                            f1dep=NULL,
                            f2dep=NULL,
                            f3dep=NULL,
                            speaker=NULL,
                            group_var=NULL,
                            timing_rm=NULL,
                            proc=TRUE
) {

  fixed <- c(f0col, f0dep, fncol, f1dep, f2dep, f3dep)
  trax <- emuR::list_ssffTrackDefinitions(db_handle)$name

  if (any(!(fixed %in% trax))) {
    stop (paste0('One or more of the specified ssff tracks \n',
                 fixed,
                 'are not available in the EMU database. Available tracks are \n',
                 trax))
  }

  init <- trax[1]
  tmp <- emuR::get_trackdata(db_handle, seg_list, ssffTrackName=init)
  tmp[[init]] <- tmp[['T1']]
  tmp <- tmp[,-which(names(tmp)=='T1')]
  for (tr in trax[-1]){
    tmp[[tr]] <- emuR::get_trackdata(db_handle, seg_list, ssffTrackName=tr)$T1
  }

  if (proc) {
    if (!is.null(f0col)) {
      tmp <- f0_proc(tmp, f0col, f0dep, speaker, group_var, timing_rm)
    }
    if (!is.null(fncol)) {
      tmp <- fn_proc(tmp, fncol, f1dep, f2dep, f3dep, speaker, group_var)
    }

    missing <- trax[which(!(trax %in% fixed))]

    for(v in missing){
      tmp <- normz(tmp, v, speaker)
    }

  }

}
