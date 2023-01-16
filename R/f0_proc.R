f0_proc <- function(df,
                    f0col='F0',
                    dep=NULL,
                    speaker=NULL,
                    group_var=NULL,
                    timing_rm=NULL) {

  rowna <- which(df[,f0col]==0)
  df[rowna,f0col] <- NA

  df$F0 <- df[[f0col]]
  if (f0col != 'F0') {
    df <- df[,-which(names(df)==f0col)]
  }

  if (!is.null(timing_rm)) {
    if(length(timing_rm) != 2) {
      stop('timing_rm must take two arguments, one indicating the transcription label
           used for automated time-based removal, and one indicating the
           duration limit above which measures must be removed.')
    } else {
      df <- df %>%
        dplyr::group_by(.data$session, .data$sl_rowIdx) %>%
        dplyr::mutate(times_rel = ifelse(labels==timing_rm[1], -rev(.data$times_rel), .data$times_rel)) %>%
        dplyr::mutate(F0 = ifelse(.data$times_rel < -as.numeric(timing_rm[2]), NA, .data$F0))
    }
  }

  if(!is.null(group_var)){
    df <- df %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
      dplyr::mutate(uppF0 = mean(.data$F0, na.rm=T) + 3*stats::sd(.data$F0, na.rm=T),
                    lowF0 = mean(.data$F0, na.rm=T) - 3*stats::sd(.data$F0, na.rm=T))
  } else {
    df <- df %>% dplyr::mutate(uppF0 = mean(.data$F0, na.rm=T) + 3*stats::sd(.data$F0, na.rm=T),
                               lowF0 = mean(.data$F0, na.rm=T) - 3*stats::sd(.data$F0, na.rm=T))
  }

  df <- df %>% dplyr::mutate(F0 = ifelse((.data$F0 > .data$uppF0 | .data$F0 < .data$lowF0), NA, .data$F0))
  df <- normz(df, 'F0', speaker)

  if (!is.null(dep)) {
    for(d in dep) {
      df[[d]] <- ifelse(is.na(df[['F0']]), NA, df[[d]])
      df <- normz(df, d, speaker)
    }
  }

  return(df)

}

#Won't give the exact same results as James' code, because relative time labels
#for 'cl' > 250 are removed *before* the rest of the automated outlier removal.
#This seems better to me -- if we remove those because we don't believe
#they're solid measurements, then they also shouldn't be part of calculating
#the mean and standard deviations for automated removal.
