f0_proc <- function(df,
                    f0col='F0',
                    dep=NULL,
                    speaker=NULL,
                    group_var=NULL,
                    timing_rm=NULL) {

  rowna <- which(df[,f0col]==0)
  df[rowna,f0col] <- NA

  df$f0 <- df[[f0col]]
  df <- df[,-which(names(df)==f0col)]

  if (!is.null(timing_rm)) {
    if(length(timing_rm) != 2) {
      stop('timing_rm must take two arguments, one indicating the transcription label
           used for automated time-based removal, and one indicating the
           duration limit above which measures must be removed.')
    } else {
      df <- df %>%
        dplyr::group_by(.data$session, .data$sl_rowIdx) %>%
        dplyr::mutate(times_rel = ifelse(labels==timing_rm[1], -rev(.data$times_rel), .data$times_rel)) %>%
        dplyr::mutate(f0 = ifelse(.data$times_rel < -as.numeric(timing_rm[2]), NA, .data$f0))
    }
  }

  if(!is.null(group_var)){
    df <- df %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
      dplyr::mutate(uppf0 = mean(.data$f0, na.rm=T) + 3*stats::sd(.data$f0, na.rm=T),
                    lowf0 = mean(.data$f0, na.rm=T) - 3*stats::sd(.data$f0, na.rm=T))
  } else {
    df <- df %>% dplyr::mutate(uppf0 = mean(.data$f0, na.rm=T) + 3*stats::sd(.data$f0, na.rm=T),
                               lowf0 = mean(.data$f0, na.rm=T) - 3*stats::sd(.data$f0, na.rm=T))
  }

  df <- df %>% dplyr::mutate(f0 = ifelse((.data$f0 > .data$uppf0 | .data$f0 < .data$lowf0), NA, .data$f0))

  if (!is.null(speaker)) {
    df <- df %>% dplyr::group_by(speaker) %>%
      dplyr::mutate(zf0 = as.vector(scale(.data$f0))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(normf0 = mean(.data$f0, na.rm = T) + .data$zf0 * stats::sd(.data$f0, na.rm = T))
  } else {
    df <- df %>% dplyr::mutate(zf0 = as.vector(scale(.data$f0))) %>%
      dplyr::mutate(normf0 = mean(.data$f0, na.rm = T) + .data$zf0 * stats::sd(.data$f0, na.rm = T))
  }

  if (!is.null(dep)) {
    for(d in dep) {
      df[[d]] <- ifelse(is.na(df[['f0']]), NA, df[[d]])
      df <- df %>% dplyr::group_by(speaker) %>%
        dplyr::mutate('z{d}' := as.vector(scale(!!as.name(d)))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate('norm{d}' := mean(!!as.name(d), na.rm=T) + !!as.name(paste0('z', d)) * stats::sd(!!as.name(d), na.rm=T))
    }
  }

  return(df)

}

#Won't give the exact same results as James' code, because relative time labels
#for 'cl' > 250 are removed *before* the rest of the automated outlier removal.
#This seems better to me -- if we remove those because we don't believe
#they're solid measurements, then they also shouldn't be part of calculating
#the mean and standard deviations for automated removal.
