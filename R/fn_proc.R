fn_proc <- function(df,
                    fncol=c('F1', 'F2', 'F3'),
                    f1dep=NULL,
                    f2dep=NULL,
                    f3dep=NULL,
                    speaker=NULL,
                    group_var=NULL){

  for (f in 1:length(fncol)){

    if (fncol[1] != 'F1'){
      df[[paste0('F', f)]] <- df[[fncol[f]]]
      fn <- colnames(df)[ncol(df)]
    } else {
      fn <- fncol[f]
    }

    if (!is.null(group_var)){
      df <- df %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
        dplyr::mutate('uppF{f}' := mean(!!as.name(fn), na.rm=T) + 3*stats::sd(!!as.name(fn), na.rm=T),
                      'lowF{f}' := mean(!!as.name(fn), na.rm=T) - 3*stats::sd(!!as.name(fn), na.rm=T))
    } else {
      df <- df %>%
        dplyr::mutate('uppF{f}' := mean(!!as.name(fn), na.rm=T) + 3*stats::sd(!!as.name(fn), na.rm=T),
                      'lowF{f}' := mean(!!as.name(fn), na.rm=T) - 3*stats::sd(!!as.name(fn), na.rm=T))
    }

    df <- df %>%
      dplyr::mutate(
        'F{f}' := ifelse((!!as.name(fn) > !!as.name(paste0('uppF', f)) |
                          !!as.name(fn) < !!as.name(paste0('lowF', f))),
                          NA, !!as.name(fn))
      )

    df <- normz(df, paste0('F', f), speaker)

  }

  if (!is.null(f1dep)) {
    for (d in f1dep) {
      df[[d]] <- ifelse(is.na(df$f0) | is.na(df$F1), NA, df[[d]])
      df <- normz(df, d, speaker)
    }
  }

  if (!is.null(f2dep)) {
    for (d in f2dep) {
      df[[d]] <- ifelse(is.na(df$f0) | is.na(df$F2), NA, df[[d]])
      df <- normz(df, d, speaker)
    }
  }

  if (!is.null(f3dep)) {
    for (d in f3dep) {
      df[[d]] <- ifelse(is.na(df$f0) | is.na(df$F3), NA, df[[d]])
      df <- normz(df, d, speaker)
    }
  }

  return(df)

}
