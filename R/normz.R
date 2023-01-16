normz <- function(df, var, speaker=NULL) {

  if(!is.null(speaker)) {
    df <- df %>% dplyr::group_by(!!as.name(speaker)) %>%
      dplyr::mutate('z{var}' := as.vector(scale(!!as.name(var)))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate('norm{var}' := mean(!!as.name(var), na.rm=T) +
                      !!as.name(paste0('z', var)) * stats::sd(!!as.name(var), na.rm=T))
  } else {
    df <- df %>%
      dplyr::mutate('z{var}' := as.vector(scale(!!as.name(var)))) %>%
      dplyr::mutate('norm{var}' := mean(!!as.name(var), na.rm=T) +
                      !!as.name(paste0('z', var)) * stats::sd(!!as.name(var), na.rm=T))
  }

  return(df)

}
