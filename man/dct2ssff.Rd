% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dct2ssff.R
\name{dct2ssff}
\alias{dct2ssff}
\title{Add first four DCT coefficients as time series to EMU database}
\usage{
dct2ssff(db_handle, freq_ceiling = 8000, step = 5)
}
\arguments{
\item{db_handle}{The handle of an EMU database which is already loaded in R.}

\item{freq_ceiling}{Maximum frequency to be included in the spectra.
Default is \code{8000}, in which case the \code{snd_vector} is resampled to 16 kHz.}

\item{step}{Duration of time steps over which to generate spectra in ms.
Default is \code{5}}
}
\description{
Given a loaded EMU database, for each sound file will calculate the first
four DCT coefficients from DFT spectra over equidistant time steps and add
them as SSFF tracks to the database.
}
\examples{
\dontrun{
datapath <- system.file('extdata/ps', package='emuhelpeR')
raw <- emuR::load_emuDB(datapath)
dct2ssff(raw)
# To have a look at an SSFF track in EMU-SDMS, run e.g.
add_signal_canvas(raw, 'k0')
emuR::serve(raw)
}
}
\seealso{
This function calls \code{\link[=dct_ts]{dct_ts()}} which can be used to calculate
DCT coefficients as time series for individual sound files.
This function partially wraps functionality from the \code{emuR} package, in
particular the \link[emuR:add_files]{emuR::add_files}, \link[emuR:AddListRemoveSsffTrackDefinition]{emuR::add_ssffTrackDefinition},
and \link[emuR:dct]{emuR::dct} functions.

\code{\link[=moments2ssff]{moments2ssff()}} is a sibling function for spectral moments.
}
