% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reflora_download.R
\name{reflora_download}
\alias{reflora_download}
\title{Download plant specimen records from REFLORA Virtual Herbarium}
\usage{
reflora_download(herbarium = NULL,
                 repatriated = TRUE,
                 verbose = TRUE,
                 dir = "reflora_download")
}
\arguments{
\item{herbarium}{A vector of specific herbarium acronyms (collection code) in
uppercase letters or leave it as NULL to download records for all REFLORA herbaria.}

\item{repatriated}{Logical. If \code{FALSE}, skips downloading records from
REFLORA-associated herbaria that have been repatriated. Default is \code{TRUE}.
Use \code{reflora_summary()} to check which collections are repatriated.}

\item{verbose}{Logical, if \code{FALSE}, a message showing steps when downloading
herbarium specimen records will not be printed in the console in full.}

\item{dir}{Pathway to the computer's directory, where the file will be saved.
The default is to create a directory named \code{reflora_download}
and the results will be saved within a subfolder named by each searched
REFLORA-associated herbarium collection.}
}
\value{
Folder with DwC-A files for an specific or all REFLORA-associated herbaria.
}
\description{
Download plant specimen records in Darwin Core Format from any
herbarium collection at \href{https://ipt.jbrj.gov.br/reflora/}{REFLORA Virtual Herbarium}
hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
}
\examples{
\dontrun{

reflora_download(herbarium = c("ALCB", "HUEFS", "K", "RB"),
                 verbose = TRUE,
                 dir = "reflora_download")
}

}
\seealso{
\code{\link{reflora_summary}}
}
\author{
Domingos Cardoso
}
