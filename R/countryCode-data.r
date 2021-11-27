#' Table with ISO-3 (ISO 3166-1 alpha-3)
#'
#' The names and ISO-3 and ISO-2 codes for countries, based on the UN codes, for use in the construction of datasets.
#'
#' @docType data
#'
#' @name countryCode
#'
#' @format An object of class data.frame
#'
#' @keywords datasets
#'
#' @source \href{http://www.trucsweb.com/tutoriels/internet/iso_3166/}{Link}
#'
#' @examples
#' data(countryCode)
#' ISO3 <- 'ZMB'
#' idx <- countryCode[,2] == ISO3
#' # what's "ZMB" full name?
#' print(countryCode[idx,1])
#' View(countryCode)
"countryCode"


