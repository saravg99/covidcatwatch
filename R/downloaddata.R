#' Descarregar dades
#'
#' Descarrega les dades més recents i crea un dataframe amb les dades de la població general de la comarca
#' @param comarca Nom de la comarca en majuscules
#' @return Dataframe amb les dades de la poblacio general de la comarca corresponent
#' @examples
#' bllobregatdata <- download_data("BAIX LLOBREGAT")
#' @importFrom utils download.file read.csv unzip
#' @export


download_data <- function(comarca) {
  dir <- tempdir()
  filepath <- file.path(dir, "comarques_diari.zip")
  download.file("https://dadescovid.cat/static/csv/comarques_diari.zip", filepath)
  unzip(filepath, exdir = dir)
  filepath2 <- file.path(dir, "comarques_diari.csv")

  comarquesdiari <- read.csv(filepath2, sep = ';', stringsAsFactors = TRUE)

  ##dades de la comarca
  comarcadiarimask <- comarquesdiari$NOM == comarca
  comarcadiari <- comarquesdiari[comarcadiarimask, ]

  ##dades poblacio general
  generalmask <- comarcadiari$RESIDENCIA == "No"
  comarcadiarigen <- comarcadiari[generalmask, ]

  return(comarcadiarigen)
}
