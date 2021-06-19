#' Crear un tweet
#'
#' Crea un tweet amb les dades diaries de la comarca
#' @param dataframe dataframe amb les dades covid de la comarca corresponent
#' @return casos confirmats, defuncions, incidencia acumulada
#' per 100.000 habitants en els ultims 7 i 14 dies i el total
#' acumulat de persones vacunades
#' @examples
#' \dontshow{
#' bllobregatdata <- download_data("BAIX LLOBREGAT")
#' }
#' tweet <- make_tweet(bllobregatdata)
#' @export


make_tweet <- function(dataframe) {
	comarcadiarigen <- dataframe

	comarca <- as.character(comarcadiarigen[1, "NOM"])

	poblacio_cat <- read.csv(system.file("extdata", "poblacio_cat.csv", package = "covidcatwatch"), sep = ";")
	comarca_mask <- poblacio_cat$comarca == comarca
	poblacio <- poblacio_cat[comarca_mask, "poblacio"]

	##dades del ultim dia
	lastdate <- Sys.Date() - 1
	dates <- as.Date(comarcadiarigen$DATA)
	datemask <- dates == lastdate
	comarcagendia <- comarcadiarigen[datemask, ]

	##dataframes
	#comarcadiarigen
	#comarcagendia


	##EXTRAURE VALORS
	casostotals <- sum(comarcagendia$CASOS_CONFIRMAT)

	exitustotals <- sum(comarcagendia$EXITUS)

	#IA7
	last7mask <- dates >= (lastdate - 6) & dates <= lastdate
	last7 <- comarcadiarigen[last7mask, ]
	casoslast7 <- sum(last7$CASOS_CONFIRMAT)
	ia7 <- sprintf("%.2f", (casoslast7/poblacio)*100000)

	#IA7
	last14mask <- dates >= (lastdate - 13) & dates <= lastdate
	last14 <- comarcadiarigen[last14mask, ]
	casoslast14 <- sum(last14$CASOS_CONFIRMAT)
	ia14 <- sprintf("%.2f", (casoslast14/poblacio)*100000)

	#total acumulat de persones vacunades i percentatge
	dosi1total <- sum(comarcadiarigen$VACUNATS_DOSI_1)
	dosi2total <- sum(comarcadiarigen$VACUNATS_DOSI_2)
	dosi1percent <- sprintf("%.2f", (dosi1total*100/poblacio))
	dosi2percent <- sprintf("%.2f", (dosi2total*100/poblacio))


	##crear el tweet
	lastdate <- format(lastdate, "%d/%m/%Y")
	casos <- paste("Casos confirmats:", casostotals)
	exitus <- paste("Defuncions:", exitustotals)
	incidencia7 <- paste("  - 7 dies:", ia7)
	incidencia14 <- paste("  - 14 dies:", ia14)
	incidencia <- "Incidencia acumulada per 100000 habitants:"
	dosi1 <- paste("Vacunats 1a dosi:", dosi1total, paste0("(", dosi1percent, "%)"))
	dosi2 <- paste("Vacunats 2a dosi:", dosi2total, paste0("(", dosi2percent, "%)"))
	hashtag_comarques <- read.csv(system.file("extdata", "hashtag_comarques.csv", package = "covidcatwatch"), sep = ";")
	hashtag_comarca <- hashtag_comarques[comarca_mask, "hashtag"]

	hashtags <- paste(hashtag_comarca, "#covid19", "#catalunya", "#incidencia", "#vacunats")

	tweet <- paste(lastdate, casos, exitus, incidencia, incidencia7, incidencia14, dosi1, dosi2, hashtags, sep = "\n")
	cat(tweet)
	return(tweet)
}
