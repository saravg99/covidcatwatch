#' Crear un gràfic
#'
#' Crea un gràfic amb la quantitat acumulada de vacunats al llarg del 2021
#' @param dataframe Dataframe amb les dades covid de la comarca corresponent
#' @param save Si save = TRUE, es guarda el fitxer i es retorna el path del fitxer
#' @return Si save = TRUE, retorna el path de la imatge guardada. Si no, nomes es visualitza el grafic en R studio
#' @examples
#' \dontshow{
#' bllobregatdata <- download_data("BAIX LLOBREGAT")
#' }
#' plot_data(bllobregatdata)
#' plotfile <- plot_data(bllobregatdata, save = TRUE)
#' @import ggplot2
#' @export


plot_data <- function(dataframe, save = FALSE) {
  months <- c("Gen.", "Febr.", "Mar.", "Abr.", "Maig", "Juny", "Jul.", "Agost", "Sept.", "Oct.", "Nov.", "Dec.")

  options(scipen=999)
  DATA <- VACUNATS_DOSI_1 <- NULL

  comarcadiarigen <- dataframe
  comarca <- as.character(comarcadiarigen[1, "NOM"])

	dates <- as.Date(comarcadiarigen$DATA)

	#obtenir dades del 2021
	years <- format(dates, "%Y")
	year_mask <- years == "2021"
	comarcagen2021 <- comarcadiarigen[year_mask, ]

	##VACUNATS
	#obtenir dades de persones vacunades agrupat per data
	vacunats1 <- split(comarcagen2021$VACUNATS_DOSI_1, droplevels(comarcagen2021$DATA))
	#sumar les dades de cada dia
	vacunats1sum <- lapply(vacunats1, sum)
	#crear dataframe amb les dades
	plotdata <- data.frame(DATA=as.Date(names(vacunats1sum)), VACUNATS_DOSI_1=unlist(vacunats1sum))
	#obtenir la suma acumulada
	plotdata$VACUNATS_DOSI_1 <- cumsum(plotdata$VACUNATS_DOSI_1)
	#calcular el percentatge
	poblacio_cat <- read.csv(system.file("extdata", "poblacio_cat2020.csv", package = "covidcatwatch"), sep = ";")
	comarca_mask <- poblacio_cat$comarca == comarca
	poblacio <- poblacio_cat[comarca_mask, "poblacio_total"]

	plotdata$VACUNATS_DOSI_1 <- (plotdata$VACUNATS_DOSI_1/poblacio)*100


	##IA14
	#obtenir els casos confirmats agrupats per data
  confirmats <- split(comarcagen2021$CASOS_CONFIRMAT, droplevels(comarcagen2021$DATA))
  #sumar les dades de cada dia
	confirmatssum <- lapply(confirmats, sum)
	#afegir les dades al dataframe
	plotdata$CONFIRMATS <- unlist(confirmatssum)

	#calcular la IA14 per cada dia
	dates21 <- as.Date(plotdata$DATA)

	for (i in (1:nrow(plotdata))) {
	  lastdate <- plotdata[i, "DATA"]
	  last14mask <- dates21 >= (lastdate - 13) & dates21 <= lastdate
	  last14 <- plotdata[last14mask, ]
	  last14sum <- sum(last14$CONFIRMATS)
	  ia14 <- as.numeric(sprintf("%.2f", (last14sum/poblacio)*100000))
	  plotdata[i, "IA14"] <- ia14
	}


	for (i in (1:13)) {
	  plotdata[i, "IA14"] <- NA
	}


	#crear el grafic
	plot <- ggplot(plotdata, aes(x=DATA)) + geom_line(color="red", aes(y=VACUNATS_DOSI_1))
	title <- paste("Dades 2021", comarca, sep = "\n")
	plot = plot + labs(title =  title, x= "Mesos")
	plot = plot + scale_x_date(date_breaks="1 month",  labels = function(x) months[as.numeric(substr(x, 6, 7))])

	#afegir ia14 + eix secundari
	scaleFactor <- 100 / max(plotdata$IA14, na.rm = TRUE)
	plot = plot + geom_line(color = "blue", aes(y=IA14 * scaleFactor))
	plot = plot + scale_y_continuous(name="% Persones vacunades 1a dosi", limits = c(0, 100),  sec.axis=sec_axis(~./scaleFactor, name="IA14"))
	plot = plot + theme(
	  plot.title = element_text(hjust = 0.5),
	  axis.title.y.left=element_text(color="red"),
	  axis.title.y.right=element_text(color="blue")
	  )
	print(plot)

	#guardar el grafic
	if (save == TRUE) {
	  filepath <- tempfile(pattern="plot", fileext = ".png")
		ggsave(filepath, plot=plot, height = 3, width = 6)
		return(filepath)
	}

}
