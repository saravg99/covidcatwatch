#' Crear un gràfic
#'
#' Crea un gràfic amb la quantitat acumulada de vacunats al llarg del 2021
#' @param dataframe Dataframe amb les dades covid de la comarca corresponent
#' @param save Si save = TRUE, es guarda el fitxer i es retorna el path del fitxer
#' @return Si save = TRUE, retorna el path de la imatge guardada. Si no, nomes es visualitza el grafic en R studio
#' @examples
#' plot_vacunats("BAIX LLOBREGAT")
#' plotfile <- plot_vacunats("BAIX LLOBREGAT", save = TRUE)
#' @import ggplot2
#' @export


plot_data <- function(dataframe, save = FALSE) {
	Sys.setlocale("LC_TIME", "catalan")
  options(scipen=999)
	
  comarcadiarigen <- dataframe
  comarca <- as.character(comarcadiarigen[1, "NOM"])

	dates <- as.Date(comarcadiarigen$DATA)

	#obtenir dades del 2021
	years <- format(dates, "%Y")
	year_mask <- years == "2021"
	comarcagen2021 <- comarcadiarigen[year_mask, ]

	#obtenir dades de persones vacunades agrupat per data
	vacunats1 <- split(comarcagen2021$VACUNATS_DOSI_1, droplevels(comarcagen2021$DATA))
	#sumar les dades de cada dia
	vacunats1sum <- lapply(vacunats1, sum)
	#crear dataframe amb les dades
	vacunats1data <- data.frame(DATA=as.Date(names(vacunats1sum)), VACUNATS_DOSI_1=unlist(vacunats1sum))
	#obtenir la suma acumulada 
	vacunats1data$VACUNATS_DOSI_1 <- cumsum(vacunats1data$VACUNATS_DOSI_1)

	#crear el grafic
	plot <- ggplot(vacunats1data, aes(x=DATA, y=VACUNATS_DOSI_1)) + geom_line(color="red") 
	title <- paste("Persones vacunades amb la 1a dosi al 2021", comarca, sep = "\n")
	plot = plot + labs(title =  title, x= "Mesos", y= "Persones vacunades")
	plot = plot + theme(plot.title = element_text(hjust = 0.5))
	
	print(plot)

	#guardar el grafic
	if (save == TRUE) {
	  filepath <- tempfile(pattern="plot", fileext = ".png")
		ggsave(filepath, plot=plot, height = 3, width = 6)
		return(filepath)
	}
	
}