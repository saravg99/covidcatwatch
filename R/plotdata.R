#' Crear un grafic
#'
#' Crea un grafic amb la quantitat acumulada de vacunats al llarg del 2021
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
#' @importFrom Rmisc multiplot
#' @importFrom grDevices dev.off png
#' @export


plot_data <- function(dataframe, save = FALSE) {
  months <- c("Gen.", "Febr.", "Mar.", "Abr.", "Maig", "Juny", "Jul.", "Agost", "Sept.", "Oct.", "Nov.", "Dec.")

  options(scipen=999)
  DATA <- VACUNATS_DOSI_1 <- IA14 <- IA14_age1 <- IA14_age2 <- IA14_age3 <- IA14_age4 <-  NULL

  comarcadiarigen <- dataframe
  comarca <- as.character(comarcadiarigen[1, "NOM"])

	dates <- as.Date(comarcadiarigen$DATA)

	#obtenir dades del 2021
	years <- format(dates, "%Y")
	year_mask <- years == "2021"
	comarcagen2021 <- comarcadiarigen[year_mask, ]


###GRAFIC 1: IA14 + % VACUNATS1

	##VACUNATS
	#obtenir dades de persones vacunades agrupat per data
	vacunats1 <- split(comarcagen2021$VACUNATS_DOSI_1, droplevels(comarcagen2021$DATA))
	#sumar les dades de cada dia
	vacunats1sum <- lapply(vacunats1, sum)
	#crear dataframe amb les dades
	plotdata1<- data.frame(DATA=as.Date(names(vacunats1sum)), VACUNATS_DOSI_1=unlist(vacunats1sum))
	#obtenir la suma acumulada
	plotdata1$VACUNATS_DOSI_1 <- cumsum(plotdata1$VACUNATS_DOSI_1)
	#calcular el percentatge
	poblacio_cat <- read.csv(system.file("extdata", "poblacio_cat2020.csv", package = "covidcatwatch"), sep = ";")
	comarca_mask <- poblacio_cat$comarca == comarca
	poblacio <- poblacio_cat[comarca_mask, "poblacio_total"]

	plotdata1$VACUNATS_DOSI_1 <- (plotdata1$VACUNATS_DOSI_1/poblacio)*100


	##IA14
	#obtenir els casos confirmats agrupats per data
  confirmats <- split(comarcagen2021$CASOS_CONFIRMAT, droplevels(comarcagen2021$DATA))
  #sumar les dades de cada dia
	confirmatssum <- lapply(confirmats, sum)
	#afegir les dades al dataframe
	plotdata1$CONFIRMATS <- unlist(confirmatssum)

	#calcular la IA14 per cada dia
	dates21 <- as.Date(plotdata1$DATA)

	for (i in (1:nrow(plotdata1))) {
	  lastdate <- plotdata1[i, "DATA"]
	  last14mask <- dates21 >= (lastdate - 13) & dates21 <= lastdate
	  last14 <- plotdata1[last14mask, ]
	  last14sum <- sum(last14$CONFIRMATS)
	  ia14 <- as.numeric(sprintf("%.2f", (last14sum/poblacio)*100000))
	  plotdata1[i, "IA14"] <- ia14
	}


	for (i in (1:13)) {
	  plotdata1[i, "IA14"] <- NA
	}

	#crear el grafic
	plot1 <- ggplot(plotdata1, aes(x=DATA)) + geom_line(color="red", aes(y=VACUNATS_DOSI_1))
	title <- paste("Dades 2021", comarca, sep = "\n")
	plot1 = plot1 + labs(title =  title, x= "Mesos")
	plot1 = plot1 + scale_x_date(date_breaks="1 month",  labels = function(x) months[as.numeric(substr(x, 6, 7))])

	#afegir ia14 + eix secundari
	scaleFactor <- 100 / max(plotdata1$IA14, na.rm = TRUE)
	plot1 = plot1 + geom_line(color = "blue", aes(y=IA14 * scaleFactor))
	plot1 = plot1 + scale_y_continuous(name="% Persones vacunades 1a dosi", limits = c(0, 100),  sec.axis=sec_axis(~./scaleFactor, name="IA14"))
	plot1 = plot1 + theme(
	  plot.title = element_text(hjust = 0.5),
	  axis.title.y.left=element_text(color="red"),
	  axis.title.y.right=element_text(color="blue")
	  )



###GRAFIC 2: IA14 per edats

	#separar per franges d'edat
	age1mask <- (comarcagen2021$GRUP_EDAT == "0 a 9") | (comarcagen2021$GRUP_EDAT == "10 a 19")
	comarca_age1 <- comarcagen2021[age1mask, ]
	age2mask <- (comarcagen2021$GRUP_EDAT == "20 a 29") | (comarcagen2021$GRUP_EDAT == "30 a 39")
	comarca_age2 <- comarcagen2021[age2mask, ]
	age3mask <- (comarcagen2021$GRUP_EDAT == "40 a 49") | (comarcagen2021$GRUP_EDAT == "50 a 59") | (comarcagen2021$GRUP_EDAT == "60 a 69")
	comarca_age3 <- comarcagen2021[age3mask, ]
	age4mask <- (comarcagen2021$GRUP_EDAT == "70 a 79") | (comarcagen2021$GRUP_EDAT == "80 o m\U00E9s")
	comarca_age4 <- comarcagen2021[age4mask, ]

	#agrupar per data
	confirmats_age1 <- split(comarca_age1$CASOS_CONFIRMAT, droplevels(comarca_age1$DATA))
	confirmats_age2 <- split(comarca_age2$CASOS_CONFIRMAT, droplevels(comarca_age2$DATA))
	confirmats_age3 <- split(comarca_age3$CASOS_CONFIRMAT, droplevels(comarca_age3$DATA))
	confirmats_age4 <- split(comarca_age4$CASOS_CONFIRMAT, droplevels(comarca_age4$DATA))

	#sumar per data
	confirmats_age1_sum <- lapply(confirmats_age1, sum)
	confirmats_age2_sum <- lapply(confirmats_age2, sum)
	confirmats_age3_sum <- lapply(confirmats_age3, sum)
	confirmats_age4_sum <- lapply(confirmats_age4, sum)

	#crear dataframe amb casos confirmats per edats
	plotdata2 <- data.frame(DATA=as.Date(names(confirmats_age1_sum)), CONFIRMATS_AGE1=unlist(confirmats_age1_sum), CONFIRMATS_AGE2=unlist(confirmats_age2_sum), CONFIRMATS_AGE3=unlist(confirmats_age3_sum), CONFIRMATS_AGE4=unlist(confirmats_age4_sum))

	#obtenir poblacio per edats
	poblacio_age1 <- poblacio_cat[comarca_mask, "de_0_a_19"]
	poblacio_age2 <- poblacio_cat[comarca_mask, "de_20_a_39"]
	poblacio_age3 <- poblacio_cat[comarca_mask, "de_40_a_69"]
	poblacio_age4 <- poblacio_cat[comarca_mask, "de_70_o_mes"]

	#calcular IA14
	for (i in (1:nrow(plotdata2))) {
	  lastdate <- plotdata2[i, "DATA"]
	  last14mask <- dates21 >= (lastdate - 13) & dates21 <= lastdate
	  last14 <- plotdata2[last14mask, ]

	  last14sum_age1 <- sum(last14$CONFIRMATS_AGE1)
	  last14sum_age2 <- sum(last14$CONFIRMATS_AGE2)
	  last14sum_age3 <- sum(last14$CONFIRMATS_AGE3)
	  last14sum_age4 <- sum(last14$CONFIRMATS_AGE4)

	  ia14_age1 <- as.numeric(sprintf("%.2f", (last14sum_age1/poblacio_age1)*100000))
	  ia14_age2 <- as.numeric(sprintf("%.2f", (last14sum_age2/poblacio_age2)*100000))
	  ia14_age3 <- as.numeric(sprintf("%.2f", (last14sum_age3/poblacio_age3)*100000))
	  ia14_age4 <- as.numeric(sprintf("%.2f", (last14sum_age4/poblacio_age4)*100000))

	  plotdata2[i, "IA14_age1"] <- ia14_age1
	  plotdata2[i, "IA14_age2"] <- ia14_age2
	  plotdata2[i, "IA14_age3"] <- ia14_age3
	  plotdata2[i, "IA14_age4"] <- ia14_age4
	}

	for (i in (1:13)) {
	  plotdata2[i, "IA14_age1"] <- NA
	  plotdata2[i, "IA14_age2"] <- NA
	  plotdata2[i, "IA14_age3"] <- NA
	  plotdata2[i, "IA14_age4"] <- NA
	}


	#crear el grafic
	plot2 <- ggplot(plotdata2, aes(x=DATA)) +
	  geom_line(aes(y=IA14_age1, colour="de 0 a 19 anys")) +
	  geom_line(aes(y=IA14_age2, colour="de 20 a 39 anys")) +
	  geom_line(aes(y=IA14_age3, colour="de 40 a 69 anys")) +
	  geom_line(aes(y=IA14_age4, colour="70 anys o mes")) +
	  scale_colour_manual("",
	                      breaks = c("de 0 a 19 anys", "de 20 a 39 anys", "de 40 a 69 anys", "70 anys o mes"),
	                      values = c("de 0 a 19 anys"="red", "de 20 a 39 anys"="gold", "de 40 a 69 anys"="green", "70 anys o mes"="blue")
	  ) +
	  scale_x_date(date_breaks="1 month",  labels = function(x) months[as.numeric(substr(x, 6, 7))])

	title2 <- paste("IA14 per franges d'edat 2021", comarca, sep = "\n")

	plot2 = plot2 + labs(title=title2, x="Mesos", y="IA14") +
	  theme(plot.title = element_text(hjust = 0.5))


##posar grafics 2 en 1

	finalplot <- multiplot(plot1, plot2, cols=1)
	print(finalplot)

	#guardar el grafic
	if (save == TRUE) {
	  filepath <- tempfile(pattern="plot", fileext = ".png")
		#png(filename = filepath, res = 400, height = 2700, width = 2400)
	  png(filename = filepath, res = 200, height = 1350, width = 1200, type = "cairo")
		multiplot(plot1, plot2, cols=1)
	  dev.off()
		return(filepath)
	}

}
