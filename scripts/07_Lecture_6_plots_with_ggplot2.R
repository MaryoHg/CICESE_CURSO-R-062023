## ----------------------------------------------------- ##
## Lecture 6: Sintaxis of ggplot2: Plotting with ggplot2 ##
## ----------------------------------------------------- ##

## Edition made by: Mario HG 01-07-2023 - 13:45 h.


set.seed(123999)
library(tidyverse)
library(ggplot2)
options(scipen = 10000, digits = 3)
	
	## 1. Loading the raw dataframe and summarizing it
	df <- read.csv('./data/fisicoquimicos_mimosa.csv', header = T)
	str(df)
	summary(df) # some NA's appears so I must be careful with this type of data
	head(df)
	
	## Visualizar "counts" de variables categóricas (o columnas con texto que agrupan datos)
	base::table(df$Treatment) # indicates 3 rows per level of "Treatment"
	base::table(df$Tratamiento) # indicates 18 rows per level of "Tratamiento"
	base::table(df$Day) # indicate 9 rows per level of "Day"
	
	## 2. Dando formato a mi tabla de datos para facilitar análisis: puliendo mi base de datos
	
	data_long <- df %>% 
		dplyr::select(-Treat_day, -CE, -CRA, -Nitrogen) %>% #voy a eliminar estas columnas porque no me interesan
		dplyr::rename(label = Treatment, Treatment = Tratamiento, Nitrogen = g.N.Kg.s, Carbono = COT) %>% # renombro columnas para facilidad de uso
		tidyr::pivot_longer(cols = -1:-3, names_to = "variable_medida", values_to = "values") %>% # convierto a long format para estadísticos
		# dplyr::mutate(variable_medida = stringr::str_replace_all(string = variable_medida, pattern = "COT", replacement = "Carbono")) %>% 
		dplyr::arrange(Day, variable_medida, Treatment); head(data_long)
		
	## 3. Determino los estadístics de mis datos: variables (factores) que agrupan mis valores son Day, Treatment ay medición (pH, N or Carbono)
	data <- data_long %>% 
		dplyr::group_by(Day, Treatment, variable_medida) %>% # agrupo datos para estadísticos
		dplyr::summarise(sample_size = n(), # cuántas réplicas tiene cada determinación
				 promedio = mean(values, na.rm = TRUE), # determinar promedio por día, tratamiento y variable
				 sd = sd(values), # desviación estándar por día, tratamiento y variable
				 se = sd(values)/sqrt(sample_size), # error estándar de la media por día, tratamiento y variable
				 .groups = "drop") %>% 
		dplyr::arrange(Day, variable_medida, Treatment); head(data) # ordernar columnas para mejorar visualización
		
	## 4. Barplot de cada variable medida:
	
	## 4.1. Ploting Soil pH
	data %>% 
		dplyr::filter(variable_medida == "pH") %>% 
		ggplot(mapping = aes(x = factor(Day), y = promedio, fill = Treatment)) + 
		theme_bw(base_size = 16) +
		labs (x = "Tiempo (días)\n", y = "pH del suelo\n", title = "Dinámica del pH del suelo por 70 días") +
		geom_bar(stat = "identity", width = 0.8, position = position_dodge(0.9)) +
		geom_errorbar(aes(ymin=promedio-se, ymax=promedio+se), colour="black", width = 0.5,
			      position = position_dodge(0.9), alpha = 0.7) + ## barras del error estándar agregadas
		scale_fill_manual(name = "Tratamientos", values = c("gray70", "steelblue", "orange")) +
		scale_y_continuous(name = "pH del suelo", limits = c(0,7), breaks = seq(0,7,1.5), expand = c(0,0)) +
		theme(panel.grid = element_blank(),
		      legend.text = element_text(size = 12, colour = "black"),
		      axis.text = element_text(size = 12, colour = "black"))
	
	## 2. Line plot
	data %>% 
		dplyr::filter(variable_medida == "pH") %>% 
		ggplot(mapping = aes(x = factor(Day), y = promedio, color = Treatment)) + 
		theme_bw(base_size = 16) +
		labs (x = "Tiempo (días)", y = "pH", title = "Dinámica del pH del suelo por 70 días") +
		geom_line(aes(group = Treatment), linewidth = 1.5) +
		geom_errorbar(aes(ymin=promedio-se, ymax=promedio+se), colour="black", width = 0.2, position = position_dodge(0.9), alpha = 0.7) +
		geom_point(mapping = aes(x = factor(Day), y = promedio, group = Treatment), color = "black", size = 3) +
		scale_color_manual(values = c("gray70", "steelblue", "orange")) +
		scale_y_continuous(name = "pH del suelo", limits = c(4,8), breaks = seq(4,8,1), expand = c(0,0)) +
		theme(panel.grid = element_blank(),
		      legend.text = element_text(size = 14, colour = "black"),
		      legend.position = c(0.8, 0.8),
		      plot.title = element_text(color = "black", size = 16, face = "bold"))
		

	## 3. Boxplot
	data_long %>% 
		dplyr::filter (variable_medida == "pH") %>%
		ggplot(aes(x=factor(Day),y=values, fill=Treatment)) +
		theme_bw(base_size = 16) +
		geom_boxplot(outlier.colour = "orange") +
		# geom_point(size = 2, position = "jitter", aes(x = factor(Day), y = values, color = Treatment)) +
		labs (x = "Tiempo (días)", y = "pH del suelo", 
		      title = "Dinámica de Carbono del suelo por 70 días") +
		scale_y_continuous(name = "pH del suelo", limits = c(4,7), breaks = seq(4,7,0.5), expand = c(0,0),
				   labels = c(4,"",5,"",6,"",7)) +
		scale_fill_manual(values = c("gray70", "steelblue", "orange")) +
		theme(panel.grid = element_blank(),
		      legend.text = element_text(size = 14, colour = "black"),
		      # legend.position = c(0.8, 0.8),
		      plot.title = element_text(color = "black", size = 16, face = "bold"),
		      axis.text = element_text(color = "black", size = 16),
		      panel.grid.major.y = element_line(linewidth = 0.3, color = "gray80", linetype = "dashed"))
	
		
	## Mergin plots:
	library(patchwork) # to save multiple plots into a single one
	grDevices::svg(filename = "pH_del_suelo.svg", width = 15, height = 5) # define where and size of figure to save
	allplots <- (barp + theme(legend.position = "none") + labs (title = element_blank())) | (lp + labs (y = element_blank())) | (bp + theme(legend.position = "none") + labs (y = element_blank(), title = element_blank())) ## all plots joined
	allplots + plot_annotation(tag_levels = 'a', tag_suffix = ")");  ## add label to each one: a), b) and c)
	dev.off() # print and save the plot into defined location on 99 line of code

	
	## terminamos.
	## ACTIVIDAD: podemos graficar Nitrógeno y CRA, sólo hay que editar el código e ir viendo que todo cuadre print.