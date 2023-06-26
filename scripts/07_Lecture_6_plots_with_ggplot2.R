## ----------------------------------------------------- ##
## Lecture 6: Sintaxis of ggplot2: Plotting with ggplot2 ##
## ----------------------------------------------------- ##

set.seed(123999)
library(tidyverse)
library(ggplot2)
options(scipen = 10000, digits = 3)
	
	## 1. Loading the raw dataframe and summarizing it
	df <- read.csv('./data/fisicoquimicos_mimosa.csv', header = T)
	str(df)
	summary(df)
	head(df)
	
	## Visualizar "counts" de variables categóricas (o columnas con texto que agrupan datos)
	base::table(df$Treatment)
	base::table(df$Tratamiento)
	base::table(df$Day)
	
	## 2. Dando formato a mi tabla de datos para facilitar análisis: puliendo mi base de datos
	
	data_long <- df %>% 
		dplyr::select(-Treat_day, -CE, -CRA, -Nitrogen) %>% #voy a eliminar estas columnas porque no me interesan
		dplyr::rename(label = Treatment, Treatment = Tratamiento, Nitrogen = g.N.Kg.s, Carbono = COT) %>% # renombro columnas para facilidad de uso
		tidyr::pivot_longer(cols = -1:-3, names_to = "variable_medida", values_to = "values") %>% # convierto a long format para estadísticos
		# dplyr::mutate(variable_medida = stringr::str_replace_all(string = variable_medida, pattern = "COT", replacement = "Carbono")) %>% 
		dplyr::arrange(Day, variable_medida, Treatment)
		
	data <- data_long %>% 
		dplyr::group_by(Day, Treatment, variable_medida) %>% # agrupo datos para estadísticos
		dplyr::summarise(sample_size = n(), # cuántas réplicas tiene cada determinación
				 promedio = mean(values, na.rm = TRUE), # determinar promedio por día, tratamiento y variable
				 sd = sd(values), # desviación estándar por día, tratamiento y variable
				 se = sd(values)/sqrt(sample_size), # error estándar de la media por día, tratamiento y variable
				 .groups = "drop") %>% 
		dplyr::arrange(Day, variable_medida, Treatment) # ordernar columnas para mejorar visualización
		
	## 1. Barplot de cada variable medida:
	data %>% 
		dplyr::filter(variable_medida == "pH") %>% 
		ggplot() + theme_bw(base_size = 20) +
		labs (x = "Tiempo (días)", y = "pH", title = "Dinámica del pH del suelo por 70 días", subtitle = "Suelos contaminados con hidrocarburos") +
		geom_bar(mapping = aes(x = factor(Day), y = promedio, fill = Treatment), 
			 stat = "identity",
			 position = position_dodge(0.9),
			 width = 0.8) +
		scale_fill_manual(name = "Tratamientos", values = c("cyan", "steelblue", "blue")) +
		scale_y_continuous(name = "pH del suelo", limits = c(0,8), breaks = seq(0,8,2), expand = c(0,0)) +
		theme(panel.grid = element_blank(),
		      legend.text = element_text(size = 10, colour = "red", face = "italic"))
	
	## 2. Line plot
	data %>% 
		dplyr::filter(variable_medida == "pH") %>% 
		ggplot() + theme_bw(base_size = 20) +
		labs (x = "Tiempo (días)", y = "pH", title = "Dinámica del pH del suelo por 70 días", subtitle = "Suelos contaminados con hidrocarburos") +
		geom_line(mapping = aes(x = factor(Day), y = promedio, group = Treatment, color = Treatment),
			  linewidth = 1.5) +
		geom_point(mapping = aes(x = factor(Day), y = promedio, group = Treatment), color = "black", size = 3) +
		scale_y_continuous(name = "pH del suelo", limits = c(4,8), breaks = seq(4,8,1), expand = c(0,0)) +
		theme(panel.grid = element_blank(),
		      legend.text = element_text(size = 14, colour = "black"),
		      legend.position = c(0.8, 0.8),
		      plot.title = element_text(color = "black", size = 20, face = "bold"),
		      plot.subtitle = element_text(color = "steelblue", size = 16, face = "italic"))
		

	## 3. Boxplot
	data_long %>% 
		dplyr::filter (variable_medida == "Carbono") %>%
		ggplot(aes(x=factor(Day),y=values, fill=Treatment)) +
		theme_bw(base_size = 20) +
		geom_boxplot(outlier.shape=NA) +
		geom_point(aes(x=factor(Day),y=values, color=Treatment), size = 3, position=position_jitterdodge(0.7)) +
		labs (x = "Tiempo (días)", y = "Carbono total (g-C/kg suelo", 
		      title = "Dinámica de Carbono del suelo por 70 días", 
		      subtitle = "Suelos contaminados con hidrocarburos") +
		scale_y_continuous(name = "Carbono del suelo (g-C/kg)", limits = c(10,25), breaks = seq(10,25,5), expand = c(0,0)) +
		theme(panel.grid = element_blank(),
		      legend.text = element_text(size = 14, colour = "black"),
		      # legend.position = c(0.8, 0.8),
		      plot.title = element_text(color = "black", size = 20, face = "bold"),
		      plot.subtitle = element_text(color = "steelblue", size = 16, face = "italic"))
	
		
	