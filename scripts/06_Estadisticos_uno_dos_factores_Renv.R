## -------------------------------------------------------- ##
## CONOCIENTO MI BASE DE DATOS: ¿QUÉ TENGO EN MI DATAFRAME? ##
## -------------------------------------------------------- ##
set.seed(123999)
library(Rmisc)
library(tidyverse, warn.conflicts = F)


	## 1. Importamos nuestra base de datos: ¡ES UN TSV! (corroborarlo!)
	df <- read.delim(file = 'data/file.tsv', stringsAsFactors = T)
	head(df)
	str(df)
	summary(df)
	
	## 2. Conociento mi base de datos: functión dim()
	dim(df) # observaciones (rows) x varirables (columnas)
	
	## 3. Conociendo mi base de datos: función summary()
	summary(df)

## ------------------------------------------------------ ##
## ESTADÍSTICOS BÁSICOS CON LA FUNCIÓN Rmisc::summarySE() ##
## ------------------------------------------------------ ##

## -------------- ##
## UN SÓLO FACTOR ##
## -------------- ##
	## Estadísticos de una sola variable de respuesta (Sepal.Lenght)
	## nota: tengo que correr la misma función sobre cada variable medida, por estar en formato wide
	Rmisc::summarySE(data = df, measurevar = "Sepal.Length", groupvars = c("Species"), na.rm = T) #%>% ggplot() + theme_bw(base_size = 20) + geom_bar(stat = "identity", position = "dodge", aes(x = Species, y = Sepal.Length, fill = Species)) + scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2))+ theme(legend.position = "none", panel.grid = element_blank())

	
	## Convertir a modo "long", y calcular estadísticas un sólo comando:
	## Si convierto a modo long, me es más fácil calcular estadísticos y graficar a la vez
	df_long <- df %>%
		tidyr::pivot_longer(cols = 1:4, names_to = "variable", values_to = "length") 
	
	head(df_long)
		
	df_long %>% 
		dplyr::group_by(Species, variable) %>%  # factores que van a agrupar la variable "lenght"
		dplyr::summarise(sample_size = n(), # sample size (n) calculation
				 tamaño_promedio = mean(length), # average calculation
				 standar_deviation = sd (length), # standard deviation calculation
				 se = sd(length)/sqrt(sample_size), # standar error of the mean calculation
				 .groups = "drop") %>% # ignore package warning
		dplyr::arrange(variable, Species) %>% #arrange data by "variable" and then by "Species" (alphabetically)
		ggplot() + 
		theme_bw(base_size = 20) + 
		labs (x = "Especie", y = "Longitud (cm)") +
		geom_bar(aes(x = Species, y = tamaño_promedio, fill = variable), 
			 stat = "identity", 
			 position = position_dodge(0.9), 
			 width = 0.8) + 
		scale_y_continuous(limits = c(0,8), breaks = seq(0,8,1), labels = c(0,"",2, "",4,"",6,"",8)) + 
		theme(panel.grid = element_blank())


##################
## ------------ ##
## DOS FACTORES ##
## ------------ ##
	
	## 1. Base de datos más grande: de la web
	df <- utils::read.csv(file = 'https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder_wide.csv')
	df <- df %>% dplyr::select(1,2, starts_with("pop")) #%>% View() ## filtro columnas con "población"
	summary(df)
	
	## 2. Stats de una sola columna por estar en formato "wide"
	Rmisc::summarySE(data = df, measurevar = "pop_2007", groupvars = c("continent"), na.rm = T)
	
	
	## 3. Convertir a "long", y determinar estadísticas de toda la base de datos (en una sóla línea)
	df_long <- df %>% 
		tidyr::pivot_longer(cols = -1:-2, names_to = "year", values_to = "population")
	
	## COPIA, PEGA Y EDITA EL CÓDIGO DE LAS LÍNEAS 42 A 56 DE ESTE SCRIPT PARA CALCULAR LOS STATS
	
	
	## dudas ?? ##
	
###########################
## --------------------- ##
## EXPRESIONES REGULARES ##
## --------------------- ##
	
	df_long %>%
		dplyr::mutate(year = stringr::str_replace_all(string = year, pattern = "pop_", replacement = ""),
			      year = stringr::str_replace_all(string = year, pattern = "pop_", replacement = "year_")) %>%
			View()
	
	df <- data.frame(Mineral = c("Zfeldspar", "Zgranite", "ZSilica"),
			Confidence = c("ZLow", "High", "Med"),
			Coverage = c("sub", "sub", "super"),
			Aspect = c("ZPos", "ZUnd", "Neg")); df
	
	df %>% 
		dplyr::mutate_all(.funs = funs(stringr::str_replace_all(., "Z", "")))
	
	