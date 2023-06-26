## -------------------------------------------------------------------- ##
## Interconvertir: formatos "ancho" y "largo" de una base de datos en R ##
## -------------------------------------------------------------------- ##
set.seed(123999)
library(tidyverse, warn.conflicts = F)

## ------------------------------------- ##
## Converting from wide to long database ##
## ------------------------------------- ##

	## 1. Cargamos nuestra base de datos
	data <- read.delim(text = "
Team	Points	Assists	Rebounds
A	88	12	22
B	91	17	28
C	99	24	30
D	94	28	31", 
			   stringsAsFactors = F)
	
	## 2. Identificamos que está en formato "ancho"
	## Modo ancho detectado
	
	
	## 3. Convertimos a formato largo con la función: pivot_longer()
	# 150 rows x 4 variables = 600 new rows.
	data_long <- data %>%
		tidyr::pivot_longer(cols = -Team, names_to = "mycolnames", values_to = "points") #%>% View()
	
	data_long <- data %>%
		tidyr::pivot_longer(cols = -1, names_to = "mycolnames", values_to = "points") #%>% View()
	
	data_long <- data %>%
		tidyr::pivot_longer(cols = 2:4, names_to = "mycolnames", values_to = "points") #%>% View()
	
	
## ------------------------------------- ##
## Converting from long to wide database ##
## ------------------------------------- ##
	data_wide <- data_long %>% 
		tidyr::pivot_wider(values_from = "points", names_from = "mycolnames")
	
	