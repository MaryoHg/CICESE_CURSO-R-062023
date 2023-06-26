##---------------------------------------------------------------------##
## CONOCIENDO MI AMBIENTE EN R: PAQUETERÍAS, VERSIONES Y CÓMO CITARLOS ##
##---------------------------------------------------------------------##

##-------------------------------##
## CONOCER MI SESIÓN POR DEFECTO:
##-------------------------------##

## ¿QUÉ PAQUETERÍAS SE CARGAN POR DEFAULT?
	## Imprime la versión de R y su información, el sistema operative, 
	## y los paquetes cargados

	sessionInfo()

## ¿Cómo cargo y una paquetería ya instaladada?

	library(ggplot2)		## cargar (loading)
	unloadNamespace("ggplot2")	## quitar (unloading)

## ¿Cómo obtengo ayuda de una paquetería o de una función de una paquetería?
	
	help(ggplot, package="ggplot2")
	??ggplot()

## Conocer la versión instalada:

	packageVersion('ggplot2')

## ¿Cómo citar una paquetería?
	
	citation(package = "ggplot2")

	
	
##-------------------------##	
## INSTALAR UNA PAQUETERÍA ##	
##-------------------------##
	## ¿Dónde están mis paquetes intalados por default?
	.libPaths()
	
	## Comando para instalar una librería o paquetería: forma default y automática
	??install.packages()
	utils::install.packages(pkgs = "ggplot2", dependencies = T)
	
	## Comando para configurar un lugar diferente a donde quiero que se instale
	utils::install.packages(pkgs = "ggplot2", dependencies = T, lib = '/home/mario/R')
	
	
	
	
##----------------------------------##	
## DENTRO DE UNA PAQUETERÍA CARGADA ##	
##----------------------------------##

	base::
	utils::
	ggplot2::theme

	## Pregunta: ¿Cuántas funciones tiene cada paquetería?