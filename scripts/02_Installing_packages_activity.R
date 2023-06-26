## Instalar paqueterías en mi ambiente R ##
##---------------------------------------##
.libPaths()


## Instalar paqueterías desde CRAN

	??install.package()
	install.packages(c('ggplot2', 'RColorBrewer', 'Rmisc', 'tidyverse'), dep = T)


## Instalar paqueterías desde Github
	## Con devtools:: package
	
	if (!requireNamespace("devtools", quietly = TRUE)){install.packages("devtools")}
	devtools::install_github("jbisanz/qiime2R")

## Con la paquetería remote::
	## Note: remotes:: package is a lightweight version of devtools function install_github() function
	## Note 2: They both do the same exactly

	remotes::install_github("nrennie/ggflowchart")


## Using Bioconductor: at <https://www.bioconductor.org/install/>
	## installing BiocManager package()
	
	if (!require("BiocManager", quietly = TRUE))
		install.packages("BiocManager")
	BiocManager::install(version = "3.17")
	
	## Package install with BiocManager() - an example
	