# Instalar paquetes necesarios. 
packages <- c("dplyr", "sf", "ggplot2", "tmap", "nlme", "emmeans", "multcompView", "multcomp")

install.packages(setdiff(packages, rownames(installed.packages())))  


