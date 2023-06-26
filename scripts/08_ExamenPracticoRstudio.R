############### EXAMEN PRACTICO EN R ##########################
###########  Mimosa pigra #####################################

## Cargando librerias y datos

library(ggplot2)

data <- read.csv("C:/Users/Vale/Desktop/Mimosa_pigra_examen.csv")

head(data)
str(data)

## GRÁFICO DE CAJAS Y BIGOTES: DE TODOS LOS MUESTREOS
## calculo max y min para mi eje "Y"
max(data$pH); min(data$pH)

ggplot(data = data, aes(x = Tratamiento, y = pH, colour = Tratamiento)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Tratamiento") +
        scale_y_continuous(name = "pH", 
                           breaks = seq(from = 0, to = 8, by = 2),
                           limits=c(0,8)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$COT, na.rm = "T")
min(data$COT, na.rm = "T")

ggplot(data = data, aes(x = Tratamiento, y = COT, colour = Tratamiento)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Tratamiento") +
        scale_y_continuous(name = "COT", 
                           breaks = seq(from = 0, to = 30, by = 5),
                           limits=c(0,30)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$CRA, na.rm = "T")
min(data$CRA, na.rm = "T")

ggplot(data = data, aes(x = Tratamiento, y = CRA, colour = Tratamiento)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Tratamiento") +
        scale_y_continuous(name = "CRA", 
                           breaks = seq(from = 0, to = 700, by = 150),
                           limits=c(0,700)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))


## este loop lo use para los estadisticos del factor tratamiento
for(i in 4:ncol(data))
{
        column <- names(data[i])
        ## aquí determino mi anova para cada columna
        avz <- summary(aov(data[,i] ~ Tratamiento, data = data))
        
        ## aquí determino mi poshoc para cada columna
        tukey<- TukeyHSD(aov(data[,i] ~ Tratamiento, data = data))
        
        ##### TUKEY HSD VALUES ####
        print(column)
        print(setNames(avz,column))
        print(setNames(tukey, column))
}

################################
head(data)

library(dplyr)
data %>% 
        group_by(Tratamiento) %>%
        summarise(
                pH =     mean(pH,na.rm = FALSE),
                COT =     mean(COT, na.rm = F),
                CRA =      mean(CRA, na.rm = F))


library(dplyr)
data %>% 
        group_by(Tratamiento) %>%
        summarise(
                pH =     sd(pH,na.rm = "F"),
                COT =     sd(COT, na.rm = "F"),
                CRA =      sd(CRA, na.rm = "F"))

#####################

## aquí separo en subset mis datos: 1 al 3

C0 <- subset(data, Tratamiento == "C0"); nrow(C0)
C1 <- subset(data, Tratamiento == "C1"); nrow(C1)
C2 <- subset(data, Tratamiento == "C2"); nrow(C2)

############## C0 #################

## calculo max y min para mi eje "Y"
max(C0$pH); min(C0$pH)

ggplot(data = C0, aes(x = Treatment_day, y = pH, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treat_day") +
        scale_y_continuous(name = "pH", 
                           breaks = seq(from = 0, to = 8, by = 2),
                           limits=c(0,8)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$COT, na.rm = "T")
min(data$COT, na.rm = "T")

ggplot(data = C0, aes(x = Treatment_day , y = COT, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "COT", 
                           breaks = seq(from = 0, to = 30, by = 5),
                           limits=c(0,30)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$CRA, na.rm = "T")
min(data$CRA, na.rm = "T")

ggplot(data = C0, aes(x = Treatment_day, y = CRA, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "CRA", 
                           breaks = seq(from = 0, to = 700, by = 150),
                           limits=c(0,700)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))


## este loop lo use para los estadisticos del muestreo 1
for(i in 4:ncol(C0))
{
        column <- names(C0[i])
        ## aquí determino mi anova para cada columna
        avz <- summary(aov(C0[,i] ~ Treatment_day, data = C0))
        
        ## aquí determino mi poshoc para cada columna
        tukey<- TukeyHSD(aov(C0[,i] ~ Treatment_day, data = C0))
        
        ##### TUKEY HSD VALUES ####
        print(column)
        print(setNames(avz,column))
        print(setNames(tukey, column))
}

################################
head(data)

library(dplyr)
C0 %>% 
        group_by(Treatment_day) %>%
        summarise(
                pH =     mean(pH,na.rm = "F"),
                COT =     mean(COT, na.rm = "F"),
                CRA =      mean(CRA, na.rm = "F"))


library(dplyr)
C0 %>% 
        group_by(Treatment_day) %>%
        summarise(
                pH =     sd(pH,na.rm = "F"),
                COT =     sd(COT, na.rm = "F"),
                CRA =      sd(CRA, na.rm = "F"))

#####################
############## C1 #################

## calculo max y min para mi eje "Y"
max(C0$pH); min(C0$pH)

ggplot(data = C1, aes(x = Treatment_day, y = pH, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "pH", 
                           breaks = seq(from = 0, to = 8, by = 2),
                           limits=c(0,8)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$COT, na.rm = "T")
min(data$COT, na.rm = "T")

ggplot(data = C1, aes(x = Treatment_day , y = COT, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "COT", 
                           breaks = seq(from = 0, to = 30, by = 5),
                           limits=c(0,30)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$CRA, na.rm = "T")
min(data$CRA, na.rm = "T")

ggplot(data = C1, aes(x = Treatment_day, y = CRA, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "CRA", 
                           breaks = seq(from = 0, to = 700, by = 150),
                           limits=c(0,700)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))


## este loop lo use para los estadisticos del muestreo 1
for(i in 4:ncol(C1))
{
        column <- names(C1[i])
        ## aquí determino mi anova para cada columna
        avz <- summary(aov(C1[,i] ~ Treatment_day, data = C1))
        
        ## aquí determino mi poshoc para cada columna
        tukey<- TukeyHSD(aov(C1[,i] ~ Treatment_day, data = C1))
        
        ##### TUKEY HSD VALUES ####
        print(column)
        print(setNames(avz,column))
        print(setNames(tukey, column))
}

################################
head(data)

library(dplyr)
C1 %>% 
        group_by(Treatment_day) %>%
        summarise(
                pH =     mean(pH,na.rm = "F"),
                COT =     mean(COT, na.rm = "F"),
                CRA =      mean(CRA, na.rm = "F"))


library(dplyr)
C1 %>% 
        group_by(Treatment_day) %>%
        summarise(
                pH =     sd(pH,na.rm = "F"),
                COT =     sd(COT, na.rm = "F"),
                CRA =      sd(CRA, na.rm = "F"))

#####################
####################################################
############## C2 #################

## calculo max y min para mi eje "Y"
max(C2$pH); min(C2$pH)

a <- ggplot(data = C2, aes(x = Treatment_day, y = pH, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "pH", 
                           breaks = seq(from = 0, to = 8, by = 2),
                           limits=c(0,8)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$COT, na.rm = "T")
min(data$COT, na.rm = "T")

b <- ggplot(data = C2, aes(x = Treatment_day , y = COT, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "COT", 
                           breaks = seq(from = 0, to = 30, by = 5),
                           limits=c(0,30)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

max(data$CRA, na.rm = "T")
min(data$CRA, na.rm = "T")

c <- ggplot(data = C2, aes(x = Treatment_day, y = CRA, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Treatment_day") +
        scale_y_continuous(name = "CRA", 
                           breaks = seq(from = 0, to = 700, by = 150),
                           limits=c(0,700)) +
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))


library(cowplot)

cowplot::plot_grid(a, b, c, labels = c("A", "B", "C"), align = "v", nrow = 3)

## este loop lo use para los estadisticos del muestreo 1
for(i in 4:ncol(C2))
{
        column <- names(C2[i])
        ## aquí determino mi anova para cada columna
        avz <- summary(aov(C2[,i] ~ Treatment_day, data = C2))
        
        ## aquí determino mi poshoc para cada columna
        tukey<- TukeyHSD(aov(C2[,i] ~ Treatment_day, data = C2))
        
        ##### TUKEY HSD VALUES ####
        print(column)
        print(setNames(avz,column))
        print(setNames(tukey, column))
}

################################
head(data)

library(dplyr)
C2 %>% 
        group_by(Treatment_day) %>%
        summarise(
                pH =     mean(pH,na.rm = "F"),
                COT =     mean(COT, na.rm = "F"),
                CRA =      mean(CRA, na.rm = "F"))


library(dplyr)
C2 %>% 
        group_by(Treatment_day) %>%
        summarise(
                pH =     sd(pH,na.rm = "F"),
                COT =     sd(COT, na.rm = "F"),
                CRA =      sd(CRA, na.rm = "F"))

########################## EMPLEANDO FACETING ###################3

############ referencia: https://ggplot2-book.org/facet.html 

#####################
library(ggplot2)
ggplot(data = data, aes(x = Treatment_day, y = CRA, colour = Treatment_day)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Tratamiento") +
        scale_y_continuous(name = "CRA", 
                           breaks = seq(from = 0, to = 700, by = 150),
                           limits=c(0,700)) +
        facet_wrap(~Tratamiento,  ncol = 1)+
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

#######

# New facet label names for dose variable
#dose.labs <- c("D0.5", "D1", "D2")
#names(dose.labs) <- c("0.5", "1", "2")

# New facet label names for supp variable
supp.labs <- c("Day 7", "Day 14", "Day 25")
names(supp.labs) <- c("7", "14", "25")


ggplot(data = data, aes(x = Tratamiento, y = CRA, colour = Tratamiento)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Tratamiento") +
        scale_y_continuous(name = "Capacidad de retención de agua (g/kg s)", 
                           breaks = seq(from = 0, to = 700, by = 150),
                           limits=c(0,700)) +
        facet_wrap(~Day,  ncol = 3, dir = "h", drop= TRUE, labeller = labeller(Day = supp.labs))+
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

################3

ggplot(data = data, aes(x = Tratamiento, y = pH, colour = Tratamiento)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("Tratamiento") +
        scale_y_continuous(name = "pH", 
                           breaks = seq(from = 0, to = 8, by = 2),
                           limits=c(0,8)) +
        facet_wrap(~Day,  ncol = 3, dir = "h", drop= TRUE, labeller = labeller(Day = supp.labs))+
        theme_light() + 
        theme(legend.position = "none",
              text = element_text(size = 16, colour = "black"))

library(ggplot2)
##################3
plot <- ggplot(data = data, aes(x = Tratamiento, y = COT, colour = Tratamiento)) +
        geom_boxplot(size = 1.5) +
        geom_point(size = 3, color = "black") +
        xlab ("\nTratamiento") +
        #ylab("\nCarbono orgánico total (g/kg s)")+
        scale_y_continuous(name = "Carbono orgánico total (g/kg s)\n", 
                           breaks = seq(from = 0, to = 30, by = 5),
                           limits=c(0,30)) +
        facet_wrap(~Day,  ncol = 3, dir = "h", drop= TRUE, labeller = labeller(Day = supp.labs))+
        theme_light() + 
        theme(legend.position = "none",
              strip.text.x = element_text(
                      size = 18, color = "black", face = "bold"),
              axis.text = element_text(size = 16, color = "black", face = "bold"),
              axis.title = element_text(size = 18, color = "black"))


plot
library(ggpubr)

plot + stat_compare_means(comparisons = "analisis") +
        stat_compare_means(label.y = 50)  


plot + stat_compare_means(comparisons = analisis)

plot + stat_compare_means(method = "anova", label.y = 40) +
        stat_compare_means(label = "p.signif", method = )


stat_compare_means(method = "anova", label.y = 40)+      # Add global p-value
        stat_compare_means(label = "p.signif", method = "t.test",
                           ref.group = ".all.") 
###################

analisis <- list (c("C0_7","C0_14"), c("C0_7","C0_25"), c("C0_14","C0_25"),
                  c("C1_7","C1_14"), c("C1_7","C1_25"), c("C1_14","C1_25"),
                  c("C2_7","C2_14"), c("C2_7","C2_25"), c("C2_14","C2_25"))
