# install.packages("xlsx")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("car")
# install.packages("rcompanion") # not available for verson 3.6.3 
# install.packages("FSA")
# install.packages("ggplot2")
# install.packages("rJava")
# install.packages("Rtools")
# install.packages("psych")
# install.packages("Hmisc")

# load libraries

library("xlsx")
library("dplyr")
library("tidyverse")
library("car")
library("rcompanion")
library("FSA")
library("ggplot2")
library("psych")
library("Hmisc")


# work dir

getwd()
setwd("e:/RNF_2021-2022/Remodel F-V physiol load-1")
# setwd("/home/atstkiy/R/Remodel F-V physiol load-1")

# load data

My_data_table <- read.xlsx("phys_reg.xlsx", sheetIndex = 1)

colnames(My_data_table) <- c("name", "afterload", 'Velocity', 'Work', 'Les', 'temperature', 'camera', 'group')

# view structure data

str(My_data_table)

# factors

My_data_table$camera <- as.factor(My_data_table$camera)
My_data_table$group <- as.factor(My_data_table$group)
My_data_table$name <- as.factor(My_data_table$name)
My_data_table$afterload <- as.factor(My_data_table$afterload)


# view design - balansed or imbalansed

table(My_data_table$camera, My_data_table$group)

vel <- My_data_table[c("name", "afterload", 'Velocity','camera', 'group')]

vel <- na.omit(vel) 
# vel <- vel[vel$afterload != 1,]
vel <- vel[vel$afterload == 0.2,]
vel <- vel[c('Velocity','camera', 'group')]

# descriptive statistics and normal distribution check

norm_distr <- vel %>%
  group_by(camera, group, na.rm=TRUE) %>%
  summarise(n = n(), var = var(Velocity, na.rm=T), sd = sd(Velocity, na.rm=T), method = shapiro.test(Velocity)$method, p.value = shapiro.test(Velocity)$p.value)

# check homogeneity of variance 
# Levene Test

leveneTest(Velocity ~ camera * group, data = vel)
leveneTest(Velocity ~ group * camera, data = vel)

# Bartlett test

bartlett.test(Velocity ~ camera, data = vel)
bartlett.test(Velocity ~ group, data = vel)
bartlett.test(Velocity ~ afterload, data = vel)

# F test (min group var, max group var)

testtable <- summarise(vel %>% group_by(camera, group), var = var(Velocity, na.rm=T))
testtable      
filter(testtable, var==min(testtable$var))
filter(testtable, var==max(testtable$var))
var.test(filter(vel, camera=="ATRIUM", group=="CONTROL", afterload==0.1)$Velocity,filter(vel, camera=="VENTRICLE", group=="MCT", afterload==0.9)$Velocity)
var.test(filter(vel, camera=="ATRIUM", group=="CONTROL", afterload==0.1)$Velocity,filter(vel, camera=="VENTRICLE", group=="MCT", afterload==0.9)$Velocity)$p.value



# Two-way Anova
# The recommended method are the Type-III sums of squares for unbalanced designs

two_way_anova = aov(Velocity ~ camera * group, data = vel)
"two_way_anova = aov(Data ~ Camera + State + Camera:State, data = My_data_table)"


summary(two_way_anova)

#if design imbalanced, do not use summary, use anova below 

Anova(two_way_anova, type = "III")

# Simple Tests Post-Hoc 

TukeyHSD(aov(Velocity ~ camera + afterload, data = filter(vel, group=="CONTROL")))
TukeyHSD(aov(Velocity ~ camera + afterload, data = filter(vel, group=="MCT")))

TukeyHSD(aov(Velocity ~ camera, data = filter(vel, group=="CONTROL")))
TukeyHSD(aov(Velocity ~ camera, data = filter(vel, group=="MCT")))
TukeyHSD(aov(Velocity ~ camera * group, data = vel))
TukeyHSD(aov(Velocity ~ camera + group, data = vel))
# TukeyHSD(aov(Velocity ~ camera, data = filter(My_data_table, group=="DM2")))
TukeyHSD(aov(Velocity ~ group, data = filter(vel, camera=="VENTRICLE")))
TukeyHSD(aov(Velocity ~ group, data = filter(vel, camera=="ATRIUM")))
# TukeyHSD(aov(Velocity ~ group, data = filter(My_data_table, camera=="S")))

# The Scheirer?Ray?Hare test is a nonparametric test used for a two-way factorial design
# Note that for unbalanced designs, the scheirerRayHare function uses a type-II sum-of-squares
vel <- vel[complete.cases(vel),]
# scheirerRayHare(Velocyty ~ camera * group, data = My_data_table, type=1)
scheirerRayHare(Velocity ~ camera * group, data = vel, type = "III")
# scheirerRayHare(Velocyty ~ camera * group, data = My_data_table, type=3)

# Appropriate post-hoc tests might be Dunn test for each significant factor or interaction. Запись идентична выше для Tukey
dunnTest(Velocity ~ camera, data = filter(vel, group=="CONTROL"), method="bonferroni")
dunnTest(Velocity ~ camera, data = filter(vel, group=="MCT"), method="bonferroni")
# dunnTest(Velocity ~ camera, data = filter(vel, group=="DM2"), method="bonferroni")
dunnTest(Velocity ~ group, data = filter(vel, camera=="VENTRICLE"), method="bonferroni")
dunnTest(Velocity ~ group, data = filter(vel, camera=="ATRIUM"), method="bonferroni")
# dunnTest(Velocity ~ group, data = filter(vel, camera=="S"), method="bonferroni")

# plots

p <- ggplot(vel, aes(x=group, y=Velocity, fill=camera)) + 
  geom_violin(scale = "width", size = 1) + 
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.9), dotsize=1.5)

ggplot(vel, aes(x=group, y=Velocity, fill=camera)) + 
  geom_boxplot(scale = "width", size = 1) + 
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.9), dotsize=1.5)

ggplot(vel, aes(x = group, y = Velocity, col = camera, fill=camera))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))+
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.9), dotsize=0.5)


mean_cl_boot(x = vel$Velocity)
# check normal distribution of residuals

shapiro.test(residuals(object = two_way_anova))

# residuals plot

plot(two_way_anova, 2)
plot(two_way_anova, 1)



