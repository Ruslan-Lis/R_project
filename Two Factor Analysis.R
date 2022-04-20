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
# install.packages("reshape2")


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
library(reshape2)
library(broom)


# work dir

getwd()
setwd("e:/RNF_2021-2022/Remodel F-V physiol load-1")
# setwd("/home/atstkiy/R/Remodel F-V physiol load-1")

# load data

My_data_table <- read.xlsx("phys_reg.xlsx", sheetIndex = 1)

colnames(My_data_table) <- c("name", "afterload", 'Velocity', 'Work', 'Les', 'temperature', 'camera', 'group')
My_data_table$g_c <- paste(My_data_table$group, My_data_table$camera)   
# view structure data

str(My_data_table)

# factors

My_data_table$camera <- as.factor(My_data_table$camera)
My_data_table$group <- as.factor(My_data_table$group)
My_data_table$name <- as.factor(My_data_table$name)
My_data_table$afterload <- as.factor(My_data_table$afterload)
My_data_table$g_c <- as.factor(My_data_table$g_c)


# view design - balansed or imbalansed

table(My_data_table$camera, My_data_table$group)

# My_data_table %>% 
#   group_by(afterload, na.rm=TRUE) %>%
#   summarise_at(c('Velocity', 'Work', 'Les'), mean, na.rm=TRUE)

# group_df <- group_by(.data = My_data_table, afterload, na.rm=TRUE)

vel <- My_data_table[c("afterload", 'Velocity','camera', 'group',"g_c")]
vel <- na.omit(vel) 
vel <- vel[vel$afterload != 0.1,]

work <- My_data_table[c( "afterload", 'Work','camera', 'group',"g_c")]
work <- na.omit(work) 
work <- work[work$afterload != 0.1,]

les <- My_data_table[c( "afterload", 'Les','camera', 'group',"g_c")]
les <- na.omit(les) 
les <- les[les$afterload != 0.1,]


# visualisation 

ggplot(vel, aes(x = afterload, y = Velocity, col = camera, shape = group, group = g_c))+
  # geom_point()+
  geom_smooth()



ggplot(vel, aes(x = afterload, y = Velocity, col = camera, shape = group, group = g_c))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))

ggplot(work, aes(x = afterload, y = Work, col = camera, shape = group, group = g_c))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))

ggplot(les, aes(x = afterload, y = Les, col = camera, shape = group, group = g_c))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))


# descriptive statistics and normal distribution check

vel <- vel[vel$afterload != 1,]
work <- work[work$afterload != 1,]
les <- les[les$afterload != 1,]


norm_distr_vel <- vel %>%
  group_by(camera, group, afterload) %>%
  summarise(n = n(), var = var(Velocity, na.rm=T), sd = sd(Velocity, na.rm=T), method = shapiro.test(Velocity)$method, p.value = shapiro.test(Velocity)$p.value)

norm_distr_work <- work %>%
  group_by(camera, group, afterload) %>%
  summarise(n = n(), var = var(Work, na.rm=T), sd = sd(Work, na.rm=T), method = shapiro.test(Work)$method, p.value = shapiro.test(Work)$p.value)

norm_distr_les <- les %>%
  group_by(camera, group, afterload) %>%
  summarise(n = n(), var = var(Les, na.rm=T), sd = sd(Les, na.rm=T), method = shapiro.test(Les)$method, p.value = shapiro.test(Les)$p.value)

# check homogeneity of variance 
# Levene Test

leveneTest(Velocity ~ camera * group * afterload, data = vel)
leveneTest(Work ~ camera * group * afterload, data = work)
leveneTest(Les ~ camera * group * afterload, data = les)

# Bartlett test

bartlett.test(Velocity ~ camera, data = vel)
bartlett.test(Velocity ~ group, data = vel)
bartlett.test(Velocity ~ afterload, data = vel)

bartlett.test(Work ~ camera, data = work)
bartlett.test(Work ~ group, data = work)
bartlett.test(Work ~ afterload, data = work)

bartlett.test(Les ~ camera, data = les)
bartlett.test(Les ~ group, data = les)
bartlett.test(Les ~ afterload, data = les)

# F test (min group var, max group var)

testtable <- summarise(vel %>% group_by(camera, group), var = var(Velocity, na.rm=T))
testtable      
filter(testtable, var==min(testtable$var))
filter(testtable, var==max(testtable$var))
var.test(filter(vel, camera=="ATRIUM", group=="CONTROL", afterload==0.1)$Velocity,filter(vel, camera=="VENTRICLE", group=="MCT", afterload==0.9)$Velocity)
var.test(filter(vel, camera=="ATRIUM", group=="CONTROL", afterload==0.1)$Velocity,filter(vel, camera=="VENTRICLE", group=="MCT", afterload==0.9)$Velocity)$p.value



# Two-way Anova
# The recommended method are the Type-III sums of squares for unbalanced designs

vel_grouped <- vel %>% group_by(afterload)
two_way_anova = aov(Velocity ~ camera * group + Error(subset=afterload), data = vel)
"two_way_anova = aov(Data ~ Camera + State + Camera:State, data = My_data_table)"



datm <- melt(vel, id="afterload", value.name="Percentage", variable.name = "Class")
res = datm %>% group_by(Species, Class) %>% 
  do(Model = aov(Percentage ~ TREATMENT, data=.))


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

for(aftl in unique(vel$afterload)){
  
  # scheirerRayHare(Velocyty ~ camera * group, data = My_data_table, type=1)
  
  print('afterload')
  print(aftl[1])
  print(scheirerRayHare(Velocity ~ camera * group, data = vel[vel$afterload==aftl,], type = "II"))

  # Appropriate post-hoc tests might be Dunn test for each significant factor or interaction. Запись идентична выше для Tukey
  print(dunnTest(Velocity ~ camera, data = filter(vel, group=="CONTROL", afterload==aftl), method="bonferroni"))
  print(dunnTest(Velocity ~ camera, data = filter(vel, group=="MCT", afterload==aftl), method="bonferroni"))
  # dunnTest(Velocity ~ camera, data = filter(vel, group=="DM2"), method="bonferroni")
  print(dunnTest(Velocity ~ group, data = filter(vel, camera=="VENTRICLE", afterload==aftl), method="bonferroni"))
  print(dunnTest(Velocity ~ group, data = filter(vel, camera=="ATRIUM", afterload==aftl), method="bonferroni"))
  # dunnTest(Velocity ~ group, data = filter(vel, camera=="S"), method="bonferroni")
}


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

ggplot(work, aes(x = afterload, y = work, col = camera, fill=camera))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))

mean_cl_boot(x = vel$Velocity)
# check normal distribution of residuals

shapiro.test(residuals(object = two_way_anova))

# residuals plot

plot(two_way_anova, 2)
plot(two_way_anova, 1)






  