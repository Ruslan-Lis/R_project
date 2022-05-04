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
isom <- read.xlsx("isom.xlsx", sheetIndex = 2)

# isom_n <- unique(isom$name)
# phys_n <- unique(My_data_table$name)
sort(unique(isom[isom$g_c == 'CONT RA', 'name']))
sort(unique(My_data_table[My_data_table$g_c == 'CONTROL ATRIUM', 'name']))
# sort(isom_n)
# sort(phys_n) 
My_data_table[My_data_table$g_c != 'MCT RA', c('name','g_c')]

isom <- na.omit(isom)
colnames(isom) <- c("name", "deformation","Tension", "camera", "group")
isom$g_c <- paste(isom$group, isom$camera)
isom <- isom[isom$deformation > 0.75,]

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

isom$camera <- as.factor(isom$camera)
isom$group <- as.factor(isom$group)
isom$name <- as.factor(isom$name)
isom$deformation <- as.factor(isom$deformation)
isom$g_c <- as.factor(isom$g_c)

# view design - balansed or imbalansed

table(My_data_table$camera, My_data_table$group)

My_data_table <- My_data_table[My_data_table$afterload != 0.1,]


# descriptive_statistics <- My_data_table %>%
#   group_by(afterload, g_c) %>%
#   summarise_at(c('Velocity', 'Work', 'Les'),c(mean, sd, n_), na.rm=TRUE)


# x <- My_data_table[My_data_table$afterload == 0.2 & My_data_table$g_c == 'MCT ATRIUM', ]
# sd(x$Velocity, na.rm=TRUE)
# n_(x$Velocity)


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

ggplot(isom, aes(x = deformation, y = Tension, col = camera, shape = group, group = g_c))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))

# function return vector(q2, q1, q3)
mediana_quantile <- function(x, na.rm=TRUE){
  z_ <- c(summary(x)[3], summary(x)[2], summary(x)[5])
  names(z_) <- c('y','ymin','ymax')
  return(z_)
}

ggplot(isom, aes(x = deformation, y = Tension, col = camera, shape = group, group = g_c))+
  stat_summary(fun.data = mediana_quantile, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mediana_quantile, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mediana_quantile, geom = 'line', position = position_dodge(0.2))


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

mean_sdl(x$Velocity)
ggplot(vel, aes(x = afterload, y = Velocity, col = camera, shape = group, group = g_c))+
  stat_summary(fun.data = mean_sdl, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
mean_se(x$Velocity)

# descriptive statistics and normal distribution check


n_ <- function(x, na.rm=TRUE){
  length(na.omit(x))
}

descriptive_statistics_isom <- isom %>%
  group_by(deformation, g_c) %>%
  summarise_at('Tension',c(mean_cl_boot, n_), na.rm=TRUE)
descriptive_statistics_isom$c_i <- descriptive_statistics_isom$fn1$y - descriptive_statistics_isom$fn1$ymin


descriptive_statistics_vel <- vel %>%
  group_by(afterload, g_c) %>%
  summarise_at('Velocity',c(mean_cl_boot, n_), na.rm=TRUE)
descriptive_statistics_vel$c_i <- descriptive_statistics_vel$fn1$y - descriptive_statistics_vel$fn1$ymin

descriptive_statistics_les <- les %>%
  group_by(afterload, g_c) %>%
  summarise_at('Les',c(mean_cl_boot, n_), na.rm=TRUE)
descriptive_statistics_les$c_i <- descriptive_statistics_les$fn1$y - descriptive_statistics_les$fn1$ymin

descriptive_statistics_work <- work %>%
  group_by(afterload, g_c) %>%
  summarise_at('Work',c(mean_cl_boot, n_), na.rm=TRUE)
descriptive_statistics_work$c_i <- descriptive_statistics_work$fn1$y - descriptive_statistics_work$fn1$ymin



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

norm_distr_isom <- isom %>%
  group_by(camera, group, deformation) %>%
  summarise(n = n(), var = var(Tension, na.rm=T), sd = sd(Tension, na.rm=T), method = shapiro.test(Tension)$method, p.value = shapiro.test(Tension)$p.value)


# check homogeneity of variance 
# Levene Test

for(aftl in unique(vel$afterload)){
  print(leveneTest(Velocity ~ camera * group, data = vel[vel$afterload==aftl,]))
  print(leveneTest(Work ~ camera * group, data = work[work$afterload==aftl,]))
  print(leveneTest(Les ~ camera * group, data = les[les$afterload==aftl,]))
}
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

# for(aftl in unique(vel$afterload)){
  
  # scheirerRayHare(Velocyty ~ camera * group, data = My_data_table, type=1)
  
  print('afterload')
  print(aftl[1])
  print(scheirerRayHare(Velocity ~ camera * group, data = vel[vel$afterload==aftl,], type = "II"))

  # Appropriate post-hoc tests might be Dunn test for each significant factor or interaction.The notation identical to Tukey, above.
  print(dunnTest(Velocity ~ camera, data = filter(vel, group=="CONTROL", afterload==aftl), method="bonferroni"))
  print(dunnTest(Velocity ~ camera, data = filter(vel, group=="MCT", afterload==aftl), method="bonferroni"))
  # dunnTest(Velocity ~ camera, data = filter(vel, group=="DM2"), method="bonferroni")
  print(dunnTest(Velocity ~ group, data = filter(vel, camera=="VENTRICLE", afterload==aftl), method="bonferroni"))
  print(dunnTest(Velocity ~ group, data = filter(vel, camera=="ATRIUM", afterload==aftl), method="bonferroni"))
  # dunnTest(Velocity ~ group, data = filter(vel, camera=="S"), method="bonferroni")
}




df <- vel
for(aftl in unique(df$afterload)){

  print('afterload')
  print(aftl[1])
  print(scheirerRayHare(Velocity ~ camera * group, data = df[df$afterload==aftl,], type = "II"))

  # Appropriate post-hoc tests might be Dunn test for each significant factor or interaction.The notation identical to Tukey, above.
  print('CONTROL')
  print(dunnTest(Velocity ~ camera, data = filter(df, group=="CONTROL", afterload==aftl), method="bonferroni"))
  print('MCT')
  print(dunnTest(Velocity ~ camera, data = filter(df, group=="MCT", afterload==aftl), method="bonferroni"))
  print('VENTRICLE')
  print(dunnTest(Velocity ~ group, data = filter(df, camera=="VENTRICLE", afterload==aftl), method="bonferroni"))
  print('ATRIUM')
  print(dunnTest(Velocity ~ group, data = filter(df, camera=="ATRIUM", afterload==aftl), method="bonferroni"))
}

df <- work
for(aftl in unique(df$afterload)){
  
  print('afterload')
  print(aftl[1])
  print(scheirerRayHare(Work ~ camera * group, data = df[df$afterload==aftl,], type = "II"))
  
  # Appropriate post-hoc tests might be Dunn test for each significant factor or interaction.The notation identical to Tukey, above.
  print('CONTROL')
  print(dunnTest(Work ~ camera, data = filter(df, group=="CONTROL", afterload==aftl), method="bonferroni"))
  print('MCT')
  print(dunnTest(Work ~ camera, data = filter(df, group=="MCT", afterload==aftl), method="bonferroni"))
  print('VENTRICLE')
  print(dunnTest(Work ~ group, data = filter(df, camera=="VENTRICLE", afterload==aftl), method="bonferroni"))
  print('ATRIUM')
  print(dunnTest(Work ~ group, data = filter(df, camera=="ATRIUM", afterload==aftl), method="bonferroni"))
}

df <- les
for(aftl in unique(df$afterload)){
  
  print('afterload')
  print(aftl[1])
  print(scheirerRayHare(Les ~ camera * group, data = df[df$afterload==aftl,], type = "II"))
  
  # Appropriate post-hoc tests might be Dunn test for each significant factor or interaction.The notation identical to Tukey, above.
  print('CONTROL')
  print(dunnTest(Les ~ camera, data = filter(df, group=="CONTROL", afterload==aftl), method="bonferroni"))
  print('MCT')
  print(dunnTest(Les ~ camera, data = filter(df, group=="MCT", afterload==aftl), method="bonferroni"))
  print('VENTRICLE')
  print(dunnTest(Les ~ group, data = filter(df, camera=="VENTRICLE", afterload==aftl), method="bonferroni"))
  print('ATRIUM')
  print(dunnTest(Les ~ group, data = filter(df, camera=="ATRIUM", afterload==aftl), method="bonferroni"))
}

df <- isom
for(aftl in unique(df$deformation)){
  
  print('preload')
  print(aftl[1])
  print(scheirerRayHare(Tension ~ camera * group, data = df[df$deformation==aftl,], type = "II"))
  
  # Appropriate post-hoc tests might be Dunn test for each significant factor or interaction.The notation identical to Tukey, above.
  print('CONTROL')
  print(dunnTest(Tension ~ camera, data = filter(df, group=="CONT", deformation==aftl), method="bonferroni"))
  print('MCT')
  print(dunnTest(Tension ~ camera, data = filter(df, group=="MCT", deformation==aftl), method="bonferroni"))
  print('VENTRICLE')
  print(dunnTest(Tension ~ group, data = filter(df, camera=="RV", deformation==aftl), method="bonferroni"))
  print('ATRIUM')
  print(dunnTest(Tension ~ group, data = filter(df, camera=="RA", deformation==aftl), method="bonferroni"))
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






  