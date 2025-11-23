# 1 set working place and loading R packages---------
setwd("D:/R work place/artificial plant")
library(tidyverse)
library(Rmisc)
library(ggpubr)
library(data.table)
library(dplyr)

mytheme <- theme_bw() + 
  theme(panel.background = element_rect(),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10,color = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 9,color = "black"),
        strip.text = element_text(size = 12),
        aspect.ratio = 3/2,
        legend.key.size = unit(0.15, "inches"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

######## Day 21 zoop #########
data <- read.csv("./data/nutrient.csv") %>% 
  dplyr::select(Days,Plant,Mandarin,Daphnia) %>%
  mutate(Daphnia=Daphnia/1000) %>%
  filter(Days == 21)
data2 <- gather(data, key = "zoop",value = "biom",4)
data2$zoop <- factor(data2$zoop, 
                     levels = c("Daphnia"),
                     labels = c("Daphnia"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
data_zoop <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","zoop")) %>% na.omit()

p1 <- ggplot(data_zoop, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  expand_limits(y=0)+
  scale_y_continuous(expand=c(0.1,0))+
  mytheme+theme(legend.position = "top",
                legend.direction = "horizontal")+
  labs(x = "",y = expression(italic(Daphnia)~(mg~L^{-1})))

tiff("./figures/Daphnia_Day21.tiff",compression = "lzw",res = 900,
     width = 14,height = 8,units = "cm")

p1
dev.off()

data <- read.csv("./data/nutrient.csv") %>% 
  dplyr::select(Days,Plant,Mandarin,Cladoceran) %>%
  mutate(Cladoceran=Cladoceran/1000) %>%
  filter(Days == 21)
#### cover * mandarin p = 0.89  ####
data2 <- gather(data, key = "zoop",value = "biom",4)
data2$zoop <- factor(data2$zoop, 
                     levels = c("Cladoceran"),
                     labels = c("Cladoceran"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
data_zoop <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","zoop")) %>% na.omit()
p2 <- ggplot(data_zoop, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  expand_limits(y=0)+
  scale_y_continuous(expand=c(0.1,0))+
  mytheme+theme(
    legend.position = "top",
    legend.direction = "horizontal")+
  annotate("text",x = 1,y = 0.055,label = "") +
  
  labs(x = "",y = expression(Cladocerans~(mg~L^{-1})))
tiff("./figures/Cladoceran_Day21.tiff",compression = "lzw",res = 900,
     width = 14,height = 8,units = "cm")

p2
dev.off()

data <- read.csv("./data/nutrient.csv") %>% 
  dplyr::select(Days,Plant,Mandarin,Copepod) %>%
  mutate(Copepod=Copepod/1000) %>%
  filter(Days == 21)
#### cover * mandarin p = 0.79  ####
data2 <- gather(data, key = "zoop",value = "biom",4)
data2$zoop <- factor(data2$zoop, 
                     levels = c("Copepod"),
                     labels = c("Copepod"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
data_zoop <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","zoop")) %>% na.omit()
p3 <- ggplot(data_zoop, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  expand_limits(y=0)+
  scale_y_continuous(expand=c(0.1,0))+
  mytheme+theme(
    legend.position = "top",
    legend.direction = "horizontal")+
  annotate("text",x = 1,y = 0.28,label = "") +
  
  labs(x = "",y = expression(Copepods~(mg~L^{-1})))
tiff("./figures/Copepod_Day21.tiff",compression = "lzw",res = 900,
     width = 14,height = 8,units = "cm")

p3
dev.off()

data <- read.csv("./data/nutrient.csv") %>% 
  dplyr::select(Days,Plant,Mandarin,Rotifer) %>%
  mutate(Rotifer=Rotifer/1000) %>%
  filter(Days == 21)
#### cover * mandarin p = 0.83  ####
data2 <- gather(data, key = "zoop",value = "biom",4)
data2$zoop <- factor(data2$zoop, 
                     levels = c("Rotifer"),
                     labels = c("Rotifer"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
data_zoop <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","zoop")) %>% na.omit()
p4 <- ggplot(data_zoop, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  expand_limits(y=0)+
  scale_y_continuous(expand=c(0.1,0))+
  mytheme+theme(
    legend.position = "top",
    legend.direction = "horizontal")+
  annotate("text",x = 1,y = 0.13,label = "") +
  
  labs(x = "",y = expression(Rotifers~(mg~L^{-1})))
tiff("./figures/Rotifer_Day21.tiff",compression = "lzw",res = 900,
     width = 14,height = 8,units = "cm")

p4
dev.off()

data <- read.csv("./data/nutrient.csv") %>% 
  dplyr::select(Days,Plant,Mandarin,Total) %>%
  mutate(Total=Total/1000) %>%
  filter(Days == 21)
data2 <- gather(data, key = "zoop",value = "biom",4)
data2$zoop <- factor(data2$zoop, 
                     levels = c("Total"),
                     labels = c("Total"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
data_zoop <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","zoop")) %>% na.omit()
p5 <- ggplot(data_zoop, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  expand_limits(y=0)+
  scale_y_continuous(expand=c(0.1,0))+
  mytheme+theme(
    legend.position = "top",
    legend.direction = "horizontal")+
  annotate("text",x = 1,y = 0.38,label = "") +
  
  labs(x = "",y = expression(Zooplankton~(mg~L^{-1})))
tiff("./figures/Total_Day21.tiff",compression = "lzw",res = 900,
     width = 14,height = 8,units = "cm")

p5
dev.off()

data <- read.csv("./data/nutrient.csv") %>% 
  dplyr::select(Days,Plant,Mandarin,Phytoplankton) %>%
  mutate(Phytoplankton = Phytoplankton * 0.29) %>%
  filter(Days == 21)
data2 <- gather(data, key = "phyt",value = "biom",4)
data2$phyt <- factor(data2$phyt, 
                     levels = c("Phytoplankton"),
                     labels = c("Phytoplankton"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

data_phyt <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","phyt")) %>% na.omit()

p6 <- ggplot(data_phyt, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  mytheme+theme(
    legend.position = c(0.7,0.9),
    legend.direction = "vertical",
    strip.text = element_blank()) +
  annotate("text",x = 1,y = 1.6,label = "") +
  
  labs(x = "",y = expression(Phytoplankton~(mg~L^{-1})~~''))
tiff("./figures/phyt_Day21.tiff",compression = "lzw",res = 900,
     width = 12,height = 8,units = "cm")
p6
dev.off()

tiff("./figures/zooplankton_Day21.tiff",compression = "lzw",res = 900,
     width = 14,height = 12,units = "cm")
ggarrange(p1,p2,p3,p4,p5,p6,label.x = 0.04,label.y = 1,
          common.legend = T,  font.label = list(size = 14, color = "black", face = "plain", family = "serif"),   
          labels = c("a","b","c","d","e","f"),
          ncol = 3,nrow = 2, align = "hv")
dev.off()
