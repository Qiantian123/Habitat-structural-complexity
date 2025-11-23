# set working place and loading R packages---------
setwd("D:/R work place/artificial plant")
library(ggplot2)
library(dplyr)
library(Rmisc)
library(tidyr)
library(ggpubr)
setwd("D:/R work place/artificial plant")
################# set my theme
mytheme <- theme_bw() + 
  theme(panel.background = element_rect(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 10.5,color = "black"),
        axis.text.y = element_text(size = 10.5,color = "black"),
        axis.text.x = element_text(size = 10,color = "black"),
        strip.text = element_text(size = 10.5),
        #strip.background = element_blank(),
        legend.key.size = unit(0.15, "inches"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
# TN ----
nutrient <- read.csv("./data/nutrient.csv",header = T)
data2 <- gather(nutrient,key = "nutrient",
                value = "conc",9) %>%
  summarySE(measurevar = "conc",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()
data2$nutrient <- factor(data2$nutrient,levels = c("TN"),
                         labels = c("TN"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
days <- c(0,3,6,9,12,15,18,21)
tiff("./figures/TN.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")
TN<-ggplot(data2,aes(Days,conc,color = Mandarin)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 0.3) + 
  geom_errorbar(aes(ymin = conc - se,
                    ymax = conc + se),
                width = 1,size = 0.2) + 
  facet_grid(~Plant,scales = "free_y") +
  labs(x = "Days", 
       y = expression(TN~(mg~L^{-1}))) + 
  ylim(0.48,0.94) +
  
  mytheme  +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(breaks = days)
TN
dev.off()

# DTN ----
nutrient <- read.csv("./data/nutrient.csv",header = T)
data2 <- gather(nutrient,key = "nutrient",
                value = "conc",10) %>%
  summarySE(measurevar = "conc",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()
data2$nutrient <- factor(data2$nutrient,levels = c("DTN"),
                         labels = c("DTN"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
days <- c(0,3,6,9,12,15,18,21)
tiff("./figures/DTN.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")
DTN<-ggplot(data2,aes(Days,conc,color = Mandarin)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 0.3) + 
  geom_errorbar(aes(ymin = conc - se,
                    ymax = conc + se),
                width = 1,size = 0.2) + 
  facet_grid(~Plant,scales = "free_y") +
  labs(x = "Days", 
       y = expression(DTN~(mg~L^{-1}))) + 
  ylim(0.4,0.7) +
  
  mytheme  +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(breaks = days)
DTN
dev.off()

# TP ----
nutrient <- read.csv("./data/nutrient.csv",header = T) %>%
  mutate(TP = TP * 1000)
data2 <- gather(nutrient,key = "nutrient",
                value = "conc",11) %>%
  summarySE(measurevar = "conc",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()
data2$nutrient <- factor(data2$nutrient,levels = c("TP"),
                         labels = c("TP"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
days <- c(0,3,6,9,12,15,18,21)
tiff("./figures/TP.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")
TP<-ggplot(data2,aes(Days,conc,color = Mandarin)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 0.3) + 
  geom_errorbar(aes(ymin = conc - se,
                    ymax = conc + se),
                width = 1,size = 0.2) + 
  facet_grid(~Plant,scales = "free_y") +
  labs(x = "Days", 
       y = expression(TP~(μg~L^{-1}))) + 
  ylim(6.8,17.1) +
  
  mytheme  +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(breaks = days)
TP
dev.off()

# DTP ----
nutrient <- read.csv("./data/nutrient.csv",header = T) %>%
  mutate(DTP = DTP * 1000)
data2 <- gather(nutrient,key = "nutrient",
                value = "conc",12) %>%
  summarySE(measurevar = "conc",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()
data2$nutrient <- factor(data2$nutrient,levels = c("DTP"),
                         labels = c("DTP"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))
days <- c(0,3,6,9,12,15,18,21)
tiff("./figures/DTP.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")
DTP<-ggplot(data2,aes(Days,conc,color = Mandarin)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 0.3) + 
  geom_errorbar(aes(ymin = conc - se,
                    ymax = conc + se),
                width = 1,size = 0.2) + 
  facet_grid(~Plant,scales = "free_y") +
  labs(x = "Days", 
       y = expression(DTP~(μg~L^{-1}))) + 
  ylim(3.2,13) +
  
  mytheme  +
  theme(legend.position = "none",
        strip.text = element_blank()) +
  scale_x_continuous(breaks = days)
DTP
dev.off()

# Chl a ----
nutrient <- read.csv("./data/nutrient.csv",header = T) 
data2 <- gather(nutrient,key = "nutrient",
                value = "conc",13) %>%
  summarySE(measurevar = "conc",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()

data2$nutrient <- factor(data2$nutrient,levels = c("Chl"),
                         labels = c("Chl"))

data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

days <- c(0,3,6,9,12,15,18,21)

tiff("./figures/Chl.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")
Chl<-ggplot(data2,aes(Days,conc,color = Mandarin)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 0.3) + 
  geom_errorbar(aes(ymin = conc - se,
                    ymax = conc + se),
                width = 1,size = 0.2) + 
  facet_grid(~Plant,scales = "free_y") +
  labs(x = "Days", 
       y = expression(Chl~italic(a)~(μg~L^{-1}))) + 
  mytheme  +
  theme(legend.position = c(0.75,0.85),
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(breaks = days)
Chl
tiff("./figures/Chl.tiff",compression = "lzw",res = 900,
     width = 14,height = 14,units = "cm")
Chl
dev.off()

nutrient_conc <- ggarrange(Chl, TN, DTN, TP, DTP, label.x = 0.01,label.y = 1,ncol=1,
                           heights = c(1,0.9,0.9,0.9,1.06),
                           align='v',font.label = list(size = 14, color = "black", face = "plain", family = "serif"),   
                           labels = c("a","b","c","d","e") )
nutrient_conc
tiff("./figures/nutrient_conc.tiff",compression = "lzw",res = 900,
     width = 14,height = 26,units = "cm")
nutrient_conc
dev.off()
