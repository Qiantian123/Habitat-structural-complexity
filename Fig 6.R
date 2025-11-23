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
        axis.text = element_text(size = 10,color = "black"),
        strip.text = element_text(size = 10.5),
        #aspect.ratio = 2/3,
        legend.key.size = unit(0.15, "inches"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

# ChlTN ----
nutrient <- read.csv("./data/nutrient.csv",header = T)  %>%
  mutate(ChlTN = ChlTN)  
names(nutrient)
head(nutrient)

data2 <- gather(nutrient,key = "nutrient",
                value = "conc",16) %>%
  summarySE(measurevar = "conc",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()

data2$nutrient <- factor(data2$nutrient,levels = c("ChlTN"),
                         labels = c("ChlTN"))

data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

days <- c(0,3,6,9,12,15,18,21)

tiff("./figures/ChlTN.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")
ChlTN<-ggplot(data2,aes(Days,conc,color = Mandarin)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 0.3) + 
  geom_errorbar(aes(ymin = conc - se,
                    ymax = conc + se),
                width = 1,size = 0.2) + 
  facet_grid(~Plant,scales = "free_y") +
  labs(x = "Days", 
       y = expression(Chl~italic(a):TN~'×'~1000~ratio)) + 
  mytheme  +
  theme(legend.position = c(0.75,0.85),
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(breaks = days)
ChlTN
dev.off()

# ChlTP ----
nutrient <- read.csv("./data/nutrient.csv",header = T)  %>%
  mutate(ChlTP = ChlTP / 1000)  
names(nutrient)
head(nutrient)

data2 <- gather(nutrient,key = "nutrient",
                value = "conc",17) %>%
  summarySE(measurevar = "conc",
            groupvars = c("Days",
                          "Plant",
                          "Mandarin",
                          "nutrient"),
            na.rm = T) %>% na.omit()

data2$nutrient <- factor(data2$nutrient,levels = c("ChlTP"),
                         labels = c("ChlTP"))

data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

days <- c(0,3,6,9,12,15,18,21)

tiff("./figures/ChlTP.tiff",res = 900, compression = "lzw",
     width = 14,height = 14,units = "cm")
ChlTP<-ggplot(data2,aes(Days,conc,color = Mandarin)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 0.3) + 
  geom_errorbar(aes(ymin = conc - se,
                    ymax = conc + se),
                width = 1,size = 0.2) + 
  facet_grid(~Plant,scales = "free_y") +
  labs(x = "Days", 
       y = expression(Chl~italic(a):TP~ratio)) + 
  mytheme  +
  theme(legend.position = "none",
        strip.text = element_blank()) +
  scale_x_continuous(breaks = days)
ChlTP
dev.off()

ChlTNTP <- ggarrange(ChlTN, ChlTP, label.x = 0.005,label.y = 1.00,ncol=1,align = "v",heights = c(1,1.02),
                     font.label = list(size = 14, color = "black", face = "plain", family = "serif"),   
                     labels = c("a","b") )
ChlTNTP
tiff("./figures/ChlTNTP.tiff",compression = "lzw",res = 900,
     width = 14,height = 12,units = "cm")

ChlTNTP

dev.off()
