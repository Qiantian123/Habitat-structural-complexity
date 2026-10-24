# 1. set working place and loading R packages---------
setwd("D:/R work place/artificial plant")
library(ggplot2)
library(dplyr)
library(Rmisc)
library(tidyr)
library(ggpubr)

# 2. set figure format--------

mytheme <- theme_bw() + 
  theme(panel.background = element_rect(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 10.5,color = "black"),
        axis.text = element_text(size = 10,color = "black"),
        strip.text = element_text(size = 10.5),
        aspect.ratio = 3/2,
        legend.key.size = unit(0.15, "inches"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

# 3. loading Day 21 zooplankton body mass data ----

data <- read.csv("./data/nutrient.csv") %>% 
  select(Days,Plant,Mandarin,Zoopbodymass) %>%
  mutate(Zoopbodymass = Zoopbodymass) %>%
  filter(Days == 21)

data2 <- gather(data, key = "Zoopbodymass",value = "biom",4)
data2$Zoopbodymass <- factor(data2$Zoopbodymass, 
                         levels = c("Zoopbodymass"),
                         labels = c("Zoopbodymass"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

data_Zoopbodymass <- summarySE(data2,measurevar = 'biom',
                           groupvars = c("Plant","Mandarin","Days","Zoopbodymass")) %>% na.omit()

# 4. plot Day 21 zooplankton body mass 

Zoopbodymass <- ggplot(data_Zoopbodymass, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  mytheme+theme(legend.key.size = unit(0.15, "inches"),
                legend.position = "none",
                strip.text = element_blank()) +
  #  annotate("text",x = 1,y = 10,label = "") +
  
  labs(x = "",y = expression(Zooplankton~body~mass~(μg~ind.^{-1})))
tiff("./figures/Zoopbodymass.tiff",compression = "lzw",res = 900,
     width = 8,height = 8,units = "cm")
Zoopbodymass
dev.off()

# 5. loading Day 21 zooplankton:phytoplankton biomass ratio data ----

data <- read.csv("./data/nutrient.csv") %>% 
  select(Days,Plant,Mandarin,zppp) %>%
  mutate(zppp = zppp /1000/ 0.29) %>%
  filter(Days == 21)

data2 <- gather(data, key = "zppp",value = "biom",4)
data2$zppp <- factor(data2$zppp, 
                     levels = c("zppp"),
                     labels = c("zppp"))
data2$Plant <- factor(data2$Plant,levels = c("Low","High"),
                      labels = c("LPD", "HPD"))
data2$Mandarin <- factor(data2$Mandarin,levels = c("Absent","Present"),
                         labels = c("Mandarin absent", "Mandarin present"))

data_zppp <- summarySE(data2,measurevar = 'biom',
                       groupvars = c("Plant","Mandarin","Days","zppp")) %>% na.omit()

# 6. plot zooplankton:phytoplankton biomass ratio

zppp <- ggplot(data_zppp, aes(factor(Plant),biom,fill = Mandarin)) + 
  geom_bar(stat = "identity",position = "dodge",
           width = 0.5) +
  geom_errorbar(aes(ymin = biom - se,
                    ymax = biom + se),
                position = position_dodge(width = 0.5),
                size = 0.2,width = 0.2) +
  theme_bw() + 
  mytheme+theme(legend.key.size = unit(0.15, "inches"),
                legend.position = "top",
                strip.text = element_blank()) +
  labs(x = "",y = expression(Zooplankton:phytoplankton~ratio))
tiff("./figures/zppp.tiff",compression = "lzw",res = 900,
     width = 8,height = 8,units = "cm")
zppp
dev.off()

# 7. plot Day 21 zooplankton body mass + zooplankton:phytoplankton biomass ratio
Zoopbodymasszppp <- ggarrange(Zoopbodymass, zppp, label.x = 0.03,label.y = 1,nrow=1,align = "v",
                          font.label = list(size = 14, color = "black", face = "plain", family = "serif"),   
                          common.legend = T,labels = c("a","b") )
Zoopbodymasszppp
tiff("./figures/Zoopsizezppp.tiff",compression = "lzw",res = 900,
     width = 14,height = 9,units = "cm")

Zoopbodymasszppp
dev.off()

# 8. set figure format--------

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

# 9. loading Chl a:TN ratio data ----

nutrient <- read.csv("./data/nutrient.csv",header = T)  %>%
  mutate(ChlTN = ChlTN)  
names(nutrient)
head(nutrient)

data2 <- gather(nutrient,key = "nutrient",
                value = "conc",11) %>%
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

# 10. plot  Chl a:TN ratio  

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
       y = expression(Chl~italic(a):TN~ratio)) + 
  mytheme  +
  theme(legend.position = c(0.75,0.85),
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_continuous(breaks = days)
ChlTN
dev.off()

# 9. loading Chl a:TP ratio data ----

nutrient <- read.csv("./data/nutrient.csv",header = T)  %>%
  mutate(ChlTP = ChlTP / 1000)  
names(nutrient)
head(nutrient)

data2 <- gather(nutrient,key = "nutrient",
                value = "conc",12) %>%
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

# 12. plot  Chl a:TP ratio  

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

# 13. plot Chl a:TN + Chl a:TP ratio 

ChlTNTP <- ggarrange(ChlTN, ChlTP, label.x = 0.005,label.y = 1.00,ncol=1,align = "v",heights = c(1,1.02),
                     font.label = list(size = 14, color = "black", face = "plain", family = "serif"),   
                     labels = c("c","d") )
ChlTNTP
tiff("./figures/ChlTNTP.tiff",compression = "lzw",res = 900,
     width = 14,height = 12,units = "cm")

ChlTNTP

dev.off()

# 14. plot plot Day 21 zooplankton body mass + zooplankton:phytoplankton biomass ratio + Chl a:TN + Chl a:TP ratio (figure 4)

ratio <- ggarrange(Zoopsizezppp, ChlTNTP, label.x = 0.01,label.y = 1.00,ncol=1,align = "h",heights = c(0.7,1),
                           font.label = list(size = 16, color = "black", face = "plain", family = "serif"),   
                           labels = c("","") )
ratio

tiff("./figures/figure4.tiff",compression = "lzw",res = 900,
     width = 14,height = 24,units = "cm")

ratio

dev.off()
