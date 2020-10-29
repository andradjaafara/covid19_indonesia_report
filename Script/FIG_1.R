# To install 'squire' package in R, please follow instructions in this link: https://mrc-ide.github.io/squire/
# To install 'odin' package in R, please follow instructiosn in this link: https://github.com/mrc-ide/odin
# All other packages are available in CRAN

## Load Required Packages
library(tidyverse); library(socialmixr); library(extraDistr); library(tictoc); 
library(zoo); library(readxl); library(lubridate); library(FSA); library(patchwork); library(gridExtra); library(ggpubr); library(corrr); library(ggcorrplot)
library(squire); library(parsedate); library(RColorBrewer); library(EpiEstim); library(incidence); library(scales); library(epitrix); library(matrixStats)
library(magrittr); library(cowplot); library(rgeos);library(rgdal); library(tidybayes); library(rgdal); library(ggplot2); library(dplyr); library(cowplot)
library(viridis); library(RhpcBLASctl); library(future)

rm(list=ls())

options(dplyr.summarise.inform = FALSE)
setwd("~/covid19_indonesia_report_cleaned") # your working directory

## FIGURE 1: Barplots and maps of cases and deaths
province_name <- c("ACEH","NORTH SUMATERA","WEST SUMATERA","RIAU","JAMBI","SOUTH SUMATERA","BENGKULU","LAMPUNG","BANGKA BELITUNG ISLANDS","RIAU ISLANDS",
                   "JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","BALI","WEST NUSA TENGGARA","EAST NUSA TENGGARA",
                   "WEST KALIMANTAN","CENTRAL KALIMANTAN","SOUTH KALIMANTAN","EAST KALIMANTAN","NORTH KALIMANTAN","NORTH SULAWESI","CENTRAL SULAWESI",
                   "SOUTH SULAWESI","SOUTHEAST SULAWESI","GORONTALO","WEST SULAWESI","MALUKU","NORTH MALUKU","WEST PAPUA","PAPUA")

java_provinces <- c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")

nat_pos <- read.csv("Data/Raw/idn_positives.csv",header = TRUE,stringsAsFactors = FALSE)
nat_deaths <- read.csv("Data/Raw/idn_deaths.csv",header = TRUE,stringsAsFactors = FALSE)
nat_region <- read.csv("Data/Raw/idn_region.csv",header = TRUE,stringsAsFactors = FALSE)

nat_pos <- nat_pos[,-ncol(nat_pos)]
nat_deaths <- nat_deaths[,-ncol(nat_deaths)]

colnames(nat_pos)[-1] <- province_name
colnames(nat_deaths)[-1] <- province_name

nat_pos <- nat_pos %>% pivot_longer(-date,names_to="prov",values_to="cases") %>% mutate(cases=replace(cases, cases<0, 0),date=ymd(date)) %>% left_join(nat_region)
nat_deaths <- nat_deaths %>% pivot_longer(-date,names_to="prov",values_to="deaths") %>% mutate(date=ymd(date)) %>% left_join(nat_region)

reg_pos <- nat_pos %>% group_by(region,date) %>% summarise(cases=sum(cases,na.rm = TRUE)) %>% ungroup()
reg_deaths <- nat_deaths %>% group_by(region,date) %>% summarise(deaths=sum(deaths,na.rm = TRUE)) %>% ungroup()

reg_pos$region <- factor(reg_pos$region,levels=c("Outside Java","Other Java Provinces","Jakarta"))
reg_deaths$region <- factor(reg_deaths$region,levels=c("Outside Java","Other Java Provinces","Jakarta"))

p_nat_cases <- reg_pos %>% ggplot() +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) +
  geom_col(aes(x=date, y = cases,fill=region)) +
  theme_bw() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  labs(y="Daily reported cases",x="Date",fill="") +
  scale_fill_manual(values=c("#7f2704","#225ea8","#ec7014")) + theme(legend.position = "top") + ylim(0,3500) +
  geom_segment(aes(x = ymd("2020-03-15"), y = 2500, xend = ymd("2020-03-15"), yend = 100),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate("text", x = ymd("2020-03-15"), y = 2500, label = "Public\nencouraged\nto social distance", size=3, vjust=-0.25) +
  annotate(geom = "text", x = ymd("2020-04-14"), y = 2000, 
           label = substitute(paste(italic('Pembatasan sosial berskala besar (PSBB)'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-04-19"), y = 2000, 
           label = "Large-scale social restrictions", angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000, 
           label = "Ramadan domestic travel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 3500, xend = ymd("2020-09-02"), yend = 3500),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3400, 
           label = substitute(paste("Transitional ",italic('PSBB'), " and")), size=3) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250, 
           label = substitute(paste(italic('Adaptasi kebiasaan baru (AKB)'))), size=3) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3100, 
           label = "The new normal period", size=3)

p_nat_deaths <- reg_deaths %>% ggplot() +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) +
  geom_col(aes(x=date, y = deaths,fill=region)) + 
  theme_bw() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  labs(y="Daily reported deaths",x="Date",fill="") +
  scale_fill_manual(values=c("#7f2704","#225ea8","#ec7014")) + theme(legend.position = "top") + ylim(0,175) +
  geom_segment(aes(x = ymd("2020-03-15"), y = 125, xend = ymd("2020-03-15"), yend = 5),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate("text", x = ymd("2020-03-15"), y = 125, label = "Public\nencouraged\nto social distance", size=3, vjust=-0.25) +
  annotate(geom = "text", x = ymd("2020-04-14"), y = 100, 
           label = substitute(paste(italic('Pembatasan sosial berskala besar (PSBB)'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-04-19"), y = 100, 
           label = "Large-scale social restrictions", angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 100, 
           label = "Ramadan domestic travel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 175, xend = ymd("2020-09-02"), yend = 175),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 170, 
           label = substitute(paste("Transitional ",italic('PSBB'), " and")), size=3) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 162.5, 
           label = substitute(paste(italic('Adaptasi kebiasaan baru (AKB)'))), size=3) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 155, 
           label = "The new normal period", size=3)

java_pos <- nat_pos %>% filter(prov %in% java_provinces) %>% group_by(prov) %>% summarise(cases=sum(cases))
java_deaths <- nat_deaths %>% filter(prov %in% java_provinces) %>% group_by(prov) %>% summarise(deaths=sum(deaths))
java_epi <- java_pos %>% left_join(java_deaths) %>% mutate(id=c("BANTEN","JAWA TENGAH","JAWA TIMUR","DKI JAKARTA","JAWA BARAT","DI YOGYAKARTA"))

java_shp <- readOGR(dsn="Data/map", layer="ADMIN1_JAVA", stringsAsFactors = FALSE)
java_shp_shp_df <- fortify(java_shp, region = "ADMIN1")
java_shp_shp_df <- java_shp_shp_df %>% left_join(java_epi)

theme_bare <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  legend.text=element_text(size=9),
  legend.title=element_text(size=10),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5)
)

map_java <- ggplot(data = java_shp_shp_df, aes(x = long, y = lat, group = group))

centroid_java_orig <- java_shp_shp_df %>% group_by(prov) %>% summarise(lat=mean(lat),long=mean(long))

centroid_java <- java_shp_shp_df %>% group_by(prov) %>% summarise(lat=mean(lat),long=mean(long))
centroid_java$lat <- c(-6.2, -7.25, -7.9, -5.7, -6.88, -8.4)
centroid_java$long <- c(105.25, 110, 112.5, 107.5, 107.6, 109.7)

line1 <- data.frame(lat=c(-6.2,-5.8),long=c(106.8,107.5))
line2 <- data.frame(lat=c(-6.455776,-6.3),long=c(106.15,105.25))
line3 <- data.frame(lat=c(-7.85,-8.3),long=c(110.4,109.7))

p_cases_java <- map_java + 
  geom_polygon(aes(fill=cases/1000),color = 'black', size = 0.25) +
  guides(fill=guide_colorbar(title="Total cases reported (thousands)")) + 
  scale_fill_gradient(high = "#bd0026", low = "#ffeda0", guide = "colorbar") +
  coord_fixed(1.3) +
  theme(legend.position="bottom") + theme_bare +
  with(centroid_java, annotate(geom="text", x = long, y=lat, label = prov, size = 2.75, fontface = 2)) +
  with(line1, annotate(geom="line", x = long, y=lat, size = 0.5)) +
  with(line2, annotate(geom="line", x = long, y=lat, size = 0.5)) +
  with(line3, annotate(geom="line", x = long, y=lat, size = 0.5))

p_deaths_java <- map_java + 
  geom_polygon(aes(fill=deaths/1000),color = 'black', size = 0.25) +
  guides(fill=guide_colorbar(title="Total deaths reported (thousands)")) + 
  scale_fill_gradient(high = "#bd0026", low = "#ffeda0", guide = "colorbar") +
  coord_fixed(1.3) +
  theme(legend.position="bottom") + theme_bare +
  with(centroid_java, annotate(geom="text", x = long, y=lat, label = prov, size = 2.75, fontface = 2)) +
  with(line1, annotate(geom="line", x = long, y=lat, size = 0.5)) +
  with(line2, annotate(geom="line", x = long, y=lat, size = 0.5)) +
  with(line3, annotate(geom="line", x = long, y=lat, size = 0.5))

p_FIG_1 <- plot_grid(p_nat_cases, p_nat_deaths, p_cases_java, p_deaths_java, 
                     rel_heights = c(1.5, 1), 
                     labels = c("A", "B", "C", "D"), nrow = 2)

pdf(file = "Output/FIG-1.pdf", width = 10, height = 7)
  p_FIG_1
dev.off()

