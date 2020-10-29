# Summary of variables

hospital_beds_region <- read.csv("Data/raw/hospital_admin2_java.csv",header = TRUE,stringsAsFactors = FALSE)

beds_per_million_region <- data.frame(hosp=hospital_beds_region$HOSPITAL_BEDS/hospital_beds_region$POP*1000000,
                                      ICU=hospital_beds_region$ICU_PER_1000*1000) # per million pop beds and ICU

## Read in movement from jakarta
## Extract movement from and to Jakarta only from Ramadan and non-Ramadan movement matrices
from_jakarta_normal <- matrix_idn$normal[1,]
to_jakarta_normal <- matrix_idn$normal[,1]
from_jakarta_ramadan <- matrix_idn$ramadan[1,]
to_jakarta_ramadan <- matrix_idn$ramadan[,1]

from_jakarta_normal_ramadan_diff <- (from_jakarta_ramadan-from_jakarta_normal)/from_jakarta_normal*100
to_jakarta_normal_ramadan_diff <- (to_jakarta_ramadan-to_jakarta_normal)/to_jakarta_normal*100

idadmin2 <- urban_rural_flag_df$IDREGION

movement_jakarta <- data.frame(IDADMIN2=idadmin2,from_jakarta_normal=from_jakarta_normal*100,to_jakarta_normal=to_jakarta_normal*100,
                               from_jakarta_ramadan_increase=from_jakarta_normal_ramadan_diff,to_jakarta_ramadan_increase=to_jakarta_normal_ramadan_diff)
movement_jakarta[1,c(2,3,4,5)] <- 0
movement_jakarta <- movement_jakarta[-2,] # remove movements to and from outside Java

##setup admin-level summary
prov_summary <- urban_rural_flag_df %>% select(prov=PROVINCE,district=REGION) %>% mutate(median_age_bracket=NA,p_hosp=NA,p_crit_given_hosp=NA,beds_hosp_reg=NA,beds_ICU=NA)

## get demographic summary
total_pop <- rowSums(pop_matrix)
median_pop <- rowSums(pop_matrix)/2
cumsum_pop <- rowCumsums(pop_matrix)
age_bracket <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")


for (i in seq_len(nrow(pop_matrix))){
  x <- 0
  j <- 1
  while (x==0){
    if(cumsum_pop[i,j]>=median_pop[i]){
      x <- 1
    } else {
      x <- 0
      j <- j + 1
    }
  } 
  prov_summary$median_age_bracket[i] <- age_bracket[j]
}
## add relevant averages based upon squire inputs
prov_summary$beds_hosp_reg <- beds_per_million_region$hosp
prov_summary$prop_over50 <- rowSums(pop_matrix[,11:16])/rowSums(pop_matrix)
prov_summary$id <- prov_summary$district
prov_summary <- prov_summary[-2,]
prov_summary$beds_hosp_reg <- prov_summary$beds_hosp_reg/1000

## apply breaks for mapping
prov_summary$beds_hosp_reg_factor <- cut(prov_summary$beds_hosp_reg,breaks = c(0,0.5,1,2,4,Inf),
                                         labels = c("<=0.5","(0.5,1]","(1,2]","(2,4]",">4"),include.lowest = TRUE)

##capture differences in movement between during and outside ramadan period
prov_summary$from_jakarta_normal <- movement_jakarta$from_jakarta_normal
prov_summary$to_jakarta_normal <- movement_jakarta$to_jakarta_normal
prov_summary$from_jakarta_ramadan_increase <- movement_jakarta$from_jakarta_ramadan_increase
prov_summary$to_jakarta_ramadan_increase <- movement_jakarta$to_jakarta_ramadan_increase

district_java_shp <- readOGR(dsn="Data/map", layer="ADMIN2_JAVA", stringsAsFactors = FALSE)
district_java_shp_df <- fortify(district_java_shp, region = "ADMIN2")
province_java_shp <- readOGR(dsn="Data/map", layer="ADMIN1_JAVA", stringsAsFactors = FALSE)
province_java_shp_df <- fortify(province_java_shp, region = "ADMIN1")
province_java_shp_df$ifr <- NA

district_java_shp_df <- district_java_shp_df %>% left_join(prov_summary)
### PLOT MAPS ####
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

map <- ggplot(data = district_java_shp_df, aes(x = long, y = lat, group = group))

p_over50_map <- map + 
  geom_polygon(aes(fill=prop_over50*100),color = 'black', size = 0.25) +
  guides(fill=guide_colorbar(title="Proportion of population\nover 50 years old (%)")) + 
  scale_fill_gradient(high = "#bd0026", low = "#ffeda0", guide = "colorbar") + 
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=0.75) + 
  theme(legend.position="bottom") + theme_bare

p_beds_map <- map + 
  geom_polygon(aes(fill=beds_hosp_reg_factor),color = 'black', size = 0.25) +
  guides(fill=guide_legend(title="Regular hospital beds\nper 1,000 population")) +
  scale_fill_manual(values = c("#f7fcb9", "#addd8e", "#78c679", "#41ab5d", "#006837")) + 
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=0.75) + 
  theme(legend.position="bottom") + theme_bare

p_from_jakarta_normal_map <- map + 
  geom_polygon(aes(fill=from_jakarta_normal),color = 'black', size = 0.25) +
  guides(fill=guide_colorbar(title="Movement from Jakarta\nnon-Ramadan (%)")) + 
  scale_fill_gradient(high = "#bd0026", low = "#ffeda0", guide = "colorbar") + 
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=0.75) + 
  theme(legend.position="bottom") + theme_bare

p_from_jakarta_ramadan_map <- map + 
  geom_polygon(aes(fill=from_jakarta_ramadan_increase),color = 'black', size = 0.25) +
  guides(fill=guide_colorbar(title="Movement from Jakarta\n(% increase during Ramadan from normal)")) + 
  scale_fill_gradient(high = "#bd0026", low = "#ffeda0", guide = "colorbar") + 
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=0.75) + 
  theme(legend.position="bottom") + theme_bare

# Output province-level Rt trends 
mov_roll_lims <- range(mobility_Rt$mov_roll_reduce)
mov_roll_grid <- seq(from=mov_roll_lims[1], to = mov_roll_lims[2], by = 0.02)

output_model_fit_list <- list()
for (i in 1:100){
  predict_split_funerals_roll_CV_true <- data.frame(mov_roll_reduce=predict(spline_model_list[[i]],mov_roll_grid)$x,
                                                    Rt=predict(spline_model_list[[i]],mov_roll_grid)$y,
                                                    model="Smoothing spline",sample=i)
  output_model_funerals_roll <- bind_rows(predict_split_funerals_roll_CV_true)
  output_model_fit_list[[i]] <- output_model_funerals_roll
}
output_model_fit_df <- bind_rows(output_model_fit_list)

p_mobility_Rt_model <- ggplot(mobility_Rt,aes(x=mov_roll_reduce,y=Rt)) + geom_point(col="gray") +
  geom_line(data=output_model_fit_df,aes(x=mov_roll_reduce,y=Rt,group=sample),alpha=0.5,col="#d95f0e") +
  labs(col="") + theme_bw() + theme(legend.position = "bottom") + xlab("Mobility reduction to baseline - 7 days rolling average (%)")

spline_output_list_df <- bind_rows(spline_output_list)
p_spline_prov <- spline_output_list_df %>% filter(prov != "OUTSIDE JAVA") %>% 
  ggplot(aes(x=date,y=Rt,group=sample)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0.8, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0.8, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0.8,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0.8,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0.8,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0.8,Inf), col="#00441b", lty=2) + 
  geom_line(alpha=0.5,col="#2c7fb8") + theme_bw() + geom_hline(yintercept = 1,col="red",lty=2) +
  facet_wrap(.~prov) + xlab("Date") + scale_x_date(date_breaks = "2 months", date_labels = "%b")

### MAKE PLOT ##########
p_3_AB <- plot_grid(p_over50_map, p_beds_map, align="h", labels=c("A","B"))
p_3_CD <- plot_grid(p_from_jakarta_normal_map, p_from_jakarta_ramadan_map, align="h", labels=c("C","D"))
p_3_EF <- plot_grid(p_mobility_Rt_model, p_spline_prov, align="h", labels=c("E","F"))

p_3 <- plot_grid(p_3_AB, p_3_CD, p_3_EF, rel_heights = c(1, 1, 1), align = "v",nrow = 3)

pdf(file = "Output/FIG-3.pdf", width = 10, height = 10)
  p_3
dev.off()