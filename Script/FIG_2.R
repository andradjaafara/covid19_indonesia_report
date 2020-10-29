################## PART 1: PLOT THE DATA FOR THREE METRICS AND TIMINGS OF KEY DATES
## Read data
jkt_data_evaluate <- read.csv("Data/raw/jakarta_covid_data_evaluate.csv",header = TRUE,stringsAsFactors = FALSE)
jkt_data_evaluate$date <- dmy(jkt_data_evaluate$date)

## No of tests
idn_tests <- read.csv("Data/raw/idn_tests.csv",header = TRUE,stringsAsFactors = FALSE) %>% mutate(date=dmy(date))
idn_tests_jkt <- idn_tests %>% filter(prov=="JAKARTA") %>% 
  mutate(data_type="Cases",people_test_pos_pct=people_test_pos_pct*100) %>% select(prov,date,people_test_pos_pct,data_type)
idn_tests_jkt$data_type <- factor(idn_tests_jkt$data_type,levels = c("Cases","Deaths","C19P funerals"))

## Plotting daily cases, deaths and funerals
jkt_title_evaluate <- c("Cases",
                        "Deaths",
                        "C19P funerals")

jkt_title_evaluate_df <- data.frame(data_type_ori=c("daily_new_cases_report","daily_new_deaths_report","daily_covid_funerals"),
                                    data_type=c("Cases",
                                                "Deaths",
                                                "C19P funerals"))

jkt_title_evaluate_long <- jkt_data_evaluate %>% select(date,daily_new_cases_report,daily_new_deaths_report,daily_covid_funerals) %>% 
  pivot_longer(-date,names_to="data_type_ori",values_to="values") %>% left_join(jkt_title_evaluate_df) %>% 
  mutate(data_type=factor(data_type,levels=c("Cases",
                                             "Deaths",
                                             "C19P funerals")))

## Figure 2A

p_2A_pt1 <- jkt_title_evaluate_long %>% filter(data_type=="Cases") %>% ggplot(aes(x=date,y=values,fill=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) + 
  geom_col(stat = "identity") +
  geom_line(data=idn_tests_jkt,aes(y=people_test_pos_pct*12,x=date),col="black",size=1.25) +
  theme_bw() + xlab("Reporting date") + ylab("") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#810F7C")) + labs(col="",fill="") + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) +
  scale_y_continuous(sec.axis = sec_axis(~./12, name = "Test positivity ratio (%)"),limits = c(0,3500/3)) + 
  scale_colour_manual(labels = "Test positivity ratio", values = "black") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/3, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/3, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 3500/3, xend = ymd("2020-09-02"), yend = 3500/3),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/3, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

p_2A_pt2 <- jkt_title_evaluate_long %>% filter(data_type=="Deaths") %>% ggplot(aes(x=date,y=values,fill=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) + 
  geom_col(stat = "identity") + ylim(0,3500/90) +
  theme_bw() + xlab("Reporting date") + ylab("N") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#BD0026")) + labs(col="",fill="") + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) +
  scale_colour_manual(labels = "Test positivity ratio", values = "black") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/90, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/90, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 3500/90, xend = ymd("2020-09-02"), yend = 3500/90),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/90, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

p_2A_pt3 <- jkt_title_evaluate_long %>% filter(data_type=="C19P funerals") %>% ggplot(aes(x=date,y=values,fill=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) + 
  geom_col(stat = "identity") + ylim(0,3500/55) +
  theme_bw() + xlab("Reporting date") + ylab("") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#993404")) + labs(col="",fill="") + theme(legend.position="none",strip.background = element_blank(),
                                                                        strip.text.x = element_blank(),legend.text = element_text(size=10)) +
  scale_colour_manual(labels = "Test positivity ratio", values = "black") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/55, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/55, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 3500/55, xend = ymd("2020-09-02"), yend = 3500/55),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/55, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

p_2A_legend <- jkt_title_evaluate_long %>% ggplot(aes(x=date,y=values,fill=data_type)) + geom_col(stat = "identity") + 
  geom_line(data=idn_tests_jkt,aes(y=people_test_pos_pct*12,x=date,col="tpr"),size=1.25) +
  theme_bw() + xlab("Reporting date") + ylab("N") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#810F7C","#BD0026","#993404")) +
  geom_vline(xintercept=ymd(c("2020-03-02","2020-03-15","2020-04-10","2020-04-24","2020-06-01")),lty=2) + 
  scale_y_continuous(sec.axis = sec_axis(~./12, name = "Test positivity ratio (%)")) + 
  scale_colour_manual(values="black",labels="Test positivity ratio") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + guides(col = guide_legend(override.aes = list(size=1.25))) + 
  labs(col="",fill="") + theme(legend.position="top",strip.background = element_blank(), strip.text.x = element_blank(),legend.text = element_text(size=10))

p_2A_legend <- cowplot::get_legend(p_2A_legend)

p_2A_graph <- plot_grid(p_2A_pt1,p_2A_pt2,p_2A_pt3,nrow=3,align = "v",rel_heights = c(0.825,0.825,1))
p_2A <- plot_grid(p_2A_legend,p_2A_graph,nrow=2,rel_heights = c(0.05,1))
p_2A
# pdf(file = "Output/Fig-2A-example.pdf", width = 5, height = 5)
# p_2A
# dev.off()

#### PART 2: RECONSTRUCT FREQUENCIES OF TIMINGS OF ONSETS 
# First section is reconstructions of onset frequencies for each metric
## gamma delay distribution onset -> death estimated from hospitalisation linelist (see methods)
gamma_shape <- 2.887089 
gamma_rate <- 0.1819211 

## gamma delay distribution onset -> observation estimated from hospitalisation linelist (see methods)
gamma_shape_cases <- 1.029509 
gamma_rate_cases <- 0.1351061 

funerals_df <- jkt_data_evaluate[,c(1,4)]
funerals_df$date <- as.Date(funerals_df$date)
funerals_date_vector <- vector()
for (i in seq_len(nrow(funerals_df))){
  funerals_date_vector <- c(funerals_date_vector,rep(funerals_df[i,1],funerals_df[i,2]))
}
funerals_date_vector <- as.Date(funerals_date_vector)

deaths_df <- jkt_data_evaluate[,c(1,3)]
deaths_df$date <- as.Date(deaths_df$date)
deaths_date_vector <- vector()
for (i in seq_len(nrow(deaths_df))){
  deaths_date_vector <- c(deaths_date_vector,rep(deaths_df[i,1],deaths_df[i,2]))
}
deaths_date_vector <- as.Date(deaths_date_vector)

cases_df <- jkt_data_evaluate[,c(1,2)]
cases_df$date <- as.Date(cases_df$date)
cases_date_vector <- vector()
for (i in seq_len(nrow(cases_df))){
  cases_date_vector <- c(cases_date_vector,rep(cases_df[i,1],cases_df[i,2]))
}
cases_date_vector <- as.Date(cases_date_vector)

n_reconstruct <- 100
# seed_deaths <- round(runif(n_reconstruct,1,1000000))
# seed_funerals <- round(runif(n_reconstruct,1,1000000))
# seed_cases <- round(runif(n_reconstruct,1,1000000))
# saveRDS(seed_deaths,"./Script/seed-deaths.rds")
# saveRDS(seed_funerals,"./Script/seed-funerals.rds")
# saveRDS(seed_cases,"./Script/seed-cases.rds")
seed_deaths <- readRDS("Script/Seeds/seed-deaths.rds")
seed_funerals <- readRDS("Script/Seeds/seed-funerals.rds")
seed_cases <- readRDS("Script/Seeds/seed-cases.rds")

# vector of dates of death: date of funeral - 1 day
funerals_adj_date_vector <- funerals_date_vector - 1

# cumulative gamma for cases adjustment
prop_adjust <- pgamma(1:365,shape=gamma_shape,rate=gamma_rate) # a maximum of a year worth of data; previously 200
prop_adjust_cases <- pgamma(1:365,shape=gamma_shape_cases,rate=gamma_rate_cases) # a maximum of a year worth of data; previously 200

# last date
last_date_deaths <- max(deaths_date_vector)
last_date_funerals <- max(funerals_date_vector)
last_date_cases <- max(cases_date_vector)

reported_deaths_list <- list()
reported_funerals_list <- list()
reported_cases_list <- list()
reported_deaths_adjusted_list <- list()
reported_funerals_adjusted_list <- list()
reported_cases_adjusted_list <- list()

# draw 100 representations of the onset frequencies of cases, deaths and funerals
for (i in 1:n_reconstruct){
  ##draw the sets of delays between onset and outcome
  set.seed(seed_deaths[i])
  delays_sim_deaths <- ceiling(rgamma(length(deaths_date_vector),shape=gamma_shape,rate=gamma_rate))
  set.seed(seed_funerals[i])
  delays_sim_funerals <- ceiling(rgamma(length(funerals_adj_date_vector),shape=gamma_shape,rate=gamma_rate))
  set.seed(seed_cases[i])
  delays_sim_cases <- ceiling(rgamma(length(cases_date_vector),shape=gamma_shape_cases,rate=gamma_rate_cases))
  
  ##generate the reconstructed frequencies using these delays
  reported_date_vector_deaths <- deaths_date_vector - delays_sim_deaths
  reported_date_vector_funerals <- funerals_adj_date_vector - delays_sim_funerals
  reported_date_vector_cases <- cases_date_vector - delays_sim_cases
  ## find extent of period ###
  all_dates_deaths <- data.frame(dates=seq(min(reported_date_vector_deaths),max(reported_date_vector_deaths),by="days"))
  all_dates_funerals <- data.frame(dates=seq(min(reported_date_vector_funerals),max(reported_date_vector_funerals),by="days"))
  all_dates_cases <- data.frame(dates=seq(min(reported_date_vector_cases),max(reported_date_vector_cases),by="days"))
  date_adjust_index_deaths <- last_date_deaths - seq(min(reported_date_vector_deaths),max(reported_date_vector_deaths),by="days")
  date_adjust_index_funerals <- last_date_funerals - seq(min(reported_date_vector_funerals),max(reported_date_vector_funerals),by="days")
  date_adjust_index_cases <- last_date_cases - seq(min(reported_date_vector_cases),max(reported_date_vector_cases),by="days")
  
  ## create dataframe, adjusting for right censoring of recent onsets
  reported_df_deaths <- as.data.frame(table(reported_date_vector_deaths))
  colnames(reported_df_deaths) <- c("dates","I")
  reported_df_deaths$dates <- ymd(reported_df_deaths$dates)
  reported_df_deaths <- reported_df_deaths %>% right_join(all_dates_deaths) %>% replace_na(list(I = 0)) %>% arrange(dates)
  reported_df_deaths_adjusted <- reported_df_deaths %>% mutate(I=I/prop_adjust[date_adjust_index_deaths])
  
  reported_df_funerals <- as.data.frame(table(reported_date_vector_funerals))
  colnames(reported_df_funerals) <- c("dates","I")
  reported_df_funerals$dates <- ymd(reported_df_funerals$dates)
  reported_df_funerals <- reported_df_funerals %>% right_join(all_dates_funerals) %>% replace_na(list(I = 0)) %>% arrange(dates)
  reported_df_funerals_adjusted <- reported_df_funerals %>% mutate(I=I/prop_adjust[date_adjust_index_funerals])
  
  reported_df_cases <- as.data.frame(table(reported_date_vector_cases))
  colnames(reported_df_cases) <- c("dates","I")
  reported_df_cases$dates <- ymd(reported_df_cases$dates)
  reported_df_cases <- reported_df_cases %>% right_join(all_dates_cases) %>% replace_na(list(I = 0)) %>% arrange(dates)
  reported_df_cases_adjusted <- reported_df_cases %>% mutate(I=I/prop_adjust_cases[date_adjust_index_cases])
  
  ### add to list 
  reported_deaths_list[[i]] <- reported_df_deaths
  reported_funerals_list[[i]] <- reported_df_funerals
  reported_cases_list[[i]] <- reported_df_cases
  reported_deaths_adjusted_list[[i]] <- reported_df_deaths_adjusted
  reported_funerals_adjusted_list[[i]] <- reported_df_funerals_adjusted
  reported_cases_adjusted_list[[i]] <- reported_df_cases_adjusted
}

# get minimum and maximum reconstructed dates
reported_deaths_adjusted_all <- bind_rows(reported_deaths_adjusted_list)
sequence_all_date_deaths <- seq(min(reported_deaths_adjusted_all$date),max(reported_deaths_adjusted_all$date),by="days")
sequence_all_date_deaths_from_2mar <- sequence_all_date_deaths[sequence_all_date_deaths>=ymd("2020-03-02")]
reported_funerals_adjusted_all <- bind_rows(reported_funerals_adjusted_list)
sequence_all_date_funerals <- seq(min(reported_funerals_adjusted_all$date),max(reported_funerals_adjusted_all$date),by="days")
sequence_all_date_funerals_from_2mar <- sequence_all_date_funerals[sequence_all_date_funerals>=ymd("2020-03-02")]
reported_cases_adjusted_all <- bind_rows(reported_cases_adjusted_list)
sequence_all_date_cases <- seq(min(reported_cases_adjusted_all$date),max(reported_cases_adjusted_all$date),by="days")
sequence_all_date_cases_from_2mar <- sequence_all_date_cases[sequence_all_date_cases>=ymd("2020-03-02")]


## Reconstruction of onset summary (Figure 1A)
# get all dates
jkt_all_adj_date_df <- data.frame(date=seq(min(sequence_all_date_deaths,sequence_all_date_funerals,sequence_all_date_cases),
                                           max(jkt_data_evaluate$date),by="days"))

tail_to_omit <- 9 # number of tails to omit; based on the gamma probability
# to measure how much % of deaths not yet observed to 'tolerate': 
# d_gamma <- distcrete::distcrete("gamma", 1, shape = gamma_shape, rate = gamma_rate)
# sum(d_gamma$d(1:tail_to_omit))

reconstructed_deaths_all <- matrix(,nrow=(n_reconstruct),ncol=length(sequence_all_date_deaths))
reconstructed_funerals_all <- matrix(,nrow=(n_reconstruct),ncol=length(sequence_all_date_funerals))
reconstructed_cases_all <- matrix(,nrow=(n_reconstruct),ncol=length(sequence_all_date_cases))

for (i in 1:n_reconstruct){
  recons_deaths <- reported_deaths_adjusted_list[[i]]
  recons_funerals <- reported_funerals_adjusted_list[[i]]
  recons_cases <- reported_cases_adjusted_list[[i]]
  idx_deaths_dates <- match(recons_deaths$date,sequence_all_date_deaths)
  idx_funerals_dates <- match(recons_funerals$date,sequence_all_date_funerals)
  idx_cases_dates <- match(recons_cases$date,sequence_all_date_cases)
  reconstructed_deaths_all[i,idx_deaths_dates] <- recons_deaths$I
  reconstructed_funerals_all[i,idx_funerals_dates] <- recons_funerals$I
  reconstructed_cases_all[i,idx_cases_dates] <- recons_cases$I
}

reconstructed_deaths_all_t <- t(reconstructed_deaths_all)
reconstructed_funerals_all_t <- t(reconstructed_funerals_all)
reconstructed_cases_all_t <- t(reconstructed_cases_all)

reconstructed_deaths_all_df <- as.data.frame(rowQuantiles(reconstructed_deaths_all_t,na.rm = TRUE,probs=c(0.025,0.5,0.975)))
colnames(reconstructed_deaths_all_df) <- c("lower_CI","median","upper_CI")
reconstructed_deaths_all_df <- reconstructed_deaths_all_df %>% mutate(date=sequence_all_date_deaths) %>% right_join(jkt_all_adj_date_df) %>% 
  mutate(data_type="Deaths - adjusted") %>% arrange(date)
reconstructed_funerals_all_df <- as.data.frame(rowQuantiles(reconstructed_funerals_all_t,na.rm = TRUE,probs=c(0.025,0.5,0.975)))
colnames(reconstructed_funerals_all_df) <- c("lower_CI","median","upper_CI")
reconstructed_funerals_all_df <- reconstructed_funerals_all_df %>% mutate(date=sequence_all_date_funerals) %>% right_join(jkt_all_adj_date_df) %>% 
  mutate(data_type="C19P funerals - adjusted") %>% arrange(date)
reconstructed_cases_all_df <- as.data.frame(rowQuantiles(reconstructed_cases_all_t,na.rm = TRUE,probs=c(0.025,0.5,0.975)))
colnames(reconstructed_cases_all_df) <- c("lower_CI","median","upper_CI")
reconstructed_cases_all_df <- reconstructed_cases_all_df %>% mutate(date=sequence_all_date_cases) %>% right_join(jkt_all_adj_date_df) %>% 
  mutate(data_type="Cases - adjusted") %>% arrange(date)

reconstructed_deaths_all_df_rmv_tail <- reconstructed_deaths_all_df
reconstructed_deaths_all_df_rmv_tail$lower_CI <- c(reconstructed_deaths_all_df_rmv_tail$lower_CI[1:(nrow(reconstructed_deaths_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_deaths_all_df_rmv_tail$median <- c(reconstructed_deaths_all_df_rmv_tail$median[1:(nrow(reconstructed_deaths_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_deaths_all_df_rmv_tail$upper_CI <- c(reconstructed_deaths_all_df_rmv_tail$upper_CI[1:(nrow(reconstructed_deaths_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_funerals_all_df_rmv_tail <- reconstructed_funerals_all_df
reconstructed_funerals_all_df_rmv_tail$lower_CI <- c(reconstructed_funerals_all_df_rmv_tail$lower_CI[1:(nrow(reconstructed_funerals_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_funerals_all_df_rmv_tail$median <- c(reconstructed_funerals_all_df_rmv_tail$median[1:(nrow(reconstructed_funerals_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_funerals_all_df_rmv_tail$upper_CI <- c(reconstructed_funerals_all_df_rmv_tail$upper_CI[1:(nrow(reconstructed_funerals_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_cases_all_df_rmv_tail <- reconstructed_cases_all_df
reconstructed_cases_all_df_rmv_tail$lower_CI <- c(reconstructed_cases_all_df_rmv_tail$lower_CI[1:(nrow(reconstructed_cases_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_cases_all_df_rmv_tail$median <- c(reconstructed_cases_all_df_rmv_tail$median[1:(nrow(reconstructed_cases_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))
reconstructed_cases_all_df_rmv_tail$upper_CI <- c(reconstructed_cases_all_df_rmv_tail$upper_CI[1:(nrow(reconstructed_cases_all_df_rmv_tail)-tail_to_omit)],rep(NA,tail_to_omit))

reconstructed_combined <- bind_rows(reconstructed_deaths_all_df,reconstructed_funerals_all_df,reconstructed_cases_all_df)
reconstructed_combined_rmv_tail <- bind_rows(reconstructed_deaths_all_df_rmv_tail,reconstructed_funerals_all_df_rmv_tail,reconstructed_cases_all_df_rmv_tail)

reconstructed_combined$data_type <- factor(reconstructed_combined$data_type,levels=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted"))
reconstructed_combined_rmv_tail$data_type <- factor(reconstructed_combined_rmv_tail$data_type,levels=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted"))

p_reconstructed_combined <- reconstructed_combined %>% ggplot(aes(x=date,y=median,fill=data_type)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() + xlab("Reporting date") + ylab("N") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#810F7C","#BD0026","#993404")) + labs(col="",fill="") + theme(legend.position="top",strip.background = element_blank(),
                                                                                            strip.text.x = element_blank(),legend.text = element_text(size=6)) +
  geom_vline(xintercept=ymd(c("2020-03-02","2020-03-15","2020-04-10","2020-04-24","2020-06-05")),lty=2)

p_reconstructed_combined_rmv_tail <- reconstructed_combined_rmv_tail %>% ggplot(aes(x=date,y=median,fill=data_type)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() + xlab("Reporting date") + ylab("N") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#810F7C","#BD0026","#993404")) + labs(col="",fill="") + theme(legend.position="top",strip.background = element_blank(),
                                                                                            strip.text.x = element_blank(),legend.text = element_text(size=6)) +
  geom_vline(xintercept=ymd(c("2020-03-02","2020-03-15","2020-04-10","2020-04-24","2020-06-05")),lty=2)

## Figure 2B
# new fig 2
cases_and_reconstructed <- bind_rows(reconstructed_combined_rmv_tail)
cases_and_reconstructed$data_type <- factor(cases_and_reconstructed$data_type, 
                                            levels=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted"))

p_2B_legend <- cases_and_reconstructed %>% ggplot(aes(x=date,y=median,fill=data_type)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() + xlab("Onset date") + ylab("N") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#810F7C","#BD0026","#993404"), labels=c("Cases","Deaths","C19P funerals")) + 
  labs(col="",fill="") + theme(legend.position="top",strip.background = element_blank(),
                               strip.text.x = element_blank(),legend.text = element_text(size=10)) +
  geom_vline(xintercept=ymd(c("2020-03-02","2020-03-15","2020-04-10","2020-04-24","2020-06-01")),lty=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

p_2B_pt1 <- cases_and_reconstructed %>% filter(data_type=="Cases - adjusted") %>% ggplot(aes(x=date,y=median,fill=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) + 
  geom_col(stat = "identity") + ylim(0,3500/4) +
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() + xlab("Onset date") + ylab("") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#810F7C")) + labs(col="",fill="") + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) +
  scale_colour_manual(labels = "Test positivity ratio", values = "black") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/4, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/4, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 3500/4, xend = ymd("2020-09-02"), yend = 3500/4),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/4, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

p_2B_pt2 <- cases_and_reconstructed %>% filter(data_type=="Deaths - adjusted") %>% ggplot(aes(x=date,y=median,fill=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) + 
  geom_col(stat = "identity") + ylim(0,3500/100) +
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() + xlab("Onset date") + ylab("N") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#BD0026")) + labs(col="",fill="") + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) +
  scale_colour_manual(labels = "Test positivity ratio", values = "black") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/100, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/100, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 3500/100, xend = ymd("2020-09-02"), yend = 3500/100),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/100, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

p_2B_pt3 <- cases_and_reconstructed %>% filter(data_type=="C19P funerals - adjusted") %>% ggplot(aes(x=date,y=median,fill=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) + 
  geom_col(stat = "identity") + ylim(0,3500/50) +
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() + xlab("Onset date") + ylab("") + facet_wrap(.~data_type,scales = "free_y",ncol=1) + 
  scale_fill_manual(values=c("#993404")) + labs(col="",fill="") + theme(legend.position="none",strip.background = element_blank(),
                                                                        strip.text.x = element_blank(),legend.text = element_text(size=10)) +
  scale_colour_manual(labels = "Test positivity ratio", values = "black") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/50, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/50, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 3500/50, xend = ymd("2020-09-02"), yend = 3500/50),
               lineend = "butt", linejoin = "mitre",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/50, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

p_2B_legend <- cowplot::get_legend(p_2B_legend)

p_2B_graph <- plot_grid(p_2B_pt1,p_2B_pt2,p_2B_pt3,nrow=3,align = "v",rel_heights = c(0.825,0.825,1))
p_2B <- plot_grid(p_2B_legend,p_2B_graph,nrow=2,rel_heights = c(0.05,1))


# pdf(file = "Indonesia/Output/Fig-2B-example.pdf", width = 8, height = 8)
# p_2B
# dev.off()

#### 

##### FIGURE 2C Rt and mobility timelines  #####
## Mobility processing
source("Script/mobility_process.R") # to read and format google mobility data

## Process google mobility data for Jakarta
mobility_rename <- data.frame(mobility_type_ori=c("retail_and_recreation_percent_change_from_baseline",
                                                  "grocery_and_pharmacy_percent_change_from_baseline",
                                                  "parks_percent_change_from_baseline",
                                                  "transit_stations_percent_change_from_baseline",
                                                  "workplaces_percent_change_from_baseline",
                                                  "residential_percent_change_from_baseline",
                                                  "average_non_residential"),
                              mobility_type=c("Retail & recreation",
                                              "Grocery & pharmacy",
                                              "Parks",
                                              "Transit stations",
                                              "Workplaces",
                                              "Residential",
                                              "Non-residential average"))
google_mobility_jkt <- google_mobility_prov %>% filter(prov == "JAKARTA") %>% select(-c(COUNTRY_ID,COUNTRY,period,prov)) %>% 
  mutate(average_non_residential=(0.2*retail_and_recreation_percent_change_from_baseline +
                                    0.2*grocery_and_pharmacy_percent_change_from_baseline +
                                    0.2*parks_percent_change_from_baseline +
                                    0.2*transit_stations_percent_change_from_baseline +
                                    0.2*workplaces_percent_change_from_baseline))
google_mobility_jkt_before_2Mar_avg <- google_mobility_jkt %>% filter(date <= ymd("2020-03-02")) %>% 
  summarise(retail_and_recreation_percent_change_from_baseline=mean(retail_and_recreation_percent_change_from_baseline),
            grocery_and_pharmacy_percent_change_from_baseline=mean(grocery_and_pharmacy_percent_change_from_baseline),
            parks_percent_change_from_baseline=mean(parks_percent_change_from_baseline),
            transit_stations_percent_change_from_baseline=mean(transit_stations_percent_change_from_baseline),
            workplaces_percent_change_from_baseline=mean(workplaces_percent_change_from_baseline),
            residential_percent_change_from_baseline=mean(residential_percent_change_from_baseline),
            average_non_residential=mean(average_non_residential))

google_mobility_jkt_updated <- google_mobility_jkt
mov_avg_2Mar_matrix <- matrix(rep(as.numeric(google_mobility_jkt_before_2Mar_avg[1,]),17),nrow=17,byrow = TRUE)
colnames(mov_avg_2Mar_matrix) <- colnames(google_mobility_jkt_updated)[-1]
google_mobility_jkt_updated %<>% mutate_at(2:8,as.double)
google_mobility_jkt_updated[1:17,2:8] <- as_tibble(mov_avg_2Mar_matrix) # this updated version has mobility before 3 Mar as the average over that period; for figure 1D

## EpiEstim
# Serial Interval distribution https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30287-5/fulltext 
# long 6.3, 4.2

mean_SI <- 6.3
std_SI <- 4.2

# EpiEstim for cases
epiestim_data_cases <- data.frame(dates=jkt_data_evaluate$date,I=jkt_data_evaluate$daily_new_cases_report)

T_start_cases <- seq(2, (nrow(epiestim_data_cases)-6)) # starting at 2 as conditional on the past observations
T_end_cases <- T_start_cases + 6 # adding 6 to get 7-day windows as bounds included in window

res_parametric_si_cases <- estimate_R(epiestim_data_cases, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = mean_SI, 
                                        std_si = std_SI,
                                        t_start=T_start_cases,
                                        t_end=T_end_cases)))

Rt_cases_dates <- res_parametric_si_cases$dates[8:length(res_parametric_si_cases$dates)]
Rt_cases <- res_parametric_si_cases$R
Rt_cases$date <- Rt_cases_dates
Rt_cases <- Rt_cases %>% right_join(jkt_date_df) %>% mutate(data_type="Cases") %>% arrange(date)
Rt_cases <- Rt_cases[,c(5,8,11,12,13)]
colnames(Rt_cases) <- c("lower_CI","median","upper_CI","date","data_type")

# Epiestim for deaths and funerals

# setup matrix of posterior samples
n_post_samples <- 100
posterior_samples_deaths_all <- matrix(,nrow=(n_post_samples*n_reconstruct),ncol=length(sequence_all_date_deaths_from_2mar))
posterior_samples_funerals_all <- matrix(,nrow=(n_post_samples*n_reconstruct),ncol=length(sequence_all_date_funerals_from_2mar))
posterior_samples_cases_all <- matrix(,nrow=(n_post_samples*n_reconstruct),ncol=length(sequence_all_date_cases_from_2mar))

# setup seed of posterior samples
n_seed_post_samples_deaths <- n_reconstruct*length(sequence_all_date_deaths_from_2mar)
n_seed_post_samples_funerals <- n_reconstruct*length(sequence_all_date_funerals_from_2mar)
n_seed_post_samples_cases <- n_reconstruct*length(sequence_all_date_cases_from_2mar)
# posterior_samples_deaths_seed <- matrix(round(runif(n_seed_post_samples_deaths,1,1000000)),nrow=(n_reconstruct),ncol=length(sequence_all_date_deaths_from_2mar))
# posterior_samples_funerals_seed <- matrix(round(runif(n_seed_post_samples_funerals,1,1000000)),nrow=(n_reconstruct),ncol=length(sequence_all_date_funerals_from_2mar))
# posterior_samples_cases_seed <- matrix(round(runif(n_seed_post_samples_cases,1,1000000)),nrow=(n_reconstruct),ncol=length(sequence_all_date_cases_from_2mar))
# saveRDS(posterior_samples_deaths_seed,"Indonesia/Script/posterior_samples_deaths_seed_20200902.rds") # _20200710
# saveRDS(posterior_samples_funerals_seed,"Indonesia/Script/posterior_samples_funerals_seed_20200902.rds")
# saveRDS(posterior_samples_cases_seed,"Indonesia/Script/posterior_samples_cases_seed_20200902.rds") # _20200710
posterior_samples_deaths_seed <- readRDS("Script/Seeds/posterior_samples_deaths_seed_20200902.rds")
posterior_samples_funerals_seed <- readRDS("Script/Seeds/posterior_samples_funerals_seed_20200902.rds")
posterior_samples_cases_seed <- readRDS("Script/Seeds/posterior_samples_cases_seed_20200902.rds")

# time window length
t_window <- 6 # 6 means 1-week, 13 means 2-week

for (i in 1:n_reconstruct){
  epiestim_data_deaths <- data.frame(dates=reported_deaths_adjusted_list[[i]]$date,I=reported_deaths_adjusted_list[[i]]$I) %>% drop_na()
  epiestim_data_funerals <- data.frame(dates=reported_funerals_adjusted_list[[i]]$date,I=reported_funerals_adjusted_list[[i]]$I) %>% drop_na()
  epiestim_data_cases <- data.frame(dates=reported_cases_adjusted_list[[i]]$date,I=reported_cases_adjusted_list[[i]]$I) %>% drop_na()
  
  ##estimate R prior to march 2nd####
    # epiestim_data_deaths_after_2mar <- epiestim_data_deaths %>% filter(dates>=ymd("2020-02-25")) # 3 March to be the 8th data, epiestim purpose (if weekly)
  # epiestim_data_funerals_after_2mar <- epiestim_data_funerals %>% filter(dates>=ymd("2020-02-25")) # 3 March to be the 8th data, epiestim purpose (if weekly)
  epiestim_data_deaths_upto_2mar <- epiestim_data_deaths %>% filter(dates<=ymd("2020-03-02"))
  epiestim_data_funerals_upto_2mar <- epiestim_data_funerals %>% filter(dates<=ymd("2020-03-02"))
  epiestim_data_cases_upto_2mar <- epiestim_data_cases %>% filter(dates<=ymd("2020-03-02"))
  
  res_parametric_si_deaths_upto_2mar <- estimate_R(epiestim_data_deaths_upto_2mar, 
                                                   method="parametric_si",
                                                   config = make_config(list(
                                                     mean_si = mean_SI, 
                                                     std_si = std_SI,
                                                     t_start = 2,
                                                     t_end=nrow(epiestim_data_deaths_upto_2mar))))
  res_parametric_si_funerals_upto_2mar <- estimate_R(epiestim_data_funerals_upto_2mar, 
                                                     method="parametric_si",
                                                     config = make_config(list(
                                                       mean_si = mean_SI, 
                                                       std_si = std_SI,
                                                       t_start = 2,
                                                       t_end = nrow(epiestim_data_funerals_upto_2mar))))
  res_parametric_si_cases_upto_2mar <- estimate_R(epiestim_data_cases_upto_2mar, 
                                                  method="parametric_si",
                                                  config = make_config(list(
                                                    mean_si = mean_SI, 
                                                    std_si = std_SI,
                                                    t_start = 2,
                                                    t_end=nrow(epiestim_data_cases_upto_2mar))))
  
  #### AFTER 2nd March allow R to vary on weekly basis ####
  T_start_deaths_after_2mar <- seq(2,(nrow(epiestim_data_deaths)-t_window))
  T_end_deaths_after_2mar <- T_start_deaths_after_2mar + t_window
  T_start_funerals_after_2mar <- seq(2,(nrow(epiestim_data_funerals)-t_window))
  T_end_funerals_after_2mar <- T_start_funerals_after_2mar + t_window
  T_start_cases_after_2mar <- seq(2,(nrow(epiestim_data_cases)-t_window))
  T_end_cases_after_2mar <- T_start_cases_after_2mar + t_window
  
  res_parametric_si_deaths_after_2mar <- estimate_R(epiestim_data_deaths, 
                                                    method="parametric_si",
                                                    config = make_config(list(
                                                    mean_si = mean_SI, 
                                                    std_si = std_SI,
                                                    t_start = T_start_deaths_after_2mar,
                                                    t_end = T_end_deaths_after_2mar)))
  res_parametric_si_funerals_after_2mar <- estimate_R(epiestim_data_funerals, 
                                                      method="parametric_si",
                                                      config = make_config(list(
                                                      mean_si = mean_SI, 
                                                      std_si = std_SI,
                                                      t_start = T_start_funerals_after_2mar,
                                                      t_end = T_end_funerals_after_2mar)))
  res_parametric_si_cases_after_2mar <- estimate_R(epiestim_data_cases, 
                                                   method="parametric_si",
                                                   config = make_config(list(
                                                   mean_si = mean_SI, 
                                                   std_si = std_SI,
                                                   t_start = T_start_cases_after_2mar,
                                                   t_end = T_end_cases_after_2mar)))
  init_index_3mar_deaths <- which(res_parametric_si_deaths_after_2mar$dates==ymd("2020-03-03"))-t_window-1
  init_index_3mar_funerals <- which(res_parametric_si_funerals_after_2mar$dates==ymd("2020-03-03"))-t_window-1
  init_index_3mar_cases <- which(res_parametric_si_cases_after_2mar$dates==ymd("2020-03-03"))-t_window-1
  
  ### SAMPLE FROM POSTERIOR ####
  posterior_samples_deaths <- matrix(,nrow=n_post_samples,ncol=(nrow(res_parametric_si_deaths_after_2mar$R)-init_index_3mar_deaths+2))
  posterior_samples_funerals <- matrix(,nrow=n_post_samples,ncol=(nrow(res_parametric_si_funerals_after_2mar$R)-init_index_3mar_funerals+2))
  posterior_samples_cases <- matrix(,nrow=n_post_samples,ncol=(nrow(res_parametric_si_cases_after_2mar$R)-init_index_3mar_cases+2))
  
  set.seed(posterior_samples_deaths_seed[i,1])
  posterior_samples_deaths[,1] <- sample_posterior_R(res_parametric_si_deaths_upto_2mar, n=n_post_samples, window=1)
  set.seed(posterior_samples_funerals_seed[i,1])
  posterior_samples_funerals[,1] <- sample_posterior_R(res_parametric_si_funerals_upto_2mar, n=n_post_samples, window=1)
  set.seed(posterior_samples_cases_seed[i,1])
  posterior_samples_cases[,1] <- sample_posterior_R(res_parametric_si_cases_upto_2mar, n=n_post_samples, window=1)
  
  for (j in 2:(nrow(res_parametric_si_deaths_after_2mar$R)-init_index_3mar_deaths+2)){
    set.seed(posterior_samples_deaths_seed[i,j])
    posterior_samples_deaths[,j] <- sample_posterior_R(res_parametric_si_deaths_after_2mar, n=n_post_samples, window=init_index_3mar_deaths+j-2)
  }
  
  for (j in 2:(nrow(res_parametric_si_funerals_after_2mar$R)-init_index_3mar_funerals+2)){
    set.seed(posterior_samples_funerals_seed[i,j])
    posterior_samples_funerals[,j] <- sample_posterior_R(res_parametric_si_funerals_after_2mar, n=n_post_samples, window=init_index_3mar_funerals+j-2)
  }
  
  for (j in 2:(nrow(res_parametric_si_cases_after_2mar$R)-init_index_3mar_cases+2)){
    set.seed(posterior_samples_cases_seed[i,j])
    posterior_samples_cases[,j] <- sample_posterior_R(res_parametric_si_cases_after_2mar, n=n_post_samples, window=init_index_3mar_cases+j-2)
  }
  
  start_row_idx <- 1+(n_post_samples*(i-1))
  end_row_idx <- n_post_samples*i
  
  posterior_samples_deaths_all[start_row_idx:end_row_idx,1:(nrow(res_parametric_si_deaths_after_2mar$R)-init_index_3mar_deaths+2)] <- posterior_samples_deaths
  posterior_samples_funerals_all[start_row_idx:end_row_idx,1:(nrow(res_parametric_si_funerals_after_2mar$R)-init_index_3mar_funerals+2)] <- posterior_samples_funerals
  posterior_samples_cases_all[start_row_idx:end_row_idx,1:(nrow(res_parametric_si_cases_after_2mar$R)-init_index_3mar_cases+2)] <- posterior_samples_cases
}



# Posterior post processing
posterior_samples_deaths_all_t <- t(posterior_samples_deaths_all)
posterior_samples_funerals_all_t <- t(posterior_samples_funerals_all)
posterior_samples_cases_all_t <- t(posterior_samples_cases_all)

posterior_samples_deaths_all_df <- as.data.frame(rowQuantiles(posterior_samples_deaths_all_t,na.rm = TRUE,probs=c(0.025,0.5,0.975)))
colnames(posterior_samples_deaths_all_df) <- c("lower_CI","median","upper_CI")
posterior_samples_deaths_all_df <- posterior_samples_deaths_all_df %>% mutate(date=sequence_all_date_deaths_from_2mar)
posterior_samples_funerals_all_df <- as.data.frame(rowQuantiles(posterior_samples_funerals_all_t,na.rm = TRUE,probs=c(0.025,0.5,0.975)))
colnames(posterior_samples_funerals_all_df) <- c("lower_CI","median","upper_CI")
posterior_samples_funerals_all_df <- posterior_samples_funerals_all_df %>% mutate(date=sequence_all_date_funerals_from_2mar)
posterior_samples_cases_all_df <- as.data.frame(rowQuantiles(posterior_samples_cases_all_t,na.rm = TRUE,probs=c(0.025,0.5,0.975)))
colnames(posterior_samples_cases_all_df) <- c("lower_CI","median","upper_CI")
posterior_samples_cases_all_df <- posterior_samples_cases_all_df %>% mutate(date=sequence_all_date_cases_from_2mar)

Rt_deaths <- posterior_samples_deaths_all_df %>% right_join(jkt_date_df) %>% mutate(data_type="Deaths - adjusted") %>% arrange(date)
Rt_funerals <- posterior_samples_funerals_all_df %>% right_join(jkt_date_df) %>% mutate(data_type="C19P funerals - adjusted") %>% arrange(date)
Rt_cases <- posterior_samples_cases_all_df %>% right_join(jkt_date_df) %>% mutate(data_type="Cases - adjusted") %>% arrange(date)
Rt_deaths$lower_CI <- c(Rt_deaths$lower_CI[1:(nrow(Rt_deaths)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_deaths$median <- c(Rt_deaths$median[1:(nrow(Rt_deaths)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_deaths$upper_CI <- c(Rt_deaths$upper_CI[1:(nrow(Rt_deaths)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_funerals$lower_CI <- c(Rt_funerals$lower_CI[1:(nrow(Rt_funerals)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_funerals$median <- c(Rt_funerals$median[1:(nrow(Rt_funerals)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_funerals$upper_CI <- c(Rt_funerals$upper_CI[1:(nrow(Rt_funerals)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_cases$lower_CI <- c(Rt_cases$lower_CI[1:(nrow(Rt_cases)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_cases$median <- c(Rt_cases$median[1:(nrow(Rt_cases)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_cases$upper_CI <- c(Rt_cases$upper_CI[1:(nrow(Rt_cases)-tail_to_omit)],rep(NA,tail_to_omit))


# Rt estimates all dates
Rt_deaths_all <- posterior_samples_deaths_all_df %>% right_join(jkt_all_adj_date_df) %>% mutate(data_type="Deaths - adjusted") %>% arrange(date)
Rt_deaths_all$lower_CI <- c(Rt_deaths_all$lower_CI[1:(nrow(Rt_deaths_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_deaths_all$median <- c(Rt_deaths_all$median[1:(nrow(Rt_deaths_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_deaths_all$upper_CI <- c(Rt_deaths_all$upper_CI[1:(nrow(Rt_deaths_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_funerals_all <- posterior_samples_funerals_all_df %>% right_join(jkt_all_adj_date_df) %>% mutate(data_type="C19P funerals - adjusted") %>% arrange(date)
Rt_funerals_all$lower_CI <- c(Rt_funerals_all$lower_CI[1:(nrow(Rt_funerals_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_funerals_all$median <- c(Rt_funerals_all$median[1:(nrow(Rt_funerals_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_funerals_all$upper_CI <- c(Rt_funerals_all$upper_CI[1:(nrow(Rt_funerals_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_cases_all <- posterior_samples_cases_all_df %>% right_join(jkt_all_adj_date_df) %>% mutate(data_type="Cases - adjusted") %>% arrange(date)
Rt_cases_all$lower_CI <- c(Rt_cases_all$lower_CI[1:(nrow(Rt_cases_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_cases_all$median <- c(Rt_cases_all$median[1:(nrow(Rt_cases_all)-tail_to_omit)],rep(NA,tail_to_omit))
Rt_cases_all$upper_CI <- c(Rt_cases_all$upper_CI[1:(nrow(Rt_cases_all)-tail_to_omit)],rep(NA,tail_to_omit))

index_15feb <- which(jkt_all_adj_date_df$date == ymd("2020-02-15"))
index_01mar <- which(jkt_all_adj_date_df$date == ymd("2020-03-01"))
index_02mar <- which(jkt_all_adj_date_df$date == ymd("2020-03-02"))

Rt_deaths_all$lower_CI[index_15feb:index_01mar] <- Rt_deaths_all$lower_CI[index_02mar]
Rt_deaths_all$median[index_15feb:index_01mar] <- Rt_deaths_all$median[index_02mar]
Rt_deaths_all$upper_CI[index_15feb:index_01mar] <- Rt_deaths_all$upper_CI[index_02mar]
Rt_funerals_all$lower_CI[index_15feb:index_01mar] <- Rt_funerals_all$lower_CI[index_02mar]
Rt_funerals_all$median[index_15feb:index_01mar] <- Rt_funerals_all$median[index_02mar]
Rt_funerals_all$upper_CI[index_15feb:index_01mar] <- Rt_funerals_all$upper_CI[index_02mar]
Rt_cases_all$lower_CI[index_15feb:index_01mar] <- Rt_cases_all$lower_CI[index_02mar]
Rt_cases_all$median[index_15feb:index_01mar] <- Rt_cases_all$median[index_02mar]
Rt_cases_all$upper_CI[index_15feb:index_01mar] <- Rt_cases_all$upper_CI[index_02mar]


jkt_Rt_cases_mobility <- Rt_cases_all %>% select(date,median,lower_CI,upper_CI) %>% left_join(google_mobility_jkt) %>% 
  pivot_longer(-c(date,median,lower_CI,upper_CI),names_to = "mobility_type_ori",values_to = "mobility_changes") %>% left_join(mobility_rename) %>% 
  select(date,mobility_type,median,lower_CI,upper_CI,mobility_changes) %>% mutate(data_type="Cases - adjusted")
jkt_Rt_deaths_mobility <- Rt_deaths_all %>% select(date,median,lower_CI,upper_CI) %>% left_join(google_mobility_jkt) %>% 
  pivot_longer(-c(date,median,lower_CI,upper_CI),names_to = "mobility_type_ori",values_to = "mobility_changes") %>% left_join(mobility_rename) %>% 
  select(date,mobility_type,median,lower_CI,upper_CI,mobility_changes) %>% mutate(data_type="Deaths - adjusted")
jkt_Rt_funerals_mobility <- Rt_funerals_all %>% select(date,median,lower_CI,upper_CI) %>% left_join(google_mobility_jkt) %>% 
  pivot_longer(-c(date,median,lower_CI,upper_CI),names_to = "mobility_type_ori",values_to = "mobility_changes") %>% left_join(mobility_rename) %>% 
  select(date,mobility_type,median,lower_CI,upper_CI,mobility_changes) %>% mutate(data_type="C19P funerals - adjusted")

jkt_Rt_mobility_combined <- bind_rows(jkt_Rt_cases_mobility,jkt_Rt_deaths_mobility,jkt_Rt_funerals_mobility)
jkt_Rt_mobility_combined$data_type <- factor(jkt_Rt_mobility_combined$data_type,levels=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted"))

jkt_Rt_mobility_combined_rmv_beginning <- jkt_Rt_mobility_combined %>% filter(date>=ymd("2020-02-14") & mobility_type=="Non-residential average")
jkt_mobility_combined_rmv_beginning <- jkt_Rt_mobility_combined_rmv_beginning
jkt_Rt_mobility_combined_rmv_beginning$data_type <- factor(jkt_Rt_mobility_combined_rmv_beginning$data_type,levels=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility"))

# Rt over 1
jkt_Rt_mobility_combined_rmv_beginning <- jkt_Rt_mobility_combined_rmv_beginning %>% mutate(Rt_over_1 = ifelse(median>1,1,0))
Rt_over_1 <- jkt_Rt_mobility_combined_rmv_beginning %>% filter(Rt_over_1 == 1)

Rt_over_1$group <- 1
group_code <- 1
min_date_group <- min(Rt_over_1$date)-1
over_1_list <- list()
for (i in seq_len(nrow(Rt_over_1))){
  current_date_group <- Rt_over_1$date[i]
  diff_date_group <- current_date_group-min_date_group
  if(diff_date_group==1){
    Rt_over_1$group[i] <- group_code
  } else {
    group_code <- group_code + 1
    Rt_over_1$group[i] <- group_code
  }
  min_date_group <- current_date_group
}

for (i in 1:max(Rt_over_1$group)){
  data_over_1_group <- Rt_over_1 %>% filter(group==i)
  df_over_1_group <- data.frame(data_type=data_over_1_group$data_type[1],date_min=min(data_over_1_group$date),date_max=max(data_over_1_group$date))
  over_1_list[[i]] <- df_over_1_group
}



over_1_df <- bind_rows(over_1_list)
over_1_c <- over_1_df %>% filter(data_type=="Cases - adjusted")
over_1_d <- over_1_df %>% filter(data_type=="Deaths - adjusted")
over_1_f <- over_1_df %>% filter(data_type=="C19P funerals - adjusted")


p_2C_legend <- jkt_Rt_mobility_combined_rmv_beginning %>% ggplot(aes(x=date,group=data_type,col=data_type,fill=data_type,shape=data_type)) + 
  geom_line(aes(y=median),size=2) + 
  geom_point(aes(y=median),size=2) + 
  geom_hline(yintercept=1,col="red",lty=2,size=2) + geom_point(aes(y=median),size=0.5) +
  geom_point(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=1.5)  + 
  geom_line(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=0.025,alpha=0.05)  + 
  geom_ribbon(aes(x=date,ymin=lower_CI,ymax=upper_CI),alpha=0.5) + 
  geom_ribbon(aes(x=date,ymin=mobility_changes/20+3-0.005,ymax=mobility_changes/20+3+0.005,col="Mobility",fill="Mobility"),alpha=0.05) +
  scale_y_continuous(sec.axis = sec_axis(~ (.-3)*20, name = "Mobility changes (%)")) +
  xlab("Date") + ylab("Rt") + theme_bw() + 
  facet_wrap(.~data_type,scales = "free_y",ncol=1) +
  scale_colour_manual(values=c("#810F7C","#BD0026","#993404","black"),breaks=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility"),
                      labels=c("Cases","Deaths","C19P funerals","Mobility")) + 
  scale_fill_manual(values=c("#810F7C","#BD0026","#993404",NA),breaks=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility"),
                    labels=c("Cases","Deaths","C19P funerals","Mobility")) + 
  scale_shape_manual(values=c(NA,NA,NA,16),breaks=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility"),
                     labels=c("Cases","Deaths","C19P funerals","Mobility")) +
  theme(legend.position="top",strip.background = element_blank(),strip.text.x = element_blank(),legend.text = element_text(size=10)) + labs(col="",fill="",shape="") +
  geom_vline(xintercept=ymd(c("2020-03-02","2020-03-15","2020-04-10","2020-04-24","2020-06-01")),lty=2) +
  guides(shape = guide_legend(size=2)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

p_2C_pt1 <- jkt_Rt_mobility_combined_rmv_beginning %>% filter(data_type=="Cases - adjusted") %>% 
  ggplot(aes(x=date,group=data_type,col=data_type,fill=data_type,shape=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(-Inf,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(-Inf,Inf), col="#00441b", lty=2) +  
  geom_line(aes(y=median),size=2) + 
  geom_point(aes(y=median),size=2) + 
  geom_hline(yintercept=1,col="red",lty=2,size=2) + geom_point(aes(y=median),size=0.5) +
  geom_point(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=1.5)  + 
  geom_line(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=0.025,alpha=0.05)  + 
  geom_ribbon(aes(x=date,ymin=lower_CI,ymax=upper_CI),alpha=0.5) + 
  geom_ribbon(aes(x=date,ymin=mobility_changes/20+3-0.005,ymax=mobility_changes/20+3+0.005,col="Mobility",fill="Mobility"),alpha=0.05) +
  scale_y_continuous(sec.axis = sec_axis(~ (.-3)*20, name = ""), limits=c(0,3.5)) +
  xlab("Date") + ylab("") + theme_bw() + 
  facet_wrap(.~data_type,scales = "free_y",ncol=1) +
  scale_colour_manual(values=c("#810F7C","black"),breaks=c("Cases - adjusted","Mobility")) + 
  scale_fill_manual(values=c("#810F7C",NA),breaks=c("Cases - adjusted","Mobility")) + 
  scale_shape_manual(values=c(NA,16),breaks=c("Cases - adjusted","Mobility")) + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) + labs(col="",fill="",shape="") +
  guides(col = guide_legend(ncol = 2), shape = guide_legend(size=2)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/1000, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/1000, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  annotate(geom = "segment", x = ymd("2020-06-10"), y = 3500/1000, xend = ymd("2020-09-02"), yend = 3500/1000,
           lineend = "butt", linejoin = "mitre",
           size = 0.5, arrow = arrow(length = unit(0.1, "inches"))) +
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/1000, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

for(i in seq_len(nrow(over_1_c))){
  p_2C_pt1 <- p_2C_pt1 +
    geom_rect(data = over_1_c[i,], aes(xmin = date_min, xmax = date_max, ymin = -Inf, ymax = Inf), alpha = 0.65, fill="grey", inherit.aes = FALSE)
}

p_2C_pt2 <- jkt_Rt_mobility_combined_rmv_beginning %>% filter(data_type=="Deaths - adjusted") %>% 
  ggplot(aes(x=date,group=data_type,col=data_type,fill=data_type,shape=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(-Inf,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(-Inf,Inf), col="#00441b", lty=2) +  
  geom_line(aes(y=median),size=2) + 
  geom_point(aes(y=median),size=2) + 
  geom_hline(yintercept=1,col="red",lty=2,size=2) + geom_point(aes(y=median),size=0.5) +
  geom_point(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=1.5)  + 
  geom_line(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=0.025,alpha=0.05)  + 
  geom_ribbon(aes(x=date,ymin=lower_CI,ymax=upper_CI),alpha=0.5) + 
  geom_ribbon(aes(x=date,ymin=mobility_changes/20+3-0.005,ymax=mobility_changes/20+3+0.005,col="Mobility",fill="Mobility"),alpha=0.05) +
  scale_y_continuous(sec.axis = sec_axis(~ (.-3)*20, name = "Mobility changes (%)"), limits=c(0,3.5)) +
  xlab("Date") + ylab("Rt") + theme_bw() + 
  facet_wrap(.~data_type,scales = "free_y",ncol=1) +
  scale_colour_manual(values=c("#BD0026","black"),breaks=c("Deaths - adjusted","Mobility")) + 
  scale_fill_manual(values=c("#BD0026",NA),breaks=c("Deaths - adjusted","Mobility")) + 
  scale_shape_manual(values=c(NA,16),breaks=c("Deaths - adjusted","Mobility")) + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) + labs(col="",fill="",shape="") +
  guides(col = guide_legend(ncol = 2), shape = guide_legend(size=2)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/1000, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/1000, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  annotate(geom = "segment", x = ymd("2020-06-10"), y = 3500/1000, xend = ymd("2020-09-02"), yend = 3500/1000,
           lineend = "butt", linejoin = "mitre",
           size = 0.5, arrow = arrow(length = unit(0.1, "inches"))) +
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/1000, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

for(i in seq_len(nrow(over_1_d))){
  p_2C_pt2 <- p_2C_pt2 +
    geom_rect(data = over_1_d[i,], aes(xmin = date_min, xmax = date_max, ymin = -Inf, ymax = Inf), alpha = 0.65, fill="grey", inherit.aes = FALSE)
}

p_2C_pt3 <- jkt_Rt_mobility_combined_rmv_beginning %>% filter(data_type=="C19P funerals - adjusted") %>% 
  ggplot(aes(x=date,group=data_type,col=data_type,fill=data_type,shape=data_type)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(-Inf,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(-Inf,Inf), col="#00441b", lty=2) +  
  geom_line(aes(y=median),size=2) + 
  geom_point(aes(y=median),size=2) + 
  geom_hline(yintercept=1,col="red",lty=2,size=2) + geom_point(aes(y=median),size=0.5) +
  geom_point(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=1.5)  + 
  geom_line(aes(y=mobility_changes/20+3,col="Mobility",shape="Mobility"),size=0.025,alpha=0.05)  + 
  geom_ribbon(aes(x=date,ymin=lower_CI,ymax=upper_CI),alpha=0.5) + 
  geom_ribbon(aes(x=date,ymin=mobility_changes/20+3-0.005,ymax=mobility_changes/20+3+0.005,col="Mobility",fill="Mobility"),alpha=0.05) +
  scale_y_continuous(sec.axis = sec_axis(~ (.-3)*20, name = ""), limits=c(0,3.5)) +
  xlab("Date") + ylab("") + theme_bw() + 
  facet_wrap(.~data_type,scales = "free_y",ncol=1) +
  scale_colour_manual(values=c("#993404","black"),breaks=c("C19P funerals - adjusted","Mobility")) + 
  scale_fill_manual(values=c("#993404",NA),breaks=c("C19P funerals - adjusted","Mobility")) + 
  scale_shape_manual(values=c(NA,16),breaks=c("C19P funerals - adjusted","Mobility")) +
  theme(legend.position="none",strip.background = element_blank(),strip.text.x = element_blank(),legend.text = element_text(size=10)) + labs(col="",fill="",shape="") +
  guides(col = guide_legend(ncol = 2), shape = guide_legend(size=2)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate(geom = "text", x = ymd("2020-04-16"), y = 2000/1000, 
           label = substitute(paste(italic('PSBB'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000/1000, 
           label = "Ramadan domestic\ntravel restrictions", angle = 90, size=3, hjust=0.5) +
  annotate(geom = "segment", x = ymd("2020-06-10"), y = 3500/1000, xend = ymd("2020-09-02"), yend = 3500/1000,
           lineend = "butt", linejoin = "mitre",
           size = 0.5, arrow = arrow(length = unit(0.1, "inches"))) +
  annotate(geom = "text", x = ymd("2020-07-17"), y = 3250/1000, 
           label = substitute(paste(italic('AKB'),"/New normal")), size=3)

for(i in seq_len(nrow(over_1_f))){
  p_2C_pt3 <- p_2C_pt3 +
    geom_rect(data = over_1_f[i,], aes(xmin = date_min, xmax = date_max, ymin = -Inf, ymax = Inf), alpha = 0.65, fill="grey", inherit.aes = FALSE)
}

p_2C_legend <- cowplot::get_legend(p_2C_legend)

p_2C_graph <- plot_grid(p_2C_pt1,p_2C_pt2,p_2C_pt3,nrow=3,align = "v",rel_heights = c(0.825,0.825,1))
p_2C <- plot_grid(p_2C_legend,p_2C_graph,nrow=2,rel_heights = c(0.05,1))


# pdf(file = "Output/Fig-2C-example.pdf", width = 8, height = 8)
# p_2C
# dev.off()


### FIGURE 2D Correlation between R and mobility ####

### extract draws from posterior of Rts #####
dates_Rt_mobility <- seq(ymd("2020-02-15"),max(jkt_data_evaluate$date)-tail_to_omit,by="days")
posterior_Rt_mobility_deaths_matrix <- matrix(,nrow=length(dates_Rt_mobility),ncol=100)
posterior_Rt_mobility_funerals_matrix <- matrix(,nrow=length(dates_Rt_mobility),ncol=100)
posterior_Rt_mobility_cases_matrix <- matrix(,nrow=length(dates_Rt_mobility),ncol=100)

# setup seed of posterior samples
# posterior_samples_Rt_mobility_deaths_seed <- round(runif(length(dates_Rt_mobility),1,1000000))
# posterior_samples_Rt_mobility_funerals_seed <- round(runif(length(dates_Rt_mobility),1,1000000))
# posterior_samples_Rt_mobility_cases_seed <- round(runif(length(dates_Rt_mobility),1,1000000))
# saveRDS(posterior_samples_Rt_mobility_deaths_seed,"Indonesia/Script/posterior_samples_Rt_mobility_deaths_seed_20200902.rds")
# saveRDS(posterior_samples_Rt_mobility_funerals_seed,"Indonesia/Script/posterior_samples_Rt_mobility_funerals_seed_20200902.rds")
# saveRDS(posterior_samples_Rt_mobility_cases_seed,"Indonesia/Script/posterior_samples_Rt_mobility_cases_seed_20200902.rds")
posterior_samples_Rt_mobility_deaths_seed <- readRDS("Script/Seeds/posterior_samples_Rt_mobility_deaths_seed_20200902.rds")
posterior_samples_Rt_mobility_funerals_seed <- readRDS("Script/Seeds/posterior_samples_Rt_mobility_funerals_seed_20200902.rds")
posterior_samples_Rt_mobility_cases_seed <- readRDS("Script/Seeds/posterior_samples_Rt_mobility_cases_seed_20200902.rds")

for (i in 1:17){
  post_deaths <- posterior_samples_deaths_all[,1][!is.na(posterior_samples_deaths_all[,1])]
  post_funerals <- posterior_samples_funerals_all[,1][!is.na(posterior_samples_funerals_all[,1])]
  post_cases <- posterior_samples_cases_all[,1][!is.na(posterior_samples_cases_all[,1])]
  set.seed(posterior_samples_Rt_mobility_deaths_seed[i])
  posterior_Rt_mobility_deaths_matrix[i,] <- sample(post_deaths,100)
  set.seed(posterior_samples_Rt_mobility_funerals_seed[i])
  posterior_Rt_mobility_funerals_matrix[i,] <- sample(post_funerals,100)
  set.seed(posterior_samples_Rt_mobility_cases_seed[i])
  posterior_Rt_mobility_cases_matrix[i,] <- sample(post_cases,100)
}

for (i in 18:length(dates_Rt_mobility)){
  post_deaths <- posterior_samples_deaths_all[,i-16][!is.na(posterior_samples_deaths_all[,i-16])]
  post_funerals <- posterior_samples_funerals_all[,i-16][!is.na(posterior_samples_funerals_all[,i-16])]
  post_cases <- posterior_samples_cases_all[,i-16][!is.na(posterior_samples_cases_all[,i-16])]
  set.seed(posterior_samples_Rt_mobility_deaths_seed[i])
  posterior_Rt_mobility_deaths_matrix[i,] <- sample(post_deaths,100)
  set.seed(posterior_samples_Rt_mobility_funerals_seed[i])
  posterior_Rt_mobility_funerals_matrix[i,] <- sample(post_funerals,100)
  set.seed(posterior_samples_Rt_mobility_cases_seed[i])
  posterior_Rt_mobility_cases_matrix[i,] <- sample(post_cases,100)
}

posterior_Rt_mobility_cases_list <- list()
posterior_Rt_mobility_deaths_list <- list()
posterior_Rt_mobility_funerals_list <- list()

corr_cases <- vector()
corr_deaths <- vector()
corr_funerals <- vector()
### get average reduction in mobility relative to maximum #####
last_date_omit <- max(jkt_data_evaluate$date)-tail_to_omit
last_date_google <- max(google_mobility_jkt_updated$date)
if (last_date_omit<=last_date_google){
  len_google <- nrow(google_mobility_jkt_updated)
  index_08mar <- which(google_mobility_jkt_updated$date==ymd("2020-03-08"))
  index_last_date_omit <- which(google_mobility_jkt_updated$date==last_date_omit)
  diff_omit_google <- as.numeric(last_date_google - last_date_omit)
  index_posterior_cases <- nrow(posterior_Rt_mobility_cases_matrix)-tail_to_omit+diff_omit_google
} else {
  len_google <- nrow(google_mobility_jkt_updated)
  index_08mar <- which(google_mobility_jkt_updated$date==ymd("2020-03-08"))
  index_last_date_omit <- len_google
  diff_omit_google <- as.numeric(last_date_google - last_date_omit)
  index_posterior_cases <- nrow(posterior_Rt_mobility_cases_matrix)-tail_to_omit+diff_omit_google
}

### create dataframe and calculate correlation ######
for (i in 1:100){
  posterior_Rt_mobility_cases_list[[i]] <- data.frame(average_non_residential=google_mobility_jkt_updated$average_non_residential[1:index_last_date_omit],
                                                      Rt=posterior_Rt_mobility_cases_matrix[,i],
                                                      date=dates_Rt_mobility,
                                                      data_type="Cases - adjusted",
                                                      x=i)
  posterior_Rt_mobility_deaths_list[[i]] <- data.frame(average_non_residential=google_mobility_jkt_updated$average_non_residential[1:index_last_date_omit],
                                                       Rt=posterior_Rt_mobility_deaths_matrix[,i],
                                                       date=dates_Rt_mobility,
                                                       data_type="Deaths - adjusted",
                                                       x=i)
  posterior_Rt_mobility_funerals_list[[i]] <- data.frame(average_non_residential=google_mobility_jkt_updated$average_non_residential[1:index_last_date_omit],
                                                         Rt=posterior_Rt_mobility_funerals_matrix[,i],
                                                         date=dates_Rt_mobility,
                                                         data_type="C19P funerals - adjusted",
                                                         x=i)
  corr_cases[i] <- cor(google_mobility_jkt_updated$average_non_residential[1:index_last_date_omit],posterior_Rt_mobility_cases_matrix[,i])
  corr_deaths[i] <- cor(google_mobility_jkt_updated$average_non_residential[1:index_last_date_omit],posterior_Rt_mobility_deaths_matrix[,i])
  corr_funerals[i] <- cor(google_mobility_jkt_updated$average_non_residential[1:index_last_date_omit],posterior_Rt_mobility_funerals_matrix[,i])
}

posterior_Rt_mobility_cases <- bind_rows(posterior_Rt_mobility_cases_list)
posterior_Rt_mobility_deaths <- bind_rows(posterior_Rt_mobility_deaths_list)
posterior_Rt_mobility_funerals <- bind_rows(posterior_Rt_mobility_funerals_list)
posterior_Rt_mobility <- bind_rows(posterior_Rt_mobility_cases,posterior_Rt_mobility_deaths,posterior_Rt_mobility_funerals)
posterior_Rt_mobility$data_type <- factor(posterior_Rt_mobility$data_type,levels=unique(posterior_Rt_mobility$data_type))

quantile_corr_cases <- round(quantile(corr_cases,probs = c(0.025,0.5,0.975)),2)
quantile_corr_deaths <- round(quantile(corr_deaths,probs = c(0.025,0.5,0.975)),2)
quantile_corr_funerals <- round(quantile(corr_funerals,probs = c(0.025,0.5,0.975)),2)

dat_text <- data.frame(
  labels = c(paste0("Corr = ",quantile_corr_cases[2],"; 95% CI = (",quantile_corr_cases[1],",",quantile_corr_cases[3],")"), 
             paste0("Corr = ",quantile_corr_deaths[2],"; 95% CI = (",quantile_corr_deaths[1],",",quantile_corr_deaths[3],")"), 
             paste0("Corr = ",quantile_corr_funerals[2],"; 95% CI = (",quantile_corr_funerals[1],",",quantile_corr_funerals[3],")")),
  data_type   = unique(posterior_Rt_mobility$data_type), x=c(1,1,1)
)

## Figure 2D
date_NN <- ymd("2020-06-05")
corr_cases_NN <- vector()
corr_deaths_NN <- vector()
corr_funerals_NN <- vector()
for (i in 1:100){
  data_corr_cases <- posterior_Rt_mobility_cases_list[[i]] %>% filter(date<date_NN)
  data_corr_deaths <- posterior_Rt_mobility_deaths_list[[i]] %>% filter(date<date_NN)
  data_corr_funerals <- posterior_Rt_mobility_funerals_list[[i]] %>% filter(date<date_NN)
  corr_cases_NN[i] <- cor(data_corr_cases$average_non_residential,data_corr_cases$Rt)
  corr_deaths_NN[i] <- cor(data_corr_deaths$average_non_residential,data_corr_deaths$Rt)
  corr_funerals_NN[i] <- cor(data_corr_funerals$average_non_residential,data_corr_funerals$Rt)
}

quantile_corr_cases_NN <- round(quantile(corr_cases_NN,probs = c(0.025,0.5,0.975)),2)
quantile_corr_deaths_NN <- round(quantile(corr_deaths_NN,probs = c(0.025,0.5,0.975)),2)
quantile_corr_funerals_NN <- round(quantile(corr_funerals_NN,probs = c(0.025,0.5,0.975)),2)

dat_text_NN <- data.frame(
  labels = c(paste0("Corr = ",quantile_corr_cases_NN[2],"; 95% CI = (",quantile_corr_cases_NN[1],",",quantile_corr_cases_NN[3],")"), 
             paste0("Corr = ",quantile_corr_deaths_NN[2],"; 95% CI = (",quantile_corr_deaths_NN[1],",",quantile_corr_deaths_NN[3],")"), 
             paste0("Corr = ",quantile_corr_funerals_NN[2],"; 95% CI = (",quantile_corr_funerals_NN[1],",",quantile_corr_funerals_NN[3],")")),
  data_type   = unique(posterior_Rt_mobility$data_type), x=c(1,1,1)
)

dat_text_NN_c <- dat_text_NN %>% filter(data_type=="Cases - adjusted")
dat_text_NN_d <- dat_text_NN %>% filter(data_type=="Deaths - adjusted")
dat_text_NN_f <- dat_text_NN %>% filter(data_type=="C19P funerals - adjusted")

# Exclude data after 'new_normal' or in the text: AKB

p_2D_legend <- posterior_Rt_mobility %>% filter(date<date_NN) %>% ggplot(aes(x=average_non_residential*-1,y=Rt,group=x,col=data_type)) + 
  theme_bw() + facet_wrap(.~data_type,scale="free",ncol=1) + geom_line(alpha=0.05) + xlab("Mobility reduction (% to baseline)") +
  geom_hline(yintercept = 1, col="red", size=1.5, lty=2) +
  geom_text(data    = dat_text_NN,
            mapping = aes(x = 15, y = Inf, label = labels),
            hjust   = -0.1,
            vjust   = 2, show.legend = FALSE) +
  scale_colour_manual(values=c("#810F7C","#BD0026","#993404","black"),
                      breaks=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility"),
                      labels=c("Cases","Deaths","C19P funerals","Mobility")) +
  labs(col="") + theme(legend.position="top",
                       strip.background = element_blank(),
                       strip.text.x = element_blank(),legend.text = element_text(size=10)) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5)))

p_2D_pt1 <- posterior_Rt_mobility %>% filter(date<date_NN & data_type=="Cases - adjusted") %>% 
  ggplot(aes(x=average_non_residential*-1,y=Rt,group=x,col=data_type)) + 
  theme_bw() + facet_wrap(.~data_type,scale="free",ncol=1) + geom_line(alpha=0.05) + xlab("Mobility reduction (% to baseline)") +
  geom_hline(yintercept = 1, col="red", size=1.5, lty=2) +
  geom_text(data    = dat_text_NN_c,
            mapping = aes(x = 15, y = Inf, label = labels),
            hjust   = -0.1,
            vjust   = 2, show.legend = FALSE) +
  scale_colour_manual(values=c("#810F7C","black"),
                      breaks=c("Cases - adjusted","Mobility")) +
  labs(col="") + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) +
  guides(color = guide_legend(ncol = 2,override.aes = list(alpha = 1, size = 1.5))) + ylab("")

p_2D_pt2 <- posterior_Rt_mobility %>% filter(date<date_NN & data_type=="Deaths - adjusted") %>% 
  ggplot(aes(x=average_non_residential*-1,y=Rt,group=x,col=data_type)) + 
  theme_bw() + facet_wrap(.~data_type,scale="free",ncol=1) + geom_line(alpha=0.05) + xlab("Mobility reduction (% to baseline)") +
  geom_hline(yintercept = 1, col="red", size=1.5, lty=2) +
  geom_text(data    = dat_text_NN_d,
            mapping = aes(x = 15, y = Inf, label = labels),
            hjust   = -0.1,
            vjust   = 2, show.legend = FALSE) +
  scale_colour_manual(values=c("#BD0026","black"),
                      breaks=c("Deaths - adjusted","Mobility")) +
  labs(col="") + 
  theme(legend.position="none",strip.background = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size=10),
        axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank()) +
  guides(color = guide_legend(ncol = 2,override.aes = list(alpha = 1, size = 1.5)))

p_2D_pt3 <- posterior_Rt_mobility %>% filter(date<date_NN & data_type=="C19P funerals - adjusted") %>% 
  ggplot(aes(x=average_non_residential*-1,y=Rt,group=x,col=data_type)) + 
  theme_bw() + facet_wrap(.~data_type,scale="free",ncol=1) + geom_line(alpha=0.05) + xlab("Mobility reduction (% to baseline)") +
  geom_hline(yintercept = 1, col="red", size=1.5, lty=2) +
  geom_text(data    = dat_text_NN_f,
            mapping = aes(x = 15, y = Inf, label = labels),
            hjust   = -0.1,
            vjust   = 2, show.legend = FALSE) +
  scale_colour_manual(values=c("#993404","black"),
                      breaks=c("C19P funerals - adjusted","Mobility")) +
  labs(col="") + theme(legend.position="none",
                       strip.background = element_blank(),
                       strip.text.x = element_blank(),legend.text = element_text(size=10)) +
  guides(color = guide_legend(ncol = 2,override.aes = list(alpha = 1, size = 1.5))) + ylab("")

p_2D_legend <- cowplot::get_legend(p_2D_legend)

p_2D_graph <- plot_grid(p_2D_pt1,p_2D_pt2,p_2D_pt3,nrow=3,align = "v",rel_heights = c(0.825,0.825,1))
p_2D <- plot_grid(p_2D_legend,p_2D_graph,nrow=2,rel_heights = c(0.05,1))



# pdf(file = "Output/Fig-2D-example.pdf", width = 8, height = 8)
# p_2D
# dev.off()

## Plot Figure 2

p_FIG_2 <- plot_grid(p_2A, p_2B, p_2C, p_2D, 
                     scale=c(1,1,1,1),
                     labels = c("A", "B", "C", "D"), nrow = 2)


pdf(file = "Output/FIG-2.pdf", width = 10, height = 10)
p_FIG_2
dev.off()



#### EXTENDED DATA FIGURE S3- correlation before and after new normal ######

p_1_4_new <- posterior_Rt_mobility %>% ggplot(aes(x=average_non_residential*-1,y=Rt,group=x,col=data_type)) + #geom_smooth(method = "lm") +
  theme_bw() + facet_wrap(.~data_type,scale="free",ncol=1) + geom_line(alpha=0.05) + xlab("Mobility reduction (% to baseline)") +
  geom_hline(yintercept = 1, col="red", size=1.5, lty=2) +
  geom_text(data    = dat_text,
            mapping = aes(x = 15, y = Inf, label = labels),
            hjust   = -0.1,
            vjust   = 2, show.legend = FALSE) +
  scale_colour_manual(values=c("#810F7C","#BD0026","#993404","black"),
                      breaks=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility")) +
  labs(col="") + theme(legend.position="top",
                       strip.background = element_blank(),
                       strip.text.x = element_blank(),legend.text = element_text(size=6)) +
  guides(color = guide_legend(ncol = 2,override.aes = list(alpha = 1, size = 1.5)))


p_1_4_new_NN <- posterior_Rt_mobility %>% filter(date<date_NN) %>% ggplot(aes(x=average_non_residential*-1,y=Rt,group=x,col=data_type)) + 
  #geom_smooth(method = "lm") +
  theme_bw() + facet_wrap(.~data_type,scale="free",ncol=1) + geom_line(alpha=0.05) + xlab("Mobility reduction (% to baseline)") +
  geom_hline(yintercept = 1, col="red", size=1.5, lty=2) +
  geom_text(data    = dat_text_NN,
            mapping = aes(x = 15, y = Inf, label = labels),
            hjust   = -0.1,
            vjust   = 2, show.legend = FALSE) +
  scale_colour_manual(values=c("#810F7C","#BD0026","#993404","black"),
                      breaks=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility")) +
  labs(col="") + theme(legend.position="top",
                       strip.background = element_blank(),
                       strip.text.x = element_blank(),legend.text = element_text(size=10)) +
  guides(color = guide_legend(ncol = 2,override.aes = list(alpha = 1, size = 1.5)))

# Only data after 'new_normal'
date_NN <- ymd("2020-06-05")
corr_cases_after_NN <- vector()
corr_deaths_after_NN <- vector()
corr_funerals_after_NN <- vector()
for (i in 1:100){
  data_corr_cases <- posterior_Rt_mobility_cases_list[[i]] %>% filter(date>=date_NN)
  data_corr_deaths <- posterior_Rt_mobility_deaths_list[[i]] %>% filter(date>=date_NN)
  data_corr_funerals <- posterior_Rt_mobility_funerals_list[[i]] %>% filter(date>=date_NN)
  corr_cases_after_NN[i] <- cor(data_corr_cases$average_non_residential,data_corr_cases$Rt)
  corr_deaths_after_NN[i] <- cor(data_corr_deaths$average_non_residential,data_corr_deaths$Rt)
  corr_funerals_after_NN[i] <- cor(data_corr_funerals$average_non_residential,data_corr_funerals$Rt)
}

quantile_corr_cases_after_NN <- round(quantile(corr_cases_after_NN,probs = c(0.025,0.5,0.975)),2)
quantile_corr_deaths_after_NN <- round(quantile(corr_deaths_after_NN,probs = c(0.025,0.5,0.975)),2)
quantile_corr_funerals_after_NN <- round(quantile(corr_funerals_after_NN,probs = c(0.025,0.5,0.975)),2)

dat_text_after_NN <- data.frame(
  labels = c(paste0("Corr = ",quantile_corr_cases_after_NN[2],"; 95% CI = (",quantile_corr_cases_after_NN[1],",",quantile_corr_cases_after_NN[3],")"), 
             paste0("Corr = ",quantile_corr_deaths_after_NN[2],"; 95% CI = (",quantile_corr_deaths_after_NN[1],",",quantile_corr_deaths_after_NN[3],")"), 
             paste0("Corr = ",quantile_corr_funerals_after_NN[2],"; 95% CI = (",quantile_corr_funerals_after_NN[1],",",quantile_corr_funerals_after_NN[3],")")),
  data_type   = unique(posterior_Rt_mobility$data_type), x=c(1,1,1)
)

p_1_4_new_after_NN <- posterior_Rt_mobility %>% filter(date>=date_NN) %>% ggplot(aes(x=average_non_residential*-1,y=Rt,group=x,col=data_type)) + 
  theme_bw() + facet_wrap(.~data_type,scale="free",ncol=1) + geom_line(alpha=0.05) + xlab("Mobility reduction (% to baseline)") +
  geom_hline(yintercept = 1, col="red", size=1.5, lty=2) +
  geom_text(data    = dat_text_after_NN,
            mapping = aes(x = 15, y = Inf, label = labels),
            hjust   = -0.1,
            vjust   = 2, show.legend = FALSE) +
  scale_colour_manual(values=c("#810F7C","#BD0026","#993404","black"),
                      breaks=c("Cases - adjusted","Deaths - adjusted","C19P funerals - adjusted","Mobility")) +
  labs(col="") + theme(legend.position="top",
                       strip.background = element_blank(),
                       strip.text.x = element_blank(),legend.text = element_text(size=10)) +
  guides(color = guide_legend(ncol = 2,override.aes = list(alpha = 1, size = 1.5)))

p_corr <- plot_grid(p_1_4_new_NN, p_1_4_new_after_NN, rel_heights = c(1, 1), align = "hv",
                    labels = c("A", "B"), nrow = 1)



pdf(file = "Output/Extended-Data-FIG-3.pdf", width = 10, height = 5)
  p_corr
dev.off()

####################################################
