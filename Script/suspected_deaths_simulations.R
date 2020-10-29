## Prepare data for suspected deaths simulations

province_name <- c("ACEH","NORTH SUMATERA","WEST SUMATERA","RIAU","JAMBI","SOUTH SUMATERA","BENGKULU","LAMPUNG","BANGKA BELITUNG ISLANDS","RIAU ISLANDS",
                   "JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","BALI","WEST NUSA TENGGARA","EAST NUSA TENGGARA",
                   "WEST KALIMANTAN","CENTRAL KALIMANTAN","SOUTH KALIMANTAN","EAST KALIMANTAN","NORTH KALIMANTAN","NORTH SULAWESI","CENTRAL SULAWESI",
                   "SOUTH SULAWESI","SOUTHEAST SULAWESI","GORONTALO","WEST SULAWESI","MALUKU","NORTH MALUKU","WEST PAPUA","PAPUA")

java_provinces <- c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")
java_provinces_not_jkt <- c("WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")

idn_pos_calib <- read.csv("Data/raw/idn_positives.csv",header = TRUE,stringsAsFactors = FALSE)
idn_pos_calib$date <- ymd(idn_pos_calib$date)
idn_pos_calib <- idn_pos_calib[,-ncol(idn_pos_calib)]
idn_deaths_calib <- read.csv("Data/raw/idn_deaths.csv",header = TRUE,stringsAsFactors = FALSE)
idn_deaths_calib$date <- ymd(idn_deaths_calib$date)
idn_deaths_calib <- idn_deaths_calib[,-ncol(idn_deaths_calib)]
idn_funerals_calib <- read.csv("Data/raw/idn_funerals.csv",header = TRUE,stringsAsFactors = FALSE)
idn_funerals_calib$date <- ymd(idn_funerals_calib$date)
confirmed_suspected_deaths_calib <- read.csv("Data/raw/confirmed_suspected_deaths.csv",header = TRUE,stringsAsFactors = FALSE)
confirmed_suspected_deaths_calib <- confirmed_suspected_deaths_calib %>% 
  mutate(prov=toupper(Province),All.deaths=Confirmed.deaths+Suspected.deaths+Probable.deaths,Suspected.Probable.deaths=Suspected.deaths+Probable.deaths)

colnames(idn_pos_calib)[-1] <- province_name
colnames(idn_deaths_calib)[-1] <- province_name
colnames(idn_funerals_calib)[-1] <- province_name

idn_pos_calib <- idn_pos_calib %>% pivot_longer(-date,names_to="prov",values_to="cases")
idn_pos_calib$prov <- factor(idn_pos_calib$prov,levels=province_name)
idn_pos_calib <- idn_pos_calib %>% filter(prov %in% java_provinces)
idn_deaths_calib <- idn_deaths_calib %>% pivot_longer(-date,names_to="prov",values_to="deaths")
idn_deaths_calib$prov <- factor(idn_deaths_calib$prov,levels=province_name)
idn_deaths_calib <- idn_deaths_calib %>% filter(prov %in% java_provinces)
idn_funerals_calib <- idn_funerals_calib %>% pivot_longer(-date,names_to="prov",values_to="deaths")
idn_funerals_calib$prov <- factor(idn_funerals_calib$prov,levels=province_name)
idn_funerals_calib <- idn_funerals_calib %>% filter(prov %in% java_provinces)

# probability of confirmed - this could change based on transmission intensity and reporting rate right??
confirmed_suspected_deaths_calib_prov <- confirmed_suspected_deaths_calib %>% group_by(prov) %>% 
  summarise(confirmed=sum(Confirmed.deaths),suspected=sum(Suspected.Probable.deaths),all=sum(All.deaths)) %>% ungroup() %>% 
  mutate(proba.confirmed=confirmed/all,proba.suspected=suspected/all) %>% 
  filter(prov %in% java_provinces_not_jkt)

idn_deaths_calib_non_jkt <- idn_deaths_calib %>% filter(prov != "JAKARTA")
no_samples_deaths_negbin <- 10
no_samples_deaths_multinom <- 10
idn_deaths_calib_non_jkt$week <- round(lubridate::week(idn_deaths_calib_non_jkt$date))
length_deaths_non_jkt <- length(unique(idn_deaths_calib_non_jkt$week))
week_non_jkt <- unique(idn_deaths_calib_non_jkt$week)
date_non_jkt <- seq(ymd("2020-03-01"),max(idn_deaths_calib$date),by="days")
length_deaths_date_non_jkt <- length(date_non_jkt)
idn_deaths_calib_non_jkt_week <- idn_deaths_calib_non_jkt %>% group_by(prov,week) %>% summarise(deaths=sum(deaths)) %>% ungroup()
idn_deaths_calib_non_jkt_week <- idn_deaths_calib_non_jkt_week %>% mutate(deaths=replace(deaths, deaths == 0, 0.1))

# list_seed_deaths_negbin_non_jkt <- list()
# list_seed_deaths_multinom_non_jkt <- list()
# for (i in 1:5){
#   list_seed_deaths_negbin_non_jkt[[i]] <- round(runif(length_deaths_non_jkt,1,1000000))
#   list_seed_deaths_multinom_non_jkt[[i]] <- round(runif(length_deaths_non_jkt,1,1000000))
# }
# 
# saveRDS(list_seed_deaths_negbin_non_jkt,"Indonesia/Data/list_seed_deaths_negbin_non_jkt_upto_02sep.rds")
# saveRDS(list_seed_deaths_multinom_non_jkt,"Indonesia/Data/list_seed_deaths_multinom_non_jkt_upto_02sep.rds")
list_seed_deaths_negbin_non_jkt <- readRDS("Script/Seeds/list_seed_deaths_negbin_non_jkt_upto_02sep.rds")
list_seed_deaths_multinom_non_jkt <- readRDS("Script/Seeds/list_seed_deaths_multinom_non_jkt_upto_02sep.rds")

suspected_deaths_list <- list()
total_deaths_list <- list()
suspected_deaths_week_list <- list()
total_deaths_week_list <- list()
for (i in 1:5){
  suspected_deaths_negbin_matrix <- matrix(0,nrow=no_samples_deaths_negbin,ncol=length_deaths_non_jkt)
  total_deaths_negbin_matrix <- matrix(0,nrow=no_samples_deaths_negbin,ncol=length_deaths_non_jkt)
  suspected_deaths_matrix <- matrix(0,nrow=no_samples_deaths_negbin*no_samples_deaths_multinom,ncol=length_deaths_date_non_jkt)
  total_deaths_matrix <- matrix(0,nrow=no_samples_deaths_negbin*no_samples_deaths_multinom,ncol=length_deaths_date_non_jkt)
  confirmed_deaths_week_vct <- idn_deaths_calib_non_jkt_week %>% filter(prov==java_provinces_not_jkt[i]) %>% pull(deaths)
  date_vct <- idn_deaths_calib_non_jkt %>% filter(prov==java_provinces_not_jkt[i]) %>% pull(date)
  proba_confirmed <- confirmed_suspected_deaths_calib_prov %>% filter(prov==java_provinces_not_jkt[i]) %>% pull(proba.confirmed)
  week_len <- idn_deaths_calib_non_jkt %>% filter(prov==java_provinces_not_jkt[i]) %>% group_by(week) %>% summarise(len=n()) %>% ungroup() %>% pull(len)
  week_len_loop <- cumsum(c(0,week_len))
  for (j in seq_len(length_deaths_non_jkt)){
    # negative binomial week
    set.seed(list_seed_deaths_negbin_non_jkt[[i]][j])
    suspected_negbin_rand <- rnbinom(n = no_samples_deaths_negbin,size = confirmed_deaths_week_vct[j],prob = proba_confirmed)
    all_negbin_rand <- suspected_negbin_rand + round(confirmed_deaths_week_vct[j]) # rounding down the 0.1
    suspected_deaths_negbin_matrix[,j] <- suspected_negbin_rand
    total_deaths_negbin_matrix[,j] <- all_negbin_rand
    
    # multinomial spread daily
    confirmed_deaths_vct <- idn_deaths_calib_non_jkt %>% filter(prov==java_provinces_not_jkt[i],week==week_non_jkt[j]) %>% pull(deaths)
    set.seed(list_seed_deaths_multinom_non_jkt[[i]][j])
    suspected_multinom_rand <- matrix(t(bind_cols(lapply(suspected_negbin_rand,rmultinom,n=no_samples_deaths_multinom,prob=rep(1,week_len[j])/week_len[j]))),
                                      ncol=week_len[j])
    all_multinom_rand <- sweep(suspected_multinom_rand, 2, confirmed_deaths_vct, "+")
    suspected_deaths_matrix[,seq(week_len_loop[j]+1,week_len_loop[j+1])] <- suspected_multinom_rand
    total_deaths_matrix[,seq(week_len_loop[j]+1,week_len_loop[j+1])] <- all_multinom_rand
  }
  suspected_deaths_df <- as_tibble(suspected_deaths_matrix)
  colnames(suspected_deaths_df) <- date_non_jkt
  suspected_deaths_df$sample <- seq_len(no_samples_deaths_negbin*no_samples_deaths_multinom)
  suspected_deaths_df$prov <- java_provinces_not_jkt[i]
  suspected_deaths_df <- suspected_deaths_df %>% pivot_longer(-c(prov,sample),names_to="date",values_to="suspected_deaths") %>% mutate(date=ymd(date))
  
  total_deaths_df <- as_tibble(total_deaths_matrix)
  colnames(total_deaths_df) <- date_non_jkt
  total_deaths_df$sample <- seq_len(no_samples_deaths_negbin*no_samples_deaths_multinom)
  total_deaths_df$prov <- java_provinces_not_jkt[i]
  total_deaths_df <- total_deaths_df %>% pivot_longer(-c(prov,sample),names_to="date",values_to="total_deaths") %>% mutate(date=ymd(date))
  
  suspected_deaths_week_df <- as_tibble(suspected_deaths_negbin_matrix)
  colnames(suspected_deaths_week_df) <- week_non_jkt
  suspected_deaths_week_df$sample <- seq_len(no_samples_deaths_negbin)
  suspected_deaths_week_df$prov <- java_provinces_not_jkt[i]
  suspected_deaths_week_df <- suspected_deaths_week_df %>% pivot_longer(-c(prov,sample),names_to="week",values_to="suspected_deaths")
  
  total_deaths_week_df <- as_tibble(total_deaths_negbin_matrix)
  colnames(total_deaths_week_df) <- week_non_jkt
  total_deaths_week_df$sample <- seq_len(no_samples_deaths_negbin)
  total_deaths_week_df$prov <- java_provinces_not_jkt[i]
  total_deaths_week_df <- total_deaths_week_df %>% pivot_longer(-c(prov,sample),names_to="week",values_to="total_deaths")
  
  suspected_deaths_list[[i]] <- suspected_deaths_df
  total_deaths_list[[i]] <- total_deaths_df
  suspected_deaths_week_list[[i]] <- suspected_deaths_week_df
  total_deaths_week_list[[i]] <- total_deaths_week_df
}

total_deaths_all_non_jkt <- bind_rows(total_deaths_list)
suspected_deaths_all_non_jkt <- bind_rows(suspected_deaths_list)

suspected_deaths_all_non_jkt_median <- suspected_deaths_all_non_jkt %>% group_by(prov,date) %>% summarise(suspected_deaths=median(suspected_deaths,na.rm = TRUE))
all_deaths_non_jkt <- suspected_deaths_all_non_jkt_median %>% left_join(idn_deaths_calib_non_jkt) %>%
  mutate(total_deaths=round(deaths+suspected_deaths))
all_deaths_non_jkt <- all_deaths_non_jkt %>% select(prov,date,total_deaths)

# add jakarta
total_deaths_jkt_single <- idn_funerals_calib %>% filter(prov=="JAKARTA") %>% select(prov,date,total_deaths=deaths)
total_deaths_jkt <- list()
for (i in 1:100){
  total_deaths_jkt[[i]] <- total_deaths_jkt_single %>% mutate(sample=i)
}
total_deaths_jkt <- bind_rows(total_deaths_jkt)
total_deaths_jkt <- total_deaths_jkt %>% select(sample,prov,date,total_deaths)

total_deaths_all <- bind_rows(total_deaths_jkt,total_deaths_all_non_jkt)

all_deaths_java <- bind_rows(total_deaths_jkt_single,all_deaths_non_jkt) %>% select(prov,date,deaths=total_deaths)

# saveRDS(idn_deaths_calib,"processed_inputs/reported_deaths_java.rds")
# saveRDS(all_deaths_java,"processed_inputs/simulated_all_deaths_java.rds")