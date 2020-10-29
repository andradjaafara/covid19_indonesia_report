
apple_mobility <- read.csv("Data/to_generate_proccessed_inputs/mobility/applemobilitytrends-2020-09-02_ID.csv",header = TRUE,stringsAsFactors = FALSE)
google_mobility <- read.csv("Data/to_generate_proccessed_inputs/mobility/Global_Mobility_Report_2020_09_01_ID.csv",header = TRUE,stringsAsFactors = FALSE)
apple_admin_ref <- read.csv("Data/to_generate_proccessed_inputs/mobility/apple_admin_ref.csv",header = TRUE,stringsAsFactors = FALSE)
google_admin_ref <- read.csv("Data/to_generate_proccessed_inputs/mobility/google_admin_ref.csv",header = TRUE,stringsAsFactors = FALSE)

apple_mobility_ID <- apple_mobility %>% filter(region %in% c("Indonesia","Jakarta")) %>% mutate(COUNTRY_ID="ID",COUNTRY="INDONESIA") %>% 
  select(COUNTRY_ID,COUNTRY,region,transportation_type,everything(),-alternative_name,-geo_type,-sub.region,-country) %>% 
  pivot_longer(-c(COUNTRY_ID,COUNTRY,region,transportation_type),names_to = "date",values_to = "changes") %>% 
  mutate(date=ymd(gsub("X","",(gsub(".","/",date,fixed = TRUE))))) %>% 
  pivot_wider(names_from = transportation_type,values_from = changes) %>% left_join(apple_admin_ref) %>% ungroup() %>% 
  select(COUNTRY_ID,COUNTRY,ADMIN1,everything(),-region) %>% mutate(driving=(driving-100),walking=(walking-100))

google_mobility_ID <- google_mobility %>% filter(country_region=="Indonesia") %>% mutate(COUNTRY="INDONESIA") %>% 
  select(COUNTRY_ID=country_region_code,COUNTRY,everything(),-sub_region_2,-country_region) %>% mutate(date=ymd(date)) %>% 
  left_join(google_admin_ref) %>% ungroup() %>% select(COUNTRY_ID,COUNTRY,ADMIN1,everything(),-sub_region_1)

apple_mobility_ID_national <- apple_mobility_ID %>% filter(ADMIN1=="NATIONAL")
apple_mobility_ID_prov <- apple_mobility_ID %>% filter(ADMIN1!="NATIONAL")
google_mobility_ID_national <- google_mobility_ID %>% filter(ADMIN1=="NATIONAL") %>% select(-metro_area)
google_mobility_ID_prov <- google_mobility_ID %>% filter(ADMIN1!="NATIONAL") %>% select(-metro_area)

mobility_ID <- list(google=list(national=as_tibble(google_mobility_ID_national %>% select(-iso_3166_2_code,-census_fips_code)),
                                province=as_tibble(google_mobility_ID_prov %>% select(-iso_3166_2_code,-census_fips_code))),
                    apple=list(national=apple_mobility_ID_national,province=apple_mobility_ID_prov))

# saveRDS(mobility_ID,"processed_inputs/mobility_summary_ID.rds")

# Create long version of google mobility by province

province_name <- c("ACEH","NORTH SUMATERA","WEST SUMATERA","RIAU","JAMBI","SOUTH SUMATERA","BENGKULU","LAMPUNG","BANGKA BELITUNG ISLANDS","RIAU ISLANDS",
                   "JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","BALI","WEST NUSA TENGGARA","EAST NUSA TENGGARA",
                   "WEST KALIMANTAN","CENTRAL KALIMANTAN","SOUTH KALIMANTAN","EAST KALIMANTAN","NORTH KALIMANTAN","NORTH SULAWESI","CENTRAL SULAWESI",
                   "SOUTH SULAWESI","SOUTHEAST SULAWESI","GORONTALO","WEST SULAWESI","MALUKU","NORTH MALUKU","WEST PAPUA","PAPUA")

province_name_national <- c("NATIONAL","ACEH","NORTH SUMATERA","WEST SUMATERA",
                            "RIAU","JAMBI","SOUTH SUMATERA","BENGKULU","LAMPUNG","BANGKA BELITUNG ISLANDS","RIAU ISLANDS",
                            "JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","BALI","WEST NUSA TENGGARA","EAST NUSA TENGGARA",
                            "WEST KALIMANTAN","CENTRAL KALIMANTAN","SOUTH KALIMANTAN","EAST KALIMANTAN","NORTH KALIMANTAN","NORTH SULAWESI","CENTRAL SULAWESI",
                            "SOUTH SULAWESI","SOUTHEAST SULAWESI","GORONTALO","WEST SULAWESI","MALUKU","NORTH MALUKU","WEST PAPUA","PAPUA")

## GOOGLE
google_mobility_prov <- mobility_ID$google$province
google_mobility_nat <- mobility_ID$google$national
google_mobility_all <- bind_rows(google_mobility_nat,google_mobility_prov)

google_mobility_prov <- google_mobility_prov %>% select(prov=ADMIN1,everything()) %>%
  mutate(prov=factor(prov,levels=province_name),date=ymd(date))
google_mobility_all <- google_mobility_all %>% select(prov=ADMIN1,everything()) %>%
  mutate(prov=factor(prov,levels=province_name_national),date=ymd(date))

google_mobility_all_long <- google_mobility_all %>% pivot_longer(-c(prov,COUNTRY_ID,COUNTRY,date),names_to = "mobility_type",values_to = "pct_change")
google_mobility_prov_long <- google_mobility_prov %>% pivot_longer(-c(prov,COUNTRY_ID,COUNTRY,date),names_to = "mobility_type",values_to = "pct_change")

min(google_mobility_prov$date)
max(google_mobility_prov$date)

period.1.len <- length(seq(ymd(min(google_mobility_prov$date)),ymd("2020-03-03"),by="days"))
period.2.len <- length(seq(ymd("2020-03-04"),ymd("2020-03-15"),by="days"))
period.3.len <- length(seq(ymd("2020-03-16"),ymd("2020-04-09"),by="days"))
period.4.len <- length(seq(ymd("2020-04-10"),ymd("2020-04-23"),by="days"))
period.5.len <- length(seq(ymd("2020-04-24"),ymd(max(google_mobility_prov$date)),by="days"))

period <- c(rep("Period 1",period.1.len),rep("Period 2",period.2.len),rep("Period 3",period.3.len),rep("Period 4",period.4.len),rep("Period 5",period.5.len))
period_mobility_linelist <- rep(period,35)

province_mobility_linelist <- vector()
for (i in 1:35){
  prov_mobility <- rep(province_name_national[i],length(period))
  province_mobility_linelist <- c(province_mobility_linelist,prov_mobility)
}

google_mobility_all$period <- period_mobility_linelist
google_mobility_prov$period <- period_mobility_linelist[1:(length(period)*34)]