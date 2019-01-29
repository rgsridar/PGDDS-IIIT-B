
#Setting the directory
setwd("C:/Users/ramachga/Desktop/UpGrad/Investment Case Study_1")

# Load packages 

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


## Checkpoint 1: Data Cleaning 1

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F)

companies <- read.delim("companies.txt", stringsAsFactors = F)


rounds2$company_permalink <- tolower(rounds2$company_permalink)

no_of_com_permalinks_rounds <- unique(rounds2$company_permalink)
length(no_of_com_permalinks_rounds)# 66368

companies$permalink <- tolower(companies$permalink)
no_of_company_permalinks <- unique(companies$permalink)
length(no_of_company_permalinks)

permalinks_match <- subset(companies, 
                           companies$permalink %in% rounds2$company_permalink)


colnames(rounds2)[1] <- "permalink"

master_frame <- merge(x = companies, y = rounds2, by = "permalink")



sum(is.na(master_frame$raised_amount_usd))
sum(is.na(master_frame$raised_amount_usd))/nrow(master_frame)


# Checkpoint -2 Funding type analysis

master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)==T] <- 0

na_indices <- which(is.na(master_frame$raised_amount_usd == T))


master_frame$raised_amount_usd[na_indices] <- 0
sum(is.na(master_frame$raised_amount_usd))






summary(factor(master_frame$funding_round_type))


venture <- subset(master_frame, funding_round_type=="venture")
mean(venture$raised_amount_usd)


angel <- subset(master_frame,funding_round_type=="angel")
mean(angel$raised_amount_usd)


seed <- subset(master_frame,funding_round_type=="seed")
mean(seed$raised_amount_usd)


private_equity <- subset(master_frame,funding_round_type=="private_equity")
mean(private_equity$raised_amount_usd)

fund_types <- filter(master_frame, 
                     funding_round_type == "venture" |
                       funding_round_type == "angel" |
                       funding_round_type == "seed" |
                       funding_round_type == "private_equity")

fund_types %>% 
  group_by(funding_round_type) %>% 
  summarise(avg_fund_by_type = mean(raised_amount_usd)) %>%
  arrange(desc(avg_fund_by_type))



## Checkpoint 3: Country Analysis


summary(factor(master_frame$country_code))


length(which(master_frame$country_code == ""))
length(which(master_frame$country_code == ""))/nrow(master_frame)


master_frame$country_code[which(master_frame$country_code=="")]="missing"



highest_funding_countries <- aggregate(raised_amount_usd~country_code, 
                                       venture, sum)

highest_funding_countries_sorted <- highest_funding_countries[order(highest_funding_countries$raised_amount_usd,
                                                                    decreasing = T), ]




top9 <- head(highest_funding_countries_sorted, 9)


### Checkpoint 4: Sector Analysis1


master_frame$category_list <- gsub("\\|.*", "", master_frame$category_list)


mapping_file <- read.csv("mapping.csv", header=T, 
                         stringsAsFactors = F,check.names = F)

View(mapping_file) 

# lowercase change
mapping_file$category_list <- tolower(mapping_file$category_list)

 
master_frame$category_list <- tolower(master_frame$category_list)


mapping_file$zeros <- str_detect(mapping_file$category_list, "0")
sum(mapping_file$zeros)


mapping_file$zeros <- str_count(mapping_file$category_list, "0")
sum(mapping_file$zeros)


mapping_file$category_list <- str_replace(mapping_file$category_list, "[0]", "na")
mapping_file$zeros <- str_count(mapping_file$category_list, "0")
sum(mapping_file$zeros)

mapping_file$category_list <- str_replace_all(mapping_file$category_list, 
                                              "[0]", "na")
mapping_file$zeros <- str_count(mapping_file$category_list, "0")
sum(mapping_file$zeros)

mapping_file$category_list[which(mapping_file$category_list=="enterprise 2.na")] <- "enterprise 2.0"

mapping_file <- gather(mapping_file,main_sector,my_val,-category_list)
mapping_file <- mapping_file[which(mapping_file$my_val==1),]
mapping_file$my_val <- NULL
mapping_file <- mapping_file[order(mapping_file$category_list),]

master_frame$check = master_frame$category_list %in% mapping_file$category_list

sum(!master_frame$check)

sector_analysis_merged <- merge(x = master_frame, y = mapping_file,
                                by = "category_list", all.x = T)

 
Nas <- subset(sector_analysis_merged,is.na(sector_analysis_merged$main_sector))


sector_analysis_merged$main_sector[which(is.na(sector_analysis_merged$main_sector))] <- "Blanks"


##Checkpoint 5: Sector Analysis 2


USA <- subset(sector_analysis_merged , country_code == "USA" 
              & funding_round_type == "venture" &
                raised_amount_usd >=5e+06 & raised_amount_usd <=15e+06 )

USA_summary <- 
  USA %>% group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>%
  arrange(desc(frequency))




USA_1 <- sector_analysis_merged %>% filter(country_code == "USA", 
                                           funding_round_type == "venture", 
                                           raised_amount_usd >= 5e+06, 
                                           raised_amount_usd <= 15e+06) %>% group_by(main_sector) %>% summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))



USA_summary <- USA_summary[order(USA_summary$frequency, decreasing = T), ]

sum(USA_summary$investment_by_sector)



USA__company_summary_1 <- 
  USA %>% 
  filter(main_sector=="Social, Finance, Analytics, Advertising")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>%
  arrange(desc(investment_by_company))



USA__company_summary_2 <- 
  USA %>% 
  filter(main_sector=="Cleantech / Semiconductors")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))



GBR <- subset(sector_analysis_merged , country_code == "GBR" 
              & funding_round_type == "venture" &
                raised_amount_usd >= 5e+06 & raised_amount_usd <= 15e+06 )


GBR_summary <- GBR %>% 
  group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>% 
  arrange(desc(frequency))


GBR_1 <- sector_analysis_merged %>% 
  filter(country_code == "GBR", funding_round_type == "venture", 
         raised_amount_usd >= 5e+06, 
         raised_amount_usd <= 15e+06) %>% group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>% 
  arrange(desc(frequency))



sum(GBR_summary$investment_by_sector)


GBR__company_summary_1 <- 
  GBR %>% 
  filter(main_sector=="Social, Finance, Analytics, Advertising")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))




GBR__company_summary_2 <- 
  GBR %>% 
  filter(main_sector=="Cleantech / Semiconductors")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))




IND <- subset(sector_analysis_merged , country_code == "IND" 
              & funding_round_type == "venture" &
                raised_amount_usd >= 5e+06 & raised_amount_usd <= 15e+06 )


IND_summary <- IND %>%
  group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>%
  arrange(desc(frequency))


IND_1 <- sector_analysis_merged %>%
  filter(country_code == "IND", funding_round_type == "venture", raised_amount_usd >= 5e+06, raised_amount_usd <= 15e+06) %>%
  group_by(main_sector) %>% 
  summarise(frequency=n(),investment_by_sector=sum(raised_amount_usd))%>%
  arrange(desc(frequency))



IND_summary <- IND_summary[order(IND_summary$frequency, decreasing = T), ]


sum(IND_summary$investment_by_sector)


# Highest  Investment in top sector.


IND__company_summary_1 <- 
  IND %>% 
  filter(main_sector=="Social, Finance, Analytics, Advertising")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))


# Highest investment in 2nd top sector check

IND__company_summary_2 <- 
  IND %>% 
  filter(main_sector=="News, Search and Messaging")%>% 
  group_by(name) %>% 
  summarise(frequency=n(),investment_by_company=sum(raised_amount_usd))%>% 
  arrange(desc(investment_by_company))



