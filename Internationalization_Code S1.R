install.packages(c("dplyr", "tidyr", "splitstackshape", "writexl", "janitor","tidyverse","networkD3","manipulateWidget","caTools","randomForest","caret", "CatEncoders","ggplot2","MLmetrics"))
library(readxl)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(writexl)
library(janitor)
library(tidyverse)
library(networkD3)
library(htmlwidgets)
library(htmltools)
library(manipulateWidget)
library(caTools)
library(randomForest)
library(caret)
library(CatEncoders)
library(ggplot2)
library(MLmetrics)
install.packages('e1071', dependencies=TRUE)

## Define your directory file path ##
directory <- "Add directory path here" 
## Define your directory file path ##


#Read data
af.raw <- read_xlsx(file.path(directory,"assetfinance.xlsx"),guess_max = 94618) 
af.raw$total_equity_m <- ifelse(af.raw$total_equity_m == 296325, af.raw$total_equity_m/1000, af.raw$total_equity_m) #Manually adjust incorrect entry
regions <- read_xlsx(file.path(directory,"countries_regions_match_IRENA.xlsx"))
o <- read_xlsx(file.path(directory,"organizations_classified.xlsx")) 
cpi <- read_xlsx(file.path(directory,"US_CPI_annual.xlsx")) 

#Filtering and technology preparation ####
#Add regions to af data
af.region <- left_join(x=af.raw, y=regions, by=c('market'='market'))

#Remove deals that are not yet closed/were abandoned
af <- filter(af.region, !grepl("Abandoned|Postponed/ cancelled|In active planning",deal_status))

#Create column that indicates pilot projects
af$pilot <- as.numeric(grepl('pilot|Pilot',  do.call(paste0, af)))

#Filter dataset according to sector scope
af <- filter(af, grepl("Solar|Wind|Biomass & Waste|Small Hydro|Marine", sector))
af <- filter(af, !grepl("biomethane", subsector))

#Identify PV and onshore/offshore wind in cases, where no subsector was reported in the raw data
af$na_subsector <- as.numeric(is.na(af$subsector))
af.solar <- filter(af, sector =="Solar" & na_subsector == 1)
af.solar$pv <- as.numeric(grepl('PV',  do.call(paste0, af.solar))) #search for "PV" in the text
af.solar$thermal <- as.numeric(grepl('Thermal Plant',  do.call(paste0, af.solar)))  #search for "Thermal" in the text
af.solar$subsector <- ifelse(af.solar$pv == 1 & af.solar$thermal == 1 ,"Solar undefined",
                                  ifelse(af.solar$pv == 1,"PV",
                                         ifelse(af.solar$thermal == 1,"Thermal","Solar undefined")))
af.solar <- subset(af.solar, select = -c(pv,thermal))
af.wind <- filter(af, sector =="Wind" & na_subsector == 1)
af.wind$onshore <- as.numeric(grepl('Onshore|onshore',  do.call(paste0, af.wind)))  #search for "onshore" in the text
af.wind$offshore <- as.numeric(grepl('Offshore|offshore',  do.call(paste0, af.wind)))  #search for "offshore" in the text
af.wind$subsector <- ifelse(af.wind$onshore == 1 & af.wind$offshore == 1 ,"Wind undefined",
                               ifelse(af.wind$onshore == 1,"Onshore",
                                      ifelse(af.wind$offshore == 1,"Offshore","Wind undefined")))
af.wind <- subset(af.wind, select = -c(onshore,offshore))
af.known <- filter(af, na_subsector == 0)
af.nas <- filter(af, na_subsector == 1)
af.nas.f <- filter(af.nas, !grepl("Wind|Solar",sector))
af.nas.f$subsector <- af.nas.f$sector
af.nas.edit <- rbind(af.nas.f,af.solar,af.wind)
af.2 <- rbind(af.nas.edit,af.known)

#Define and summarize technology types according to our classification
af.2$technology_detail <- ifelse(af.2$subsector == "PV","Solar PV",
                                 ifelse(af.2$subsector == "Thermal","Solar Thermal",
                                        ifelse(af.2$subsector == "Offshore","Wind Offshore",
                                               ifelse(af.2$subsector == "Onshore","Wind Onshore", af.2$sector))))

af.2$technology <- ifelse(af.2$technology_detail == "Solar PV",af.2$technology_detail,
                          ifelse(af.2$technology_detail == "Wind Offshore",af.2$technology_detail,
                                 ifelse(af.2$technology_detail == "Wind Onshore", af.2$technology_detail, 
                                        ifelse(af.2$technology_detail == "Biomass & Waste", af.2$technology_detail,"Other"))))

#Date of close imputation ####
#Determine date of close for missing entries by using average days between announcement and close by technology for deals where both are known and apply to deals with missing data
af.2$date_of_close <- as.Date(af.2$date_of_close) 
af.2$date_of_announcement <- as.Date(af.2$date_of_announcement) 
af.2$year_close <- as.numeric(format(af.2$date_of_close, '%Y'))
af.2$year_announcement <- as.numeric(format(af.2$date_of_announcement, '%Y'))

af.2$date_of_close_flag <- as.numeric(is.na(af.2$date_of_close)) #flag for missing date of close
af.2$date_of_announcement_flag <- as.numeric(is.na(af.2$date_of_announcement)) #flag for missing date of announcement

af.2.date.sample <- filter(af.2, date_of_close_flag == 0 & date_of_announcement_flag == 0) #extract rows where both date of close and announcement is existing
af.2.date.sample$announcemenet_to_close <- as.numeric(af.2.date.sample$date_of_close - af.2.date.sample$date_of_announcement) #define new row with time lag between announcement and close dates
af.2.date.sample <- filter(af.2.date.sample, announcemenet_to_close > 0 & announcemenet_to_close < 7000 & year_announcement < 2023) #exclude incorrect datapoints (negative, 0, and double digit thousands)

tech.means <- aggregate(af.2.date.sample$announcemenet_to_close, by=list(af.2.date.sample$technology_detail), FUN=mean) #calculate mean per technology

colnames(tech.means)[1]='technology_detail'
colnames(tech.means)[2]='lag_days'

#Output for methods description
#date.solar <- filter(af.2.date.sample, technology_detail == "Solar PV")
#summary(date.solar$announcemenet_to_close)
#tech.sd <- aggregate(af.2.date.sample$announcemenet_to_close, by=list(af.2.date.sample$technology_detail), FUN=sd) #calculate mean per technology

af.3 <- left_join(x=af.2, y=tech.means, by=c('technology_detail'='technology_detail'))

date.exist <- filter(af.3, date_of_close_flag == 0)
date.exist$date <- date.exist$date_of_close

date.missing <- filter(af.3, date_of_close_flag == 1)
date.missing$date <- date.missing$date_of_announcement + date.missing$lag_days

af.3 <- rbind(date.exist, date.missing)
af.3$year <- as.numeric(format(af.3$date, '%Y'))


#Filter dataset according to year scope
af.3 <- filter(af.3, year < 2023)
af.3 <- filter(af.3, year > 2003)

#Do inflation adjustment
af.4 <- left_join(x=af.3, y=cpi, by=c('year'='year'))
af.4$transaction_value_infl <- af.4$disclosed_transaction_value_m/af.4$cpi*100
af.4$equity_value_infl <- af.4$total_equity_m/af.4$cpi*100
af.4$debt_value_infl <- af.4$total_debt_m/af.4$cpi*100

#Random Forest - Value imputation ####
#Calibrate random forest model
rf.usable <- subset(af.4, select = c(transaction_value_infl,
                                          market,
                                          transaction_type,
                                          financing_type,
                                          capacity_total_mw,
                                          technology_detail,
                                          deal_specifics,
                                          year,
                                          pilot,
                                          deal_status))

lab.market <- LabelEncoder.fit(rf.usable$market)
lab.tt <- LabelEncoder.fit(rf.usable$transaction_type)
lab.ft <- LabelEncoder.fit(rf.usable$financing_type)
lab.subs <- LabelEncoder.fit(rf.usable$technology_detail)
lab.dsp <- LabelEncoder.fit(rf.usable$deal_specifics)
lab.dst <- LabelEncoder.fit(rf.usable$deal_status)

rf.usable$market = transform(lab.market, rf.usable$market)
rf.usable$transaction_type = transform(lab.tt, rf.usable$transaction_type)
rf.usable$financing_type = transform(lab.ft, rf.usable$financing_type)
rf.usable$technology_detail = transform(lab.subs, rf.usable$technology_detail)
rf.usable$deal_specifics = transform(lab.dsp, rf.usable$deal_specifics)
rf.usable$deal_status = transform(lab.dst, rf.usable$deal_status)

rf.input <- na.omit(rf.usable)

set.seed(13)
rf = randomForest(transaction_value_infl ~ market + transaction_type + financing_type + capacity_total_mw + technology_detail + deal_specifics + year + pilot + deal_status, data = rf.input)

#Predict value/distribution from model
pred <- predict(rf, newdata = rf.usable,  predict.all = TRUE)
pred.distr <- data.frame(pred$individual)
#histogram(pred_distr[1,])

pred.distr$st_dev <- apply(pred.distr,1, sd, na.rm=FALSE)
estimates <- data.frame(pred$aggregate)
colnames(estimates)[1]="rf_estimate"
af.4$rf_estimate <- estimates$rf_estimate
af.4$rf_std <- pred.distr$st_dev
af.5 <- af.4

#Flag data according to existing/estimated value and equity/debt info availability
af.5$value_flag <- as.numeric(is.na(af.5$transaction_value_infl))
af.5$estimate_value_flag <- as.numeric(is.na(af.5$rf_estimate))
af.5$eq_flag <- as.numeric(is.na(af.5$equity_value_infl))
af.5$db_flag <- as.numeric(is.na(af.5$debt_value_infl))

#Debt share imputation ####
#Write data for debt imputation model tests
debt.test <- filter(af.5, eq_flag == 0 & db_flag == 0)
write_xlsx(debt.test, "T:/Documents/01_Projects/01_FINDIT/03_Working folder/02_Data analysis/02_Processed data/02_Outputs/data_for_imputation_model_testing_ds.xlsx")

#Identify data, where debt share is given

#Calibrate random forest model
af.5$debt_share <- af.5$debt_value_infl/(af.5$debt_value_infl + af.5$equity_value_infl)
rf2.usable <- subset(af.5, select = c(debt_share,
                                      market,
                                      transaction_type,
                                      financing_type,
                                      capacity_total_mw,
                                      technology_detail,
                                      deal_specifics,
                                      year,
                                      pilot,
                                      deal_status))

lab.market <- LabelEncoder.fit(rf2.usable$market)
lab.tt <- LabelEncoder.fit(rf2.usable$transaction_type)
lab.ft <- LabelEncoder.fit(rf2.usable$financing_type)
lab.subs <- LabelEncoder.fit(rf2.usable$technology_detail)
lab.dsp <- LabelEncoder.fit(rf2.usable$deal_specifics)
lab.dst <- LabelEncoder.fit(rf2.usable$deal_status)

rf2.usable$market = transform(lab.market, rf2.usable$market)
rf2.usable$transaction_type = transform(lab.tt, rf2.usable$transaction_type)
rf2.usable$financing_type = transform(lab.ft, rf2.usable$financing_type)
rf2.usable$technology_detail = transform(lab.subs, rf2.usable$technology_detail)
rf2.usable$deal_specifics = transform(lab.dsp, rf2.usable$deal_specifics)
rf2.usable$deal_status = transform(lab.dst, rf2.usable$deal_status)


rf2.input <- na.omit(rf2.usable)

set.seed(13)
rf2 = randomForest(debt_share ~ market + transaction_type + financing_type + capacity_total_mw + technology_detail + deal_specifics + year + pilot + deal_status, data = rf2.input)

#Predict value/distribution from model
pred2 <- predict(rf2, newdata = rf2.usable,  predict.all = TRUE)
pred2.distr <- data.frame(pred2$individual)
#histogram(pred_distr[1,])

pred2.distr$st_dev <- apply(pred2.distr,1, sd, na.rm=FALSE)
estimates2 <- data.frame(pred2$aggregate)
colnames(estimates2)[1]="ds_estimate"
af.5$ds_estimate <- estimates2$ds_estimate
af.5$ds_std <- pred2.distr$st_dev

#Based on availability, assign either given value or imputed value to data (both for value and debt)####
#Splitting up dataset according to value info availability
tv <- filter(af.5, value_flag == 0)
etv <- filter(af.5, value_flag == 1 & estimate_value_flag == 0)
nv <- filter(af.5, value_flag == 1 & estimate_value_flag == 1)

##tv dataset (total value is known)
#Splitting dataset according to D/E info availability
tv.1 <- filter(tv, eq_flag == 0 & db_flag == 0)
tv.2 <- filter(tv, eq_flag == 0 & db_flag == 1)
tv.3 <- filter(tv, eq_flag == 1 & db_flag == 0)
tv.4 <- filter(tv, eq_flag == 1 & db_flag == 1)

#identify tv.1 matches and fill/calculate debt & equity info
tv.1$value_match <- round(tv.1$equity_value_infl+tv.1$debt_value_infl - tv.1$transaction_value_infl,4)
tv.1.1 <- filter(tv.1, value_match == 0)

tv.1.1$debt_share <- tv.1.1$debt_value_infl/tv.1.1$transaction_value_infl
tv.1.1$debt_share_std <- 0
tv.1.1$investment_value <- tv.1.1$transaction_value_infl
tv.1.1$investment_value_std <- 0
tv.1.1 <- subset(tv.1.1, select = -c(value_match))

tv.1.2 <- filter(tv.1, value_match != 0)
tv.1.2$debt_correct <- as.numeric(grepl('in debt for|debt financing for|in bond for|indebt for|debt funding for|in debt finance for|in debt from|in construction debt|) debt for|) debt financing for|m in debt|in long term debt|) loan from|m loan for|as debt for',  do.call(paste0, tv.1.2)))

tv.1.2.1 <- filter(tv.1.2, debt_correct == 0)
tv.1.2.1$debt_share <- tv.1.2.1$debt_value_infl/(tv.1.2.1$debt_value_infl+tv.1.2.1$equity_value_infl)
tv.1.2.1$debt_share_std <- 0
tv.1.2.1$investment_value <- tv.1.2.1$transaction_value_infl
tv.1.2.1$investment_value_std <- 0
tv.1.2.1 <- subset(tv.1.2.1, select = -c(value_match, debt_correct))

tv.1.2.2 <- filter(tv.1.2, debt_correct == 1)
tv.1.2.2$debt_share <- tv.1.2.2$debt_value_infl/tv.1.2.2$transaction_value_infl
tv.1.2.2$debt_share_std <- 0
tv.1.2.2$investment_value <- tv.1.2.2$transaction_value_infl
tv.1.2.2$investment_value_std <- 0
tv.1.2.2 <- subset(tv.1.2.2, select = -c(value_match, debt_correct))

#identify tv.2 matches and fill/calculate debt & equity info
tv.2$debt_share <- ifelse(1-(tv.2$equity_value_infl/tv.2$transaction_value_infl)>=0,1-(tv.2$equity_value_infl/tv.2$transaction_value_infl),0)
tv.2$debt_share_std <- 0
tv.2$investment_value <- tv.2$transaction_value_infl
tv.2$investment_value_std <- 0

#identify tv.3 matches and fill/calculate debt & equity info
tv.3$debt_share <- ifelse(tv.3$debt_value_infl/tv.3$transaction_value_infl >= 0,tv.3$debt_value_infl/tv.3$transaction_value_infl,0)
tv.3$debt_share_std <- 0
tv.3$investment_value <- tv.3$transaction_value_infl
tv.3$investment_value_std <- 0

#identify tv.4 matches and fill/calculate debt & equity info
tv.4$debt_share <- tv.4$ds_estimate
tv.4$debt_share_std <- tv.4$ds_std
tv.4$investment_value <- tv.4$transaction_value_infl
tv.4$investment_value_std <- 0
tv.4 <- tv.4 %>% drop_na(debt_share)

##etv dataset (total value not nown, however value was imputed)
#Splitting dataset according to D/E info availability
tv.5 <- filter(etv, eq_flag == 0 & db_flag == 0)
tv.6 <- filter(etv, eq_flag == 0 & db_flag == 1)
tv.7 <- filter(etv, eq_flag == 1 & db_flag == 0)
tv.8 <- filter(etv, eq_flag == 1 & db_flag == 1)

#identify tv.5 matches and fill/calculate debt & equity info
tv.5$debt_share <- tv.5$debt_value_infl/(tv.5$equity_value_infl+tv.5$debt_value_infl)
tv.5$debt_share_std <- 0
tv.5$investment_value <- tv.5$equity_value_infl+tv.5$debt_value_infl
tv.5$investment_value_std <- 0

#identify tv.6 matches and fill/calculate debt & equity info
tv.6$debt_share <- 0
tv.6$debt_share_std <- 0
tv.6$investment_value <- tv.6$equity_value_infl
tv.6$investment_value_std <- 0

#identify tv.7 matches and fill/calculate debt & equity info
tv.7$debt_share <- 1
tv.7$debt_share_std <- 0
tv.7$investment_value <- tv.7$debt_value_infl
tv.7$investment_value_std <- 0

#identify tv.8 matches and fill/calculate debt & equity info
tv.8$debt_share <- tv.8$ds_estimate
tv.8$debt_share_std <- tv.8$ds_std
tv.8$investment_value <- tv.8$rf_estimate
tv.8$investment_value_std <- tv.8$rf_std
tv.8 <- tv.8 %>% drop_na(debt_share)

##nv dataset (total value not know, no imputation possible)
#Splitting dataset according to D/E info availability
tv.9 <- filter(nv, eq_flag == 0 & db_flag == 0)
tv.10 <- filter(nv, eq_flag == 0 & db_flag == 1)
tv.11 <- filter(nv, eq_flag == 1 & db_flag == 0)
tv.12 <- filter(nv, eq_flag == 1 & db_flag == 1)

#identify tv.9 matches and fill/calculate debt & equity info
tv.9$debt_share <- tv.9$debt_value_infl/(tv.9$equity_value_infl+tv.9$debt_value_infl)
tv.9$debt_share_std <- 0
tv.9$investment_value <- tv.9$equity_value_infl+tv.9$debt_value_infl
tv.9$investment_value_std <- 0

#identify tv.10 matches and fill/calculate debt & equity info
tv.10$debt_share <- 0
tv.10$debt_share_std <- 0
tv.10$investment_value <- tv.10$equity_value_infl
tv.10$investment_value_std <- 0

#identify tv.11 matches and fill/calculate debt & equity info
tv.11$debt_share <- 1
tv.11$debt_share_std <- 0
tv.11$investment_value <- tv.11$debt_value_infl
tv.11$investment_value_std <- 0

#bind all individual datasets back together
af.6 <- rbind(tv.1.1, tv.1.2.1, tv.1.2.2, tv.2, tv.3, tv.4, tv.5, tv.6, tv.7, tv.8, tv.9, tv.10, tv.11)


#Assign investment values according to investors ####
#Create unique identifier previous to investor split
af.6$identifier <- seq.int(nrow(af.6))

#Identify equity providers
eq <- unique(cSplit(af.6,"sponsors_equity_providers_acquirers"," ; ","long"))

eq.count <- count(eq, identifier)
colnames(eq.count)[2]='n_investors'
eq.2 <- left_join(x=eq, y=eq.count, by=c('identifier'='identifier'))
eq.c <- filter(eq.2, !grepl("Missing|Not Reported|Private Investor|Private Investors",sponsors_equity_providers_acquirers))

#Join equity data with organization data & add regions
eq.j <- left_join(x=eq.c, y=o[ , c("organization_name", "market", "bics_cross_border_classification_1","bics_cross_border_classification_2","bics_cross_border_classification_3","bics_flag","class_flag","number_of_employees")], by=c('sponsors_equity_providers_acquirers'='organization_name'))
eq.j.f <- eq.j[!is.na(eq.j$market.y),]
eq.j2 <- left_join(x=eq.j.f, y=regions, by=c('market.y'='market'))
eq.j2$investment_class <- "equity"


#Identify debt providers
db <- unique(cSplit(af.6,"lead_debt_provider_arranger"," ; ","long"))

db.count <- count(db, identifier)
colnames(db.count)[2]='n_investors'
db.2 <- left_join(x=db, y=db.count, by=c('identifier'='identifier'))
db.c <- filter(db.2, !grepl("Missing|Not Reported|Private Investor|Private Investors",lead_debt_provider_arranger))

#Join debt data with organization data & add regions
db.j <- left_join(x=db.c, y=o[ , c("organization_name", "market", "bics_cross_border_classification_1","bics_cross_border_classification_2","bics_cross_border_classification_3","bics_flag","class_flag","number_of_employees")], by=c('lead_debt_provider_arranger'='organization_name'))
db.j.f <- db.j[!is.na(db.j$market.y),]
db.j2 <- left_join(x=db.j.f, y=regions, by=c('market.y'='market'))
db.j2$investment_class <- "debt"


#Finalize data preparation (rename and export) ####
#Rename datasets and join them together and print 
et <- eq.j2
et$investor <- et$sponsors_equity_providers_acquirers
et <- et %>% 
  rename("asset_country" = "market.x",
          "asset_region" = "region.x",
          "asset_oecd_flag" = "oecd_flag.x",
          "investor_country" = "market.y",
          "investor_region" = "region.y",
          "investor_oecd_flag" = "oecd_flag.y")
et <- subset(et, select = -c(date_of_close,date_of_announcement,transaction_value_infl,equity_value_infl,debt_value_infl,sponsors_equity_providers_acquirers,identifier,lead_debt_provider_arranger,estimated_transaction_value,capacity_announced,capacity_commisioned,capacity_permitted,capacity_total_m_lpa,storage_capacity_m3,flow_capacity_m3,gearing_ratio_debt_as_a_percentage_of_total_project_cost,year_close,year_announcement,lag_days,rf_estimate,rf_std,ds_estimate,ds_std))

dt <- db.j2
dt$investor <- dt$lead_debt_provider_arranger
dt <- dt %>% 
  rename("asset_country" = "market.x",
         "asset_region" = "region.x",
         "asset_oecd_flag" = "oecd_flag.x",
         "investor_country" = "market.y",
         "investor_region" = "region.y",
         "investor_oecd_flag" = "oecd_flag.y")
dt <- subset(dt, select = -c(date_of_close,date_of_announcement,transaction_value_infl,equity_value_infl,debt_value_infl,sponsors_equity_providers_acquirers,identifier,lead_debt_provider_arranger,estimated_transaction_value,capacity_announced,capacity_commisioned,capacity_permitted,capacity_total_m_lpa,storage_capacity_m3,flow_capacity_m3,gearing_ratio_debt_as_a_percentage_of_total_project_cost,year_close,year_announcement,lag_days,rf_estimate,rf_std,ds_estimate,ds_std))

dataset <- rbind(et, dt)

