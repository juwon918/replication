# Replication code for
# An Unexpected Link between Export Sanctions and Cyberespionage
# Juwon Lee
# contact juwon918@gmail.com

# Set working directories
setwd("/Users/juwon/downloads/ESCE_replication/")

# Create directories for output
dir.create(path=paste0(getwd(), "/tables"))

# Install Packages -----------------------------------------------

install.packages("tidyverse")
install.packages("countrycode")
install.packages("peacesciencer")
install.packages("mice")
install.packages("fixest")
install.packages("pROC")
install.packages("stargazer")
install.packages("htmlTable")
install.packages("texreg")

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(haven)
library(countrycode)
library(peacesciencer)
library(mice)
library(fixest)
library(pROC)
library(stargazer)
library(htmlTable)
library(htmltools)
library(texreg)

# Load Data ------------------------------------------------------

data_dcid <- read_csv("data/Master_DCID_2.0_Release_public_Sep2022.csv")
data_eurepoc <- read_csv("data/eurepoc_dyadic_dataset_0_1.csv")
data_gsdb <- read_dta("data/GSDB_V4_Dyadic.dta")
data_vdem <- readRDS("data/V-Dem-CY-Full+Others-v15.rds")
data_intuse <- read_csv("data/individuals-using-the-internet_1748123763984.csv")
load("data/AgreementScoresAll_Jun2024.Rdata") # dfAgree
data_gdp <- read_excel("data/Download-GDPcurrent-USD-countries.xlsx", skip=2)
data_imts <- readRDS("data/IMTS.rds")
data_ucdp <- read_csv("data/Dyadic_v24_1.csv")
data_eci <- read_csv("data/hs92_country_year.csv")

# Data Preprocessing ------------------------------------------------------

# ccode1 = sanctioner, espionage target / ccode2 = sanctionee, espionage initiator

# DCID (2001~2020)
cyber_espionage <- data_dcid %>%
  mutate(initiator = if_else(Cyberincidentnum %in% c(313, 335), 710, initiator),
         ccode2 = initiator,
         ccode1 = as.numeric(mapply(gsub, pattern=initiator, x=Dyadpair, replacement="")),
         startyear = str_sub(interactionstartdate, -4, -1),
         endyear = str_sub(interactionenddate, -4, -1)) %>%
  filter(cyber_objective == 2 | cyber_objective == 3) %>%
  select(ccode1, ccode2, startyear, endyear) %>%
  mutate(year = map2(startyear, endyear, seq)) %>%
  select(ccode1, ccode2, year) %>%
  unnest(year) %>%
  group_by(ccode1, ccode2, year) %>%
  summarise(cyber_espionage = n())

# EuRepoC (2002~2024)
cyber_espionage2 <- data_eurepoc %>%
  mutate(ccode2 = countrycode(initiator_country, "country.name", "cown"),
         ccode1 = countrycode(receiver_country, "country.name", "cown"),
         startyear = as.numeric(str_sub(start_date, 1, 4)),
         endyear = as.numeric(str_sub(end_date, 1, 4)),
         endyear = if_else(is.na(endyear), startyear, endyear)) %>%
  filter(`Data theft` == 1,
         !is.na(ccode1),
         !is.na(ccode2),
         !is.na(startyear)) %>%
  select(ccode1, ccode2, startyear, endyear) %>%
  mutate(year = map2(startyear, endyear, seq)) %>%
  select(ccode1, ccode2, year) %>%
  unnest(year) %>%
  group_by(ccode1, ccode2, year) %>%
  summarise(cyber_espionage2 = n())

# GSDB (1999~2023)
export_sanctions <- data_gsdb %>%
  mutate(across(everything(), ~ ifelse(. == "Ethiopia (excludes Eritrea)", "Ethiopia", .))) %>%
  mutate(ccode1 = countrycode(sanctioning_state, "country.name", "cown"),
         ccode2 = countrycode(sanctioned_state, "country.name", "cown")) %>%
  filter(year >= 1999,
         str_detect(descr_trade, "exp"),
         !is.na(ccode1),
         !is.na(ccode2)) %>%
  select(ccode1, ccode2, year) %>%
  mutate(export_sanctions = 1)

# Vdem - Government cyber security capacity (2000~2024)
cyber_capacity1 <- data_vdem %>%
  select(ccode1 = COWcode, COWcode, year, cyber_capacity1 = v2smgovcapsec) %>%
  filter(year >= 2000)
cyber_capacity2 <- cyber_capacity1 %>%
  rename(ccode2 = ccode1,
         cyber_capacity2 = cyber_capacity1)

# Vdem - Electoral democracy index (1789~2024)
democracy1 <- data_vdem %>%
  select(ccode1 = COWcode, year, democracy1 = v2x_polyarchy) %>%
  filter(year >= 1999)
democracy2 <- democracy1 %>%
  rename(ccode2 = ccode1,
         democracy2 = democracy1)

# Internet Usage (2000~2023)
internet_usage1 <- data_intuse %>%
  mutate(ccode1 = countrycode(entityIso, "iso3c", "cown")) %>%
  filter(!is.na(ccode1)) %>%
  select(ccode1, year = dataYear, internet_usage1 = dataValue)
internet_usage2 <- internet_usage1 %>%
  rename(ccode2 = ccode1,
         internet_usage2 = internet_usage1)

# UNGA Voting (1946~2023)
unga_agree <- dfAgree %>%
  ungroup() %>%
  select(ccode1, ccode2, year, unga_agree = agree) %>%
  filter(year >= 1999)

# GDP (1970~2023) - $1B
gdp1 <- data_gdp %>%
  mutate(ccode1 = countrycode(Country, "country.name", "cown")) %>%
  filter(!is.na(ccode1),
         IndicatorName == "Gross Domestic Product (GDP)") %>%
  select(ccode1, "1999":"2023") %>%
  gather(key = year, value = gdp1, -ccode1) %>%
  filter(!is.na(gdp1)) %>%
  mutate(year = as.numeric(year),
         gdp1 = gdp1/1000000000) %>% 
  distinct(ccode1, year, .keep_all = TRUE) # Sudan 2008~2010
gdp2 <- gdp1 %>%
  rename(ccode2 = ccode1,
         gdp2 = gdp1)

# Trade (1948~2024) - $1M
trade1 <- data_imts %>%
  filter(FREQUENCY == "Annual",
         INDICATOR %in% c("Imports of goods, Cost insurance freight (CIF), US dollar",
                          "Exports of goods, Free on board (FOB), US dollar")) %>%
  select(COUNTRY, COUNTERPART_COUNTRY, INDICATOR, matches("^\\d{4}$")) %>%
  filter(!str_detect(COUNTRY, "SACCA") & !str_detect(COUNTERPART_COUNTRY, "SACCA")) %>%
  mutate(across(everything(),
                ~ str_remove_all(., ", United Kingdom-British Overseas Territory$|, Kingdom of the Netherlands$| Special Administrative Region, People's Republic of China"))) %>%
  mutate(ccode1 = countrycode(COUNTRY, "country.name", "cown"),
         ccode2 = countrycode(COUNTERPART_COUNTRY, "country.name", "cown")) %>%
  select(ccode1, ccode2, "1999":"2024") %>%
  filter(!is.na(ccode1),
         !is.na(ccode2)) %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0))) %>%
  pivot_longer(cols = -c(ccode1, ccode2),
               names_to = "year",
               values_to = "trade1") %>%
  mutate(year = as.numeric(year),
         trade1 = as.numeric(trade1)) %>%
  group_by(ccode1, ccode2, year) %>%
  summarise(trade1 = sum(trade1, na.rm = TRUE), .groups = "drop")
trade2 <- trade1 %>%
  rename(trade2 = trade1) %>%
  rename(tmp = ccode1) %>%
  rename(ccode1 = ccode2) %>%
  rename(ccode2 = tmp)
trade <- full_join(trade1, trade2, by = c("ccode1", "ccode2", "year")) %>%
  mutate(trade = pmax(trade1, trade2, na.rm = TRUE)) %>%
  select(ccode1, ccode2, year, trade)

# Strategic Rivalries (1494~2020, extended to 2023)
rivalry1 <- tss_rivalries %>%
  filter(end >= 1999) %>%
  mutate(year = map2(start, end, seq)) %>%
  unnest(year) %>%
  select(ccode1, ccode2, year) %>%
  filter(year >= 1999) %>%
  group_by(ccode1, ccode2, year) %>%
  summarise(rivalry = n())
rivalry_extended <- bind_rows( # fill data after 2020
  rivalry1,
  rivalry1 %>%
    filter(year == 2020) %>%
    select(-year) %>%
    crossing(year = 2021:2023))
rivalry2 <- rivalry_extended %>%
  rename(tmp = ccode1) %>%
  rename(ccode1 = ccode2) %>%
  rename(ccode2 = tmp)
rivalry <- rbind(rivalry_extended, rivalry2)

# Armed Conflict - UCDP
armed_conflict1 <- data_ucdp %>%
  select(ccode1 = gwno_a, ccode2 = gwno_b, year) %>%
  filter(year >= 1999,
         !is.na(ccode1),
         !is.na(ccode2)) %>%
  separate_rows(ccode2, sep = ",  ") %>%
  mutate(armed_conflict = 1,
         ccode2 = as.numeric(ccode2))
armed_conflict2 <- armed_conflict1 %>%
  rename(tmp = ccode1) %>%
  rename(ccode1 = ccode2) %>%
  rename(ccode2 = tmp)
armed_conflict <- rbind(armed_conflict1, armed_conflict2)

# Economic Complexity Index (1995~2023)
eci1 <- data_eci %>%
  mutate(ccode1 = countrycode(country_iso3_code, "iso3c", "cown")) %>%
  filter(year >= 1999) %>%
  select(ccode1, year, eci1 = eci)
eci2 <- eci1 %>%
  rename("ccode2"="ccode1",
         "eci2"="eci1")

# Politically Relevant Dyads (1920~2016, extended to 2023)
prd <- cow_ddy %>%
  filter(year <= 2023 & year >= 1999) %>%
  filter_prd()
prd_extended <- bind_rows( # fill data after 2016
  prd,
  prd %>%
    filter(year == 2016) %>%
    select(-year) %>%
    crossing(year = 2017:2023)) %>%
  mutate(dyad = paste(ccode1, ccode2, sep = "-"))

# Join Data ----------------------------------------------------

# Join data
finaldata <- bind_rows(
  cow_ddy,
  cow_ddy %>%
    filter(year == 2022) %>%
    select(-year) %>%
    crossing(year = 2023)) %>%
  left_join(cyber_espionage, by = c("ccode1", "ccode2", "year")) %>%
  left_join(cyber_espionage2, by = c("ccode1", "ccode2", "year")) %>%
  left_join(export_sanctions, by = c("ccode1", "ccode2", "year")) %>%
  left_join(cyber_capacity1, by = c("ccode1", "year")) %>%
  left_join(cyber_capacity2, by = c("ccode2", "year")) %>%
  left_join(democracy1, by = c("ccode1", "year")) %>%
  left_join(democracy2, by = c("ccode2", "year")) %>%
  left_join(internet_usage1, by = c("ccode1", "year")) %>%
  left_join(internet_usage2, by = c("ccode2", "year")) %>%
  left_join(unga_agree, by = c("ccode1", "ccode2", "year")) %>%
  left_join(gdp1, by = c("ccode1", "year")) %>%
  left_join(gdp2, by = c("ccode2", "year")) %>%
  left_join(trade, by = c("ccode1", "ccode2", "year")) %>%
  left_join(rivalry, by = c("ccode1", "ccode2", "year")) %>%
  left_join(armed_conflict, by = c("ccode1", "ccode2", "year")) %>%
  left_join(eci1, by = c("ccode1", "year")) %>%
  left_join(eci2, by = c("ccode2", "year"))

# Variable selection
used_data <- finaldata %>%
  subset(year <= 2023 & year >= 2000) %>%
  mutate(dyad = paste(ccode1, ccode2, sep = "-")) %>%
  select(c("dyad", "year", "cyber_espionage", "cyber_espionage2", "export_sanctions", "cyber_capacity1", "cyber_capacity2", "democracy1", "democracy2", "internet_usage1", "internet_usage2", "unga_agree", "gdp1", "gdp2", "trade", "rivalry", "armed_conflict", "eci1", "eci2")) %>%
  group_by(dyad) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(prior_sanctions = lag(export_sanctions, n = 1),
         prior_espionage = lag(cyber_espionage, n = 1),
         prior_espionage2 = lag(cyber_espionage2, n = 1))

# Event data NA to 0
event <- c("export_sanctions", "prior_sanctions", "cyber_espionage", "prior_espionage", "cyber_espionage2", "prior_espionage2", "rivalry", "armed_conflict")
used_data[event] <- lapply(used_data[event], function(x) replace(x, is.na(x), 0))

# Check incomplete rows
incomplete_rows <- sum(!complete.cases(used_data))

# Multiple Imputation -----------------------------------------------------

# Multiple Imputation by Chained Equations
imp_data <- cbind(used_data, logintuse = NA, democracy_dyad = NA, eci_diff = NA, trade_dependence_diff_21 = NA)
ini <- mice(imp_data, max=0, print=FALSE)
meth <- ini$meth
meth["logintuse"] <- "~I(log(internet_usage1 + internet_usage2 + 1e-8))"
meth["democracy_dyad"] <- "~I(democracy1 * democracy2)"
meth["eci_diff"] <- "~I(abs(eci1 - eci2))"
meth["trade_dependence_diff_21"] <- "~I(log((trade/gdp2) + 1e-8) - log((trade/gdp1) + 1e-8))"

imp <- mice(imp_data, meth=meth, m=20, maxit=10, seed=123)
# saveRDS(imp, "imp.rds")
imp <- readRDS("imp.rds")

# Select Data -------------------------------------------------------------

# Full dyads
cyber_data <- lapply(1:imp$m, function(i) {
  data <- complete(imp, i) %>%
    filter(year <= 2020 & year >= 2001) %>%
    select(c("dyad", "year", "cyber_espionage", "export_sanctions", "prior_espionage", "prior_sanctions", "cyber_capacity2", "eci_diff", "logintuse", "rivalry", "armed_conflict", "democracy_dyad", "unga_agree", "trade_dependence_diff_21"))
  data})

# PRD Only
cyber_data_prd <- lapply(1:imp$m, function(i) {
  data <- complete(imp, i) %>%
    subset(year <= 2020 & year >= 2001) %>%
    semi_join(prd_extended, by = c("dyad", "year")) %>%
    select(c("dyad", "year", "cyber_espionage", "export_sanctions", "prior_espionage", "prior_sanctions", "cyber_capacity2", "eci_diff", "logintuse", "rivalry", "armed_conflict", "democracy_dyad", "unga_agree", "trade_dependence_diff_21"))
  data})

# Alternative DV
cyber_data_alt <- lapply(1:imp$m, function(i) {
  data <- complete(imp, i) %>%
    subset(year <= 2023 & year >= 2002) %>%
    select(c("dyad", "year", "cyber_espionage2", "export_sanctions", "prior_espionage2", "prior_sanctions", "cyber_capacity2", "eci_diff", "logintuse", "rivalry", "armed_conflict", "democracy_dyad", "unga_agree", "trade_dependence_diff_21"))
  data})

# Complete case
cyber_data_complete <- used_data %>%
  filter(year <= 2020 & year >= 2001) %>%
  mutate(logintuse = log(internet_usage1 + internet_usage2 + 1e-8),
         democracy_dyad = democracy1 * democracy2,
         eci_diff = abs(eci1 - eci2),
         trade_dependence_diff_21 = log((trade/gdp2) + 1e-8) - log((trade/gdp1) + 1e-8)) %>%
  select(c("dyad", "year", "cyber_espionage", "export_sanctions", "prior_espionage", "prior_sanctions", "cyber_capacity2", "eci_diff", "logintuse", "rivalry", "armed_conflict", "democracy_dyad", "unga_agree", "trade_dependence_diff_21")) %>%
  na.omit()

# Stage 1: Logistic Regression for Export Sanctions --------------------------------------------

# Model 1, 2 - Naive Model, Standard Model
logit_model <- lapply(1:imp$m, function(i) {
  data <- cyber_data[[i]]
  feglm(export_sanctions ~ prior_sanctions + democracy_dyad + unga_agree + trade_dependence_diff_21 | year,
        family = binomial(link = "logit"),
        cluster = ~ dyad,
        data = data)})
logit_model_pooled <- pool(logit_model)

cyber_data2 <- lapply(1:imp$m, function(i) {
  data <- cyber_data[[i]]
  data$residuals <- residuals(logit_model[[i]], type = "response")
  data})

# Model 3 - No Year FE
logit_model_nofe <- lapply(1:imp$m, function(i) {
  data <- cyber_data[[i]]
  feglm(export_sanctions ~ prior_sanctions + democracy_dyad + unga_agree + trade_dependence_diff_21,
        family = binomial(link = "logit"),
        cluster = ~ dyad,
        data = data)})
logit_model_nofe_pooled <- pool(logit_model_nofe)

cyber_data_nofe <- lapply(1:imp$m, function(i) {
  data <- cyber_data[[i]]
  data$residuals <- residuals(logit_model_nofe[[i]], type = "response")
  data})

# Model 4 - PRD Only
logit_model_prd <- lapply(1:imp$m, function(i) {
  data <- cyber_data_prd[[i]]
  feglm(export_sanctions ~ prior_sanctions + democracy_dyad + unga_agree + trade_dependence_diff_21 | year,
        family = binomial(link = "logit"),
        cluster = ~ dyad,
        data = data)})
logit_model_prd_pooled <- pool(logit_model_prd)

cyber_data_prd2 <- lapply(1:imp$m, function(i) {
  data <- cyber_data_prd[[i]]
  data$residuals <- residuals(logit_model_prd[[i]], type = "response")
  data})

# Model 5 - Alternative DV
logit_model_alt <- lapply(1:imp$m, function(i) {
  data <- cyber_data_alt[[i]]
  feglm(export_sanctions ~ prior_sanctions + democracy_dyad + unga_agree + trade_dependence_diff_21 | year,
        family = binomial(link = "logit"),
        cluster = ~ dyad,
        data = data)})
logit_model_alt_pooled <- pool(logit_model_alt)

cyber_data_alt2 <- lapply(1:imp$m, function(i) {
  data <- cyber_data_alt[[i]]
  data$residuals <- residuals(logit_model_alt[[i]], type = "response")
  data})

# Model 6 - Conplete Case
logit_model_complete <- feglm(export_sanctions ~ prior_sanctions + democracy_dyad + unga_agree + trade_dependence_diff_21 | year,
                              family = binomial(link = "logit"),
                              cluster = ~ dyad,
                              data = cyber_data_complete)
cyber_data_complete2 <- cyber_data_complete %>%
  ungroup() %>%
  mutate(residuals = residuals(logit_model_complete, type = "response"))

# Stage 2: Poisson Pseudo Maximum Likelihood ---------------------------------

# Model 1 - Naive Model
ppml_model_naive <- lapply(1:imp$m, function(i) {
  data <- cyber_data2[[i]]
  feglm(cyber_espionage ~ export_sanctions + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict + democracy_dyad | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
ppml_model_naive_pooled <- pool(ppml_model_naive)

# Model 2 - Standard Model
ppml_model <- lapply(1:imp$m, function(i) {
  data <- cyber_data2[[i]]
  feglm(cyber_espionage ~ export_sanctions + residuals + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict + democracy_dyad | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
ppml_model_pooled <- pool(ppml_model)

# Model 3 - No Year FE
ppml_model_nofe <- lapply(1:imp$m, function(i) {
  data <- cyber_data_nofe[[i]]
  feglm(cyber_espionage ~ export_sanctions + residuals + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict + democracy_dyad,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
ppml_model_nofe_pooled <- pool(ppml_model_nofe)

# Model 4 - PRD Only
ppml_model_prd <- lapply(1:imp$m, function(i) {
  data <- cyber_data_prd2[[i]]
  feglm(cyber_espionage ~ export_sanctions + residuals + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict + democracy_dyad | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
ppml_model_prd_pooled <- pool(ppml_model_prd)

# Model 5 - Alternative DV
ppml_model_alt <- lapply(1:imp$m, function(i) {
  data <- cyber_data_alt2[[i]]
  feglm(cyber_espionage2 ~ export_sanctions + residuals + prior_espionage2 + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict + democracy_dyad | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
ppml_model_alt_pooled <- pool(ppml_model_alt)

# Model 6 - Complete Case
ppml_model_complete <- feglm(cyber_espionage ~ export_sanctions + residuals + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict + democracy_dyad | year,
                   family = poisson(),
                   cluster = ~ dyad,
                   data = cyber_data_complete2)

# Stage 1 Tests -------------------------------------------------------

# Exclusion Restriction Test - Zero-First-Stage Test
zfst_prior_sanctions <- lapply(1:imp$m, function(i) {
  data <- subset(cyber_data2[[i]], export_sanctions == 0)
  feglm(cyber_espionage ~ prior_sanctions + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
zfst_prior_sanctions_pooled <- pool(zfst_prior_sanctions)

zfst_democracy_dyad <- lapply(1:imp$m, function(i) {
  data <- subset(cyber_data2[[i]], export_sanctions == 0)
  feglm(cyber_espionage ~ democracy_dyad + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
zfst_democracy_dyad_pooled <- pool(zfst_democracy_dyad)

zfst_unga_agree <- lapply(1:imp$m, function(i) {
  data <- subset(cyber_data2[[i]], export_sanctions == 0)
  feglm(cyber_espionage ~ unga_agree + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
zfst_unga_agree_pooled <- pool(zfst_unga_agree)

zfst_trade_dependence_diff_21 <- lapply(1:imp$m, function(i) {
  data <- subset(cyber_data2[[i]], export_sanctions == 0)
  feglm(cyber_espionage ~ trade_dependence_diff_21 + prior_espionage + cyber_capacity2 + eci_diff + logintuse + rivalry + armed_conflict | year,
        family = poisson(),
        cluster = ~ dyad,
        data = data)})
zfst_trade_dependence_diff_21_pooled <- pool(zfst_trade_dependence_diff_21)

# First Stage Pseudo R^2
pseudo_r2_logit <- sapply(logit_model, function(model) {
  fitstat(model, type = "pr2")})
s1_pr2_mean <- mean(as.numeric(pseudo_r2_logit))
s1_pr2_range <- range(pseudo_r2_logit)

# First Stage AUC
auc_values <- sapply(1:imp$m, function(i) {
  data <- cyber_data2[[i]]
  pred_probs <- predict(logit_model[[i]], newdata = data, type = "response")
  roc_obj <- roc(response = data$export_sanctions, predictor = pred_probs)
  auc(roc_obj)})
s1_auc_mean <- mean(auc_values)
s1_auc_range <- range(auc_values)

# Stage 2 Model fit ---------------------------------------------------------------

# Pseudo R^2
pr2_m1 <- sapply(ppml_model_naive, function(m) {
  as.numeric(fitstat(m, type = "pr2"))})
pr2_m2 <- sapply(ppml_model, function(m) {
  as.numeric(fitstat(m, type = "pr2"))})
pr2_m3 <- sapply(ppml_model_nofe, function(m) {
  as.numeric(fitstat(m, type = "pr2"))})
pr2_m4 <- sapply(ppml_model_prd, function(m) {
  as.numeric(fitstat(m, type = "pr2"))})
pr2_m5 <- sapply(ppml_model_alt, function(m) {
  as.numeric(fitstat(m, type = "pr2"))})
pr2_m6 <- as.numeric(fitstat(ppml_model_complete, type = "pr2"))

# Loglik
loglik_m1 <- sapply(ppml_model_naive, logLik)
loglik_m2 <- sapply(ppml_model, logLik)
loglik_m3 <- sapply(ppml_model_nofe, logLik)
loglik_m4 <- sapply(ppml_model_prd, logLik)
loglik_m5 <- sapply(ppml_model_alt, logLik)
loglik_m6 <- ppml_model_complete %>% logLik

# Case Studies --------------------------------------------------------------

incident_dcid <- data_dcid %>%
  mutate(initiator = if_else(Cyberincidentnum %in% c(313, 335), 710, initiator),
         ccode2 = initiator,
         ccode1 = as.numeric(mapply(gsub, pattern=initiator, x=Dyadpair, replacement="")),
         startyear = str_sub(interactionstartdate, -4, -1),
         endyear = str_sub(interactionenddate, -4, -1)) %>%
  select(ccode1, ccode2, startyear, endyear) %>%
  mutate(year = map2(startyear, endyear, seq)) %>%
  select(ccode1, ccode2, year) %>%
  unnest(year) %>%
  group_by(ccode1, ccode2, year) %>%
  summarise(cyber_incident = n())
case_dcid <- incident_dcid %>%
  left_join(cyber_espionage, by = c("ccode1", "ccode2", "year")) %>%
  left_join(export_sanctions, by = c("ccode1", "ccode2", "year"))

case_dcid_nk <- case_dcid %>%
  ungroup() %>%
  filter(ccode2 == 731) %>%
  select(year, cyber_incident, cyber_espionage) %>%
  group_by(year) %>%
  summarise(cyber_incident = sum(cyber_incident, na.rm = TRUE),
            cyber_espionage = sum(cyber_espionage, na.rm = TRUE))

case_dcid_cn_us <- case_dcid %>%
  ungroup() %>%
  filter(ccode2 == 710, ccode1 == 2) %>%
  select(year, cyber_incident, cyber_espionage) %>%
  group_by(year) %>%
  summarise(cyber_incident = sum(cyber_incident, na.rm = TRUE),
            cyber_espionage = sum(cyber_espionage, na.rm = TRUE))

# Tables ------------------------------------------------------

# Descriptive statistics
cyber_data_ds <- used_data %>%
  filter(year <= 2020 & year >= 2001) %>%
  mutate(logintuse = log(internet_usage1 + internet_usage2 + 1e-8),
         democracy_dyad = democracy1 * democracy2,
         eci_diff = abs(eci1 - eci2),
         trade_dependence_diff_21 = log((trade/gdp2) + 1e-8) - log((trade/gdp1) + 1e-8)) %>%
  select(c("dyad", "year", "cyber_espionage", "export_sanctions", "prior_espionage", "prior_sanctions", "cyber_capacity2", "eci_diff", "logintuse", "rivalry", "armed_conflict", "democracy_dyad", "unga_agree", "trade_dependence_diff_21"))
stargazer(as.data.frame(cyber_data_ds), type = "html", title="Descriptive Statistics", out = "tables/DS_D1_all.html")

cyber_data_prd_ds <- used_data %>%
  semi_join(prd_extended, by = c("dyad", "year")) %>%
  filter(year <= 2020 & year >= 2001) %>%
  mutate(logintuse = log(internet_usage1 + internet_usage2 + 1e-8),
         democracy_dyad = democracy1 * democracy2,
         eci_diff = abs(eci1 - eci2),
         trade_dependence_diff_21 = log((trade/gdp2) + 1e-8) - log((trade/gdp1) + 1e-8)) %>%
  select(c("dyad", "year", "cyber_espionage", "export_sanctions", "prior_espionage", "prior_sanctions", "cyber_capacity2", "eci_diff", "logintuse", "rivalry", "armed_conflict", "democracy_dyad", "unga_agree", "trade_dependence_diff_21"))
stargazer(as.data.frame(cyber_data_prd_ds), type = "html", title="Descriptive Statistics", out = "tables/DS_D2_all.html")

cyber_data_alt_ds <- used_data %>%
  filter(year <= 2023 & year >= 2002) %>%
  mutate(logintuse = log(internet_usage1 + internet_usage2 + 1e-8),
         democracy_dyad = democracy1 * democracy2,
         eci_diff = abs(eci1 - eci2),
         trade_dependence_diff_21 = log((trade/gdp2) + 1e-8) - log((trade/gdp1) + 1e-8)) %>%
  select(c("dyad", "year", "cyber_espionage2", "export_sanctions", "prior_espionage2", "prior_sanctions", "cyber_capacity2", "eci_diff", "logintuse", "rivalry", "armed_conflict", "democracy_dyad", "unga_agree", "trade_dependence_diff_21"))
stargazer(as.data.frame(cyber_data_alt_ds), type = "html", title="Descriptive Statistics", out = "tables/DS_D3_all.html")

stargazer(as.data.frame(cyber_data_complete), type = "html", title="Descriptive Statistics", out = "tables/DS_D4_all.html")

# incomplete rows
incomplete_rows <- data.frame(
  Data = c("Main"),
  Rows = c(
    incomplete_rows))
incomplete_rows_table <- htmlTable(incomplete_rows,
                               caption = "Incomplete Rows",
                               rnames = FALSE,
                               align = "lc")
save_html(HTML(incomplete_rows_table), file = "tables/Incomplete Rows.html")

# First stage result
htmlreg(logit_model_pooled, digits = 5, file = "tables/S1_D1.html", title = "Logit Model First Stage")
htmlreg(logit_model_complete, digits = 5, file = "tables/S1_D4.html", title = "Logit Model First Stage")

# First stage pseudo R^2, AUC and wald
summary_table <- data.frame(
  Statistic = c("Pseudo R²", "AUC"),
  Mean = c(
    round(s1_pr2_mean, 3),
    round(s1_auc_mean, 3)),
  Min = c(
    round(s1_pr2_range[1], 3),
    round(s1_auc_range[1], 3)),
  Max = c(
    round(s1_pr2_range[2], 3),
    round(s1_auc_range[2], 3)))
logit_diagnostics <- htmlTable(summary_table,
          caption = "First-Stage Diagnostics",
          rnames = FALSE,
          align = "lccc")
save_html(HTML(logit_diagnostics), file = "tables/S1_D1_diagnostics.html")

# Zero-first stage test results
htmlreg(zfst_prior_sanctions_pooled, digits = 5, file = "tables/S1_D1_zfst_prior_sanctions.html", title = "Zero-First-Stage Test")
htmlreg(zfst_democracy_dyad_pooled, digits = 5, file = "tables/S1_D1_zfst_democracy_dyad.html", title = "Zero-First-Stage Test")
htmlreg(zfst_unga_agree_pooled, digits = 5, file = "tables/S1_D1_zfst_unga_agree.html", title = "Zero-First-Stage Test")
htmlreg(zfst_trade_dependence_diff_21_pooled, digits = 5, file = "tables/S1_D1_zfst_trade_dependence_diff_21.html", title = "Zero-First-Stage Test")

# Second stage results
htmlreg(ppml_model_naive_pooled, digits = 5, file = "tables/S2_M1.html", title = "Model 1: Naive Model")
htmlreg(ppml_model_pooled, digits = 5, file = "tables/S2_M2.html", title = "Model 2: Standard Model")
htmlreg(ppml_model_nofe_pooled, digits = 5, file = "tables/S2_M3.html", title = "Model 3: No Year FE")
htmlreg(ppml_model_prd_pooled, digits = 5, file = "tables/S2_M4.html", title = "Model 4: PRD Only)")
htmlreg(ppml_model_alt_pooled, digits = 5, file = "tables/S2_M5.html", title = "Model 5: Alternative DV")
htmlreg(ppml_model_complete, digits = 5, file = "tables/S2_M6.html", title = "Model 6: Complete Case")

# Model fit
model_fit <- data.frame(
  Model = c("Naive Model", "Basic Model", "No Year FE", "PRD Only", "Alternative DV", "Complete Case"),
  Pseudo_R2 = c(
    sprintf("%.3f (%.3f, %.3f)", mean(pr2_m1), min(pr2_m1), max(pr2_m1)),
    sprintf("%.3f (%.3f, %.3f)", mean(pr2_m2), min(pr2_m2), max(pr2_m2)),
    sprintf("%.3f (%.3f, %.3f)", mean(pr2_m3), min(pr2_m3), max(pr2_m3)),
    sprintf("%.3f (%.3f, %.3f)", mean(pr2_m4), min(pr2_m4), max(pr2_m4)),
    sprintf("%.3f (%.3f, %.3f)", mean(pr2_m5), min(pr2_m5), max(pr2_m5)),
    sprintf("%.3f", pr2_m6)),
  LogLik =  c(
    sprintf("%.1f (%.1f, %.1f)", mean(loglik_m1), min(loglik_m1), max(loglik_m1)),
    sprintf("%.1f (%.1f, %.1f)", mean(loglik_m2), min(loglik_m2), max(loglik_m2)),
    sprintf("%.1f (%.1f, %.1f)", mean(loglik_m3), min(loglik_m3), max(loglik_m3)),
    sprintf("%.1f (%.1f, %.1f)", mean(loglik_m4), min(loglik_m4), max(loglik_m4)),
    sprintf("%.1f (%.1f, %.1f)", mean(loglik_m5), min(loglik_m5), max(loglik_m5)),
    sprintf("%.1f", loglik_m6)))
model_fit_table <- htmlTable(model_fit,
                             caption = "Model Fit (Mean and Range)",
                             rnames = FALSE)
save_html(HTML(model_fit_table), file = "tables/S2_fit.html")

# Case studies
case_nk_table <- htmlTable(case_dcid_nk,
                           caption = "North Korea Cyber Indicents",
                           rnames = FALSE)
save_html(HTML(case_nk_table), file = "tables/CS_D1_nk.html")
case_cn_us_table <- htmlTable(case_dcid_cn_us,
                              caption = "China-US Cyber Indicents",
                              rnames = FALSE)
save_html(HTML(case_cn_us_table), file = "tables/CS_D1_cnus.html")