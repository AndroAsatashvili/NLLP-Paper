#' @name Data
#' @title Data retrieval for data matrix
#' @description Remote and automated data retrieval for analysis
#' @note Fred API used, as well as local data
#' @author Andro Asatashvili

rm(list = ls())

# Load pacman
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(tidyverse, fredr, readxl, lubridate, foreach, gridExtra, magrittr,
       tseries, openxlsx, ggpubr, ivreg)


start <- as.Date("1997-08-31")
end <- as.Date("2023-12-31")

################################################################################
################################################################################
#                 API Data: FRED
################################################################################
################################################################################

# API
key <- "c27bf13d09598a184acdcb2ba94aa28f" #If not working, create your own at FRED
fredr_set_key(key)
fredr_has_key() 

#Federal FUNDS
fedfunds <- fredr_series_observations(series_id = "FEDFUNDS",
                                      observation_start = start,
                                      observation_end = end,
                                      frequency = "m",
                                      aggregation_method = "sum",
                                      units = "lin")
fedfunds <- fedfunds %>%
  rename(fedfunds = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Spread (10 years - 3 months) / Yield Curve
T10Y3M <- fredr_series_observations(series_id = "T10Y3M",
                                    observation_start = start,
                                    observation_end = end,
                                    frequency = "m",
                                    aggregation_method = "sum",
                                    units = "lin")
T10Y3M <- T10Y3M %>%
  rename(T10Y3M = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Monetary Base
BOGMBASE <- fredr_series_observations(series_id = "BOGMBASE",
                                      observation_start = start,
                                      observation_end = end,
                                      frequency = "m",
                                      aggregation_method = "sum",
                                      units = "lin")
ldBOGMBASE <- BOGMBASE %>%
  rename(BOGMBASE = value) %>%
  mutate(lBOGMBASE = log(BOGMBASE)) %>%
  mutate(ldBOGMBASE = 100*(lBOGMBASE - lag(lBOGMBASE))) %>% #log-difference
  select(-series_id, -realtime_start, -realtime_end)


# Retail Sales
RSXFS <- fredr_series_observations(series_id = "RSXFS",
                                   observation_start = start,
                                   observation_end = end,
                                   frequency = "m",
                                   aggregation_method = "sum",
                                   units = "lin")
ldRSXFS <- RSXFS %>%
  rename(RSXFS = value) %>%
  mutate(lRSXFS = log(RSXFS)) %>%
  mutate(ldRSXFS = 100*(lRSXFS - lag(lRSXFS))) %>% #log-difference
  select(-series_id, -realtime_start, -realtime_end)

# CPI
cpi <- fredr_series_observations(series_id = "CPIAUCSL",
                                 observation_start = start,
                                 observation_end = end,
                                 frequency = "m",
                                 aggregation_method = "sum",
                                 units = "lin")
ldcpi <- cpi %>%
  rename(cpi = value) %>%
  mutate(lcpi = 100*log(cpi)) %>%
  mutate(ldcpi = (lcpi - lag(lcpi))) %>% #log-difference
  select(-series_id, -realtime_start, -realtime_end)


# Industrial Production
INDPROD <- fredr_series_observations(series_id = "INDPRO",
                                     observation_start = start,
                                     observation_end = end,
                                     frequency = "m",
                                     aggregation_method = "avg",
                                     units = "lin")
ldINDPROD <- INDPROD %>%
  rename(INDPROD = value) %>%
  mutate(lINDPROD = 100*log(INDPROD)) %>%
  mutate(ldINDPROD = (lINDPROD - lag(lINDPROD))) %>%    #log-difference
  select(-series_id, -realtime_start, -realtime_end, -INDPROD)


# Inflation Expectations (12 months)
EXPINF1YR <- fredr_series_observations(series_id = "EXPINF1YR",
                                       observation_start = start,
                                       observation_end = end,
                                       frequency = "m",
                                       aggregation_method = "avg",
                                       units = "lin")
EXPINF1YR <- EXPINF1YR %>%
  rename(EXPINF1YR = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Inflation Expectations (24 months)
EXPINF2YR <- fredr_series_observations(series_id = "EXPINF2YR",
                                       observation_start = start,
                                       observation_end = end,
                                       frequency = "m",
                                       aggregation_method = "avg",
                                       units = "lin")
EXPINF2YR <- EXPINF2YR %>%
  rename(EXPINF2YR = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Treasury 10 years
DGS10 <- fredr_series_observations(series_id = "DGS10",
                                   observation_start = start,
                                   observation_end = end,
                                   frequency = "m",
                                   aggregation_method = "avg",
                                   units = "lin")
DGS10 <- DGS10 %>%
  rename(DGS10 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)


# Treasury 1 years
DGS1 <- fredr_series_observations(series_id = "DGS1",
                                  observation_start = start,
                                  observation_end = end,
                                  frequency = "m",
                                  aggregation_method = "avg",
                                  units = "lin")
DGS1 <- DGS1 %>%
  rename(DGS1 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Treasury 2 years
DGS2 <- fredr_series_observations(series_id = "DGS2",
                                  observation_start = start,
                                  observation_end = end,
                                  frequency = "m",
                                  aggregation_method = "avg",
                                  units = "lin")
DGS2 <- DGS2 %>%
  rename(DGS2 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Treasury 5 years
DGS5 <- fredr_series_observations(series_id = "DGS5",
                                  observation_start = start,
                                  observation_end = end,
                                  frequency = "m",
                                  aggregation_method = "avg",
                                  units = "lin")
DGS5 <- DGS5 %>%
  rename(DGS5 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Treasury 30 years
DGS30 <- fredr_series_observations(series_id = "DGS30",
                                   observation_start = start,
                                   observation_end = end,
                                   frequency = "m",
                                   aggregation_method = "avg",
                                   units = "lin")
DGS30 <- DGS30 %>%
  rename(DGS30 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Corporate Spread AAA
BAA10Y <- fredr_series_observations(series_id = "BAA10Y",
                                    observation_start = start,
                                    observation_end = end,
                                    frequency = "m",
                                    aggregation_method = "avg",
                                    units = "lin")
BAA10Y <- BAA10Y %>%
  rename(BAA10Y = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Mean Fixed Rate Mortgage Rate
MORTGAGE30US <- fredr_series_observations(series_id = "MORTGAGE30US",
                                          observation_start = start,
                                          observation_end = end,
                                          frequency = "m",
                                          aggregation_method = "avg",
                                          units = "lin")
MORTGAGE30US <- MORTGAGE30US %>%
  rename(MORTGAGE30US = value) %>%
  select(-series_id, -realtime_start, -realtime_end)


# PPI
ppi <- fredr_series_observations(series_id = "PPIIDC",
                                 observation_start = start,
                                 observation_end = end,
                                 frequency = "m",
                                 aggregation_method = "avg",
                                 units = "lin")
ppi <- ppi %>%
  rename(ppi = value) %>%
  mutate(lppi = 100*log(ppi)) %>%
  mutate(ldppi = (lppi - lag(lppi))) %>%   #log-difference
  select(-series_id, -realtime_start, -realtime_end, -ppi)


# Coincident Economic Activity Index
econacti <- fredr_series_observations(series_id = "USPHCI",
                                      observation_start = start,
                                      observation_end = end,
                                      frequency = "m",
                                      aggregation_method = "avg",
                                      units = "lin")
econacti <- econacti %>%
  rename(econacti = value) %>%
  mutate(leconacti = log(econacti)) %>%
  mutate(ldeconacti = 100*(leconacti - lag(leconacti))) %>%   #log-difference
  select(-series_id, -realtime_start, -realtime_end, -econacti)

# Unemployment Rate
UNRATE <- fredr_series_observations(series_id = "UNRATE",
                                    observation_start = start,
                                    observation_end = end,
                                    frequency = "m",
                                    aggregation_method = "avg",
                                    units = "lin")
UNRATE <- UNRATE %>%
  rename(unrate = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# Total Consumer Credit
REVOLSL <- fredr_series_observations(series_id = "TOTALSL",
                                     observation_start = start,
                                     observation_end = end,
                                     frequency = "m",
                                     aggregation_method = "avg",
                                     units = "pch")
REVOLSL <- REVOLSL %>%
  rename(revolsl = value) %>%
  mutate(ldrevol = log(revolsl) - lag(log(revolsl))) %>%  #log-difference
  select(-series_id, -realtime_start, -realtime_end)

REVOLSL <- REVOLSL[-1,]

# VIX
VIX <- fredr_series_observations(series_id = "VIXCLS",
                                 observation_start = start,
                                 observation_end = end,
                                 frequency = "m",
                                 aggregation_method = "avg",
                                 units = "lin")
VIX <- VIX %>%
  rename(VIX = value) %>%
  mutate(lvix = 100*log(VIX)) %>%
  mutate(ldvix = (lvix - lag(lvix))) %>%    #log-difference
  select(-series_id, -realtime_start, -realtime_end, )

# West Texas
WTI <- fredr_series_observations(series_id = "DCOILWTICO",
                                 observation_start = start,
                                 observation_end = end,
                                 frequency = "m",
                                 aggregation_method = "avg",
                                 units = "lin")
WTI <- WTI %>%
  rename(WTI = value) %>%
  mutate(lwti = log(WTI)) %>%
  mutate(ldwti = 100*(lwti - lag(lwti))) %>%     #log-difference
  select(-series_id, -realtime_start, -realtime_end, -WTI)

#EPU Index
guncert <- fredr_series_observations(series_id = "GEPUCURRENT",
                                     observation_start = start,
                                     observation_end = end,
                                     frequency = "m",
                                     aggregation_method = "avg",
                                     units = "lin")
guncert <- guncert %>%
  rename(guncert = value) %>%
  mutate(lguncert = 100*log(guncert)) %>%
  mutate(ldguncert = lguncert - lag(lguncert)) %>%    #log-difference
  select(-series_id, -realtime_start, -realtime_end)


# G7 Manufacturing
G7 <- fredr_series_observations(series_id = "G7PRMNTO01GPSAM",
                                observation_start = start,
                                observation_end = end,
                                frequency = "m",
                                aggregation_method = "avg",
                                units = "chg")
G7 <- G7 %>%
  rename(G7 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# NFP
PAYEMS <- fredr_series_observations(series_id = "PAYEMS",
                                    observation_start = start,
                                    observation_end = end,
                                    frequency = "m",
                                    units = "lin")
PAYEMS<- PAYEMS %>%
  rename(PAYEMS = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

# SP500
SP500 <- fredr_series_observations(series_id = "SP500",
                                   observation_start = start,
                                   observation_end = end,
                                   frequency = "m",
                                   units = "lin")
SP500 <- SP500 %>%
  rename(SP500 = value) %>%
  mutate(lSP500 = log(SP500)) %>%
  mutate(ldSP500 = 100*(lSP500 - lag(lSP500))) %>%    #log-difference
  select(-series_id, -realtime_start, -realtime_end)


commodities <- fredr_series_observations(series_id = "PALLFNFINDEXM",
                                         observation_start = start,
                                         observation_end = end,
                                         frequency = "m",
                                         aggregation_method = "sum",
                                         units = "lin")
commodities <- commodities %>%
  rename(comm = value) %>%
  mutate(lcomm = 100*log(comm)) %>%
  mutate(ldcomm = (lcomm - lag(lcomm))) %>%     #log-difference
  select(-series_id, -realtime_start, -realtime_end)


energy <- fredr_series_observations(series_id = "PNRGINDEXM",
                                    observation_start = start,
                                    observation_end = end,
                                    frequency = "m",
                                    aggregation_method = "sum",
                                    units = "lin")
energy <- energy %>%
  rename(ener = value) %>%
  mutate(lener = 100*log(ener)) %>%
  mutate(ldener = (lener - lag(lener))) %>%     #log-difference
  select(-series_id, -realtime_start, -realtime_end)




################################################################################
################################################################################
#                 Local Data
################################################################################
################################################################################

# Shadow Rate
shadow <- read_excel("WuXiaShadowRate.xlsx")
shadow <- shadow %>%
  rename(date = ...1) 


# GSCPI
gscpi_data <- read_excel("gscpi_data.xls", 
                         sheet = "Hoja1")
gscpi_data <- gscpi_data %>%
  rename(Period = periodo, GSCPI = GSCPI) %>%
  mutate(Period = as.Date(Period)) 

# Karadi Shocks
karadi_shocks <- read.csv("shocks_fed_jk_m.csv")
karadi_shocks$date <- seq(from = start, by = "month", length.out = nrow(karadi_shocks))


# Global Uncertainty Index
uncert <- read_excel("Global_Policy_Uncertainty_Data.xlsx")
uncert <- uncert %>%
 rename(Period = periodo, uncert = GEPU_ppp) %>%
mutate(Period = as.Date(Period))

# Excess Bond Premium
ebp_csv <- read_csv("ebp_csv.csv")

# SP500
sp500 <- read_xlsx("sp500.xlsx")
sp500 <- as.data.frame(sp500)
sp500 <- sp500 %>%
  mutate(lsp500 = log(sp500)) %>%
  mutate(ldsp500 = log(sp500) - lag(log(sp500)))   #log-difference


# Bauer & Swanson MP Shocks
mp_surprises <- read_excel("monetary-policy-surprises-data.xlsx", 
                           sheet = "Monthly (update 2023)")
mp_surprises <- as.data.frame(mp_surprises)
#mp_surprises <- mp_surprises[-1,]
fomc <- read_excel("monetary-policy-surprises-data.xlsx", 
                   sheet = "FOMC (update 2023)")
fomc <- as.data.frame(fomc)
fomc <- fomc[-1,]


# SP
sp <- read_excel("sp.xls", sheet = "Data")
sp <- sp %>%
  mutate(lsp = 100*log(SP)) %>%
  mutate(ldsp = lsp - lag(lsp))


################################################################################
#          Main Data Frame (Join all variables by date)
##############################################################################
df <- ldcpi %>%
  left_join(UNRATE, by = "date") %>%
  left_join(ppi, by = "date") %>%
  left_join(EXPINF1YR, by = "date") %>%
  mutate(dEXPINF1YR = EXPINF1YR - lag(EXPINF1YR)) %>%
  left_join(EXPINF2YR, by = "date") %>%
  mutate(dEXPINF2YR = EXPINF2YR - lag(EXPINF2YR)) %>%
  left_join(econacti, by = "date") %>%
  left_join(DGS1, by = "date") %>% 
  mutate(dDGS1 = (DGS1 - lag(DGS1))) %>%
  left_join(DGS2, by = "date") %>% 
  mutate(dDGS2 = (DGS2 - lag(DGS2))) %>%
  left_join(fedfunds,  by = "date") %>%
  left_join(T10Y3M, by = "date") %>%
  mutate(dfedfunds = (fedfunds - lag(fedfunds))) %>% 
  left_join(ldINDPROD, by = "date") %>%
  left_join(shadow, by = "date") %>%
  left_join(DGS10, by = "date") %>%
  mutate(dDGS10 = DGS10 - lag(DGS10)) %>%
  mutate(dshadow = shadow - lag(shadow))%>%
  left_join(WTI, by = "date") %>%
  left_join(VIX, by = "date") %>%
  left_join(guncert, by = "date") %>%
  left_join(DGS5, by = "date") %>%
  mutate(dDGS5 = (DGS5 - lag(DGS5))) %>%
  left_join(DGS30, by ="date") %>%
  mutate(dDGS30 = (DGS30 - lag(DGS30))) %>%
  left_join(MORTGAGE30US, by = "date") %>%
  mutate(dMORTGAGE30US = (MORTGAGE30US - lag(MORTGAGE30US))) %>%
  left_join(BAA10Y, by = "date") %>%
  mutate(dBAA10Y = (BAA10Y - lag(BAA10Y))) %>%
  left_join(ldBOGMBASE, by= "date") %>%
  left_join(ldRSXFS, by= "date") %>%
  left_join(PAYEMS, by ="date") %>%
  left_join(SP500, by = "date")%>%
  left_join(commodities, by = "date") %>%
  left_join(energy, by = "date")

# Merge Local Data:

df <- df %>%
  mutate(shadow = ifelse(is.na(shadow), fedfunds, shadow))
df <- cbind(df, sp)
#df$cpi <- CPIAUCSL_2_$cpi
df <- df[-1,]
df <- cbind(df, mp_surprises$MPS)
df <- cbind(df, mp_surprises$MPS_ORTH)
df$revolsl <- REVOLSL$revolsl

df$GSCPI <- gscpi_data$GSCPI

df <- cbind(df, karadi_shocks$pc1_hf)
df$ebp <- ebp_csv$ebp
df$gz <- ebp_csv$gz_spread




