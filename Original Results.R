rm(list = ls())
################################################################################
############### Armar Instrumento: MP 'Surprises'###############################
################################################################################
library(tidyverse)
library(fredr)
library(readxl)
library(lubridate)
library(foreach)
library(gridExtra)
library(magrittr)
library(tseries)
library(openxlsx)
library(ggpubr)
library(gridExtra)
library(ivreg)
library(lmtest)
library(patchwork)
library(sandwich)


key <- "c27bf13d09598a184acdcb2ba94aa28f"
fredr_set_key(key)
fredr_has_key() 

fedfunds <- fredr_series_observations(series_id = "FEDFUNDS",
                                      observation_start = as.Date("1997-08-31"),
                                      observation_end = as.Date("2024-03-31"),
                                      frequency = "m",
                                      aggregation_method = "sum",
                                      units = "lin")
fedfunds <- fedfunds %>%
  rename(fedfunds = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

BOGMBASE <- fredr_series_observations(series_id = "BOGMBASE",
                                      observation_start = as.Date("1997-08-01"),
                                      observation_end = as.Date("2024-03-31"),
                                      frequency = "m",
                                      aggregation_method = "sum",
                                      units = "lin")
ldBOGMBASE <- BOGMBASE %>%
  rename(BOGMBASE = value) %>%
  mutate(lBOGMBASE = log(BOGMBASE)) %>%
  mutate(ldBOGMBASE = 100*(lBOGMBASE - lag(lBOGMBASE))) %>%
  select(-series_id, -realtime_start, -realtime_end)


RSXFS <- fredr_series_observations(series_id = "RSXFS",
                                   observation_start = as.Date("1997-08-01"),
                                   observation_end = as.Date("2024-03-31"),
                                   frequency = "m",
                                   aggregation_method = "sum",
                                   units = "lin")
ldRSXFS <- RSXFS %>%
  rename(RSXFS = value) %>%
  mutate(lRSXFS = log(RSXFS)) %>%
  mutate(ldRSXFS = 100*(lRSXFS - lag(lRSXFS))) %>%
  select(-series_id, -realtime_start, -realtime_end)




shadow <- read_excel("C:/Users/andro/Downloads/WuXiaShadowRate.xlsx", 
                     sheet = "DataOriginal")
shadow <- shadow %>%
  rename(date = ...1)



cpi <- fredr_series_observations(series_id = "CPIAUCSL",
                                 observation_start = as.Date("1997-08-01"),
                                 observation_end = as.Date("2024-03-31"),
                                 frequency = "m",
                                 aggregation_method = "sum",
                                 units = "lin")
ldcpi <- cpi %>%
  rename(cpi = value) %>%
  mutate(lcpi = log(cpi)) %>%
  mutate(ldcpi = 100*(lcpi - lag(lcpi))) %>%
  select(-series_id, -realtime_start, -realtime_end)

pchCPI <- fredr_series_observations(series_id = "CPIAUCSL",
                                    observation_start = as.Date("1997-08-01"),
                                    observation_end = as.Date("2024-03-31"),
                                    frequency = "m",
                                    aggregation_method = "avg",
                                    units = "pch")
pchCPI <- pchCPI %>%
  rename(pchCPI = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

INDPROD <- fredr_series_observations(series_id = "INDPRO",
                                     observation_start = as.Date("1997-08-01"),
                                     observation_end = as.Date("2024-03-31"),
                                     frequency = "m",
                                     aggregation_method = "avg",
                                     units = "lin")
ldINDPROD <- INDPROD %>%
  rename(INDPROD = value) %>%
  mutate(lINDPROD = log(INDPROD)) %>%
  mutate(ldINDPROD = 100*(lINDPROD - lag(lINDPROD))) %>%
  select(-series_id, -realtime_start, -realtime_end, -INDPROD)

pchINDPROD <- fredr_series_observations(series_id = "INDPRO",
                                        observation_start = as.Date("1997-08-01"),
                                        observation_end = as.Date("2024-03-31"),
                                        frequency = "m",
                                        aggregation_method = "avg",
                                        units = "pch")
pchINDPROD <- pchINDPROD %>%
  rename(pchINDPROD = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

EXPINF1YR <- fredr_series_observations(series_id = "EXPINF1YR",
                                       observation_start = as.Date("1997-08-01"),
                                       observation_end = as.Date("2024-03-31"),
                                       frequency = "m",
                                       aggregation_method = "avg",
                                       units = "lin")
EXPINF1YR <- EXPINF1YR %>%
  rename(EXPINF1YR = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

EXPINF2YR <- fredr_series_observations(series_id = "EXPINF2YR",
                                       observation_start = as.Date("1997-08-01"),
                                       observation_end = as.Date("2024-03-31"),
                                       frequency = "m",
                                       aggregation_method = "avg",
                                       units = "lin")
EXPINF2YR <- EXPINF2YR %>%
  rename(EXPINF2YR = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

DGS10 <- fredr_series_observations(series_id = "DGS10",
                                   observation_start = as.Date("1997-08-01"),
                                   observation_end = as.Date("2024-03-31"),
                                   frequency = "m",
                                   aggregation_method = "avg",
                                   units = "lin")
DGS10 <- DGS10 %>%
  rename(DGS10 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

DGS1 <- fredr_series_observations(series_id = "DGS1",
                                  observation_start = as.Date("1997-08-01"),
                                  observation_end = as.Date("2024-03-31"),
                                  frequency = "m",
                                  aggregation_method = "avg",
                                  units = "lin")
DGS1 <- DGS1 %>%
  rename(DGS1 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

DGS2 <- fredr_series_observations(series_id = "DGS2",
                                  observation_start = as.Date("1997-08-01"),
                                  observation_end = as.Date("2024-03-31"),
                                  frequency = "m",
                                  aggregation_method = "avg",
                                  units = "lin")
DGS2 <- DGS2 %>%
  rename(DGS2 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

DGS5 <- fredr_series_observations(series_id = "DGS5",
                                  observation_start = as.Date("1997-08-01"),
                                  observation_end = as.Date("2024-03-31"),
                                  frequency = "m",
                                  aggregation_method = "avg",
                                  units = "lin")
DGS5 <- DGS5 %>%
  rename(DGS5 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

DGS30 <- fredr_series_observations(series_id = "DGS30",
                                   observation_start = as.Date("1997-08-01"),
                                   observation_end = as.Date("2024-03-31"),
                                   frequency = "m",
                                   aggregation_method = "avg",
                                   units = "lin")
DGS30 <- DGS30 %>%
  rename(DGS30 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

BAA10Y <- fredr_series_observations(series_id = "BAA10Y",
                                    observation_start = as.Date("1997-08-01"),
                                    observation_end = as.Date("2024-03-31"),
                                    frequency = "m",
                                    aggregation_method = "avg",
                                    units = "lin")
BAA10Y <- BAA10Y %>%
  rename(BAA10Y = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

MORTGAGE30US <- fredr_series_observations(series_id = "MORTGAGE30US",
                                          observation_start = as.Date("1997-08-01"),
                                          observation_end = as.Date("2024-03-31"),
                                          frequency = "m",
                                          aggregation_method = "avg",
                                          units = "lin")
MORTGAGE30US <- MORTGAGE30US %>%
  rename(MORTGAGE30US = value) %>%
  select(-series_id, -realtime_start, -realtime_end)






ppi <- fredr_series_observations(series_id = "PPIIDC",
                                 observation_start = as.Date("1997-08-01"),
                                 observation_end = as.Date("2024-03-31"),
                                 frequency = "m",
                                 aggregation_method = "avg",
                                 units = "lin")
ppi <- ppi %>%
  rename(ppi = value) %>%
  mutate(lppi = log(ppi)) %>%
  mutate(ldppi = 100*(lppi - lag(lppi))) %>%
  select(-series_id, -realtime_start, -realtime_end, -ppi)

econacti <- fredr_series_observations(series_id = "USPHCI",
                                      observation_start = as.Date("1997-08-01"),
                                      observation_end = as.Date("2024-03-31"),
                                      frequency = "m",
                                      aggregation_method = "avg",
                                      units = "lin")
econacti <- econacti %>%
  rename(econacti = value) %>%
  mutate(leconacti = log(econacti)) %>%
  mutate(ldeconacti = 100*(leconacti - lag(leconacti))) %>%
  select(-series_id, -realtime_start, -realtime_end, -econacti)

UNRATE <- fredr_series_observations(series_id = "UNRATE",
                                    observation_start = as.Date("1997-08-01"),
                                    observation_end = as.Date("2024-03-31"),
                                    frequency = "m",
                                    aggregation_method = "avg",
                                    units = "lin")
UNRATE <- UNRATE %>%
  rename(unrate = value) %>%
  mutate(lunrate = log(unrate)) %>%
  mutate(ldunrate = (unrate - lag(unrate)))%>%
  select(-series_id, -realtime_start, -realtime_end)

REVOLSL <- fredr_series_observations(series_id = "TOTALSL",
                                     observation_start = as.Date("1997-08-01"),
                                     observation_end = as.Date("2024-03-31"),
                                     frequency = "m",
                                     aggregation_method = "avg",
                                     units = "pc1")
REVOLSL <- REVOLSL %>%
  rename(revolsl = value) %>%
  select(-series_id, -realtime_start, -realtime_end)

REVOLSL <- REVOLSL[-1,]

VIX <- fredr_series_observations(series_id = "VIXCLS",
                                 observation_start = as.Date("1997-08-01"),
                                 observation_end = as.Date("2024-03-31"),
                                 frequency = "m",
                                 aggregation_method = "avg",
                                 units = "lin")
VIX <- VIX %>%
  rename(VIX = value) %>%
  mutate(lvix = log(VIX)) %>%
  mutate(ldvix = 100*(lvix - lag(lvix))) %>%
  select(-series_id, -realtime_start, -realtime_end, )

WTI <- fredr_series_observations(series_id = "DCOILWTICO",
                                 observation_start = as.Date("1997-08-01"),
                                 observation_end = as.Date("2024-03-31"),
                                 frequency = "m",
                                 aggregation_method = "avg",
                                 units = "lin")
WTI <- WTI %>%
  rename(WTI = value) %>%
  mutate(lwti = log(WTI)) %>%
  mutate(ldwti = 100*(lwti - lag(lwti))) %>%
  select(-series_id, -realtime_start, -realtime_end, -WTI)

guncert <- fredr_series_observations(series_id = "GEPUCURRENT",
                                     observation_start = as.Date("1997-08-01"),
                                     observation_end = as.Date("2024-03-31"),
                                     frequency = "m",
                                     aggregation_method = "avg",
                                     units = "chg")
guncert <- guncert %>%
  rename(guncert = value) %>%
  mutate(lguncert = log(guncert)) %>%
  mutate(ldguncert = lguncert - lag(lguncert)) %>%
  select(-series_id, -realtime_start, -realtime_end, -lguncert)

G7 <- fredr_series_observations(series_id = "G7PRMNTO01GPSAM",
                                observation_start = as.Date("1997-08-01"),
                                observation_end = as.Date("2024-03-31"),
                                frequency = "m",
                                aggregation_method = "avg",
                                units = "chg")
G7 <- G7 %>%
  rename(G7 = value) %>%
  select(-series_id, -realtime_start, -realtime_end)


PAYEMS <- fredr_series_observations(series_id = "PAYEMS",
                                    observation_start = as.Date("1997-08-01"),
                                    observation_end = as.Date("2024-03-31"),
                                    frequency = "m",
                                    units = "chg")
PAYEMS<- PAYEMS %>%
  rename(PAYEMS = value) %>%
  select(-series_id, -realtime_start, -realtime_end)


SP500 <- fredr_series_observations(series_id = "SP500",
                                   observation_start = as.Date("1997-08-01"),
                                   observation_end = as.Date("2024-03-31"),
                                   frequency = "m",
                                   units = "lin")
SP500 <- SP500 %>%
  rename(SP500 = value) %>%
  mutate(lSP500 = log(SP500)) %>%
  mutate(ldSP500 = 100*(lSP500 - lag(lSP500))) %>%
  select(-series_id, -realtime_start, -realtime_end)




uncert <- read_excel("Global_Policy_Uncertainty_Data.xlsx")
uncert <- uncert %>%
  rename(Period = periodo, uncert = GEPU_ppp) %>%
  mutate(Period = as.Date(Period))



gscpi_data <- read_excel("C:/Users/andro/Downloads/gscpi_data.xls", 
                         sheet = "Hoja1")

karadi_shocks <- read.csv("C:/Users/andro/OneDrive/Escritorio/NLLP-IV/shocks_fed_jk_m.csv")
karadi_shocks$date <- seq(from = as.Date("1997-08-01"), by = "month", length.out = nrow(karadi_shocks))

# View the dataset with the new date variable
tail(karadi_shocks)

gscpi_data <- gscpi_data %>%
  rename(Period = periodo, GSCPI = GSCPI) %>%
  mutate(Period = as.Date(Period)) 

gscpi_data$Period

ebp_csv <- read_csv("ebp_csv.csv")
sp500 <- read_xlsx("sp500.xlsx")
sp500 <- as.data.frame(sp500)
sp500 <- sp500 %>%
  mutate(lsp500 = log(sp500)) %>%
  mutate(ldsp500 = log(sp500) - lag(log(sp500)))

mp_surprises <- read_excel("monetary-policy-surprises-data.xlsx", 
                           sheet = "Monthly (update 2023)")






#gscpi_data <- gscpi_data[-1,]

################################################################################
# IV: Transformación de variables y construcción de sorpresas
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
  left_join(SP500, by = "date")
df <- df[-1,]


#' @name Trigger_Variables
#' @title Compute values of transition function to separate regimes for 2 different trigger variables
#' @description Computes transition values by using a smooth transition function as
#' used in Auerbach and Gorodnichenko (2012). The time series used in the transition function
#' can be detrended via the Hodrick-Prescott filter (see Auerbach and Gorodnichenko, 2013).
#' @param triggers A numeric vector or a panel data set, depending on the model to estimate:
#' z is the 1st trigger and w is the 2nd trigger
#' @param specs A \link{list} with inputs:
#' @param lambda_z: Lambda parameter for z trigger in the HP filter. Must be numeric
#' @param lambda_w: Lambda parameter for w trigger in the HP filter. Must be numeric
#' @param gamma_w:  Gamma parameter of w in the logistic function. Must be numeric
#' @param gamma_z:  Gamma parameter of w in the logistic function. Must be numeric  
#' @param use_hp_z: Use Hodrick-Prescott filter for z. TRUE or FALSE
#' @param use_hp_w: Use Hodrick-Prescott filter for W. TRUE or FALSE
#' @param lag_switching_z: Use logistic function for z. TRUE or FALSE
#' @param lag_switching_w: Use logistic function for w. TRUE or FALSE
#'
#' 
#' @return A numeric vector with values from the smooth transition function \eqn{F(z_{t-1})}.}
#' @author Andro Asatashvili


trigger_z <- function(z,specs){
  
  specs <- list()
  specs$lambda_z        <- lambda_z
  specs$gamma_z         <- gamma_z
  specs$use_hp_z        <- use_hp_z
  specs$lag_switching_z <- lag_switching_z
  z                     <- as.data.frame(z)
  names(z)              <- "z_var"
  
  if(specs$use_hp_z == TRUE){
    
    filter_results_z     <-   lpirfs::hp_filter(as.matrix(z), specs$lambda_z)
    gamma_z              <-   specs$gamma_z
    z_0_z                <-   as.numeric(scale(filter_results_z[[1]], center = TRUE))
    fz_z                 <-   (1 - exp(((-1)*gamma_z*z_0_z))/(1 + (exp((-1)*gamma_z*z_0_z))))
    
    # Use first lag of value from szitching function?
    if(isTRUE(specs$lag_switching_z)){
      
      fz_z               <-     (1 - exp(((-1)*gamma_z*(dplyr::lag(z_0_z, 1))))
                                 /
                                   (1 + (exp((-1)*gamma_z*(dplyr::lag(z_0_z, 1))))))
      
    }
    
    
  } 
  
  else  {
    
    fz_z               <-  (1 - exp((-1)*specs$gamma_z*z$z_var)
                            /
                              (1 + exp((-1)*specs$gamma_z*z$z_var)))
    
    # Use first lag of value from szitching function var1?
    if(isTRUE(specs$lag_switching_z)){
      
      
      fz_z               <-    dplyr::lag(fz_z, 1)
      
      
      
    }
    
    
  }
  
  
  
  return(fz_z)
}


###############################################################################
# 2nd trigger var: w
###############################################################################

trigger_w <- function(w,spec){
  spec                 <- list()
  spec$lambda_w        <- lambda_w
  spec$gamma_w         <- gamma_w
  spec$use_hp_w        <- use_hp_w
  spec$lag_switching_w <- lag_switching_w
  w                    <- as.data.frame(w)
  names(w)             <- "w_var"
  
  if(spec$use_hp_w == TRUE){
    
    filter_results_w     <-   lpirfs::hp_filter(as.matrix(w), spec$lambda_w)
    gamma_w              <-   spec$gamma_w
    w_0_w                <-   as.numeric(scale(filter_results_w[[1]], center = TRUE))
    fz_w                 <-   (1 - exp(((-1)*gamma_w*w_0_w))
                               /
                                 (1 + (exp((-1)*gamma_w*w_0_w))))
    
    # Use first lag of value from switching function?
    if(isTRUE(spec$lag_switching_w)){
      
      fz_w               <-     (1 - exp(((-1)*gamma_w*(dplyr::lag(w_0_w, 1))))
                                 /
                                   (1 + (exp((-1)*gamma_w*(dplyr::lag(w_0_w, 1))))))
      
    }
    
    
  } 
  
  else  {
    
    fz_w               <-  (1 - exp((-1)*spec$gamma_w*w$w_var)
                            /
                              (1 + exp((-1)*spec$gamma_w*w$w_var)))
    
    # Use first lag of value from switching function var1?
    if(isTRUE(spec$lag_switching_w)){
      
      fz_w               <-    dplyr::lag(fz_w, 1)
      
    }
    
    
  }
  
  
  
  return(fz_w)
  
}


###############################################################################
###############################################################################
#          Variables
###############################################################################
###############################################################################

###############################################################################
###############################################################################
#          Instrumento y Primer etapa
###############################################################################
###############################################################################

v <- lm(dshadow ~ ldINDPROD + ldcpi + ldppi + dEXPINF1YR + dDGS10 , data = df)$residuals







iv <- lm(dDGS1 ~ v  + dDGS5 + dDGS10 + dDGS30 + dBAA10Y+ lag(ldcpi)   + lag(v) + dEXPINF1YR+
           ldINDPROD + ldppi + ldeconacti, data = df)$fitted.values
iv <- data.frame(iv = iv)
#row.names(iv) <- 1:307
#iv <- iv[-1,]
iv <-
  iv <- data.frame(iv = iv)

# Horizonte
H <- 24

# variable dependiente y creamos "long-difference"
y <- data.frame(df$lcpi)
for (i in 0:H) {
  y <- y %>%
    mutate(!!paste0("y_cum_", i) := 100 * (lead(df$lcpi, i) - lag(df$lcpi, 1)))
}
y <- y[-c(1:1),]

# vector de controles NO invariantes
x <- data.frame(    ldcpi_lag1 = lag(df$ldcpi, 1),
                    ldcpi_lag2 = lag(df$ldcpi, 2),
                    ldcpi_lag3 = lag(df$ldcpi, 3),
                    ldcpi_lag4 = lag(df$ldcpi, 4),
                    ldcpi_lag5 = lag(df$ldcpi, 5),
                    ldcpi_lag6 = lag(df$ldcpi, 6),
                    ldcpi_lag7 = lag(df$ldcpi, 7),
                    ldcpi_lag8 = lag(df$ldcpi, 8),
                    ldcpi_lag9 = lag(df$ldcpi, 9),
                    ldcpi_lag10 = lag(df$ldcpi, 10),
                    ldcpi_lag11 = lag(df$ldcpi, 11),
                    ldcpi_lag12 = lag(df$ldcpi, 12),
                    
                    d = df$dEXPINF1YR,
                    f = df$ldINDPROD,
                    h = (df$ldppi),
                    aa = df$ldeconacti,
                    kkkk = df$ldvix
)


v <- data.frame(iv = iv )



x <- x[-c(1:1),]
x <- cbind(x,v)
# Combine x_lagged with iv dataframe (assuming iv is already defined)
#x <- cbind(x, iv)


# vector de controles exógenos
exog <- data.frame( a = df$dDGS5,
                    b = df$dDGS10,
                    ldwti = df$ldwti,
                    aaaa = df$dBAA10Y)
exog <- exog[-c(1:1),]

# Variable endogena

#data <- cbind(y,x,exog)
#data <- data[-1,]


###############################################################################
###############################################################################
#          Regimenes y Trigger variables
###############################################################################
###############################################################################

z <- gscpi_data$GSCPI
w <- REVOLSL$revolsl

# Logistic Functions Specs: Check trigger function

lambda_w <- 129600
lambda_z <- 129600
gamma_w  <- 8
gamma_z  <- 6
use_hp_z <- TRUE
use_hp_w <- TRUE
lag_switching_z <- TRUE
lag_switching_w <- TRUE

z <- trigger_z(z)
w <- trigger_w(w)

z <- z[-c(1:1)]
w <- w[-c(1:1)]

r1 <- ((1-z)*(1-w))
r2 <- ((1-z)*w)
r3 <- (z*(1-w))
r4 <- (z*w)

R1 <- z
R2 <- (1-z)


################## Transformamos datos ################################
x_nl_r1 <- r1 * x
x_nl_r2 <- r2 * x
x_nl_r3 <- r3 * x
x_nl_r4 <- r4 * x

x_nl_R1 <- R1*x
x_nl_R2 <- R2*x

colnames(x_nl_r1) <- paste0(colnames(x), "_r1")
colnames(x_nl_r2) <- paste0(colnames(x), "_r2")
colnames(x_nl_r3) <- paste0(colnames(x), "_r3")
colnames(x_nl_r4) <- paste0(colnames(x), "_r4")

# Create interaction terms for iv
iv_r1 <- iv * r1
iv_r2 <- iv * r2
iv_r3 <- iv * r3
iv_r4 <- iv * r4

colnames(iv_r1) <- paste0("iv_r1_", seq_along(iv_r1))
colnames(iv_r2) <- paste0("iv_r2_", seq_along(iv_r2))
colnames(iv_r3) <- paste0("iv_r3_", seq_along(iv_r3))
colnames(iv_r4) <- paste0("iv_r4_", seq_along(iv_r4))


dataset <- cbind(y, x_nl_r1, x_nl_r2, x_nl_r3, x_nl_r4, exog)
#dataset <- cbind(y,dataset,iv_r1, iv_r2, iv_r3, iv_r4, exog)
dataset <- drop_na(dataset)




b_iv_r1 <- rep(0, H)
u_iv_r1 <- rep(0, H)
d_iv_r1 <- rep(0, H)

b_iv_r2 <- rep(0, H)
u_iv_r2 <- rep(0, H)
d_iv_r2 <- rep(0, H)

b_iv_r3 <- rep(0, H)
u_iv_r3 <- rep(0, H)
d_iv_r3 <- rep(0, H)

b_iv_r4 <- rep(0, H)
u_iv_r4 <- rep(0, H)
d_iv_r4 <- rep(0, H)

for (h in 0:H) {
  formula_str <- as.formula(paste0("y_cum_", h, " ~ ", 
                                   # paste(colnames(iv_r1), collapse = " + "), " + ",
                                   paste(colnames(x_nl_r1), collapse = " + "), " + ",
                                   # paste(colnames(iv_r2), collapse = " + "), " + ",
                                   paste(colnames(x_nl_r2), collapse = " + "), " + ",
                                   # paste(colnames(iv_r3), collapse = " + "), " + ",
                                   paste(colnames(x_nl_r3), collapse = " + "), " + ",
                                   # paste(colnames(iv_r4), collapse = " + "), " + ",
                                   paste(colnames(x_nl_r4), collapse = " + "), " + ",
                                   paste(colnames(exog), collapse = " + ")))
  model_iv <- lm(formula_str, data = dataset) 
  coeftest_model_iv <- coeftest(model_iv, vcov = NeweyWest(model_iv, lag = h))
  b_iv_r1[h] <- coef(coeftest_model_iv)["iv_r1"]
  u_iv_r1[h] <- confint(coeftest_model_iv, parm = "iv_r1", level = 0.90)[,2]
  d_iv_r1[h] <- confint(coeftest_model_iv, parm = "iv_r1", level = 0.90)[,1]
  
  b_iv_r2[h] <- coef(coeftest_model_iv)["iv_r2"]
  u_iv_r2[h] <- confint(coeftest_model_iv, parm = "iv_r2", level = 0.90)[,2]
  d_iv_r2[h] <- confint(coeftest_model_iv, parm = "iv_r2", level = 0.90)[,1]
  
  b_iv_r3[h] <- coef(coeftest_model_iv)["iv_r3"]
  u_iv_r3[h] <- confint(coeftest_model_iv, parm = "iv_r3", level = 0.90)[,2]
  d_iv_r3[h] <- confint(coeftest_model_iv, parm = "iv_r3", level = 0.90)[,1]
  
  b_iv_r4[h] <- coef(coeftest_model_iv)["iv_r4"]
  u_iv_r4[h] <- confint(coeftest_model_iv, parm = "iv_r4", level = 0.90)[,2]
  d_iv_r4[h] <- confint(coeftest_model_iv, parm = "iv_r4", level = 0.90)[,1]
  
}



plot_data <- data.frame(
  Horizon = 1:H,
  b_iv_r1 = b_iv_r1,
  u_iv_r1 = u_iv_r1,
  d_iv_r1 = d_iv_r1,
  b_iv_r2 = b_iv_r2,
  u_iv_r2 = u_iv_r2,
  d_iv_r2 = d_iv_r2,
  b_iv_r3 = b_iv_r3,
  u_iv_r3 = u_iv_r3,
  d_iv_r3 = d_iv_r3,
  b_iv_r4 = b_iv_r4,
  u_iv_r4 = u_iv_r4,
  d_iv_r4 = d_iv_r4
)


plot1 <- ggplot(plot_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = d_iv_r1, ymax = u_iv_r1), fill = "darkblue", alpha = 0.15) +
  geom_line(aes(y = b_iv_r1), color = "blue", size = 1) +
  geom_line(aes(y = 0), color = "black") +
  labs(
    y = "%",
    x = "Meses",
    title = "Bajo Estrés, Baja Deuda"
  ) +
  scale_y_continuous(limits = c(-30, 20)) +  # Adjust y-axis limits
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  # Increase x-axis title size and margin
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),  # Increase y-axis title size and margin
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)
  )

plot2 <- ggplot(plot_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = d_iv_r2, ymax = u_iv_r2), fill = "darkred", alpha = 0.15) +
  geom_line(aes(y = b_iv_r2), color = "red", size = 1) +
  geom_line(aes(y = 0), color = "black") +
  labs(
    y = "%",
    x = "Meses",
    title = "Alto Estrés, Baja Deuda"
  ) +
  scale_y_continuous(limits = c(-30, 20)) +  # Adjust y-axis limits
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  # Increase x-axis title size and margin
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),  # Increase y-axis title size and margin
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)
  )

plot3 <- ggplot(plot_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = d_iv_r3, ymax = u_iv_r3), fill = "darkgreen", alpha = 0.15) +
  geom_line(aes(y = b_iv_r3), color = "green", size = 1) +
  geom_line(aes(y = 0), color = "black") +
  labs(
    y = "%",
    x = "Meses",
    title = "Bajo Estrés, Alta Deuda"
  ) +
  scale_y_continuous(limits = c(-30, 20)) +  # Adjust y-axis limits
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  # Increase x-axis title size and margin
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),  # Increase y-axis title size and margin
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)
  )

plot4 <- ggplot(plot_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = d_iv_r4, ymax = u_iv_r4), fill = "darkorange", alpha = 0.15) +
  geom_line(aes(y = b_iv_r4), color = "orange", size = 1) +
  geom_line(aes(y = 0), color = "black") +
  labs(
    y = "%",
    x = "Meses",
    title = "Alto Estrés, Alta Deuda"
  ) +
  scale_y_continuous(limits = c(-30, 20)) +  # Adjust y-axis limits
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),  # Increase x-axis title size and margin
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),  # Increase y-axis title size and margin
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)
  )

grid.arrange(plot2, plot4, plot1, plot3, nrow = 2)