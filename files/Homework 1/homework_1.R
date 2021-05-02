#install.packages("devtools")
#devtools::install_github("algopoly/EVDS")

library(tidyverse)
library(EVDS)
library(lubridate)
library(readxl)
library(zoo)

set_evds_key("5qNUJJB1Tr")

# Question is "Are unemployment rate statistics related to exchange rate of dollar, interest rates and USD reserves of CBRT?" 

# TP.TIG08 => Unemployment Rate (%) (Level)
# TP.DK.USD.A => USD Dollar Exchange Rate (Buying Rate)
# TP.KTFTUK => Weighted Average Interest Rates Applied to Loans Extended by Banks - Consumer Credit (Level)
# TP.AB.C2 => CBRT Foreign Exchange Reserves (Million USD Dollar)

# This code is not working as the web service of EVDS have a problem in connection.
##data <- get_series(series = c("TP.TIG08", "TP.DK.USD.A", "TP.KTFTUK", "TP.AB.C2"), start_date = "01-01-2017", end_date = "01-01-2021")
##df <- as_tibble(data$items) %>% 
##  select(-UNIXTIME) %>% 
##  na.omit() %>% 
##  rename(DATE = Tarih,
##         UNEMP_RATE = TP_TIG08,
##         USD_TRY = TP_DK_USD_A, 
##         INT_RATE = TP_KTFTUK, 
##         USD_RESERVE = TP_AB_C2)

df <- read_excel("EVDS_data.xlsx") %>% 
  rename(DATE = Tarih,
         UNEMP_RATE = TP_TIG08,
         USD_TRY = TP_DK_USD_A,
         INT_RATE = TP_KTFTUK,
         USD_RESERVE = TP_AB_C2)

df2 <-
  df %>% 
  pivot_longer(-DATE, names_to = "KEY", values_to = "VALUE") %>% 
  mutate(VALUE = as.numeric(VALUE),
         DATE = as.yearmon(DATE))

#Line plot for all measures
ggplot(df2, aes(x=DATE,y=VALUE, colour = KEY, group = 1)) +
  geom_line() + 
  facet_wrap(vars(KEY), scales = "free_y", dir  = "v") +
  theme_bw()+
  theme(legend.position="none" , legend.background = element_rect(fill="gray", linetype="solid")) + 
  labs(y=NULL) +
  scale_color_brewer("", palette = "Set1") 

#"ISSIZLIK" search data from Google Trends and its line plot 
gtrends <- read_csv("issizlik_google_trends.csv") %>% mutate(DATE = as_date(DATE, format = "%d.%m.%Y"))

ggplot(gtrends, aes(x=DATE,y=SEARCH_COUNT)) +
  geom_line() + 
  theme_bw()+
  theme(legend.position="none" , legend.background = element_rect(fill="gray", linetype="solid")) + 
  labs(y=NULL, title = "ISSIZLIK SEARCH COUNT") +
  scale_color_brewer("", palette = "Set1")

#Line plot for unemployment measure
ggplot(df2 %>% filter(KEY == "UNEMP_RATE"), aes(x=DATE,y=VALUE, colour = KEY, group = 1)) +
  geom_line() + 
  theme_bw()+
  theme(legend.position="none" , legend.background = element_rect(fill="gray", linetype="solid")) + 
  labs(y=NULL, title = "UNEMPLOYMENT RATE") +
  scale_color_brewer("", palette = "Set1") 




