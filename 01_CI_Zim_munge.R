setwd("C:/Users/jStephens/Documents/Zim")

df <-read_xlsx("Master custom report template_FY21Q2_results.xlsx", sheet="Data")
view(df)
glimpse(df)
names(df)
names(df3)



df3 <- df %>% 
  clean_names() %>% 
  mutate(operatingunit="Zimbabwe", orgunit=NA, orgunitid=NA) %>% 
  subset(indicator==c("DREAMS_FP", "DREAMS_GEND_NORM", "GEND_GBV", "TX_NEW_VERIFY", "TX_CURR_VERIFY", "TX_PVLS_VERIFY", "TX_RTT_VERIFY", 
         "TX_PVLS_VERIFY","TX_PVLS_SAMPLE", "TX_PVLS_RESULT_DOCUMENTED", "PMTCT_EID_ELIGIBLE", "PMTCT_EID_SAMPLE", "PMTCT_EID_RESULT_DOCUMENTED", 
         "PrEP_SCREEN", "PrEP_ELIGIBLE", "PrEP_1MONTH", "PrEP_NEW_VERIFY", "PrEP_CURR_VERIFY", "SC_ARVDISP", "SC_LMIS", "SC_CURR", 
         "VMMC_AE", "OVC_OFFER", "OVC_ENROLL")) %>% 
  rename(mech_code=mechanism_id, partner=partner_name, numdenom=numerator_denominator, population=population_type, otherdisaggregate=other_disaggregate,
        val=result_value) %>% 
  select(c("period", "orgunit", "orgunitid", "mech_code", "partner", "operatingunit", "psnu", "indicator", "sex", "age", "population", "otherdisaggregate", "numdenom", "val")) %>% 

  mutate(indicator=ifelse(indicator=="TX_PVLS_ELIGIBLE" & population=="OVC", "OVC_VL_ELIGIBLE", 
                            ifelse(indicator=="TX_PVLS_RESULT_DOCUMENTED" & population=="OVC", "OVC_VLR", 
                                   ifelse(indicator=="TX_PVLS_VERIFY" & population=="OVC", "OVC_VLS", 
                                   indicator)))) %>% 
    view

  # mutate(indicatortype=ifelse(indicator=="OVC_OFFER"
  #                             |indicator== "OVC_ENROLL"
  #                             |indicator== "OVC_VL_ELIGIBLE"
  #                             |indicator== "OVC_VLS"
  #                             |indicator=="OVC_VLR", "OVC", NA) %>% 
  
  mutate(reportingperiod=ifelse(period>="2020-10-01" & period<="2020-12-31",
                                 "FY21 Q1",  
                         ifelse(period>="2021-01-01" & period<="2021-03-31",
                                 "FY21 Q2", 
                         ifelse(period>="2021-04-01" & period<="2021-06-30",
                                  "FY21 Q3", 
                         ifelse(period>="2021-07-01" & period<="2021-09-30",
                                  "FY21 Q4", 
                         # ifelse(period>="2020-10-01" & period<="2021-03-31" & indicatortype=="OVC",
                         #              "FY21 Q1 - Q2", 
                         # ifelse(period>="2021-04-01" & period<="2021-09-30" & indicatortype=="OVC",
                         #             "FY21 Q3 - Q4",            
                                   NA))))) %>% 
  view



 

  
  
  # semi-annual indicators
  #   OVC_OFFER
  #   OVC_ENROLL
  #   OVC_VL_ELIGIBLE (<18) == TX_PVLS_ELIGIBLE
  #   OVC_VLR (<18) === TX_PVLS_RESULT_DOCUMENTED
  #   OVC_VLS (<18) === TX_PVLS_VERIFY
  
  

# date <- df %>% 
#   clean_names() %>% 
#   mutate(reportingperiod=ifelse(period %in% "2020-10-01":"2020-12-31",
#                                 "FY21 Q1",  
#                                 ifelse(period %in% "2021-01-01":"2021-03-31",
#                                        "FY21 Q2", 
#                                        ifelse(period %in% "2021-04-01":"2021-06-30",
#                                               "FY21 Q3", 
#                                               ifelse(period %in% "2021-07-01":"2021-09-30",
#                                                      "FY21 Q4",         
#                                                      NA))))) %>% 
#   view



