#line 316: period code with multiple attempts/errors

source("Scripts/00_setup.R")
setwd("./Data/CIRG FY21 Q3")
getwd()

# data from Q2
# df <- read_xlsx(path = "Data/Master custom report template_FY21Q2_results.xlsx",
#                 sheet = "Data",
#                 col_types = "text") %>%
#   janitor::clean_names() %>%
#   mutate(period = as.Date(as.integer(period),
#                           origin = "1900-01-01"),
#          result_value = as.integer(result_value))


#data for Q3 - line 60 attempt/errors to read in together
df_1 <- read_xlsx(path = "FHI360_ Master Custom Report FY21Q3_20.July.2021.xlsx",
                sheet = "Data",
                col_types = "text") %>%
  janitor::clean_names() %>%
  mutate(period = as.Date(as.integer(period),
                          origin = "1900-01-01"),
         result_value = as.integer(result_value))

df_2 <- read_xlsx(path = "GHSC-PSM Master custom report_FY21Q3.xlsx",
                sheet = "Data",
                col_types = "text") %>%
  janitor::clean_names() %>%
  mutate(period = as.Date(as.integer(period),
                          origin = "1900-01-01"),
         result_value = as.integer(result_value))

df_3 <- read_xlsx(path = "Master custom report template_CeSHHAR_FY21Q3.xlsx",
                sheet = "Data",
                col_types = "text") %>%
  janitor::clean_names() %>%
  mutate(period = as.Date(as.integer(period),
                          origin = "1900-01-01"),
         result_value = as.integer(result_value))

df_4 <- read_xlsx(path = "Master custom report template_FY21Q3 PSI Final_June.xlsx",
                  sheet = "June_Data",
                  col_types = "text") %>%
  janitor::clean_names() %>%
  mutate(period = as.Date(as.integer(period),
                          origin = "1900-01-01"),
         result_value = as.integer(result_value))

df_5 <- read_xlsx(path = "OPHID_Custom_Indicator_FY21Q3_report_16_07_2021_v2.xlsx",
                sheet = "Data",
                col_types = "text") %>%
  janitor::clean_names() %>%
  mutate(period = as.Date(as.integer(period),
                          origin = "1900-01-01"),
         result_value = as.integer(result_value))

df <- bind_rows(df_1,df_2,df_3,df_4, df_5, .id = "id")
write_tsv(df, "FY21Q3_Zimbabwe_CI_submissions_dirty", na = " ") #use to create pivots for QC



#consider loading via api? https://drive.google.com/drive/folders/1wyak7m6fNWeFfF5NOH7jkimSemNGo4x4
setwd("./Data/CIRG FY21 Q3")

file.list <- list.files(path = ".",pattern='*.xlsx', full.names = TRUE)

df.list<- file.list %>%
  map_dfr(function(file){
  print(file)
    sheet=if_else(str_detect(file, "Master custom report template_FY21Q3 PSI Final_June"),"June_Data", "Data")
    df=read_xlsx(path=file,
   sheet = sheet,
   col_types = "text") %>%
  janitor::clean_names() %>%
  mutate(period = as.Date(as.integer(period),
                          origin = "1900-01-01"),
         result_value = as.integer(result_value))
 return(df)
   })

glimpse(df.list)

#2021-07-01
#map_dfr loops through each file, return combines the dataset in the most recent loop to the last

#view(df)
glimpse(df)
names(df)
#
# df %>%
#   distinct(indicator) %>%
#   pull()
#
# df %>% distinct(indicator)
#
#   df %>% distinct(population_type)
#
#   df %>% distinct(other_disaggregate) %>% pull()
#
#   df6 %>% distinct(partner)




  ####################################################################
  #                       FORMAT DATA
  ####################################################################

##### Bring in MSD to give orgunituids

msd<-read_msd("C:/Users/jstephens/Documents/MSD/Zim_Genie_SITE_IM_MultipleOUs_Daily_89756ed1-21b5-46ad-854c-73803d9f23c4.txt")
msd_psnu<-msd %>%
  select(c("psnu", "psnuuid")) %>%
  distinct()
#add columns for OU (Zimbabwe), Orgunit (unknown) and Orgunitid (unknown)
  df2 <- df %>%
    mutate(operatingunit = "Zimbabwe",
           orgunit = psnu)
#add orgunitud for zim psnu's by zim msd'
df2_join<-df2 %>%
  left_join(msd_psnu,  by = c("psnu"="psnu"), `copy` = TRUE) %>%
  rename(orgunituid=psnuuid)
glimpse(df2_join)


  #df2 %>% view

  #check clean names and new columns
  # test <- df2 %>%
  #   filter(indicator == c("TX_PVLS_ELIGIBLE"))
  #
  # #test %>% view
  #
  # test <- df2 %>%
  #   subset(!is.na(other_disaggregate))
  #
  # #test %>% view
  #
  # test <- df2 %>%
  #   subset(!is.na(population_type))
  #
  # test %>%
  #   view
  #
  # df2 %>%
  #   group_by(population_type) %>%
  #   skim


  df3 <- df2_join %>%
    #pull only the CIGB indicators, none of Zim CI
    #If Zim changes indicators (ie OVC) or collects new indicators this will need to be updated
    filter(indicator %in% c("DREAMS_FP", "DREAMS_GEND_NORM", "GEND_GBV", "TX_NEW_VERIFY", "TX_CURR_VERIFY", "TX_PVLS_ELIGIBLE", "TX_PVLS_VERIFY",
           "TX_RTT_VERIFY", "TX_PVLS_SAMPLE", "TX_PVLS_RESULT_DOCUMENTED", "PMTCT_EID_ELIGIBLE", "PMTCT_EID_SAMPLE", "PMTCT_EID_SAMPLE_DOCUMENTED",
           "PrEP_SCREEN", "PrEP_ELIGIBLE", "PrEP_1MONTH", "PrEP_NEW_VERIFY", "PrEP_CURR_VERIFY", "SC_ARVDISP", "SC_LMIS", "SC_CURR",
           "VMMC_AE", "OVC_OFFER", "OVC_ENROLL")) %>%
    #name variables so the match CIGB template, expect for period which will occur when recoding period format
    rename(mech_code = mechanism_id,
           partner = partner_name,
           numdenom = numerator_denominator,
           population = population_type,
           otherdisaggregate = other_disaggregate,
           val = result_value) %>%
    #keep only the CIGB template variables (drop 5)
    select(!c(annual_target_value,id,
              starts_with("required"),
              starts_with("x")))
glimpse(df3)
  # df3 %>%
  #   distinct(indicator) %>%
  #   pull()
  # test <- df3 %>%
  #   filter(indicator %in% c("TX_PVLS_ELIGIBLE")) %>%
  #   view

  ####################################################################
  #                        OVC from PVLS - only q2/q4
  ####################################################################

  # TX_PVLS_ELIGIBLE ->                OVC_VL_ELIGIBLE
  # TX_PVLS_RESULT_DOCUMENTED ->       OVC_VLR
  # TX_PVLS_VERIFY ->                  OVC_VLS

 # df4<- df3 %>%
 #    mutate(indicator = case_when(
 #      indicator == "TX_PVLS_ELIGIBLE" & population == "OVC" ~ "OVC_VL_ELIGIBLE",
 #      indicator == "TX_PVLS_RESULT_DOCUMENTED" & population=="OVC" ~ "OVC_VLR",
 #      indicator=="TX_PVLS_VERIFY" & population=="OVC" ~ "OVC_VLS",
 #      TRUE ~ indicator
 #    ))
 #
 #  #confirm recoding worked
 #  test <- df3 %>%
 #     filter(indicator %in% c("TX_PVLS_ELIGIBLE") & population %in% c("OVC")) %>%
 #    view
 #  #count=314
 #  test <- df4 %>%
 #    filter(indicator %in% c("OVC_VL_ELIGIBLE")) %>%
 #    view
 #  #count 314
 #
 #  df4 %>%
 #    distinct(indicator) %>%
 #    pull()
 #  df5 %>%
 #           filter(indicator==c("TX_PVLS_ELIGIBLE")) %>%
 #          distinct(population) %>%
 #         pull()

df4<-df3
  ####################################################################
  #                        CLEANING/MUTATES ACROSS INDICATORS
  ####################################################################

  df5 <- df4 %>%
    mutate(
       #add numerator to all missing numerator, recode numdenom to N/D
         numdenom=ifelse(numdenom=="Numerator" |numdenom=="numerator"|is.na(numdenom), "N",
                  ifelse(numdenom=="Denominator" | numdenom=="denominator", "D", numdenom)),
        # Remove population of "Non-KP (general population)" for most indicator (only in PrEP - but not 1 mo & VERIFY vars)
       population = case_when(
         (indicator == "DREAMS_FP" | indicator == "DREAMS_GEND_NORM" | indicator == "GEND_GBV" | indicator == "OVC_ENROLL" | indicator == "OVC_OFFER" | indicator == "OVC_VL_ELIGIBLE" | indicator == "OVC_VLR" | indicator == "OVC_VLS" |
           indicator == "PMTCT_EID_ELIGIBLE" | indicator == "PMTCT_EID_SAMPLE_DOCUMENTED" | indicator == "PrEP_1MONTH" | indicator == "SC_ARVDISP" | indicator == "SC_CURR" | indicator == "SC_LMIS" |
           indicator == "TX_PVLS_ELIGIBLE" | indicator == "TX_PVLS_RESULT_DOCUMENTED" | indicator == "TX_PVLS_SAMPLE" | indicator == "VMMC_AE")
         & (population == "Non-KP (general population)") ~ NA_character_,
         (indicator == "OVC_VL_ELIGIBLE" | indicator == "OVC_VLR" | indicator == "OVC_VLS")  & (population== "OVC") ~ NA_character_,
         TRUE ~ population),
       sex=case_when(
         sex=="Females"~"Female",
         sex=="Males"~"Male",
         sex=="Not recorded"~NA_character_,
         TRUE~sex  ))



##test for recoding worked
  # test <- df4 %>%
  #    filter(indicator %in% c("OVC_VLR") & population %in% c("OVC")) %>%
  #   view
  ## count 168
  # test <- df5 %>%
  #    filter(indicator %in% c("OVC_VLR") & population %in% c("OVC")) %>%
  #   view
  ##count 0
  # test <- df5 %>%
  #    filter(indicator %in% c("OVC_VLR")) %>%
  #   view
  ## count 168

# df4 %>%
#   filter(indicator==c("TX_PVLS_ELIGIBLE")) %>%
#   distinct(population) %>%
#   pull()
# #[1] NA                                "Female sex workers (FSW)"        "Men who have sex with men (MSM)" "Transgender people (TG)"         "Non-KP (general population)
# df5 %>%
#   filter(indicator==c("TX_PVLS_ELIGIBLE")) %>%
#   distinct(population) %>%
#   pull()
# #NA
# df4 %>%
#   distinct(sex) %>%
#   pull()
# df5 %>%
#   distinct(sex) %>%
#   pull()



  #issues in  age
  # df5  %>%
  #   # filter(indicator==c("OVC_ENROLL")) %>%
  #   distinct(age) %>%
  #   pull()
  # [1] "Infant Age: 2-12mo" "Infant Age: <=2mo"  "10-14"              "15-19"              "20-24"              "25-29"
  # [7] "30-34"              "35-39"              "40-44"              "45-49"              "5-9"                "50+"
  # [13] "<1"                 "1-4"                "20/24"              NA                   "15-17"              "18+ caregiver"
  # [19] "18-20 OVC"          "30-36"              "30-35"              "<1 (Specific)"      "1-4 (Inclusive)"    "5-9 (Specific)"
  # [25] "10-14 (Specific)"   "15-17 (Specific)"   "15-19 (Specific)"   "20-24 (Specific)"   "OVC: 18-20"         "OVC: 18+ caregiver"
  # [31] "15 - 19"            "20 - 24"            "25 - 29"            "30 - 34"            "35 - 39"            "40 - 44"
  # [37] "45 - 49"            "0-1"

  # "OVC: 18-20"         "OVC: 18+ caregiver"    "18+ caregiver"


  df6 <- df5 %>%
      #drop from the string anything after "(" to remove (specific) & (inclusive)
    mutate(age= gsub("\\(.*","",age)%>%
             str_trim(side = "both")) %>%
    ##recode disaggs when removing ovc or caregiver from age
    mutate(otherdisaggregate=case_when(
      age=="OVC: 18+ caregiver"|age=="18+ caregiver" ~"Caregiver",
      age=="OVC:18-20"~"OVC",
      TRUE~otherdisaggregate)) %>%
    #drop characters from age (ovc specific age groups)
    mutate(age= gsub("OVC","",age)%>%
             str_trim(side = "both")) %>%
    mutate(age= gsub("caregiver","",age)%>%
             str_trim(side = "both")) %>%
    rowwise() %>%
    mutate(age= age %>%
             str_split(":") %>%
             unlist() %>%
             last() %>%
             str_trim(side = "both")) %>%
    ungroup() %>%
    mutate(age=  case_when(
      age=="20 - 24"~"20-24",
      age=="20/24"~"20-24",
      age=="25 - 29"~"25-29",
      age=="30 - 34"~"30-34",
      age=="30-36"~"30-34",
      age=="35 - 39"~"35-39",
      age=="40 - 44"~"40-44",
      age=="45 - 49"~"45-49",
      age=="0-1"~"<1",
      age=="15 - 19"~"15-19",
      age=="2-12mo"~"2-12 months",
      age=="<=2mo"~"<2 months",
      TRUE~age))


  ## check recode
  # df6 %>%
  #   distinct(age) %>%
  #   pull()


  ############################################################################################################


  ####################################################################
  #                        RECODE PERIOD
  ####################################################################

  # check periods
  df6 %>%
    distinct(period) %>%
    pull()

glimpse(df6)

# library(lubridate)

#need to also recode the ovc indicators to semi-annual periods
#previous version of code created "indicatortype" but that has been removed
#how to recode ovc periods without creating indicator type?

date <- df6 %>%
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


date <- df6 %>%
  mutate(reportingperiod=case_when(
    (period>="2020-10-01" & period<="2020-12-31")~"FY21 Q1",
    (period>="2021-01-01" & period<="2021-03-31")~"FY21 Q2",
    (period>="2021-04-01" & period<="2021-06-30")~"FY21 Q3",
    (period>="2021-07-01" & period<="2021-09-30")~"FY21 Q4",   TRUE~period))
rowwise() %>%
  select(indicator=contains("OVC")) %>%
  mutate(reportingperiod=case_when(
    (period>="2020-10-01" & period<="2021-03-31")~"FY21 Q1 - Q2"
    (period>="2021-04-01" & period<="2021-09-30")~"FY21 Q3 - Q4",
    TRUE~period))
  ungroup()


date <- df6 %>%
  mutate(reportingperiod=case_when(
    period %in% "2020-10-01":"2020-12-31"~"FY21 Q1",
    period %in% "2021-01-01":"2021-03-31"~"FY21 Q2",
    period %in% "2021-04-01":"2021-06-30"~"FY21 Q3",
    period %in% "2021-07-01":"2021-09-30"~ "FY21 Q4",
             TRUE~ period))

date <- df6 %>%
  mutate(reportingperiod=case_when(
    period %in% "2020-10-01":"2020-12-31"~"FY21 Q1",
    period %in% "2021-01-01":"2021-03-31"~"FY21 Q2",
    period %in% "2021-04-01":"2021-06-30"~"FY21 Q3",
    period %in% "2021-07-01":"2021-09-30"~ "FY21 Q4",
    TRUE~ period))


date <- df6 %>%
  mutate(reportingperiod=case_when(
    between(period,"2020-10-01","2020-12-31")~"FY21 Q1",
    between(period, "2021-01-01","2021-03-31")~"FY21 Q2",
    between(period, "2021-04-01","2021-06-30")~"FY21 Q3",
    between(period, "2021-07-01","2021-09-30")~ "FY21 Q4",
    TRUE~ NA_character_))


  ############################################################################################################





















  ####################################################################
  #                        DREAM_FP
  ####################################################################

  #keep age/sex in main dataset but remove the other diaggregate
  Dreams_agesex<- df6 %>%
    mutate(otherdisaggregate=ifelse(!is.na(age) & indicator=="DREAMS_FP", NA, otherdisaggregate)) %>%
    view
  #9142 obs


  #create subset of just the other disaggregates for dreams in order to add to main dataset with age/sex

  ##check if any otherdisaggs need to be recoded
  # df5 %>%
  #   filter(indicator==c("DREAMS_FP")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()

  Dreams_FP2<- df6 %>%
    subset(indicator==c("DREAMS_FP") & !is.na(otherdisaggregate)) %>%
    #  logic to make age and sex null if other disaggregate is not null,
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age),
           sex=ifelse(!is.na(otherdisaggregate), NA, sex))
    #consider using the opposite logic to make otherdisagg null if age or sex is not null, but currently no use case
  #2 obs

  #combine Dreams_FP2 with Dreams_agesex: row bind
  Dreams<-rbind(Dreams_agesex, Dreams_FP2)
  #9144 obs



  ####################################################################
  #                        DREAM_GEND_NORM
  ####################################################################

  # #check if any otherdisaggs need to be recoded
  # df5 %>%
  #   filter(indicator==c("DREAMS_GEND_NORM")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()
  # #[1] "Activity type: Small group" NA
  # #no changes to otherdisagg necessary

  # #review data for this indicator
  # Dreams_Gend<- Dreams %>%
  #   subset(indicator==c("DREAMS_GEND_NORM")) %>% #use subset to check work
  #   view
  # #36 have age/sex/other disagg and need to be separated

  #keep age/sex in main dataset but remove the other diaggregate
  Dreams_Gend_agesex<- Dreams %>%
    mutate(otherdisaggregate=ifelse(!is.na(age) & indicator=="DREAMS_GEND_NORM", NA, otherdisaggregate)) %>%
    view


  #create subset of just the other disaggregates for dreams in order to add to main dataset with age/sex
    #  logic to make age and sex null if other disaggregate is not null,
  Dreams_Gend_disagg<- Dreams %>%
    subset(indicator==c("DREAMS_GEND_NORM") & !is.na(otherdisaggregate)) %>%
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age),
           sex=ifelse(!is.na(otherdisaggregate), NA, sex)) %>%
    view


  #combine: row bind
  Dreams_Gend_Norm<-rbind(Dreams_Gend_agesex, Dreams_Gend_disagg)



  ####################################################################
  #                        GEND_GBV
  ####################################################################

  # Dreams_Gend_Norm %>%
  #   filter(indicator==c("GEND_GBV")) %>%
  #   distinct(population) %>%
  #   pull()
  # # [1] NA    "OVC"  -> only NA is allowable, recoded OVC to NA
  #
  # #check if any otherdisaggs need to be recoded
  # Dreams_Gend_Norm %>%
  #   filter(indicator==c("GEND_GBV")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()
  #[1] "Sexual violence - received PEP"        "Physical and/or emotional violence"    "Sexual violence - did not receive PEP"
  # [4] "Physical or emotional violence"


  # "Physical or emotional violence"   -->     "Physical and/or emotional violence"
  GEND_GBV<-Dreams_Gend_Norm %>%
    mutate(otherdisaggregate=case_when(
          indicator=="GEND_GBV" & otherdisaggregate=="Physical or emotional violence" ~ "Violence Service Type: Physical and/or emotional violence",
          TRUE~ otherdisaggregate),
          population=case_when(indicator=="GEND_GBV"~NA_character_,
          TRUE~population) )



  # Sexual violence - received PEP  -->   Violence Service Type: Sexual violence &  PEP: completed PEP
  #requires 2 steps to recode into 2 different disaggs


# keep sexual violence in main dataset
  GEND_GBV_type<- GEND_GBV %>%
    mutate(otherdisaggregate=ifelse(indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - received PEP", "Violence Service Type: Sexual violence", otherdisaggregate))

  #keep other disaggregates for pep in subset
  GEND_GBV_pep<- GEND_GBV %>%
    subset(indicator==c("GEND_GBV") & otherdisaggregate=="Sexual violence - received PEP") %>% #create subset of only the sexual violenxe - received pep to duplicate
    mutate(otherdisaggregate=ifelse(indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - received PEP", "PEP: Completed PEP", otherdisaggregate))

  #combine: row bind
  GEND_GBV<-rbind(GEND_GBV_type, GEND_GBV_pep)




  ####################################################################
  #                       OVC_ENROLL/OVC_OFFER
  ####################################################################

  # GEND_GBV %>%
  #   filter(indicator==c("OVC_OFFER")) %>%
  #   distinct(population) %>%
  #   pull()
## population ovc only - > recode to NA

  #check if any otherdisaggs need to be recoded
  # GEND_GBV %>%
  #   filter(indicator==c("OVC_OFFER")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()
  # GEND_GBV %>%
  #   filter(indicator==c("OVC_ENROLL")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()
  #no otherdisaggregates for these indicators
  # #Tableau identified -> other disaggreagate recoded to "OVC or Caregiver: OVC"


  OVC_ENROLL_OFFER<-GEND_GBV %>%
  mutate(otherdisaggregate=case_when(
    (indicator=="OVC_OFFER" | indicator=="OVC_ENROLL") ~ "OVC or Caregiver: OVC",
    TRUE~ otherdisaggregate),
    population=case_when((indicator=="OVC_OFFER" | indicator=="OVC_ENROLL")~NA_character_,
    TRUE~population))

  #   # OVC_ENROLL (d) = OVC_OFFER (n)
  #need to create a subest of the OVC_OFFER to duplicate as OVC_ENROLL D and add back to dataset
   OVC_ENROLL_OFFER_D<-OVC_ENROLL_OFFER %>%
    subset(indicator==c("OVC_OFFER")) %>%
    mutate(indicator=="OVC_ENROLL",numdenom=="D")

   OVC_ENROLL_OFFER<-rbind(OVC_ENROLL_OFFER, OVC_ENROLL_OFFER_D)

  * OVC_OFFER-> may need to bring in TX_CURR<20 (age/sex disagg) for denominator

  ####################################################################
  #                       OVC_VL_ELIGIBLE
  ####################################################################

#   #check if any otherdisaggs/pop need to be recoded
  # OVC_ENROLL_OFFER %>%
  #   filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()

# [1] "OVC"
# no changes to disagg

  # OVC_ENROLL_OFFER %>%
  #   filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
  #   distinct(population) %>%
  #   pull()
#NA
  #no changes

  ####################################################################
  #                       PMTCT_EID_ELIGIBLE/PMTCT_EID_SAMPLE_DOCUMENTED
  ####################################################################

  #check populations are NA
  # OVC_ENROLL_OFFER %>%
  #   filter(indicator==c("PMTCT_EID_SAMPLE_DOCUMENTED")) %>%
  #   distinct(population) %>%
  #   pull()

  # Error report: Recode "<01" to "0-12 months"

  # OVC_ENROLL_OFFER %>%
  #   filter(indicator==c("PMTCT_EID_ELIGIBLE")) %>%
  #   distinct(age) %>%
  #   pull()
#
#   OVC_ENROLL_OFFER %>%
#     filter(indicator==c("PMTCT_EID_SAMPLE_DOCUMENTED")) %>%
#     distinct(age) %>%
#     pull()
  #don't actually see <01 in this dataset, but its in the errors report for previous entry?

  PMTCT <- OVC_ENROLL_OFFER %>%
    mutate(age=  ifelse(indicator=="PMTCT_EID_ELIGIBLE"| indicator=="PMTCT_EID_SAMPLE_DOCUMENTED" & age=="<01", "0-12 months", age))

  ## check recode
  PMTCT %>%
    filter(indicator=="PMTCT_EID_ELIGIBLE"| indicator=="PMTCT_EID_SAMPLE_DOCUMENTED") %>%
    distinct(age) %>%
    pull()
# [1] "0-12 months" "2-12 months" "<2 months"


  ####################################################################
  #                       PREP_1MONTH
  ####################################################################

  # PMTCT %>%
  #   filter(indicator==c("PrEP_1MONTH")) %>%
  #   distinct(population) %>%
  #   pull()
  #should have NA as populations

  #Zim only collecting the 1st month. add this to the otherdisaggregate


  PREP_1MO<-PMTCT %>%
    mutate(otherdisaggregate=case_when(
      indicator=="PrEP_1MONTH"  ~  "Reporting Month: Month 1 of Reporting Quarter",
      TRUE~ otherdisaggregate),
      population=case_when(indicator=="PrEP_1MONTH" ~NA_character_,
                           TRUE~population) )



  PREP_1MO %>%
    filter(indicator==c("PrEP_1MONTH")) %>%
    distinct(population) %>%
    pull()


  ####################################################################
  #                       PREP_ELIGIBLE
  ####################################################################


  #no other disaggregates identified (pregnant/breastfeeding), no changes necessary at this time

  #prep_elg can have all populations -> no changes made
  # PREP_1MO %>%
  #    filter(indicator==c("PrEP_ELIGIBLE")) %>%
  #   distinct(population) %>%
  #   pull()
  # [1] NA                                "Men who have sex with men (MSM)" "Female sex workers (FSW)"
  # [4] "Transgender people (TG)"         "Non-KP (general population)"


  ####################################################################
  #                       PREP_SCREEN
  ####################################################################

#
#   #no other disaggregates, but population and age overlap
  # PREP_1MO %>%
  #   filter(indicator==c("PrEP_SCREEN")) %>%
  #   distinct(population) %>%
  #   pull()
  #some reporting age and other disaggregate together, need to separate

  # keep ages in in main dataset, but remove population type
  PrEP_SCREEN_age<- PREP_1MO %>%
    mutate(population=ifelse(!is.na(age)| !is.na(sex), NA, population))
  #obs 9497

  #keep population in subset, clean out age/sex
  PrEP_SCREEN_type<- PREP_1MO %>%
    filter(indicator==c("PrEP_SCREEN") & !is.na(population)) %>%
    mutate(age=ifelse(!is.na(population), NA, age),
           sex=ifelse(!is.na(population), NA, sex)) %>%
    view
  #  obs 68

  #combine: row bind
  PrEP_SCREEN<-rbind(PrEP_SCREEN_type, PrEP_SCREEN_age)



  ####################################################################
  #                       SC_ARVDISP
  ####################################################################

  # PrEP_SCREEN %>%
  #   filter(indicator==c("SC_ARVDISP")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()
  #
  # # [3] "ARV Category: DTG 10 bottles (30-count)" wont get added to cigb because not a valid disag - recode to other pediatric
  # # also checked age, sex, pop all correctly NA

  SC_ARVDISP<- PrEP_SCREEN %>%
    mutate(numdenom=ifelse(indicator=="SC_ARVDISP","N","N"),
           #recode from categories seen in excel pivot
           otherdisaggregate=ifelse(otherdisaggregate=="ARV Category: DTG 10 bottles (30-count)", "ARV Category: Other (Pediatric) bottles dispensed" , otherdisaggregate),
          #recode from errors in tableau report - note seen in this data
           otherdisaggregate=ifelse(otherdisaggregate=="NVP (Pediatric), (not including NVP 10) bottles", "ARV Category: NVP (Pediatric) bottles dispensed - not including NVP 10" , otherdisaggregate))

  #check recoding
  SC_ARVDISP %>%
    filter(indicator==c("SC_ARVDISP")) %>%
    distinct(otherdisaggregate) %>%
    pull()

  ####################################################################
  #                       SC_CURR
  ####################################################################
#   SC_ARVDISP %>%
#     filter(indicator==c("SC_CURR")) %>%
#     distinct(otherdisaggregate) %>%
#     pull()

# #sc_arvdisp fixed errors in sc_curr - code below is duplicative

    # SC_CURR<- SC_ARVDISP %>%
    # mutate(numdenom=ifelse(indicator=="SC_CURR","N","N"),
    #        otherdisaggregate=ifelse(otherdisaggregate=="ARV Category: DTG 10 bottles (30-count)", "ARV Category: Other (Pediatric) bottles dispensed" , otherdisaggregate)) %>%
    #        view

  ####################################################################
  #                       SC_LMIS
  ####################################################################
  #not currently collecting

    ####################################################################
    #                       TX_PVLS_ELIGIBLE
    ####################################################################
 ``## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
     # SC_ARVDISP %>%
    #   filter(indicator==c("TX_PVLS_ELIGIBLE")) %>%
    #   distinct(population) %>%
    #   pull()


    ####################################################################
    #                       TX_PVLS_RESULT_DOCUMENTED
    ####################################################################
  ``## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
  # SC_ARVDISP %>%
  #   filter(indicator==c("TX_PVLS_RESULT_DOCUMENTED")) %>%
  #   distinct(population) %>%
  #   pull()

    ####################################################################
    #                       TX_PVLS_SAMPLE
    ####################################################################
  ## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
  # SC_ARVDISP %>%
  #   filter(indicator==c("TX_PVLS_SAMPLE")) %>%
  #   distinct(population) %>%
  #   pull()

    ####################################################################
    #                       TX_PVLS_VERIFY
    ####################################################################
  ## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
  # SC_ARVDISP %>%
  #   filter(indicator==c("TX_PVLS_VERIFY")) %>%
  #   distinct(population) %>%
  #   pull()

    ####################################################################
    #                       OVC_VL_ELIGIBLE
    ####################################################################

  #already dropped population as OVC, only otherdisagg=OVC

    # SC_ARVDISP %>%
    #   filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
    #   distinct(otherdisaggregate) %>%
    #   pull()
# "OVC"-  appropriate other disagg -> no changes

  #18-20 not in CIGB --> recode to 18+
  # SC_ARVDISP %>%
  #   filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
  #   distinct(age) %>%
  #   pull()
  # # [1] "15-17" "5-9"   "18+"   "10-14" "1-4"   "18-20" "<1"
  #
  df6 %>%
    filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
    distinct(age) %>%
    pull()




    ####################################################################
    #                      OVC_VLR
    ####################################################################
  SC_ARVDISP %>%
    filter(indicator==c("OVC_VLR")) %>%
    distinct(age) %>%
    pull()
  #18-20 not in CIGB --> recode to 18+

    ####################################################################
    #                       OVC_VLS
    ####################################################################

  # SC_ARVDISP %>%
  #   filter(indicator==c("OVC_VLS")) %>%
  #   distinct(age) %>%
  #   pull()
  #18-20 not in CIGB --> recode to 18+



    ####################################################################
    #                       TX_RTT_VERIFY
    ####################################################################

#no clear issues in pivot

    ####################################################################
    #                       VMMC_AE
    ####################################################################

  #need to separate age from other disaggregates

  # keep ages in in main dataset, but remove population type
  VMMC_AE_age<- SC_ARVDISP %>%
    mutate(otherdisaggregate=ifelse(!is.na(age)| !is.na(sex), NA, otherdisaggregate))
  #obs 9497

  #keep population in subset, clean out age/sex
  VMMC_AE_type<- SC_ARVDISP %>%
    filter(indicator==c("VMMC_AE") & !is.na(otherdisaggregate)) %>%
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age),
           sex=ifelse(!is.na(otherdisaggregate), NA, sex))

  #combine: row bind
  VMMC_AE<-rbind(VMMC_AE_age, VMMC_AE_type)


  ####################################################################
  #                       EXPORT DATA AS EXCEL FILE
  ####################################################################


  write_tsv(VMMC_AE, "FY21Q3_Zimbabwe_CI_clean", na = " ")


