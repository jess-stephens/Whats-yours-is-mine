source("Scripts/00_setup.R")

df <- read_xlsx(path = "Data/Master custom report template_FY21Q2_results.xlsx",
                sheet = "Data",
                col_types = "text") %>%
  janitor::clean_names() %>%
  mutate(period = as.Date(as.integer(period),
                          origin = "1900-01-01"),
         result_value = as.integer(result_value))

#view(df)
glimpse(df)
names(df)

df %>%
  distinct(indicator) %>%
  pull()


df %>% distinct(indicator)

  df %>% distinct(population_type)

  df %>% distinct(other_disaggregate) %>% pull()




  ####################################################################
  #                       FORMAT DATA
  ####################################################################

   #add columns for OU (Zimbabwe), Orgunit (unknown) and Orgunitid (unknown)
  df2 <- df %>%
    mutate(operatingunit = "Zimbabwe",
           orgunit = NA,
           orgunitid = NA)

  #df2 %>% view

  #check clean names and new columns
  test <- df2 %>%
    filter(indicator == c("TX_PVLS_ELIGIBLE"))

  #test %>% view

  test <- df2 %>%
    subset(!is.na(other_disaggregate))

  #test %>% view

  test <- df2 %>%
    subset(!is.na(population_type))

  test %>%
    view

  df2 %>%
    group_by(population_type) %>%
    skim

  #where are the other population types seen in the excel file???



  df3 <- df2 %>%
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
    select(!c(annual_target_value,
              starts_with("required")))

  df3 %>%
    distinct(indicator) %>%
    pull()


  test <- df3 %>%
    filter(indicator %in% c("TX_PVLS_ELIGIBLE")) %>%
    view

  ####################################################################
  #                        OVC from PVLS
  ####################################################################

  # TX_PVLS_ELIGIBLE ->                OVC_VL_ELIGIBLE
  # TX_PVLS_RESULT_DOCUMENTED ->       OVC_VLR
  # TX_PVLS_VERIFY ->                  OVC_VLS

 df4<- df3 %>%
    mutate(indicator = case_when(
      indicator == "TX_PVLS_ELIGIBLE" & population == "OVC" ~ "OVC_VL_ELIGIBLE",
      indicator == "TX_PVLS_RESULT_DOCUMENTED" & population=="OVC" ~ "OVC_VLR",
      indicator=="TX_PVLS_VERIFY" & population=="OVC" ~ "OVC_VLS",
      TRUE ~ indicator
    )) %>%
    view

  ##confirm recoding worked
  # test <- df3 %>%
  #    filter(indicator %in% c("TX_PVLS_ELIGIBLE") & population %in% c("OVC")) %>%
  #   view
  # #count=314
  # test <- df4 %>%
  #   filter(indicator %in% c("OVC_VL_ELIGIBLE")) %>%
  #   view
  # #count 314
  #
  # df4 %>%
  #   distinct(indicator) %>%
  #   pull()



  ####################################################################
  #                        CLEANING/MUTATES ACROSS INDICATORS
  ####################################################################

  df5 <- df4 %>%
       #add numerator to all DREAMS_FP/DREAMS_GEND_NORM, Zim not reporting Denom
      mutate(numdenom = ifelse(indicator == "DREAMS_FP" | indicator=="DREAMS_GEND_NORM", "N", NA),
        # recode numdenom to N/D
         numdenom=ifelse(numdenom=="Numerator" |numdenom=="numerator", "N",
                  ifelse(numdenom=="Denominator" | numdenom=="denominator", "D", numdenom)),
        # Remove population of "Non-KP (general population)" for most indicator (only in PrEP - but not 1 mo & VERIFY vars)
    #     population = ifelse(indicator == "DREAMS_FP" | indicator == "DREAMS_GEND_NORM" | indicator == "GEND_GBV" | indicator == "OVC_ENROLL" | indicator == "OVC_OFFER" | indicator == "OVC_VL_ELIGIBLE" | indicator == "OVC_VLR" | indicator == "OVC_VLS" |
    #                          indicator == "PMTCT_EID_ELIGIBLE" | indicator == "PMTCT_EID_SAMPLE_DOCUMENTED" | indicator == "PrEP_1MONTH" | indicator == "SC_ARVDISP" | indicator == "SC_CURR" | indicator == "SC_LMIS" |
    #                          indicator == "TX_PVLS_ELIGIBLE" | indicator == "TX_PVLS_RESULT_DOCUMENTED" | indicator == "TX_PVLS_SAMPLE" | indicator == "VMMC_AE"
    #                        & population == "Non-KP (general population)", NA, population))) %>%
    # view
       population = case_when(
                   indicator == "DREAMS_FP" | indicator == "DREAMS_GEND_NORM" | indicator == "GEND_GBV" | indicator == "OVC_ENROLL" | indicator == "OVC_OFFER" | indicator == "OVC_VL_ELIGIBLE" | indicator == "OVC_VLR" | indicator == "OVC_VLS" |
                        indicator == "PMTCT_EID_ELIGIBLE" | indicator == "PMTCT_EID_SAMPLE_DOCUMENTED" | indicator == "PrEP_1MONTH" | indicator == "SC_ARVDISP" | indicator == "SC_CURR" | indicator == "SC_LMIS" |
                        indicator == "TX_PVLS_ELIGIBLE" | indicator == "TX_PVLS_RESULT_DOCUMENTED" | indicator == "TX_PVLS_SAMPLE" | indicator == "VMMC_AE"
                      & population == "Non-KP (general population)" ~ NA_character_,
                  indicator == "OVC_VL_ELIGIBLE" | indicator == "OVC_VLR" | indicator == "OVC_VLS"  & population== "OVC" ~ NA_character_,
                  TRUE ~ population))

##test for rrecoding worked
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



  #issues in  age
  df5  %>%
    # filter(indicator==c("OVC_ENROLL")) %>%
    distinct(age) %>%
    pull()
  # [1] "Infant Age: 2-12mo" "Infant Age: <=2mo"  "10-14"              "15-19"              "20-24"              "25-29"
  # [7] "30-34"              "35-39"              "40-44"              "45-49"              "5-9"                "50+"
  # [13] "<1"                 "1-4"                "20/24"              NA                   "15-17"              "18+ caregiver"
  # [19] "18-20 OVC"          "30-36"              "30-35"              "<1 (Specific)"      "1-4 (Inclusive)"    "5-9 (Specific)"
  # [25] "10-14 (Specific)"   "15-17 (Specific)"   "15-19 (Specific)"   "20-24 (Specific)"   "OVC: 18-20"         "OVC: 18+ caregiver"
  # [31] "15 - 19"            "20 - 24"            "25 - 29"            "30 - 34"            "35 - 39"            "40 - 44"
  # [37] "45 - 49"            "0-1"


  #drop from the string anything after (
  df6 <- df5 %>%
    mutate(age= gsub("\\(.*","",age)%>%
             str_trim(side = "both")) %>%
    ## might have to do something to disaggs or pops when removing ovc or caregiver from age?
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
      age=="35 - 39"~"35-39",
      age=="40 - 44"~"40-44",
      age=="45 - 49"~"45-49",
      age=="0-1"~"<1",
      age=="15 - 19"~"15-19",
      age=="2-12mo"~"2-12 months",
      age=="<=2mo"~"<2 months",
      TRUE~age))


  ## check recode
  df6 %>%
    distinct(age) %>%
    pull()

  ####################################################################
  #                        DREAM_FP
  ####################################################################

  #keep age/sex in main dataset but remove the other diaggregate
  Dreams_agesex<- df6 %>%
    # subset(indicator==c("DREAMS_FP")) %>% #use subset to check work
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
           sex=ifelse(!is.na(otherdisaggregate), NA, sex)) %>%
    #consider using the opposite logic to make otherdisagg null if age or sex is not null, but currently no use case
    view
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

  #review data for this indicator
  test<- Dreams_Gend_Norm %>%
    subset(indicator==c("GEND_GBV")) %>%
    view
  #951 obs

  #check if any otherdisaggs need to be recoded
  Dreams_Gend_Norm %>%
    filter(indicator==c("GEND_GBV")) %>%
    distinct(otherdisaggregate) %>%
    pull()
  #[1] "Sexual violence - received PEP"        "Physical and/or emotional violence"    "Sexual violence - did not receive PEP"
  # [4] "Physical or emotional violence"


  # "Physical or emotional violence"   -->     "Physical and/or emotional violence"
  GEND_GBV<-Dreams_Gend_Norm %>%
    mutate(otherdisaggregate=case_when(
          indicator=="GEND_GBV" & otherdisaggregate=="Physical or emotional violence" ~ "Violence Service Type: Physical and/or emotional violence",
          TRUE~ otherdisaggregate))


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
  # #-> other disaggreagate recoded to "OVC or Caregiver: OVC"


  OVC_ENROLL_OFFER<-GEND_GBV %>%
    mutate(otherdisaggregate=ifelse(indicator=="OVC_OFFER"|indicator=="OVC_ENROLL", "OVC or Caregiver: OVC", otherdisaggregate)) %>%
    view





  ####################################################################
  #                       OVC_VL_ELIGIBLE
  ####################################################################

#   #check if any otherdisaggs/pop need to be recoded
#   OVC_ENROLL_OFFER %>%
#     filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
#     distinct(otherdisaggregate) %>%
#     pull()

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


  #Zim only collecting the 1st month. add this to the otherdisaggregate

  PREP_1MO <- PMTCT %>%
    mutate(otherdisaggregate=  ifelse(indicator=="PrEP_1MONTH", "Reporting Month: Month 1 of Reporting Quarter", otherdisaggregate))



  ####################################################################
  #                       PREP_ELIGIBLE
  ####################################################################


  #no other disaggregates identified (pregnant/breastfeeding), no changes necessary at this time
#
#   PREP_1MO %>%
#      filter(indicator==c("PrEP_ELIGIBLE")) %>%
#     distinct(population) %>%
#     pull()
  # [1] NA                                "Men who have sex with men (MSM)" "Female sex workers (FSW)"
  # [4] "Transgender people (TG)"         "Non-KP (general population)"


  ####################################################################
  #                       PREP_SCREEN
  ####################################################################

#
#   #no other disaggregates, but population and age overlap
#   PREP_1MO %>%
#     filter(indicator==c("PrEP_SCREEN")) %>%
#     distinct(population) %>%
#     pull()
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


  ***************************************************************************************************************
#2021.07.24

  ####################################################################
  #                       SC_ARVDISP
  ####################################################################

  #test
  # df4 %>%
  #   subset(indicator==c("SC_ARVDISP")) %>%
  #   group_by(otherdisaggregate) %>%
  #   skim

  SC_ARVDISP<- df4 %>%
    mutate(numdenom=ifelse(indicator=="SC_ARVDISP","N","N"),
           #recode from categories seen in excel pivot
           otherdisaggregate=ifelse(otherdisaggregate=="ARV Category: DTG 10 bottles (30-count)", "ARV Category: Other (Pediatric) bottles dispensed" , otherdisaggregate),
          #recode from errors in tableau report - note seen in this data
           otherdisaggregate=ifelse(otherdisaggregate=="NVP (Pediatric), (not including NVP 10) bottles", "ARV Category: NVP (Pediatric) bottles dispensed - not including NVP 10" , otherdisaggregate)) %>%
     view

  #check recoding
  SC_ARVDISP %>%
      subset(indicator==c("SC_ARVDISP")) %>%
      group_by(otherdisaggregate) %>%
      skim


  ####################################################################
  #                       SC_CURR
  ####################################################################

   df4 %>%
    subset(indicator==c("SC_CURR")) %>%
       group_by(otherdisaggregate) %>%
      skim
    view

    SC_CURR<- df4 %>%
    mutate(numdenom=ifelse(indicator=="SC_CURR","N","N"),
           otherdisaggregate=ifelse(otherdisaggregate=="ARV Category: DTG 10 bottles (30-count)", "ARV Category: Other (Pediatric) bottles dispensed" , otherdisaggregate)) %>%
           view


    SC_CURR %>%
    subset(indicator==c("SC_CURR")) %>%
    group_by(otherdisaggregate) %>%
    skim


    ####################################################################
    #                       TX_PVLS_ELIGIBLE
    ####################################################################


    ####################################################################
    #                       TX_PVLS_PVLS_RESULT_DOCUMENTED
    ####################################################################


    ####################################################################
    #                       TX_PVLS_SAMPLE
    ####################################################################


    ####################################################################
    #                       TX_PVLS_VERIFY
    ####################################################################


    ####################################################################
    #                       OVC_VL_ELIGIBLE
    ####################################################################


    ####################################################################
    #                      OVC_VLR
    ####################################################################


    ####################################################################
    #                       OVC_VLS
    ####################################################################


    ####################################################################
    #                       TX_RTT_VERIFY
    ####################################################################


    ####################################################################
    #                       VMMC_AE
    ####################################################################














  ####################################################################
  #                        RECODE PERIOD
  ####################################################################
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

  ####################################################################
  #                        RECODE NUMDENOM
  ####################################################################

  #Numerator -> n/N, Denominator -> d/D



