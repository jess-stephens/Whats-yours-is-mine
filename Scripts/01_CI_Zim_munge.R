
  ####################################################################
  #                       FORMAT DATA
  ####################################################################


#add columns for OU (Zimbabwe), Orgunit (unknown) and Orgunitid (unknown)
  df2 <- df.list %>%
    mutate(operatingunit = "Zimbabwe",
           orgunit = psnu)

#add orgunitud for zim psnu's by zim msd'
df2_join<-df2 %>%
  left_join(msd_psnu,  by = c("psnu"="psnu"), `copy` = TRUE) %>%
  rename(orgunituid=psnuuid)
glimpse(df2_join)


### select CIGB indicators, rename vars and order

  df3 <- df2_join %>%
    mutate(indicator=case_when(
      indicator=="PREP_1MONTH"  ~  "PrEP_1MONTH",
      indicator=="PREP_SCREEN" ~  "PrEP_SCREEN",
      indicator=="PREP_ELIGIBLE"  ~  "PrEP_ELIGIBLE",
      TRUE~ indicator), ) %>%
    #pull only the CIGB indicators, none of Zim CI
    #If Zim changes indicators (ie OVC) or collects new indicators this will need to be updated
    filter(indicator %in% c("TX_NEW_VERIFY")) %>%
    #name variables so the match CIGB template, expect for period which will occur when recoding period format
    rename(mech_code = mechanism_id,
           partner = partner_name,
           numdenom = numerator_denominator,
           population = population_type,
           otherdisaggregate = other_disaggregate,
           val = result_value) %>%
    #keep only the CIGB template variables (drop 5)
    select(!c(annual_target_value,
              starts_with("required"),
              starts_with("x"))) %>%
   select(c(period, orgunit, orgunituid, mech_code, partner, operatingunit, psnu, indicator, sex, age, population, otherdisaggregate,numdenom,val))
glimpse(df3)

# df3 %>%
#   distinct(partner) %>%
#   pull()

  ####################################################################
  #                        OVC from PVLS - only q2/q4
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
   ))

 # #confirm recoding worked
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
 # df5 %>%
 #          filter(indicator==c("TX_PVLS_ELIGIBLE")) %>%
 #         distinct(population) %>%
 #        pull()


  ####################################################################
  #                        CLEANING/MUTATES ACROSS INDICATORS
  ####################################################################
#if Q1/Q3 and skipping OVC code, must convert df3 to df4
# df4<-df3

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

  # # check periods
  # df6 %>%
  #   distinct(period) %>%
  #   pull()

date <- df6 %>%
  mutate(rep_period = quarter(period, with_year = TRUE, fiscal_start = 10)) %>%
  separate(rep_period, into = c("fiscal_year", "reportingperiod"), sep = "[.]") %>%
  mutate(reportingperiod = case_when(
    str_detect(indicator, "OVC") & reportingperiod == 2 ~ paste0("FY", str_sub(fiscal_year, 3, 4), " Q1 - Q", reportingperiod),
    str_detect(indicator, "OVC") & reportingperiod == 4 ~ paste0("FY", str_sub(fiscal_year, 3, 4), " Q3 - Q", reportingperiod),
    TRUE ~ paste0("FY", str_sub(fiscal_year, 3, 4), " Q", reportingperiod))
  ) %>%
  relocate(reportingperiod, .before = period) %>%
  select(!c(period, fiscal_year))

# date %>%
#   distinct(reportingperiod) %>%
#   pull()


  ####################################################################
  #                        DREAM_FP
  ####################################################################

  #keep age/sex in main dataset but remove the other diaggregate
  Dreams_agesex<- date %>%
    mutate(otherdisaggregate=ifelse(!is.na(age) & indicator=="DREAMS_FP", NA, otherdisaggregate))
  #8531 obs


  #create subset of just the other disaggregates for dreams in order to add to main dataset with age/sex

  ##check if any otherdisaggs need to be recoded
  # date %>%
  #   filter(indicator==c("DREAMS_FP")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()

  Dreams_FP2<- date %>%
    subset(indicator==c("DREAMS_FP") & !is.na(otherdisaggregate)) %>%
    #  logic to make age and sex null if other disaggregate is not null,
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age),
           sex=ifelse(!is.na(otherdisaggregate), NA, sex))
    #consider using the opposite logic to make otherdisagg null if age or sex is not null, but currently no use case
  #2 obs

  #combine Dreams_FP2 with Dreams_agesex: row bind
  Dreams<-rbind(Dreams_agesex, Dreams_FP2)
  #8533 obs



  ####################################################################
  #                        DREAM_GEND_NORM
  ####################################################################

  # #check if any otherdisaggs need to be recoded
  # Dreams %>%
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
    mutate(otherdisaggregate=ifelse(!is.na(age) & indicator=="DREAMS_GEND_NORM", NA, otherdisaggregate))


  #create subset of just the other disaggregates for dreams in order to add to main dataset with age/sex
    #  logic to make age and sex null if other disaggregate is not null,
  Dreams_Gend_disagg<- Dreams %>%
    subset(indicator==c("DREAMS_GEND_NORM") & !is.na(otherdisaggregate)) %>%
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age),
           sex=ifelse(!is.na(otherdisaggregate), NA, sex))
#36 obs

  #combine: row bind
  Dreams_Gend_Norm<-rbind(Dreams_Gend_agesex, Dreams_Gend_disagg)

  ####################################################################
  #                        GEND_GBV
  ####################################################################

  Dreams_Gend_Norm %>%
    filter(indicator==c("GEND_GBV")) %>%
    # distinct(population) %>%
    view()
    # pull()

  # # [1] NA    "OVC"  -> only NA is allowable, recoded OVC to NA <- not relevant in Q3
  #
  # #check if any otherdisaggs need to be recoded
  # Dreams_Gend_Norm %>%
  #   filter(indicator==c("GEND_GBV")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()
  #[1] "Sexual violence - received PEP"        "Physical and/or emotional violence"    "Sexual violence - did not receive PEP"


  # "Physical or emotional violence"   -->     "Physical and/or emotional violence" in Q2 but not Q3
  GEND_GBV<-Dreams_Gend_Norm %>%
    mutate(otherdisaggregate=case_when(
          indicator=="GEND_GBV" & otherdisaggregate=="Physical or emotional violence" ~ "Violence Service Type: Physical and/or emotional violence",
          TRUE~ otherdisaggregate),
          population=case_when(indicator=="GEND_GBV"~NA_character_,
          TRUE~population) )

  # Sexual violence - received PEP  -->   Violence Service Type: Sexual violence &  PEP: completed PEP
  #requires 2 steps to recode into 2 different disaggs

# keep sexual violence in main data set
  # Full sexual violence disaggregate is the received and did not received
  GEND_GBV_type<- GEND_GBV %>%
  mutate(otherdisaggregate=case_when(
    (indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - received PEP") ~ "Violence Service Type: Sexual violence",
    (indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - did not receive PEP") ~ "Violence Service Type: Sexual violence",
    TRUE~ otherdisaggregate))

  #keep other disaggregates for pep in subset
  GEND_GBV_pep<- GEND_GBV %>%
    subset(indicator==c("GEND_GBV") & otherdisaggregate=="Sexual violence - received PEP") %>% #create subset of only the sexual violence - received pep to duplicate
    mutate(otherdisaggregate=ifelse(indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - received PEP", "PEP: Completed PEP", otherdisaggregate))

  #combine: row bind
  GEND_GBV<-rbind(GEND_GBV_type, GEND_GBV_pep)

  # GEND_GBV %>%
  #   filter(indicator==c("GEND_GBV")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()



  ####################################################################
  #                       OVC_ENROLL/OVC_OFFER
  ####################################################################

  # GEND_GBV %>%
  # filter(indicator==c("OVC_OFFER")) %>%
  # distinct(population) %>%
  # pull()
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

  # OVC_OFFER-> may need to bring in TX_CURR<20 (age/sex disagg) for denominator - CIGB says automatic

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
# NA
  #no changes

  ####################################################################
  #                       PMTCT_EID_ELIGIBLE/PMTCT_EID_SAMPLE_DOCUMENTED
  ####################################################################

  #check populations are NA
  # OVC_ENROLL_OFFER %>%
  #   filter(indicator==c("PMTCT_EID_SAMPLE_DOCUMENTED")) %>%
  #   distinct(population) %>%
  #   pull()

  # Error report: Recode "<01" to "0-12 months" -> no errors in q3 data

  # OVC_ENROLL_OFFER %>%
  #   filter(indicator==c("PMTCT_EID_ELIGIBLE")) %>%
  #   distinct(age) %>%
  #   pull()
# #[1] "<2 months"   "2-12 months"

  # OVC_ENROLL_OFFER %>%
  #   filter(indicator==c("PMTCT_EID_ELIGIBLE")) %>%
  #   distinct(age) %>%
  #   pull()
  #don't actually see <01 in this dataset, but its in the errors report for previous entry?
   #0-12 not applicable for PMTCT_EID_SAMPLE_DOCUMENTED

  PMTCT <- OVC_ENROLL_OFFER %>%
       mutate(age=case_when(
             (indicator=="PMTCT_EID_ELIGIBLE"& age=="<01") ~ "0-12 months", TRUE~ age))

  ## check recode
  # PMTCT %>%
  #   filter(indicator=="PMTCT_EID_ELIGIBLE"| indicator=="PMTCT_EID_SAMPLE_DOCUMENTED") %>%
  #   distinct(age) %>%
  #   pull()
# [1] "2-12 months" "<2 months"


  ####################################################################
  #                       PREP_1MONTH
  ####################################################################

  # PMTCT %>%
  #   filter(indicator==c("PrEP_1MONTH")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()
  #should have NA as populations


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
  # PMTCT %>%
  #   filter(indicator==c("PrEP_SCREEN")) %>%
  #   distinct(population) %>%
  #   pull()
  #some (FHI) reporting age and other disaggregate together, need to separate

  # keep ages in in main dataset, but remove population type
  PrEP_SCREEN_age<- PMTCT %>%
    mutate(population=ifelse(!is.na(age)| !is.na(sex), NA, population))
  #obs 9497

  #keep population in subset, clean out age/sex
  PrEP_SCREEN_type<- PMTCT %>%
    filter(indicator==c("PrEP_SCREEN") & !is.na(population)) %>%
    mutate(age=ifelse(!is.na(population), NA, age),
           sex=ifelse(!is.na(population), NA, sex))
  #  obs 68

  #combine: row bind
  PrEP_SCREEN<-rbind(PrEP_SCREEN_type, PrEP_SCREEN_age)

  PrEP_SCREEN<- PrEP_SCREEN %>%
    mutate(age=case_when((indicator %in% c("PrEP_SCREEN", "PrEP_ELIGIBLE")) & is.na(age)~"unknown",
                         TRUE~age))

  # PrEP_SCREEN %>%
  #   filter(indicator %in% c("PrEP_SCREEN", "PrEP_ELIGIBLE") ) %>%
  #   distinct(age) %>%
  #   pull()


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
    mutate( #recode from categories seen in excel pivot
           otherdisaggregate=ifelse(otherdisaggregate=="ARV Category: DTG 10 bottles (30-count)", "ARV Category: Other (Pediatric) bottles dispensed" , otherdisaggregate),
          #recode from errors in tableau report - note seen in this data
           otherdisaggregate=ifelse(otherdisaggregate=="NVP (Pediatric), (not including NVP 10) bottles", "ARV Category: NVP (Pediatric) bottles dispensed - not including NVP 10" , otherdisaggregate))

  #check recoding
  # SC_ARVDISP %>%
  #   filter(indicator==c("SC_ARVDISP")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()



  ####################################################################
  #                       SC_CURR
  ####################################################################
  # SC_ARVDISP %>%
  #   filter(indicator==c("SC_CURR")) %>%
  #   distinct(otherdisaggregate) %>%
  #   pull()

# #sc_arvdisp fixed errors in sc_curr - code below is duplicative

    # SC_CURR<- SC_ARVDISP %>%
    # mutate(numdenom=ifelse(indicator=="SC_CURR","N","N"),
    #        otherdisaggregate=ifelse(otherdisaggregate=="ARV Category: DTG 10 bottles (30-count)", "ARV Category: Other (Pediatric) bottles dispensed" , otherdisaggregate)) %>%
    #        view

  ####################################################################
  #                       SC_LMIS
  ####################################################################
  #started reporting in Q3, no changes
  # SC_ARVDISP %>%
  #   distinct(indicator) %>%
  #   pull()



  ####################################################################
  #                       VERIFY indicators
  ####################################################################
  #Recode Verify indicators to PEPFAR Reported Sites "Site Support Type: PEPFAR supported"

   view(SC_ARVDISP)

  VERIFY<- SC_ARVDISP %>%
    mutate(otherdisaggregate=ifelse(is.na(otherdisaggregate), "Site Support Type: PEPFAR supported",otherdisaggregate),
      age=case_when(( age %in% c("<1", "1-4", "5-9", "10-14", "15-19")) ~"<20",
        TRUE~age      ))

  VERIFY %>%
    # filter(indicator %in% c("TX_NEW_VERIFY") ) %>%
    distinct(age) %>%
    pull()



  ####################################################################
  #                       TX_CURR_VERIFY
  ####################################################################
  #started reporting in Q3, no changes
  # VERIFY %>%
  #   distinct(indicator) %>%
  #   pull()




    ####################################################################
    #                       TX_PVLS_ELIGIBLE
    ####################################################################
 ``## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
#   VERIFY %>%
#       filter(indicator==c("TX_PVLS_ELIGIBLE")) %>%
#       distinct(numdenom) %>%
#       pull()
#   #9755
#   TX_PVLS_ELIGIBLE_d<- VERIFY %>%
#     subset(indicator=="TX_PVLS_ELIGIBLE" & numdenom=="D")
# #144

  #drop denominator from TX_PVLS_ELIGIBLE
  TX_PVLS_ELIGIBLE<- VERIFY [!(VERIFY$indicator=="TX_PVLS_ELIGIBLE" & VERIFY$numdenom=="D"),]
#9755-144=9611


  # TX_PVLS_ELIGIBLE %>%
  #   filter(indicator==c("TX_PVLS_ELIGIBLE")) %>%
  #    distinct(numdenom) %>%
  #   pull()

    ####################################################################
    #                       TX_PVLS_RESULT_DOCUMENTED
    ####################################################################
  ``## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
  # TX_PVLS_ELIGIBLE %>%
  #   filter(indicator==c("TX_PVLS_RESULT_DOCUMENTED")) %>%
  #   distinct(population) %>%
  #   pull()

    ####################################################################
    #                       TX_PVLS_SAMPLE
    ####################################################################
  ## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
  # TX_PVLS_ELIGIBLE %>%
  #   filter(indicator==c("TX_PVLS_SAMPLE")) %>%
  #   distinct(population) %>%
  #   pull()

    ####################################################################
    #                       TX_PVLS_VERIFY
    ####################################################################
  ## checked pop, otherdisag, age, sex. No issues with age/sex and population overlap -> no changes
  # TX_PVLS_ELIGIBLE %>%
  #   filter(indicator==c("TX_PVLS_VERIFY")) %>%
  #   distinct(population) %>%
  #   pull()

    ####################################################################
    #                       OVC_VL_ELIGIBLE
    ####################################################################

  #already dropped population as OVC, only otherdisagg=OVC

    # TX_PVLS_ELIGIBLE %>%
    #   filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
    #   distinct(otherdisaggregate) %>%
    #   pull()
# "OVC"-  appropriate other disagg -> no changes

  #18-20 not in CIGB --> recode to 18+
  # TX_PVLS_ELIGIBLE %>%
  #   filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
  #   distinct(age) %>%
  #   pull()
  # # [1] "15-17" "5-9"   "18+"   "10-14" "1-4"   "18-20" "<1"
  #
  # TX_PVLS_ELIGIBLE %>%
  #   filter(indicator==c("OVC_VL_ELIGIBLE")) %>%
  #   distinct(age) %>%
  #   pull()




    ####################################################################
    #                      OVC_VLR
    ####################################################################
  # TX_PVLS_ELIGIBLE %>%
  #   filter(indicator==c("OVC_VLR")) %>%
  #   distinct(age) %>%
  #   pull()
  #18-20 not in CIGB --> recode to 18+

    ####################################################################
    #                       OVC_VLS
    ####################################################################

  # TX_PVLS_ELIGIBLE %>%
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
  VMMC_AE_age<- TX_PVLS_ELIGIBLE %>%
    mutate(otherdisaggregate=ifelse(!is.na(age)& indicator=="VMMC_AE"| !is.na(sex) & indicator=="VMMC_AE", NA, otherdisaggregate))
  #obs 9497

  #keep population in subset, clean out age/sex
  VMMC_AE_type<- TX_PVLS_ELIGIBLE %>%
    filter(indicator==c("VMMC_AE") & !is.na(otherdisaggregate)) %>%
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age))
           # sex=ifelse(!is.na(otherdisaggregate), NA, sex)

  #combine: row bind
  VMMC_AE<-rbind(VMMC_AE_age, VMMC_AE_type)

  ####################################################################
  #                       CHECK MECH_CODE
  ####################################################################

  VMMC_AE %>%
     distinct(mech_code) %>%
     pull()

  #Recode as numeric
  mech_code<- VMMC_AE %>%
    mutate(mech_code=case_when(partner=="Family Health International"~ "17578",
                               partner== "Population Services International"~"70473",
                               partner=="ORGANIZATION FOR PUBLIC HEALTH INTERVENTIONS AND DEVELOPMENT"~"82102",
                               partner== "Centre for Sexual Health and HIV/AIDS Research Zimbabwe"~"85142",
                               partner== "Chemonics International, Inc."~"18353",
                               TRUE~mech_code),
    mech_code=as.integer(mech_code))



 mech_code %>%
   distinct(mech_code) %>%
   pull()


 # mech_code %>%
 #   filter(is.na(mech_code)) %>%
 #   view()
  ####################################################################
  #                       EXPORT DATA AS EXCEL FILE
  ####################################################################

  # mech_code %>%
  #   distinct(indicator) %>%
  #   pull()

  # remove pmtct_eid_sample_documented AND PrEP_1MONTH - will not load without disaggregates

 # FINAL<- mech_code %>%
 #  filter(!indicator %in% c("PMTCT_EID_SAMPLE_DOCUMENTED", "PrEP_1MONTH") )
 #
 # FINAL_q3<- FINAL %>%
 #   filter(reportingperiod %in% c("FY21 Q3") )

  write_tsv(mech_code, "CIRG_FY21_Q2_Zimbabwe_20210908", na = " ")


