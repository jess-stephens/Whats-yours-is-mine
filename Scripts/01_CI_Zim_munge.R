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

  df2 <- df %>%
    mutate(operatingunit = "Zimbabwe",
           orgunit = NA,
           orgunitid = NA) #add column for OU (Zimbabwe), Orgunit (unknown) and Orgunitid (unknown)

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
    # something is happening that is dropping some of the correct indicators....
    filter(indicator %in% c("DREAMS_FP", "DREAMS_GEND_NORM", "GEND_GBV", "TX_NEW_VERIFY", "TX_CURR_VERIFY", "TX_PVLS_VERIFY", "TX_RTT_VERIFY",
           "TX_PVLS_VERIFY","TX_PVLS_SAMPLE", "TX_PVLS_RESULT_DOCUMENTED", "PMTCT_EID_ELIGIBLE", "PMTCT_EID_SAMPLE", "PMTCT_EID_RESULT_DOCUMENTED",
           "PrEP_SCREEN", "PrEP_ELIGIBLE", "PrEP_1MONTH", "PrEP_NEW_VERIFY", "PrEP_CURR_VERIFY", "SC_ARVDISP", "SC_LMIS", "SC_CURR",
           "VMMC_AE", "OVC_OFFER", "OVC_ENROLL")) %>% #pull only the CIGB indicators, none of Zim CI
    rename(mech_code = mechanism_id,
           partner = partner_name,
           numdenom = numerator_denominator,
           population = population_type,
           otherdisaggregate = other_disaggregate,
           val = result_value) #name variables so the match CIGB template, expect for period which happens below


  df3 <- df3 %>%
    select(!c(annual_target_value,
              starts_with("required")))
    # select(c("period", "orgunit", "orgunitid", "mech_code", "partner",
    #          "operatingunit", "psnu", "indicator", "sex", "age", "population",
    #          "otherdisaggregate", "numdenom", "val")) %>%
    #keep only the CIGB template variables
    #CHECK ERRORS RECODING TX_PVLS_ELIGIBLE TO OVC - tried 2 options

  df3 %>%
    #filter(indicator == "TX_PVLS_ELIGIBLE" & otherdisaggregate == "OVC")
    mutate(indicator = case_when(
      indicator == "TX_PVLS_ELIGIBLE" & otherdisaggregate == "OVC" ~ "OVC_VL_ELIGIBLE",
      indicator == "TX_PVLS_RESULT_DOCUMENTED" & otherdisaggregate=="OVC" ~ "OVC_VLR",
      indicator=="TX_PVLS_VERIFY" & otherdisaggregate=="OVC" ~ "OVC_VLS",
      TRUE ~ indicator
    ))

    mutate(indicator = ifelse(indicator == "TX_PVLS_ELIGIBLE" & otherdisaggregate == "OVC", "OVC_VL_ELIGIBLE", "TX_PVLS_ELIGIBLE"))
    #       indicator= ifelse(indicator=="TX_PVLS_RESULT_DOCUMENTED" &  otherdisaggregate=="OVC", "OVC_VLR", "TX_PVLS_RESULT_DOCUMENTED"),
    #         indicator= ifelse(indicator=="TX_PVLS_VERIFY" & otherdisaggregate=="OVC", "OVC_VLS", "TX_PVLS_VERIFY")) %>% #separating out OVC from TX_PVLS
    #
    # mutate(indicator=ifelse(indicator=="TX_PVLS_ELIGIBLE" &  otherdisaggregate=="OVC", "OVC_VL_ELIGIBLE",
    #                         ifelse(indicator=="TX_PVLS_RESULT_DOCUMENTED" &  otherdisaggregate=="OVC", "OVC_VLR",
    #                                ifelse(indicator=="TX_PVLS_VERIFY" & otherdisaggregate=="OVC", "OVC_VLS",
    #                                       indicator)))) %>% #separating out OVC from TX_PVLS
     view

  test <- df3 %>%
     subset(indicator == c("PrEP_1MONTH")) %>%
    view

  #count


  df3 %>%
    group_by(population) %>%
    skim



  ####################################################################
  #                        CLEANING ACROSS INDICATORS
  ####################################################################

  df4 <- df3 %>%
       #add numerator to all DREAMS_FP/DREAMS_GEND_NORM, Zim not reporting Denom
  mutate(numdenom = ifelse(indicator == "DREAMS_FP", "N", NA),
         # Remove population of "Non-KP (general population)" for most indicators
         population = ifelse(indicator == "DREAMS_FP" | indicator == "DREAMS_GEND_NORM" | indicator == "GEND_GBV" | indicator == "OVC_ENROLL" | indicator == "OVC_OFFER" | indicator == "OVC_VL_ELIGIBLE" | indicator == "OVC_VLR" | indicator == "OVC_VLS" |
                             indicator == "PMTCT_EID_ELIGIBLE" | indicator == "PMTCT_EID_SAMPLE_DOCUMENTED" | indicator == "PrEP_1MONTH" | indicator == "SC_ARVDISP" | indicator == "SC_CURR" | indicator == "SC_LMIS" |
                             indicator == "TX_PVLS_ELIGIBLE" | indicator == "TX_PVLS_RESULT_DOCUMENTED" | indicator == "TX_PVLS_SAMPLE" | indicator == "VMMC_AE"
                           & population == "Non-KP (general population)", NA, population),
         # recode numdenom to N/D
         numdenom=ifelse(numdenom=="Numerator" |numdenom=="numerator", "N",
                  ifelse(numdenom=="Denominator" | numdenom=="denominator", "D", numdenom))) %>%
    view


   #use subset to check work, indicators interchangable
  # subset <-df4 %>%
  #    subset(indicator==c("DREAMS_FP")) %>%
  #  #  subset(indicator==c("DREAMS_GEND_NORM")) %>%
  #   view





  ####################################################################
  #                        DREAM_FP
  ####################################################################

  #correct age/sex in main dataset
  Dreams_agesex<- df3 %>%
    # subset(indicator==c("DREAMS_FP")) %>% #use subset to check work
    mutate(otherdisaggregate=ifelse(!is.na(age) & indicator=="DREAMS_FP", NA, otherdisaggregate)) %>%
    view
  #22504 obs

  #create subset of just the other disaggregates for dreams in order to add to main dataset

  # #use subset to check work
  # Dreams_FP<- df3 %>%
  #   subset(indicator==c("DREAMS_FP") & !is.na(otherdisaggregate)) %>%
  #          view


  Dreams_FP2<- df3 %>%
    subset(indicator==c("DREAMS_FP") & !is.na(otherdisaggregate)) %>%
    #  logic to make age and sex null if other disaggregate is not null,
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age),
           sex=ifelse(!is.na(otherdisaggregate), NA, sex),
           otherdisaggregate=ifelse(otherdisaggregate=="Contraceptive service/method: FP counseling plus method provisions - Fertility awareness method", "FP counseling plus method provisions - Fertility awareness method", otherdisaggregate)) %>%
    #consider using the opposite logic to make otherdisagg null if age or sex is not null, but currently no use case
    view
  #2 obs

  #combine Dreams_FP2 with Dreams_agesex: row bind
  Dreams<-rbind(Dreams_agesex, Dreams_FP2)
  #22506 obs



  ####################################################################
  #                        DREAM_GEND_NORM
  ####################################################################

  Dreams %>%
    filter(indicator=="DREAMS_GEND_NORM") %>%
    rowwise() %>%
    mutate(otherdisaggregate = otherdisaggregate %>% str_split(":") %>% unlist() %>% last() %>% str_trim(side = "both")) %>%
    ungroup()

  Dreams_Gend<-Dreams %>%
    # subset(indicator==c("DREAMS_GEND_NORM")) %>% #use subset to check work
    #removed "activity type: " from name by using drop 1st 16 string values
    mutate(otherdisaggregate=ifelse(indicator=="DREAMS_GEND_NORM", substring(otherdisaggregate, 16), otherdisaggregate)) %>%
    view
  #22506 obs


  test<-Dreams %>%
    subset(indicator==c("DREAMS_GEND_NORM")) %>% #use subset to check work
    #removed "activity type: " from name by using drop 1st 16 string values
    mutate(otherdisaggregate=ifelse(indicator=="DREAMS_GEND_NORM", substring(otherdisaggregate, 16), otherdisaggregate)) %>%
    view

  #correct age/sex in main dataset
  Dreams_Gend_agesex<- Dreams_Gend %>%
    #   subset(indicator==c("DREAMS_GEND_NORM")) %>% #use subset to check work
    mutate(otherdisaggregate=ifelse(!is.na(age) & indicator=="DREAMS_GEND_NORM", NA, otherdisaggregate)) %>%
    view
  #22504 obs  -- WHERE DID I LOSE 2 OBS

  #create subset of just the other disaggregates for dreams in order to add to main dataset
  # #use subset to check work
  # Dreams_Gend_act<- Dreams %>%
  #   subset(indicator==c("DREAMS_GEND_NORM") & !is.na(otherdisaggregate)) %>%
  #   view


  Dreams_Gend_act2<- Dreams_Gend %>%
    subset(indicator==c("DREAMS_GEND_NORM") & !is.na(otherdisaggregate)) %>%
    #  logic to make age and sex null if other disaggregate is not null,
    mutate(age=ifelse(!is.na(otherdisaggregate), NA, age),
           sex=ifelse(!is.na(otherdisaggregate), NA, sex)) %>%
    view
  #36 obs

  #combine: row bind
  Dreams_Gend_Norm<-rbind(Dreams_Gend_agesex, Dreams_Gend_act2)
  #22542 obs == 22506 + 36 -- -those lost rows are back??






  ####################################################################
  #                        GEND_GBV
  ####################################################################

  GEND_GBV<-Dreams_Gend_Norm %>%
    mutate(otherdisaggregate=ifelse(indicator=="GEND_GBV" & otherdisaggregate=="Physical or emotional violence", "Violence Service Type: Physical and/or emotional violence", otherdisaggregate)) %>%
  mutate(otherdisaggregate=ifelse(indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - did not receive PEP", "Violence Service Type: Sexual Violence", otherdisaggregate)) %>%
      view
  #22542 obs

  test<-GEND_GBV %>%
   subset(indicator==c("GEND_GBV")) %>% #use subset to check work
  view
 # 951

  # Sexual violence - received PEP  -->   Violence Service Type: Sexual violence &  PEP: completed PEP


# correct sexual violence in main dataset
  GEND_GBV_type<- GEND_GBV %>%
    mutate(otherdisaggregate=ifelse(indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - received PEP", "Violence Service Type: Sexual violence", otherdisaggregate)) %>%
     view
  #22542 obs

  #correct pep in subset
  GEND_GBV_pep<- GEND_GBV %>%
    subset(indicator==c("GEND_GBV") & otherdisaggregate=="Sexual violence - received PEP") %>% #create subset of only the sexual violenxe - received pep to duplicate
    mutate(otherdisaggregate=ifelse(indicator=="GEND_GBV" & otherdisaggregate=="Sexual violence - received PEP", "PEP: Completed PEP", otherdisaggregate)) %>%
    view
  #317 obs

  #combine: row bind
  GEND_GBV<-rbind(GEND_GBV_type, GEND_GBV_pep)
  #22859 obs == 22542 + 317



  ####################################################################
  #                       OVC_ENROLL/OVC_OFFER
  ####################################################################

  #issues in ovc_enroll age
  #issues in ovc_enroll and ovc_offer disaggregates
  # #-> other disaggreagate recoded to "OVC or Caregiver: OVC"

  OVC_ENROLL_OFFER<-GEND_GBV %>%
    mutate(otherdisaggregate==ifelse(indicator=="OVC_OFFER"|indicator=="OVC_ENROLL", "OVC or Caregiver: OVC", otherdisaggregate)) %>%
    view

  # test<-OVC_ENROLL_OFFER %>%
  #     subset(indicator==c("OVC_OFFER")) %>% #use subset to check work
  #     view



  ####################################################################
  #                       OVC_VL_ELIGIBLE
  ####################################################################


#
#   test<-GEND_GBV %>%
#     subset(indicator==c("OVC_VL_ELIGIBLE")) %>% #use subset to check work
#     view
#







  ####################################################################
  #                       PMTCT_EID_ELIGIBLE
  ####################################################################


  # Error report: Recode "<01" to "0-12 months"

  test<-GEND_GBV %>%
    subset(indicator==c("PMTCT_EID_ELIGIBLE")) %>% #use subset to check work
    view

  #don't actually see <01 in this dataset, but its in the errors report for previous entry? found age with "infant age" in this verison to recode

          ######################
           #THESE ARE NOT WORKING
           #####################


  # PMTCT_EID_ELIGIBLE<-GEND_GBV %>%
  # mutate(age==ifelse(indicator=="PMTCT_EID_ELIGIBLE" & age=="<01", "0-12 months", age),
  #        age==ifelse(indicator=="PMTCT_EID_ELIGIBLE" & age=="Infant Age: 2-12mo", "2-12 months", age),
  #        age==ifelse(indicator=="PMTCT_EID_ELIGIBLE"& age=="Infant Age: <=2mo", "<2 months", age)) %>%
  #      view
  #
  # PMTCT_EID_ELIGIBLE<-GEND_GBV %>%
  #   mutate(age==ifelse(indicator=="PMTCT_EID_ELIGIBLE" & age=="<01", "0-12 months", age)) %>%
  #   mutate(age==ifelse(indicator=="PMTCT_EID_ELIGIBLE" & age=="Infant Age: 2-12mo", "2-12 months", age)) %>%
  #   mutate(age==ifelse(indicator=="PMTCT_EID_ELIGIBLE" & age=="Infant Age: <=2mo", "<2 months", age)) %>%
  #   view
  #

  # PMTCT_EID_ELIGIBLE<-GEND_GBV %>%
  # mutate(age==ifelse(indicator=="PMTCT_EID_ELIGIBLE" & age=="<01", "0-12 months", age),
  #               ifelse(indicator=="PMTCT_EID_ELIGIBLE" & age=="Infant Age: 2-12mo", "2-12 months", age),
  #                 ifelse(indicator=="PMTCT_EID_ELIGIBLE"& age=="Infant Age: <=2mo", "<2 months", age)) %>%
  #      view

  #   test<-PMTCT_EID_ELIGIBLE %>%
  #     subset(indicator==c("PMTCT_EID_ELIGIBLE")) %>% #use subset to check work
  #     view





    ####################################################################
    #                       PMTCT_EID_SAMPLE_DOCUMENTED
    ####################################################################


    #needs same age recoding as PMTCT_EID_ELIGIBLE




  ####################################################################
  #                       PREP_1MONTH
  ####################################################################


  #will not enter into CIGB because no disaggregates - no changed made

    test<-PMTCT_EID_ELIGIBLE %>%
      subset(indicator==c("PrEP_1MONTH")) %>% #use subset to check work
      view


  ####################################################################
  #                       PREP_ELIGIBLE
  ####################################################################


  #no other disaggregates identified (pregnant/breastfeeding), no changes necessary at this time

  test<-df4 %>%
    subset(indicator==c("PrEP_ELIGIBLE")) %>% #use subset to check work
    view




  ####################################################################
  #                       PREP_SCREEN
  ####################################################################


  #no other disaggregates identified (pregnant/breastfeeding), no changes necessary
  #some reporting age and other disaggregate together, need to separate
  ## checking with Sara/Brilliant if more appropriate to drop non-kp

  # keep ages in in main dataset, but drop population type
  PrEP_SCREEN_age<- df4 %>%
    mutate(otherdisaggregate=ifelse(!is.na(age), NA, otherdisaggregate)) %>%
    view
  # obs
  test<-PrEP_SCREEN_age %>%
    subset(indicator==c("PrEP_SCREEN")) %>%
    view

  #keep population in subset
  PrEP_SCREEN_type<- df4 %>%
    subset(indicator==c("PrEP_SCREEN") & population=="Non-KP (general population)") %>% #create subset of only the Non-KP (general population) to duplicate
    mutate(is.na(age)) %>%
    view
  #  obs

  #combine: row bind
  GEND_GBV<-rbind(PrEP_SCREEN_type, PrEP_SCREEN_age)
  #22859 obs == 22542 + 317



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



