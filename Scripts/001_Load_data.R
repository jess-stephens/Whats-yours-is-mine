###################
#Quarterly Data Pull
###################

#load library's
source("Scripts/00_setup.R")

setwd("./Data/Q2 resubmissions")


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

write_tsv(df.list, "FY21Q2_Zimbabwe_CI_PSI_CeSHHAR", na = " ") #use to create pivots for QC







##### Bring in MSD to give orgunituids (PSNU)
msd<-read_msd("C:/Users/jstephens/Documents/MSD/Zim_Genie_SITE_IM_MultipleOUs_Daily_89756ed1-21b5-46ad-854c-73803d9f23c4.txt")
msd_psnu<-msd %>%
  select(c("psnu", "psnuuid")) %>%
  distinct()