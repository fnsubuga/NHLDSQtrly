pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization,
  kableExtra,  # Build and manipulate complex tables
  scales,       # easily convert proportions to percents  
  flextable,     # converting tables to pretty images
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  officer,        # helper functions for tables
  parsedate,
  knitr,
  sf,
  pandoc,
  openxlsx,
  dplyr,
  stringr,
  fuzzyjoin)
#######################################################################
#### Functions ####
#### shading columns depending on value 
#### set the target coloring

RctTAT_color   <-  function(column_data){
  ifelse(column_data <= 7, "green",
         ifelse(column_data > 8 & column_data <= 14, "yellow","red"))
}
# followed by code like this
### List Columns to Apply Coloring
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  tble_HtimelyRct  <-   tble_HtimelyRct %>% 
    bg(j = col, bg = RctPct_color(Hub_timelyRct[[col]]), part = "body")
}


########################################################################
###Importing Databases####
Regions       <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/Regions.xlsx")
RRH_Hubs      <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/NSRTN/RRH_Hubs.xlsx")

TB_unique     <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/POC/TB_unique_sites.xlsx")
TB_unique$HFacility <- toupper(TB_unique$HFacility)

modules       <-  import("D:/R works/CPHL_Lab/CPHL_Data/Databases/POC/GeneXpert sites - 2024 Dec.xls")
POC_Sites     <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/POC/POC_List(field).xlsx")
dhis2names    <-  import("D:/R works/CPHL_Lab/CPHL_Data/Databases/POC/dhis2names.xlsx")
gxp_error     <-  import("D:/CPHL-MOH/M&E/Databases & Lists/GxP error rates.xlsx")
gxp_error$`Error Code`  <-  as.character(gxp_error$`Error Code`)

mPima_error   <-  import("D:/CPHL-MOH/M&E/Databases & Lists/mPima_error classification.xlsx")
mPima_error$Error_Code  <-  as.character(mPima_error$Error_Code)

IP            <-  import("D:/R works/CPHL_Lab/CPHL_Data/Databases/POC/Districts, Regions, IP-IM.xlsx")
testHF    <-  import("D:/R works/CPHL_Lab/CPHL_Data/Databases/HF_test.xlsx")
master    <-  import("D:/R works/CPHL_Lab/CPHL_Data/Databases/Health Facility Master File.xlsx"
)
#######################################################################
#### Import data sets ####
#### VL
VLRaw1     <-import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/CQI and research/Lab_NSTRN/VL/VLOct-Dec24.xlsx")
VLRaw2     <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/VL/VLJan_Mar.xlsx")

#### date
VLRaw1    <-  VLRaw1 %>% 
  select(form_number,tracking_code,facility,district,hub,dhis2_name,date_collected,date_received,date_created,sample_type,
                                   test_date,released_at,date_downloaded,status,rejection_reason_id,rejection_reason,delivered_at,picked_from_facility_on,dhis2_uid
              )
VLRaw2    <-  VLRaw2 %>% 
  select(form_number,tracking_code,facility,district,hub,dhis2_name,date_collected,date_received,date_created,sample_type,
         test_date,released_at,date_downloaded,status,rejection_reason_id,rejection_reason,delivered_at,picked_from_facility_on,dhis2_uid
  )

#### combine the data sets
VLRaw   <-  bind_rows(VLRaw1,VLRaw2)

#### EID
EIDRaw     <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/EID/EIDOct24-Mar25.xlsx")

#### Tracking
df_track  <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/siteVisitsOct-Mar.xlsx")
e_tracked <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/sampletrackingOct-Mar25.xlsx")

########################################################################
########################################################################
#### Cleaning Data sets #### 
#### VL Data set ####
VLRaw <- VLRaw %>% 
  rename(# New            #Old
    HFacility   =      facility,
    HName       =      hub,
    District    =      district)

VLRaw$HName <- gsub(" Hub", "", as.character(VLRaw$HName))     #  Removing word Hub from hub name

VLRaw <- VLRaw %>% 
  mutate(District  =        recode(District,
                                   # Old               # New
                                   "Luweero"           =      "Luwero",
                                   "Maracha-Terego"    =       "Maracha",
                                   "Ssembabule"         =       "Sembabule"))
VLRaw$District   <- toupper(VLRaw$District)

VLRaw <- VLRaw %>% 
  left_join(Regions, by = "District")

#### Adding columns for month and quarters
#### Add year and month
VLRaw <- VLRaw %>%
  mutate(
    Yr  =  year(date_collected),
    Month =  month(date_collected)
  ) 
# Add quarter, "Qtr"
VLRaw <- VLRaw %>%
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))


#### Create TAT columns in VL data set (df)
## Converting the data variable type
Tconvert <- VLRaw %>%  
  mutate(date_received = lubridate::ymd(date_received),
         date_collected =lubridate::ymd(date_collected),
         released_at =lubridate::ymd(released_at),
         test_date   = lubridate::ymd(test_date),
         date_downloaded =lubridate::ymd(date_downloaded)) 
TATData <- Tconvert %>%                                              # Created TAT Columns                               
  mutate(
    TATR = date_received - date_collected,
    TATL = released_at   - date_received,
    TATD = date_downloaded - released_at,
    TATO = date_downloaded - date_collected) %>% 
  mutate_at(c("TATR", "TATL", "TATD", "TATO"), as.numeric) 

df<- TATData %>%                                                      # VL working data frame
  mutate(TATR = if_else(TATR < 0 | TATR > 90, NA_real_, TATR),
         TATL = if_else(TATL < 0 | TATL > 90, NA_real_, TATL),
         TATD = if_else(TATD < 0 | TATD > 90, NA_real_, TATD),
         TATO = if_else(TATO < 0 | TATO > 90, NA_real_, TATO)
       )

#### EID Data set ####
EIDRaw <- EIDRaw %>% 
  select(No,`Facility Name`,District,Hub,`Age in Months`,`Entry Point`,`Date Collected`,`Date Received`,`Date Tested`,`Date dispatched to cphl`,`Date Dispatched`,`Printed at`,Status,`Rejection Reason`,`PCR 1st/2nd`
  )

EIDRaw$District <- trimws(EIDRaw$District)    # Trim the district names

# Cleaning column names and extracking working columns
EIDRaw <- EIDRaw %>%                           # rename to join
  rename(
    "HFacility" =   "Facility Name",
    "age"       =    "Age in Months",
    "POE"       =   "Entry Point",
    "date_collected"   = "Date Collected",
    "date_received"    =  "Date Received",
    "date_tested"      =  "Date Tested",
    "released_at"      =  "Date Dispatched",
    "date_downloaded"  =  "Printed at",
    "PCR"              =   "PCR 1st/2nd",
    "Ddispatch_to_CPHL" =   "Date dispatched to cphl",
    "Reject_reason"     =    "Rejection Reason",
    "HName"             =     "Hub")

EIDRaw$HName <- gsub(" Hub", "", as.character(EIDRaw$HName))     #  Removing word Hub from hub name

EIDRaw <- EIDRaw %>% 
  mutate(District  =        recode(District,
                                   # Old               # New
                                   "Luweero"           =      "Luwero",
                                   "Maracha-Terego"    =       "Maracha",
                                   "Ssembabule"         =       "Sembabule"))
# Capitalize the District names
EIDRaw$District   <-  toupper(EIDRaw$District)

# Attache RRH columns

EIDRaw <- EIDRaw %>% 
  left_join(Regions, by = "District")

#### Add year and month
EIDRaw <- EIDRaw %>%
  mutate(
    Yr  =  year(date_collected),
    Month =  month(date_collected)
  ) 
# Add quarter, "Qtr"
EIDRaw <- EIDRaw %>%
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))

## Create TAT columns in EID data set
# convert all dates
ETconvert <- EIDRaw %>%  
  mutate(date_received = lubridate::ymd(date_received),
         date_collected =lubridate::ymd(date_collected),
         released_at =lubridate::ymd(released_at),
         date_tested = lubridate::ymd(date_tested),
         date_downloaded =lubridate::ymd(date_downloaded)) 
# Created TAT Columns 
ETATData <- ETconvert %>%                                                                            
  mutate(
    TATR = date_received - date_collected,
    TATL = released_at   - date_received,
    TATD = date_downloaded - released_at,
    TATO = date_downloaded - date_collected) %>% 
  mutate_at(c("TATR", "TATL", "TATD", "TATO"), as.numeric) 

# Working data frame
Edf<- ETATData %>%                                                      
  mutate(TATR = if_else(TATR < 0 | TATR > 90, NA_real_, TATR),
         TATL = if_else(TATL < 0 | TATL > 90, NA_real_, TATL),
         TATD = if_else(TATD < 0 | TATD > 90, NA_real_, TATD),
         TATO = if_else(TATO < 0 | TATO > 90, NA_real_, TATO))

##########################################################################
##########################################################################
#### TAT Reports ####
#### OVERALL EID/VL - Both EID and VL ####
###### Receipt tat
# VL TAT
VL_RTAT <- df %>% 
  group_by(Qtr) %>% 
    select(TATR)
# EID TAT
EID_RTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATR)
# Both
VL_EIDRctTAT <- bind_rows(VL_RTAT,EID_RTAT)

# the median
median_EID_VL_Rct <- VL_EIDRctTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    TATRct   =  median(TATR, na.rm = TRUE),
    .groups = "drop"
  )

######## Lab TAT
VL_LTAT <- df %>% 
  group_by(Qtr) %>% 
  select(TATL)

EID_LTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATL)

VL_EIDLabTAT <- bind_rows(VL_LTAT,EID_LTAT)

# Compute the median Lab TAT
EID_VL_LabTAT <- VL_EIDLabTAT %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(TATLab = median(TATL, na.rm = TRUE),
   .groups = "drop"
   )


###### Download TAT
VL_DTAT <- df %>% 
  group_by(Qtr) %>% 
  select(TATD)

EID_DTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATD)

VL_EIDDldTAT <- bind_rows(VL_DTAT,EID_DTAT)

# the median
VL_EID_DldTAT <- VL_EIDDldTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(TATDld = median(TATD, na.rm = TRUE),
            .groups = "drop")

#### Overall TAT
VL_OTAT <- df %>% 
  group_by(Qtr) %>% 
  select(TATO)

EID_OTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATO)

VL_EIDovllTAT <- bind_rows(VL_OTAT,EID_OTAT)

# the median
VL_EID_ovllTAT <- VL_EIDovllTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(TATOvll = median(TATO, na.rm = TRUE),
            .groups = "drop")

###### Table with all TATs
all_TATs   <- median_EID_VL_Rct %>% 
  left_join(EID_VL_LabTAT, by = "Qtr") %>% 
  left_join(VL_EID_DldTAT, by = "Qtr") %>%
  left_join(VL_EID_ovllTAT, by = "Qtr")
  
#### Re-arrange the table
all_TATsPvt  <-  all_TATs %>% 
  pivot_longer(
    cols = c("TATRct","TATLab","TATDld" ,"TATOvll"),
    names_to = "Section",
    values_to = "TAT"
  )
#### Pivot wider
TATReport  <-   all_TATsPvt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TAT
  ) %>% 
  select(Section,`Oct-Dec`,`Jan-Mar`)

## The report table
tbl_TATReport <- flextable(TATReport)

#### format the table
tbl_TATReport  <-  tbl_TATReport %>% 
  add_header_row(
    values = c(
      Section   =  "NSRTN Section",
      "TAT for the respective quarters", ""
    )
  )

#### set header
tbl_TATReport  <-  tbl_TATReport %>% 
  set_header_labels(
    Section   =  "NSRTN Section",
    `Oct-Dec`  =  "Oct-Dec",
    `Jan-Ma`   =  "Jan-Ma"
  )

#### merge column
tbl_TATReport  <-  tbl_TATReport %>%
  merge_at(i = 1, j=2:3, part = "header")

#### merge vertical columns
tbl_TATReport  <-  tbl_TATReport %>%
  merge_v(j=1, part = "header")

#### the title
tbl_TATReport  <-  tbl_TATReport %>% 
  add_header_row(values = "Overall EID/VL TAT across the NSRTN sections",
                 colwidths = ncol(TATReport))

tbl_TATReport
#### Report-EID/VL TATs Rct, Lab, Download ####
###### Receipt tat
#### VL TAT
VL_RTAT <- df %>% 
  group_by(Qtr) %>% 
    select(TATR)

#### re-organize the table
VL_RTATPvt  <-  VL_RTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
    summarise(
      VLRct  =  median(TATR, na.rm = TRUE)
    )
#### EID TAT
EID_RTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATR)
# re-organize the table
EID_RTAT <- EID_RTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    EIDRct  =  median(TATR, na.rm = TRUE)
  )

#### combine the download
Rct_TAT   <-  bind_cols(VL_RTATPvt, EID_RTAT)

#### re-arrange the data set
Rct_TAT  <-  Rct_TAT %>% 
  select(-"Qtr...3") %>% 
  rename(
    "Qtr"   =  "Qtr...1"
  )

#### arrange
Rct_TATPvt  <-  Rct_TAT %>% 
  pivot_longer(
    cols = c("EIDRct","VLRct"),
    names_to = "Section",
    values_to = "TATRct"
  )

####
Rct_TATRpt  <-   Rct_TATPvt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATRct
  )
#### rename the values
TATRct_Rpt   <-    Rct_TATRpt %>% 
  mutate(Section   =   recode(Section,
                              "EIDRct" = "EID",
                              "VLRct"  =  "VL")
  )


######## Lab TAT
VL_LTAT <- df %>% 
  group_by(Qtr) %>% 
    select(TATL)

# re-organize the table
VL_LTAT <- VL_LTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    VLLab  =  median(TATL, na.rm = TRUE)
  )

#### EID Lab
EID_LTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATL)
# median
EID_LTAT <- EID_LTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    EIDLab  =  median(TATL, na.rm = TRUE)
  )


#### combine the download
Lab_TAT   <-  bind_cols(VL_LTAT, EID_LTAT)

#### re-arrange the data set
Lab_TAT  <-  Lab_TAT %>% 
  select(-"Qtr...3") %>% 
  rename(
    "Qtr"   =  "Qtr...1"
  )

#### arrange
Lab_TATPvt  <-  Lab_TAT %>% 
  pivot_longer(
    cols = c("EIDLab","VLLab"),
    names_to = "Section",
    values_to = "Lab_TAT"
  )

####
Lab_TATRpt  <-   Lab_TATPvt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Lab_TAT
  )
#### rename the values
LabTATRpt   <-    Lab_TATRpt %>% 
  mutate(Section   =   recode(Section,
                              "EIDLab" = "EID",
                              "VLLab"  =  "VL")
  )


###### Download TAT
VL_DTAT <- df %>% 
  group_by(Qtr) %>% 
  select(TATD)
# median
VL_DTAT   <-  VL_DTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
   group_by(Qtr) %>% 
  summarise(
    VLdld  =  median(TATD, na.rm = TRUE)
  )

## EID Download
EID_DTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATD)
# Median
EID_DTAT  <- EID_DTAT %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
    group_by(Qtr) %>% 
  summarise(
    EIDdld  =  median(TATD, na.rm = TRUE)
  )

#### combine the download
DownloadTAT   <-  bind_cols(VL_DTAT, EID_DTAT)

#### re-arrange the data set
DownloadTAT  <-  DownloadTAT %>% 
  select(-"Qtr...3") %>% 
  rename(
    "Qtr"   =  "Qtr...1"
  )

#### arrange
DownloadTATPvt  <-  DownloadTAT %>% 
  pivot_longer(
    cols = c("EIDdld","VLdld"),
    names_to = "Section",
    values_to = "Download_TAT"
  )

####
DownloadTAT_Rpt  <-   DownloadTATPvt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Download_TAT
  )
#### rename the values
DownloadTAT_Rpt   <-    DownloadTAT_Rpt %>% 
  mutate(Section   =   recode(Section,
                              "EIDdld" = "EID",
                              "VLdld"  =  "VL")
      )


#### Overall TAT
VL_OTAT <- df %>% 
  group_by(Qtr) %>% 
  select(TATO)
# Median
VL_OTAT  <-  VL_OTAT %>% 
group_by(Qtr) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
  summarise(
    VLOvll  =  median(TATO, na.rm = TRUE)
  )

## EID Overall
EID_OTAT <- Edf %>% 
  group_by(Qtr) %>% 
  select(TATO)

# median
EID_OTAT  <-  EID_OTAT %>% 
group_by(Qtr) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
  summarise(
    EIDOvll  =  median(TATO, na.rm = TRUE)
  )

#### combine overall EID
overallTAT   <- bind_cols(EID_OTAT,VL_OTAT) 

#### re-arrange the data set
overallTAT  <-  overallTAT %>% 
  select(-"Qtr...3") %>% 
    rename(
      "Qtr"   =  "Qtr...1"
    )
#### arrange
overallTATPvt  <-  overallTAT %>% 
  pivot_longer(
    cols = c("EIDOvll","VLOvll"),
    names_to = "Section",
    values_to = "Overall_TAT"
  )
####
overallTAT_Rpt  <-   overallTATPvt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Overall_TAT
  )
#### rename the values
overallTAT_Rpt   <-    overallTAT_Rpt %>% 
  mutate(Section   =   recode(Section,
          "EIDOvll" = "EID",
          "VLOvll"  =  "VL")
      )

#### combine all the tables to obtain the final result
Labcombined  <- bind_cols(TATRct_Rpt,LabTATRpt,
                          DownloadTAT_Rpt,overallTAT_Rpt)

#### selecting columns of interest
Labcombined   <-   Labcombined %>% 
  select(- c(Section...4,Section...7,Section...10))

#### renaming the columns
Labcombined   <-   Labcombined %>%
   rename(
     "Section"   =  "Section...1",
     "Jan-MarR"   =  "Jan-Mar...2",
     "Oct-DecR"   =   "Oct-Dec...3",
     "Jan-MarL"   =  "Jan-Mar...5",
     "Oct-DecL"   =   "Oct-Dec...6",
     "Jan-MarD"   =   "Jan-Mar...8",
     "Oct-DecD"   =   "Oct-Dec...9",
     "Jan-MarO"   =   "Jan-Mar...11",
     "Oct-DecO"   =  "Oct-Dec...12"   
       )

## The report table
tble_Labcombined <- flextable(Labcombined)

#### format the table
tble_Labcombined  <-  tble_Labcombined %>% 
  add_header_row(
    values = c(
      Section   =  "Lab Test",
      "Sample Receipt TAT by Qtr", "",
      "Lab Dispatch TAT by Qtr", "",
      "Results Download TAT by Qtr","",
      "Overall TAT by Qtr",""
    )
  )

#### set header
tble_Labcombined  <-  tble_Labcombined %>%  
  set_header_labels(
    Section   =  "Lab Test",
    `Jan-MarR`  =  "Jan-Mar",
    `Oct-DecR`   =  "Oct-Dec",
    `Jan-MarL`   =  "Jan-Mar",
    `Oct-DecL`   =  "Oct-Dec",
    `Jan-MarD`  = "Jan-Mar",
    `Oct-DecD`   =  "Oct-Dec",
    `Jan-MarO`   =   "Jan-Mar",
    `Oct-DecO`   =   "Oct-Dec"
  )

#### merge column
tble_Labcombined  <-  tble_Labcombined %>%
  merge_at(i = 1, j=2:3, part = "header") %>% 
  merge_at(i = 1, j=4:5, part = "header") %>% 
  merge_at(i = 1, j=6:7, part = "header") %>% 
  merge_at(i = 1, j=8:9, part = "header")  
  
#### merge vertical columns
tble_Labcombined  <-  tble_Labcombined %>%
  merge_v(j=1, part = "header")


#### Add vertical lines
tble_Labcombined  <-  tble_Labcombined %>%
  vline(j=c(1,3,5,7,9), part = "all")

#### the title
tble_Labcombined  <-  tble_Labcombined %>%
  add_header_row(values = "NSRTN sectional TAT by Lab Test",
                 colwidths = ncol(Labcombined))


tble_Labcombined

#### Report - EID RTAT by RRH#####
Efiltered_df <- Edf %>%
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Eall_reg_TATR <- Efiltered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>%
  summarise(
    TATRct = median(TATR, na.rm = TRUE))

#### re-arrange the table
RRH_TATRPvt   <-  Eall_reg_TATR %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATRct
  )

#### set the target coloring

RctTAT_color   <-  function(column_data){
  ifelse(column_data <= 7, "green",
         ifelse(column_data > 8 & column_data <= 14, "yellow","red"))
}

# the table
ERRH_RTAT <- flextable(RRH_TATRPvt)

### format the table
ERRH_RTAT  <- ERRH_RTAT %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
ERRH_RTAT  <- ERRH_RTAT %>%
  set_header_labels(
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  ERRH_RTAT  <- ERRH_RTAT %>%
    bg(j = col, bg = RctTAT_color(RRH_TATRPvt[[col]]), part = "body")
}


ERRH_RTAT  <- ERRH_RTAT %>%
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
ERRH_RTAT  <- ERRH_RTAT %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
ERRH_RTAT  <- ERRH_RTAT %>%
  vline(j = c(1, 2, 3), part = "all")


ERRH_RTAT   <-  ERRH_RTAT %>% 
  add_header_row(values="EID TAT Rct, by RRH", colwidths = ncol(Eall_reg_TATR))

ERRH_RTAT

#### Report - VL RTAT by RRH#####
filtered_df <- df %>%
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
all_reg_TATR <- filtered_df %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>%
  summarise(
    TATRct = median(TATR, na.rm = TRUE))

#### re-arrange the table
RRH_VLTATRPvt   <-  all_reg_TATR %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATRct
  )

#### set the target coloring

RctTAT_color   <-  function(column_data){
  ifelse(column_data <= 7, "green",
         ifelse(column_data > 8 & column_data <= 14, "yellow","red"))
}

# the table
VLRRH_RTAT <- flextable(RRH_VLTATRPvt)

### format the table
VLRRH_RTAT  <- VLRRH_RTAT %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
VLRRH_RTAT  <- VLRRH_RTAT %>%
  set_header_labels(
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  VLRRH_RTAT  <- VLRRH_RTAT %>%
    bg(j = col, bg = RctTAT_color(RRH_VLTATRPvt[[col]]), part = "body")
}


VLRRH_RTAT  <- VLRRH_RTAT %>%
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
VLRRH_RTAT  <- VLRRH_RTAT %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
VLRRH_RTAT  <- VLRRH_RTAT %>%
  vline(j = c(1, 2, 3), part = "all")


VLRRH_RTAT  <- VLRRH_RTAT %>%
  add_header_row(values="VL TAT Rct, by RRH", colwidths = ncol(Eall_reg_TATR))

VLRRH_RTAT

#####################################################################
#####################################################################
#### Report- INCMOMEPLETE# of EID and VL samples tested, and originating health facilities ####
# number VL tested (filtered out samples with test date)
No_test  <- VLRaw %>%          #  data frame of samples with results date
  filter(!is.na(test_date)) 
# number tested by sample type
No_test_type <- No_test %>% 
  group_by(sample_type) %>% 
  summarise(
    No_tested  = n()
  )
# the report table 
VL_testype <- flextable(No_test_type)
VL_testype


# number of facilities for each test type
vl_facilities <- No_test %>%
  group_by(sample_type) %>%
  summarise(No_facilities = n_distinct(HFacility)) %>%
  ungroup()

total_facilities <- No_test %>%
  summarise(sample_type = "Total", No_facilities = n_distinct(HFacility))

vl_facilities <- bind_rows(vl_facilities, total_facilities)

# Report table
vl_fac <- flextable(vl_facilities)
vl_fac

### EID report
# number tested (filtered out samples with test date)
ENo_test  <- EIDRaw %>% 
  filter(!is.na(date_tested)) 
# number tested by sample type
ENo_samples <- ENo_test %>% 
  summarise(
    No_samples = n(),
    No_facilities = n_distinct(HFacility)
  )

# the report table 
E_samples <- flextable(ENo_samples)
E_samples


######################################################################
#########################################################################
44
#### Reports - Percentage samples NSRTN ####
#### Report - Percentage samples NSRTN ####
#### VL 
VL7Days   <-  df %>%
  filter(!is.na(TATR)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  )

#### EID
EID7Days   <-  Edf %>%
  filter(!is.na(TATR)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE),
    .groups = "drop"
  )

#### calculate the percentage
PctTrp  <-   bind_cols(VL7Days,EID7Days)

#### the calculation
PctTrpdf <- PctTrp %>% 
  mutate(
    No.samples = rowSums(across(starts_with("No.samples")), na.rm = TRUE),
    No.timely  = rowSums(across(starts_with("No7Days")), na.rm = TRUE)
  ) %>% 
  mutate(
    `%within7Days` = round((No.timely / No.samples) * 100, 0)
  ) %>% 
  select(`Qtr...1`, No.samples, No.timely, `%within7Days`) %>% 
  rename(
    Qtr = `Qtr...1`
  ) %>% 
  mutate(
    Qtr = factor(Qtr, levels = c("Oct-Dec", "Jan-Mar"))
  ) %>% 
  arrange(Qtr)

#### the table
tbl_PctTrpdf  <-  flextable(PctTrpdf)

tbl_PctTrpdf  <-  tbl_PctTrpdf %>% 
  add_header_row(values = "%age samples received at CPHL within 7 day TAT",
                 colwidths = ncol(PctTrpdf))

tbl_PctTrpdf



#### Report - %age of VL samples received at CPHL within 7 days by RRH ####
RRH_VL7Days   <-  df %>%
  filter(!is.na(TATR)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
    group_by(RRH,Qtr) %>% 
    summarise(
           No.samples  =  n(),
           No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
       )
### calculate the proportion
RRH_VL7DaysRpt  <-  RRH_VL7Days %>% 
  mutate(
    `%within7 days`  =  round((No7Days/No.samples)*100,0)
  ) 

### select columns of interest
RRH_VL7DaysRpt   <-   RRH_VL7DaysRpt %>% 
    select(RRH,Qtr,`%within7 days`)

#### Pivot
timelyRct  <-  RRH_VL7DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within7 days`
  )


#### set the target coloring
RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_timelyRct   <-  flextable(timelyRct)

#### Format the table
tble_timelyRct  <-   tble_timelyRct %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
tble_timelyRct  <-   tble_timelyRct %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  tble_timelyRct  <-   tble_timelyRct %>% 
    bg(j = col, bg = RctPct_color(timelyRct[[col]]), part = "body")
}


tble_timelyRct  <-   tble_timelyRct %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
tble_timelyRct  <-   tble_timelyRct %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tble_timelyRct  <-   tble_timelyRct %>% 
  vline(j = c(1, 2, 3), part = "all")


tble_timelyRct  <-   tble_timelyRct %>% 
  add_header_row(values="%age of VL samples received at CPHL within 7 days, by RRH", 
                 colwidths = ncol(timelyRct))

tble_timelyRct

#### Report - %age of EID samples received within 7 days by RRH ####
RRH_EID7Days   <-  Edf %>%
  filter(!is.na(TATR)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE),
    .groups = "drop"
  )

### calculate the proportion
RRH_EID7DaysRpt  <-  RRH_EID7Days %>% 
  mutate(
    `%within7 days`  =  round((No7Days/No.samples)*100,0)
  ) 

### select columns of interest
RRH_EID7DaysRpt   <-   RRH_EID7DaysRpt %>% 
  select(RRH,Qtr,`%within7 days`)

#### Pivot
EtimelyRct  <-  RRH_EID7DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within7 days`
  )

#### set the target coloring
RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
Etble_timelyRct   <-  flextable(EtimelyRct)

#### Format the table
Etble_timelyRct  <-   Etble_timelyRct %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
Etble_timelyRct  <-   Etble_timelyRct %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Etble_timelyRct  <-   Etble_timelyRct %>% 
    bg(j = col, bg = RctPct_color(EtimelyRct[[col]]), part = "body")
}


Etble_timelyRct  <-   Etble_timelyRct %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
Etble_timelyRct  <-   Etble_timelyRct %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
Etble_timelyRct  <-   Etble_timelyRct %>% 
  vline(j = c(1, 2, 3), part = "all")


Etble_timelyRct  <-   Etble_timelyRct %>% 
  add_header_row(values="%age of EID samples received at CPHL within 7 days, by RRH", 
                 colwidths = ncol(EtimelyRct))

Etble_timelyRct

############################################################################
############################################################################
#### Report - Percentage downloaded within 3 days ####
#### VL
VL3Days   <-  df %>%
  filter(!is.na(TATD)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE)
  )
#### EID
EID3Days   <-  Edf %>%
  filter(!is.na(TATD)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE),
    .groups = "drop"
  )

#### calculate the percentage
PctDwld  <-   bind_cols(VL3Days,EID3Days)

#### the calculation
PctDwlddf <- PctDwld %>% 
  mutate(
    No.samples = rowSums(across(starts_with("No.samples")), na.rm = TRUE),
    No.timely  = rowSums(across(starts_with("No3Days")), na.rm = TRUE)
  ) %>% 
  mutate(
    `%within4Days` = round((No.timely / No.samples) * 100, 0)
  ) %>% 
  select(`Qtr...1`, No.samples, No.timely, `%within4Days`) %>% 
  rename(
    Qtr = `Qtr...1`
  ) %>% 
  mutate(
    Qtr = factor(Qtr, levels = c("Oct-Dec", "Jan-Mar"))
  ) %>% 
  arrange(Qtr)

#### the table
tbl_PctDwlddf  <-  flextable(PctDwlddf)

tbl_PctDwlddf  <-  tbl_PctDwlddf %>% 
  add_header_row(values = "%age samples downloaded within 3 days",
                 colwidths = ncol(PctDwlddf))

tbl_PctDwlddf

#### Report - %age of Samples Downloaded within 3 days by RRH ####
#### VL samples ####
RRH_VL3Days   <-  df %>%
  filter(!is.na(TATD)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE)
  )

### calculate the proportion
RRH_VL3DaysRpt  <-  RRH_VL3Days %>% 
  mutate(
    `%within3 days`  =  round((No3Days/No.samples)*100,0)
  ) 

### select columns of interest
RRH_VL3DaysRpt   <-   RRH_VL3DaysRpt %>% 
  select(RRH,Qtr,`%within3 days`)

#### Pivot
timelyDLD  <-  RRH_VL3DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within3 days`
  )

#### set the target coloring
RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_timelyDLD   <-  flextable(timelyDLD)

#### Format the table
tble_timelyDLD  <-   tble_timelyDLD %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
tble_timelyDLD  <-   tble_timelyDLD %>%
  set_header_labels(
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  tble_timelyDLD  <-   tble_timelyDLD %>%
    bg(j = col, bg = RctPct_color(timelyDLD[[col]]), part = "body")
}


tble_timelyDLD  <-   tble_timelyDLD %>%
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
tble_timelyDLD  <-   tble_timelyDLD %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tble_timelyDLD  <-   tble_timelyDLD %>%
  vline(j = c(1, 2, 3), part = "all")


tble_timelyDLD  <-   tble_timelyDLD %>% 
  add_header_row(values="%age of VL samples downloaded within 3 days, by RRH", 
                 colwidths = ncol(timelyDLD))

tble_timelyDLD

#### EID samples ####
RRH_EID3Days   <-  Edf %>%
  filter(!is.na(TATD)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE),
    .groups = "drop"
  )

### calculate the proportion
RRH_EID3DaysRpt  <-  RRH_EID3Days %>% 
  mutate(
    `%within3 days`  =  round((No3Days/No.samples)*100,0)
  ) 

### select columns of interest
RRH_EID3DaysRpt   <-   RRH_EID3DaysRpt %>% 
  select(RRH,Qtr,`%within3 days`)

#### Pivot
EtimelyDLD  <-  RRH_EID3DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within3 days`
  )

#### set the target coloring
RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
Etble_timelyDLD   <-  flextable(EtimelyDLD)

#### Format the table
Etble_timelyDLD  <-   Etble_timelyDLD %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
Etble_timelyDLD  <-   Etble_timelyDLD %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Etble_timelyDLD  <-   Etble_timelyDLD %>% 
    bg(j = col, bg = RctPct_color(EtimelyDLD[[col]]), part = "body")
}


Etble_timelyDLD  <-   Etble_timelyDLD %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
Etble_timelyDLD  <-   Etble_timelyDLD %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
Etble_timelyDLD  <-   Etble_timelyDLD %>% 
  vline(j = c(1, 2, 3), part = "all")


Etble_timelyDLD  <-   Etble_timelyDLD %>% 
  add_header_row(values="%age of EID samples downloaded within 3 days, by RRH", 
                 colwidths = ncol(timelyDLD))

Etble_timelyDLD

#############################################################################
#### %age samples received within 7 days (both EID and VL)####
#### VL
OvallVLPct   <-  df %>%
  filter(!is.na(TATR)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  ) %>% 
  mutate(
    VL =   round((No7Days/No.samples)*100,0)
  )
### The report
OvallVLRpt  <-   OvallVLPct %>% 
  select(Qtr,VL)

#### VL
OvallEIDPct   <-  Edf %>%
  filter(!is.na(TATR)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  ) %>% 
  mutate(
    EID   =   round((No7Days/No.samples)*100,0)
  )
### The report
OvallEIDRpt  <-   OvallEIDPct %>% 
  select(Qtr,EID)

#### add the data frames
allRctPct   <-  bind_cols(OvallVLRpt,OvallEIDRpt) %>% 
  select(Qtr...1,VL,EID) %>% 
  rename(
    "Qtr"   =  "Qtr...1"
  )
#### re-arrange the table
allRcrRpt  <-  allRctPct %>% 
  pivot_longer(
    cols = c("VL","EID"),
    names_to = "Sample Type",
    values_to = "%age samples"
  )
#### the graph
allRcrRpt  <-  allRcrRpt %>% 
  mutate(color_category   =  case_when(
    `%age samples` >= 85 ~ ">=85%",
    `%age samples` >= 50 & `%age samples` < 85 ~ "50%-84%",
    `%age samples` >= 0   & `%age samples` < 50   ~ "<50"
  ))

allRcrRpt$Qtr   <-  factor(allRcrRpt$Qtr, levels = c("Oct-Dec","Jan-Mar"))

#### the graph
ggplot(allRcrRpt, aes(x = `Sample Type`, y = `%age samples`, fill = Qtr)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = `%age samples`), vjust = -0.3, size = 3.5, position = position_dodge(width = 0.9)) +  # Ensure text aligns with bars
  geom_hline(yintercept = 85, color = "#950606", linetype = "dashed") +  
  annotate("text", x = 2, y = 85, label = "Target (85%)", vjust = -0.5, color = "#950606") +  
  theme_minimal() +
  labs(
    title = "Percentage of samples received at CPHL within 7 days",
    x = "Sample Type",
    y = "Percentage of Samples"
  )

#### Report - %age of samples received within 7 days ####
#### VL
OvallVLPct1   <-  df %>%
  filter(!is.na(TATR)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  ) 

#### EID
OvallEIDPct1   <-  Edf %>%
  filter(!is.na(TATR)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  )

#### add the data frames
allRctPct1   <-  bind_cols(OvallVLPct1,OvallEIDPct1) 

### ADD THE COLUMNS
allRctPct1  <-  allRctPct1 %>% 
  mutate(
    No.samples = rowSums(select(., starts_with("No.")), na.rm = TRUE),
    No7Days    = rowSums(select(., starts_with("No7")), na.rm = TRUE)
  ) %>% 
    mutate(
        PctSamples   =   round((No7Days/No.samples)*100,0)
  )
#### Clean the table
allRctRpt  <-  allRctPct1 %>% 
  select(Qtr...1,PctSamples) %>% 
    rename(
      "Qtr"  =  "Qtr...1"
    )
#### Ensure systematic naming
allRctRpt$Qtr   <-  factor(allRctRpt$Qtr, levels = c("Oct-Dec", "Jan-Mar"))

#### the graph
ggplot(allRctRpt, aes(x = Qtr, y = PctSamples)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = PctSamples), vjust = -0.3, size = 3.5, position = position_dodge(width = 0.9)) +  # Ensure text aligns with bars
  geom_hline(yintercept = 85, color = "#950606", linetype = "dashed") +  
  annotate("text", x = 2, y = 85, label = "Target (85%)", vjust = -0.5, color = "#950606") +  
  theme_minimal() +
  labs(
    title = "Overall (EID/VL) %age of samples received at CPHL within 7 days",
    x = "Quarter",
    y = "Percentage of Samples"
  )


#############################################################
#############################################################
#### Annex Section ####
#### Report - VL TATR by Hub#####
filtered_df <- df %>%
  filter(!is.na(HName)) %>% 
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Hub_VLTATR <- filtered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName,Qtr) %>%
  summarise(
    TATRct = median(TATR, na.rm = TRUE))

#### re-arrange the table
Hub_VLTATRPvt   <-  Hub_VLTATR %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATRct
  ) %>% 
  select(RRH,HName,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

RctTAT_color   <-  function(column_data){
  ifelse(column_data <= 7, "green",
         ifelse(column_data >= 8 & column_data <= 14, "yellow","red"))
}

# the table
Hub_VLTATR <- flextable(Hub_VLTATRPvt)

### format the table
Hub_VLTATR  <- Hub_VLTATR %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
Hub_VLTATR  <- Hub_VLTATR %>%
  set_header_labels(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Hub_VLTATR  <- Hub_VLTATR %>%
    bg(j = col, bg = RctTAT_color(Hub_VLTATRPvt[[col]]), part = "body")
}


Hub_VLTATR  <- Hub_VLTATR %>%
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
Hub_VLTATR  <- Hub_VLTATR %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
Hub_VLTATR  <- Hub_VLTATR %>%
  vline(j = c(1, 2, 3,4), part = "all")


Hub_VLTATR  <- Hub_VLTATR %>%
  add_header_row(values="EID TAT Rct, by Hub", colwidths = ncol(Hub_VLTATRPvt))

Hub_VLTATR
#### Report - EID TATR by Hub#####
Efiltered_df <- Edf %>%
  filter(!is.na(HName)) %>% 
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Hub_EIDTATR <- Efiltered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName,Qtr) %>%
  summarise(
    TATRct = median(TATR, na.rm = TRUE))

#### re-arrange the table
Hub_EIDTATRPvt   <-  Hub_EIDTATR %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATRct
  )

#### set the target coloring

RctTAT_color   <-  function(column_data){
  ifelse(column_data <= 7, "green",
         ifelse(column_data >= 8 & column_data <= 14, "yellow","red"))
}

# the table
Hub_EIDTATR <- flextable(Hub_EIDTATRPvt)

### format the table
Hub_EIDTATR  <- Hub_EIDTATR %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
Hub_EIDTATR  <- Hub_EIDTATR %>%
  set_header_labels(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Hub_EIDTATR  <- Hub_EIDTATR %>%
    bg(j = col, bg = RctTAT_color(Hub_EIDTATRPvt[[col]]), part = "body")
}


Hub_EIDTATR  <- Hub_EIDTATR %>%
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
Hub_EIDTATR  <- Hub_EIDTATR %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
Hub_EIDTATR  <- Hub_EIDTATR %>%
  vline(j = c(1, 2, 3,4), part = "all")


Hub_EIDTATR  <- Hub_EIDTATR %>%
  add_header_row(values="EID TAT Rct, by Hub", colwidths = ncol(Hub_EIDTATRPvt))

Hub_EIDTATR


#### Report - VL TATR poorly performing  Health facilities#####
filtered_df <-  df %>% 
  filter(!is.na(HName)) %>% 
   filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Hf_VLTATR <- filtered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(TATR > 7) %>% 
  group_by(RRH,HName,HFacility,Qtr) %>%
  summarise(
    TATRct = median(TATR, na.rm = TRUE))

#### re-arrange the table
Hf_VLTATRPvt   <-  Hf_VLTATR %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATRct
  ) %>% 
  select(RRH,HName,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

RctTAT_color   <-  function(column_data){
  ifelse(column_data <= 7, "green",
         ifelse(column_data >= 8 & column_data <= 14, "yellow","red"))
}

# the table
Hf_VLTATR <- flextable(Hf_VLTATRPvt)

### format the table
Hf_VLTATR  <- Hf_VLTATR %>% 
  add_header_row(values = c(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
Hf_VLTATR  <- Hf_VLTATR %>% 
  set_header_labels(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
      )

### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Hf_VLTATR  <- Hf_VLTATR %>% 
    bg(j = col, bg = RctTAT_color(Hf_VLTATRPvt[[col]]), part = "body")
}


Hf_VLTATR  <- Hf_VLTATR %>% 
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
Hf_VLTATR  <- Hf_VLTATR %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 3, part = "header")

# Adding vertical lines to improve readability
Hf_VLTATR  <- Hf_VLTATR %>%
  vline(j = c(1, 2, 3,4,5), part = "all")


Hf_VLTATR  <- Hf_VLTATR %>%
  add_header_row(values="Sites with >7 Day VL sample receipt TAT", 
                 colwidths = ncol(Hf_VLTATRPvt))

Hf_VLTATR

#### Report - EID TATR poor performing sites#####
Efiltered_df <- Edf %>%
  filter(!is.na(HName)) %>% 
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
EHf_VLTATR <- Efiltered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(TATR > 7) %>% 
  group_by(RRH,HName,HFacility,Qtr) %>%
  summarise(
    TATRct = median(TATR, na.rm = TRUE))

#### re-arrange the table
EHf_VLTATRPvt   <-  EHf_VLTATR %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATRct
  ) %>% 
  select(RRH,HName,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

RctTAT_color   <-  function(column_data){
  ifelse(column_data <= 7, "green",
         ifelse(column_data >= 8 & column_data <= 14, "yellow","red"))
}

# the table
EHf_VLTATR <- flextable(EHf_VLTATRPvt)

### format the table
EHf_VLTATR  <- EHf_VLTATR %>% 
  add_header_row(values = c(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
EHf_VLTATR  <- EHf_VLTATR %>% 
  set_header_labels(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  EHf_VLTATR  <- EHf_VLTATR %>% 
    bg(j = col, bg = RctTAT_color(EHf_VLTATRPvt[[col]]), part = "body")
}


EHf_VLTATR  <- EHf_VLTATR %>% 
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
EHf_VLTATR  <- EHf_VLTATR %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 3, part = "header")

# Adding vertical lines to improve readability
EHf_VLTATR  <- EHf_VLTATR %>%
  vline(j = c(1, 2, 3,4,5), part = "all")


EHf_VLTATR  <- EHf_VLTATR %>%
  add_header_row(values="Sites with >7 Day EID Sample Receipt TAT", colwidths = ncol(Hf_VLTATRPvt))

EHf_VLTATR


###########################################################################
#### Report - %age of VL samples received at CPHL within 7 days by Hub####
Hub_VL7Days   <-  df %>%
  filter(!is.na(TATR)) %>%
  filter(!is.na(RRH)) %>% 
  filter(!is.na(HName)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH, HName, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  )
### calculate the proportion
Hub_VL7DaysRpt  <-  Hub_VL7Days %>% 
  mutate(
    `%within7 days`  =  round((No7Days/No.samples)*100,0)
  ) 

### select columns of interest
Hub_VL7DaysRpt   <-   Hub_VL7DaysRpt %>% 
  select(RRH,Qtr,`%within7 days`)

#### Pivot
Hub_timelyRct  <-  Hub_VL7DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within7 days`
  ) %>% 
  select(HName,RRH,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_HtimelyRct   <-  flextable(Hub_timelyRct)

#### Format the table
tble_HtimelyRct  <-   tble_HtimelyRct %>% 
  add_header_row(values = c(
    HName   =  "Hub Name",
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
tble_HtimelyRct  <-   tble_HtimelyRct %>% 
  set_header_labels(
    HName   =  "Hub Name",
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  tble_HtimelyRct  <-   tble_HtimelyRct %>% 
    bg(j = col, bg = RctPct_color(Hub_timelyRct[[col]]), part = "body")
}


tble_HtimelyRct  <-   tble_HtimelyRct %>% 
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
tble_HtimelyRct  <-   tble_HtimelyRct %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_HtimelyRct  <-   tble_HtimelyRct %>%
  vline(j = c(1, 2, 3, 4), part = "all")


tble_HtimelyRct  <-   tble_HtimelyRct %>%
  add_header_row(values="%age of VL samples received at CPHL within 7 days, by Hub", 
                 colwidths = ncol(Hub_timelyRct))

tble_HtimelyRct

#### Report - %age of EID sample received within 7 days by hub ####
EHub_VL7Days   <-  Edf %>%
  filter(!is.na(TATR)) %>%
  filter(!is.na(RRH)) %>% 
  filter(!is.na(HName)) %>%
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH, HName, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  )
### calculate the proportion
EHub_VL7DaysRpt  <-  EHub_VL7Days %>% 
  mutate(
    `%within7 days`  =  round((No7Days/No.samples)*100,0)
  ) 

### select columns of interest
EHub_VL7DaysRpt   <-   EHub_VL7DaysRpt %>% 
  select(RRH,Qtr,`%within7 days`)

#### Pivot
EHub_timelyRct  <-  EHub_VL7DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within7 days`
  ) %>% 
  select(HName,RRH,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
Etble_HtimelyRct   <-  flextable(EHub_timelyRct)

#### Format the table
Etble_HtimelyRct  <-   Etble_HtimelyRct %>% 
  add_header_row(values = c(
    HName   =  "Hub Name",
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
Etble_HtimelyRct  <-   Etble_HtimelyRct %>% 
  set_header_labels(
    HName   =  "Hub Name",
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Etble_HtimelyRct  <-   Etble_HtimelyRct %>% 
    bg(j = col, bg = RctPct_color(EHub_timelyRct[[col]]), part = "body")
}


Etble_HtimelyRct  <-   Etble_HtimelyRct %>% 
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
Etble_HtimelyRct  <-   Etble_HtimelyRct %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
Etble_HtimelyRct  <-   Etble_HtimelyRct %>% 
  vline(j = c(1, 2, 3, 4), part = "all")


Etble_HtimelyRct  <-   Etble_HtimelyRct %>% 
  add_header_row(values="%age of EID samples received at CPHL within 7 days, by Hub", 
                 colwidths = ncol(EHub_timelyRct))

Etble_HtimelyRct

#### Report - list of HFs with <50% of samples received within 7 days ####
Hf_VL7Days   <-  df %>%
  filter(!is.na(TATR)) %>%
  filter(!is.na(RRH)) %>% 
    filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH, HName,HFacility, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  )
### calculate the proportion
Hf_VL7Days  <-  Hf_VL7Days %>% 
  mutate(
    `%within7 days`  =  round((No7Days/No.samples)*100,0)
  ) 
#### filter out poorly performing
Hf_VL7DaysRpt  <-  Hf_VL7Days %>% 
  filter(`%within7 days`  < 50)

### select columns of interest
Hf_VL7DaysRpt   <-   Hf_VL7DaysRpt %>% 
  select(RRH,Qtr,HName,HFacility,`%within7 days`)

#### Pivot
Hf_VL7DaysPvt  <-  Hf_VL7DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within7 days`
  ) %>% 
  select(HFacility, HName,RRH,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_Hf_VL7DaysPvt   <-  flextable(Hf_VL7DaysPvt)

#### Format the table
tble_Hf_VL7DaysPvt  <-   tble_Hf_VL7DaysPvt %>% 
  add_header_row(values = c(
    HFacility  =  "Health Facility",
    HName   =  "Hub Name",
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
tble_Hf_VL7DaysPvt  <-   tble_Hf_VL7DaysPvt %>% 
  set_header_labels(
    HFacility  =  "Health Facility",
    HName   =  "Hub Name",
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Oct-Dec","Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tble_Hf_VL7DaysPvt  <-   tble_Hf_VL7DaysPvt %>% 
    bg(j = col, bg = RctPct_color(Hf_VL7DaysPvt[[col]]), part = "body")
}


tble_Hf_VL7DaysPvt  <-   tble_Hf_VL7DaysPvt %>% 
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
tble_Hf_VL7DaysPvt  <-   tble_Hf_VL7DaysPvt %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_Hf_VL7DaysPvt  <-   tble_Hf_VL7DaysPvt %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tble_Hf_VL7DaysPvt  <-   tble_Hf_VL7DaysPvt %>% 
  add_header_row(values="Line list of sites with <50% of VL samples received at CPHL within 7 Days", 
                 colwidths = ncol(Hf_VL7DaysPvt))

tble_Hf_VL7DaysPvt

#### Report - %age EID samples received with targeted TAT by health facility ####

EHf_VL7Days   <-  Edf %>%
  filter(!is.na(TATR)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH, HName,HFacility, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No7Days     =  sum(TATR  <= 7, na.rm = TRUE)
  )
### calculate the proportion
EHf_VL7Days  <-  EHf_VL7Days %>% 
  mutate(
    `%within7 days`  =  round((No7Days/No.samples)*100,0)
  ) 
#### filter out poorly performing
EHf_VL7DaysRpt  <-  EHf_VL7Days %>% 
  filter(`%within7 days`  < 50)

### select columns of interest
EHf_VL7DaysRpt   <-   EHf_VL7DaysRpt %>% 
  select(RRH,Qtr,HName,HFacility,`%within7 days`)

#### Pivot
EHf_VL7DaysPvt  <-  EHf_VL7DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within7 days`
  ) %>% 
  select(HFacility, HName,RRH,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

RctPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
Etble_Hf_VL7DaysPvt   <-  flextable(EHf_VL7DaysPvt)

#### Format the table
Etble_Hf_VL7DaysPvt   <-   Etble_Hf_VL7DaysPvt  %>% 
  add_header_row(values = c(
    HFacility  =  "Health Facility",
    HName   =  "Hub Name",
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
Etble_Hf_VL7DaysPvt   <-   Etble_Hf_VL7DaysPvt  %>% 
  set_header_labels(
    HFacility  =  "Health Facility",
    HName   =  "Hub Name",
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Oct-Dec","Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  Etble_Hf_VL7DaysPvt   <-   Etble_Hf_VL7DaysPvt  %>% 
    bg(j = col, bg = RctPct_color(EHf_VL7DaysPvt[[col]]), part = "body")
}


Etble_Hf_VL7DaysPvt   <-   Etble_Hf_VL7DaysPvt  %>% 
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
Etble_Hf_VL7DaysPvt   <-   Etble_Hf_VL7DaysPvt  %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
Etble_Hf_VL7DaysPvt   <-   Etble_Hf_VL7DaysPvt  %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


Etble_Hf_VL7DaysPvt   <-   Etble_Hf_VL7DaysPvt  %>% 
  add_header_row(values="Line list of sites with <50% of EID samples received at CPHL within 7 Days", 
                 colwidths = ncol(Hf_VL7DaysPvt))

Etble_Hf_VL7DaysPvt
###########################################################################
#### Report - VL results Download TAT by HUB ####
filtered_df <- df %>%
  filter(!is.na(HName)) %>% 
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Hub_VLTATD <- filtered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName,Qtr) %>%
  summarise(
    TATDld = median(TATD, na.rm = TRUE))

#### re-arrange the table
Hub_VLTATDPvt   <-  Hub_VLTATD %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATDld
  ) %>% 
  select(RRH,HName,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

DldTAT_color   <-  function(column_data){
  ifelse(column_data <= 3, "green",
         ifelse(column_data >= 4 & column_data <= 7, "yellow","red"))
}

# the table
Hub_VLTATDRpt <- flextable(Hub_VLTATDPvt)

### format the table
Hub_VLTATDRpt  <- Hub_VLTATDRpt %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
Hub_VLTATDRpt  <- Hub_VLTATDRpt %>% 
  set_header_labels(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Hub_VLTATDRpt  <- Hub_VLTATDRpt %>% 
    bg(j = col, bg = DldTAT_color(Hub_VLTATDPvt[[col]]), part = "body")
}


Hub_VLTATDRpt  <- Hub_VLTATDRpt %>% 
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
Hub_VLTATDRpt  <- Hub_VLTATDRpt %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
Hub_VLTATDRpt  <- Hub_VLTATDRpt %>% 
  vline(j = c(1, 2, 3,4), part = "all")


Hub_VLTATDRpt  <- Hub_VLTATDRpt %>% 
  add_header_row(values="EID TAT Download, by Hub", colwidths = ncol(Hub_VLTATDPvt))

Hub_VLTATDRpt
#### Report - EID results Download TAT by HUB ####
filtered_Edf <- Edf %>%
filter(!is.na(HName)) %>% 
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Hub_EIDTATD <- filtered_Edf %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName,Qtr) %>%
  summarise(
    TATDld = median(TATD, na.rm = TRUE))

#### re-arrange the table
Hub_EIDTATDPvt   <-  Hub_EIDTATD %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATDld
  ) %>% 
  select(RRH,HName,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

DldTAT_color   <-  function(column_data){
  ifelse(column_data <= 3, "green",
         ifelse(column_data >= 4 & column_data <= 7, "yellow","red"))
}

# the table
Hub_EIDTATDRpt <- flextable(Hub_EIDTATDPvt)

### format the table
Hub_EIDTATDRpt  <- Hub_EIDTATDRpt %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
Hub_EIDTATDRpt  <- Hub_EIDTATDRpt %>%
  set_header_labels(
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Hub_EIDTATDRpt  <- Hub_EIDTATDRpt %>%
    bg(j = col, bg = DldTAT_color(Hub_EIDTATDPvt[[col]]), part = "body")
}


Hub_EIDTATDRpt  <- Hub_EIDTATDRpt %>%
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
Hub_EIDTATDRpt  <- Hub_EIDTATDRpt %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
Hub_EIDTATDRpt  <- Hub_EIDTATDRpt %>%
  vline(j = c(1, 2, 3,4), part = "all")


Hub_EIDTATDRpt  <- Hub_EIDTATDRpt %>%
  add_header_row(values="EID TAT Download, by Hub", 
                 colwidths = ncol(Hub_EIDTATDPvt))

Hub_EIDTATDRpt

#### Report - sites with poor VL sample results download TAT ####
filtered_df <-  df %>% 
  filter(!is.na(HName)) %>% 
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Hf_VLTATD <- filtered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(TATD > 3) %>% 
  group_by(RRH,HName,HFacility,Qtr) %>%
  summarise(
    TATDld = median(TATD, na.rm = TRUE))

#### re-arrange the table
Hf_VLTATDPvt   <-  Hf_VLTATD %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATDld
  ) %>% 
  select(RRH,HName,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

DldTAT_color   <-  function(column_data){
  ifelse(column_data <= 3, "green",
         ifelse(column_data > 3 & column_data <= 7, "yellow","red"))
}

# the table
Hf_VLTATD <- flextable(Hf_VLTATDPvt)

### format the table
Hf_VLTATD  <- Hf_VLTATD %>% 
  add_header_row(values = c(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
Hf_VLTATD  <- Hf_VLTATD %>% 
  set_header_labels(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Hf_VLTATD  <- Hf_VLTATD %>% 
    bg(j = col, bg = DldTAT_color(Hf_VLTATDPvt[[col]]), part = "body")
}


Hf_VLTATD  <- Hf_VLTATD %>% 
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
Hf_VLTATD  <- Hf_VLTATD %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 3, part = "header")

# Adding vertical lines to improve readability
Hf_VLTATD  <- Hf_VLTATD %>% 
  vline(j = c(1, 2, 3,4,5), part = "all")


Hf_VLTATD  <- Hf_VLTATD %>% 
  add_header_row(values="Sites with >3 Day VL sample download TAT", 
                 colwidths = ncol(Hf_VLTATDPvt))

Hf_VLTATD

#### Report - sites with poor EID sample results download TAT#####
Efiltered_df <- Edf %>%
  filter(!is.na(HName)) %>% 
  filter(!is.na(RRH))

# Group by RRH, district, and HName, then calculate the median of TATR
Hf_EIDTATD <- Efiltered_df %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(TATD > 3) %>% 
  group_by(RRH,HName,HFacility,Qtr) %>%
  summarise(
    TATDld = median(TATD, na.rm = TRUE))

#### re-arrange the table
Hf_EIDTATDPvt   <-  Hf_EIDTATD %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = TATDld
  ) %>% 
  select(RRH,HName,`Oct-Dec`,`Jan-Mar`)

#### set the target coloring

DldTAT_color   <-  function(column_data){
  ifelse(column_data <= 3, "green",
         ifelse(column_data > 3 & column_data <= 7, "yellow","red"))
}

# the table
Hf_EIDTATD <- flextable(Hf_EIDTATDPvt)

### format the table
Hf_EIDTATD  <- Hf_EIDTATD %>% 
  add_header_row(values = c(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
Hf_EIDTATD  <- Hf_EIDTATD %>% 
  set_header_labels(
    HFacility  =  "Health Facility Name",
    RRH  =  "RRH",
    HName   =  "Hub Name",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  Hf_EIDTATD  <- Hf_EIDTATD %>% 
    bg(j = col, bg = DldTAT_color(Hf_EIDTATDPvt[[col]]), part = "body")
}


Hf_EIDTATD  <- Hf_EIDTATD %>%
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
Hf_EIDTATD  <- Hf_EIDTATD %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 3, part = "header")

# Adding vertical lines to improve readability
Hf_EIDTATD  <- Hf_EIDTATD %>%
  vline(j = c(1, 2, 3,4,5), part = "all")


Hf_EIDTATD  <- Hf_EIDTATD %>%
  add_header_row(values="Sites with >3 Day EID sample download TAT", 
                 colwidths = ncol(Hf_EIDTATDPvt))

Hf_EIDTATD
###########################################################################
#### Report - %age VL samples downloaded within 3 days by HUB ####
Hub_VL3Days   <-  df %>%
  filter(!is.na(TATD)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE)
  )

### calculate the proportion
Hub_VL3DaysRpt  <-  Hub_VL3Days %>% 
  mutate(
    `%within3 days`  =  round((No3Days/No.samples)*100,0)
  ) 

### select columns of interest
Hub_VL3DaysRpt   <-   Hub_VL3DaysRpt %>% 
  select(RRH,Qtr,`%within3 days`)

#### Pivot
timelyHubDLD  <-  Hub_VL3DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within3 days`
  ) %>% 
  select(HName,RRH,HName, `Oct-Dec`,`Jan-Mar`)

#### set the target coloring
DldPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_timelyHubDLD    <-  flextable(timelyHubDLD)

#### Format the table
tble_timelyHubDLD  <-   tble_timelyHubDLD %>% 
  add_header_row(values = c(
    HName   =  "Hub Name",
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
tble_timelyHubDLD  <-   tble_timelyHubDLD %>%
  set_header_labels(
    HName   =  "Hub Name",
    RRH  =  "RRH",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
      )

### List Columns to Apply Coloring ###
columns <- c("Oct-Dec","Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tble_timelyHubDLD  <-   tble_timelyHubDLD %>%
    bg(j = col, bg = RctPct_color(timelyHubDLD[[col]]), part = "body")
}


tble_timelyHubDLD  <-   tble_timelyHubDLD %>%
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
tble_timelyHubDLD  <-   tble_timelyHubDLD %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_timelyHubDLD  <-   tble_timelyHubDLD %>%
  vline(j = c(1, 2, 3, 4), part = "all")


tble_timelyHubDLD  <-   tble_timelyHubDLD %>%
  add_header_row(values="%age of VL samples downloaded within 3 days, by Hub", 
                 colwidths = ncol(timelyHubDLD))

tble_timelyHubDLD

#### Report - %age EID samples downloaded within 3 days by HUB ####
Hub_EID3Days   <-  Edf %>%
  filter(!is.na(TATD)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE)
  )

### calculate the proportion
Hub_EID3DaysRpt  <-  Hub_EID3Days %>% 
  mutate(
    `%within3 days`  =  round((No3Days/No.samples)*100,0)
  ) 

### select columns of interest
Hub_EID3DaysRpt   <-   Hub_EID3DaysRpt %>% 
  select(RRH,Qtr,HName, `%within3 days`)

#### Pivot
EtimelyDLD  <-  Hub_EID3DaysRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within3 days`
  )%>% 
  select(HName,RRH,HName, `Oct-Dec`,`Jan-Mar`)

#### set the target coloring
DldPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_EtimelyDLD   <-  flextable(EtimelyDLD)

#### Format the table
tble_EtimelyDLD <-   tble_EtimelyDLD %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    HName  =  "Hub Name",
    "Reporting Period", ""
  ))
## set the labels
tble_EtimelyDLD <-   tble_EtimelyDLD %>% 
  set_header_labels(
    RRH  =  "RRH",
    HName  =  "Hub Name",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
      )

### List Columns to Apply Coloring ###
columns <- c("Oct-Dec","Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tble_EtimelyDLD <-   tble_EtimelyDLD %>% 
    bg(j = col, bg = RctPct_color(EtimelyDLD[[col]]), part = "body")
}


tble_EtimelyDLD <-   tble_EtimelyDLD %>% 
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") 

# Merging vertically for the first five columns in the header
tble_EtimelyDLD <-   tble_EtimelyDLD %>% 
  merge_v(j = 1, part = "header") %>%  
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_EtimelyDLD <-   tble_EtimelyDLD %>% 
  vline(j = c(1, 2, 3, 4), part = "all")


tble_EtimelyDLD <-   tble_EtimelyDLD %>% 
  add_header_row(values="%age of EID samples downloaded within 3 days, by Hub", 
                 colwidths = ncol(EtimelyDLD))

tble_EtimelyDLD
#### Report - Line list of Hfs with <50% of VL samples downloaded within 3 days ####
VLPct_3Days   <-  df %>%
  filter(!is.na(TATD)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName,HFacility, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE)
  )

### calculate the proportion
VLPct_3DaysRpt  <-  VLPct_3Days %>% 
  mutate(
    `%within3 days`  =  round((No3Days/No.samples)*100,0)
  ) 

### select columns of interest
VLPct_3DaysRpt   <-   VLPct_3DaysRpt %>% 
  select(RRH,Qtr,HName,HFacility, `%within3 days`)

#### Pivot
VLhf_3DaysRpt  <-  VLPct_3DaysRpt %>% 
  filter(`%within3 days`  < 50) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within3 days`
  )%>% 
  select(HName,RRH,HName,HFacility, `Oct-Dec`,`Jan-Mar`)

#### set the target coloring
DldPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_VLhf_3DaysRpt   <-  flextable(VLhf_3DaysRpt)

#### Format the table
tble_VLhf_3DaysRpt <-   tble_VLhf_3DaysRpt %>% 
  add_header_row(values = c(
    HName  =  "Hub Name",
    RRH  =  "RRH",
    HFacility  =  "Health Facility Name",
    "Reporting Period", ""
  ))
## set the labels
tble_VLhf_3DaysRpt <-   tble_VLhf_3DaysRpt %>% 
  set_header_labels(
    HName  =  "Hub Name",
    RRH  =  "RRH",
    HFacility  =  "Health Facility Name",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Oct-Dec","Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tble_VLhf_3DaysRpt <-   tble_VLhf_3DaysRpt %>% 
    bg(j = col, bg = RctPct_color(VLhf_3DaysRpt[[col]]), part = "body")
}


tble_VLhf_3DaysRpt <-   tble_VLhf_3DaysRpt %>%
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
tble_VLhf_3DaysRpt <-   tble_VLhf_3DaysRpt %>%
  merge_v(j = 1, part = "header") %>%  
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 3, part = "header")


# Adding vertical lines to improve readability
tble_VLhf_3DaysRpt <-   tble_VLhf_3DaysRpt %>%
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tble_VLhf_3DaysRpt <-   tble_VLhf_3DaysRpt %>%
  add_header_row(values="Linelist of sites with < 50% of samples downloaded within 3 days", 
                 colwidths = ncol(VLhf_3DaysRpt))

tble_VLhf_3DaysRpt
#### Report - Line list of Hfs with <50% of EID samples downloaded within 3 days ####
EIDPct_3Days   <-  Edf %>%
  filter(!is.na(TATD)) %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HName,HFacility, Qtr) %>% 
  summarise(
    No.samples  =  n(),
    No3Days     =  sum(TATD  <= 3, na.rm = TRUE)
  )

### calculate the proportion
EIDPct_3DaysRpt  <-  EIDPct_3Days %>% 
  mutate(
    `%within3 days`  =  round((No3Days/No.samples)*100,0)
  ) 

### select columns of interest
EIDPct_3DaysRpt   <-   EIDPct_3DaysRpt %>% 
  select(RRH,Qtr,HName,HFacility, `%within3 days`)

#### Pivot
EIDPct_3DaysRpt  <-  EIDPct_3DaysRpt %>% 
  filter(`%within3 days`  < 50) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%within3 days`
  )%>% 
  select(HName,RRH,HName,HFacility, `Oct-Dec`,`Jan-Mar`)

#### set the target coloring
DldPct_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_EIDPct_3DaysRpt   <-  flextable(EIDPct_3DaysRpt)

#### Format the table
tble_EIDPct_3DaysRpt <-   tble_EIDPct_3DaysRpt  %>% 
  add_header_row(values = c(
    HName  =  "Hub Name",
    RRH  =  "RRH",
    HFacility  =  "Health Facility Name",
    "Reporting Period", ""
  ))
## set the labels
tble_EIDPct_3DaysRpt <-   tble_EIDPct_3DaysRpt  %>% 
  set_header_labels(
    HName  =  "Hub Name",
    RRH  =  "RRH",
    HFacility  =  "Health Facility Name",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Oct-Dec","Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tble_EIDPct_3DaysRpt <-   tble_EIDPct_3DaysRpt  %>% 
    bg(j = col, bg = RctPct_color(EIDPct_3DaysRpt[[col]]), part = "body")
}


tble_EIDPct_3DaysRpt <-   tble_EIDPct_3DaysRpt  %>% 
  # Merging columns 
  merge_at(i = 1, j = 4:5, part = "header") 

# Merging vertically for the first five columns in the header
tble_EIDPct_3DaysRpt <-   tble_EIDPct_3DaysRpt  %>% 
  merge_v(j = 1, part = "header") %>%  
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 3, part = "header")


# Adding vertical lines to improve readability
tble_EIDPct_3DaysRpt <-   tble_EIDPct_3DaysRpt  %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tble_EIDPct_3DaysRpt <-   tble_EIDPct_3DaysRpt  %>% 
  add_header_row(values="Linelist of sites with < 50% of EID samples downloaded within 3 days", 
                 colwidths = ncol(EIDPct_3DaysRpt))

tble_EIDPct_3DaysRpt
#############################################################################
#############################################################################

#### Site visits' reports ####
####  Data cleaning:  Site visits as scheduled, sample tracking cleaning section ####
#### Obtaining the denominator - VL databases (Number of sites where CPHL has received a VL sample) ####
df4 <- VLRaw %>%                                                
  distinct(HFacility, .keep_all = TRUE)
# selecting the columns of interest
df4  <-  df4 %>% 
  select(RRH,District,HName,HFacility,dhis2_name,date_received,dhis2_uid)
# Capitalize the names
df4$HFacility   <-   toupper(df4$HFacility)
df4$HName       <-  toupper(df4$HName)
df4$RRH         <-  toupper(df4$RRH)
df4$dhis2_name  <-  toupper(df4$dhis2_name)
# Change H/C to HC 
df4$HFacility  <- gsub("H/C","HC",df4$HFacility)
df4$HFacility  <- gsub("HCIII","HC III",df4$HFacility)
df4$HFacility  <- gsub("HCIV","HC IV",df4$HFacility)
#### Cleaning VL the health facility names ####
df4    <-    df4 %>% 
  mutate(HFacility   =   recode(HFacility,
                                
                                "MATUGGA HC III"="MATUGA HC II",
                                "KYENGEZA HC II"="KYENGEZA HC II",
                                "NGOMA HC IV (NAKASEKE)"="NGOMA HC IV",
                                "ST KIZITO BWAISE CLINIC"="ST. KIZITO BWAYIISE CLINIC HC II",
                                "KAYANZI HC II"="KAYANZI HC II",
                                "LACOR OPIT HC III (OMORO)"="LACOR OPIT HC III",
                                "BUHAMANA HC III"="BUHANAMA HC II",
                                "AMALER HC III"="AMALER HC III",
                                "KABUUBWA HC III"="KABUUBWA HC III",
                                "KALEMUNGOLE HC II"="KALEMUNGOLE HC II",
                                "MAJANJI HC III"="MAJANJI HC II",
                                "AKERIAU HC III"="AKERIAU HC III",
                                "NAMAGGWA HC III"="NAMAGGWA MATERNITY CLINIC HC II",
                                "WAKAWAKA HC II"="WAKAWAKA HC II",
                                "SIKUDA HC III"="SIKUDA HC II",
                                "MBALE CITY HC II"="MBALE CITY HC II",
                                "KIREKA HC II"="KIREKA HC II",
                                "KIRINYA HC II"="KIRINYA (BWEYOGERERE) HC II",
                                "RWABIGYEMANA HC III"="RWABIGYEMANO HC III",
                                "BULEGENI HC III"="BULEGENI HC III",
                                "ALIAKMER HC III"="ALIAKAMER HC III",
                                "BUNDEGE HC III"="BUNDEGE HC II",
                                "MATALE HC III"="MATALE HC III",
                                "MIGOGWE HC II"="MIGONGWE HC II",
                                "ST BALIKUDDEMBE MARKET/UGANDA CARES  AHF"="ST. BALIKUDEMBE UGANDA CARES HC II",
                                "BUBUNGI HC III"="BUBUNGI HC III",
                                "MAHEGA HC III"="MAHEGA HC II",
                                "BISOZI HC IV"="BISOZI HC IV",
                                "MWITANZIGE HC III"="MWITANZIGE HC III",
                                "MAWUJJO HC II"="MAWUJJO HC II",
                                "KATARAZA HC III"="KATARAZA HC III",
                                "KABIGI MUSLIM HC II"="KABIGI MUSLIM HC III",
                                "YOTKOM MEDICAL CENTRE"="YOT KOM HC III",
                                "NAMAYUMBA EPICENTRE HC III"="NAMAYUMBA EPI CENTRE HC III",
                                "MUNYONYI HC III"="MUNYONYI HC III",
                                "KIGOGOLA HC II"="KIGOGOLA HC II",
                                "NTOOMA HC II"="NTOOMA HC II",
                                "NAWANGISA HC III"="NAWANGISA HC III",
                                "MINANI HC III"="MINANI HC III",
                                "KYABASARA HC III"="KYABASARA HC II",
                                "MUHORO HC III"="MUHORRO GVT HC II",
                                "KISARU TEA HC II"="KISAARU TEA HC II",
                                "JOY HOSPICE"="JOY HOSPICE HC II",
                                "NYABUHIKYE PRISON HC II"="NYABUHIKYE PRISON HC II",
                                "KIBAZI HC III"="KIBAZI HC III",
                                "BUSHENYI UGANDA PRISONS HC III"="BUSHENYI UGANDA PRISONS HC III",
                                "RAPHA MEDICAL CENTRE(GOMBA)"="RAPHA MEDICAL HC III",
                                "KIGEZI HC II"="KIGEZI HC II",
                                "NANTABULIRWA HC II"="NANTABULIRWA HC II",
                                "BUGANA HC III"="BUGANA HC III",
                                "KYABINUNGA HC II"="KYABINUNGA HC II",
                                "BUKOTO HC II"="BUKOTO HC III",
                                "KYARWABUGANDA HC III"="KYARWABUGANDA HC III",
                                "MASAKA HC III"="KAKUMIRO - MASAKA HC III",
                                "KASENGE HC II"="KASENGE HC II",
                                "KIBOTA HC III"="KIBOTA HC II",
                                "YAWE MEDICAL CENTER"="YAWE HC II",
                                "PANDWONG HC III"="PANDWONG HC III",
                                "KIBAIRE HC II"="KIBAIRE HC II",
                                "KATUM HC III"="KATUM HC II",
                                "MARY QUEEN OF PEACE HC III"="MARY QUEEN OF PEACE HC II",
                                "4TH DIVISION LUGORE TRAINING SCHOOL  HC III"="4TH DIVISION LUGORE TRAINING SCHOOL  HC III",
                                "KATABI HC III"="KATABI HC III",
                                "KABALUNGI HC II"="KABALUNGI HC II",
                                "KISHAMI HC III"="KISHAMI HC III",
                                "GAYAZA HC III"="GAYAZA HC II",
                                "KIHUUKYA HEALTH CENTER"="KIHUUKYA HC II",
                                "RAMBIA HC III"="RAMBIA HC III",
                                "KAKOMA HC III (LWENGO)"="KAKOMA HC III",
                                "KAGUMBA HC III"="KAGUMBA HC III",
                                "ARINYAPI HC III"="ARINYAPI HC III",
                                "KYEIBANGA HC III"="KYEIBANGA HC III",
                                "GOOD SAMARITAN HC III"="GOOD SAMARITAN HEALTH CLINIC HC II",
                                "KATASENYWA"="KATASENYWA HC III",
                                "KOOKI COMMUNITY HOSPITAL"="ST. ANDREA KAHWA KOOKI COMMUNITY HOSPITAL",
                                "KITAIHUKA HC III"="KITAIHUKA HC III",
                                "ST FRANCISCA MAKONJE HC II"="ST. FRANCISKA MAKONJE HC II",
                                "BUWEMBE HC III"="BUWEMBE HC II",
                                "BUMUNJI HC III"="BUMUNJI HC II",
                                "MAISKA HC III"="MAISUKA HC III",
                                "KABASEKENDE HC III"="KABASEKENDE HC III",
                                "NTENJERU PRISON HC II"="NTENJERU PRISON HC II",
                                "BUBANGO HC II"="BUBANGO HC II",
                                "KINONI HC III (LWENGO)"="LWENGO KINONI GOVT HC III",
                                "RWETAMU HC III"="RWETAMU HC III",
                                "MPASAANA HC III"="MPASAANA HC III",
                                "NSHWERE HC II"="NSHWERE HC III",
                                "BIREMBO HC III"="BIREMBO HC III",
                                "BUTAAKA HC III"="BUTAAKA HC III",
                                "KIYUNGA HC III"="KIYUNGA HC II",
                                "BUDOMERO HC III"="BUDOMERO HC III",
                                "BUKENDI HC III"="BUKENDI HC II",
                                "KIRU HC II"="KIRU HC II",
                                "KATABOKE HC III"="KATABOK HC III",
                                "KOTIDO COU HC III"="KOTIDO COU HC III",
                                "RUSHAMBYA HC III"="RUSHAMBYA HC II",
                                "NASSOLO WAMALA HC III"="NASSOLO WAMALA HC II",
                                "BBIRA HC II"="BBIRA HC II",
                                "NSAGGU HC II"="NSAGGU HC II",
                                "AJIKORO HC III"="AJIKORO HC III",
                                "ST DENIS KYANGO  HC III"="KYANGO HC III",
                                "ST MAGDALENE HEALTH CENTER"="ST. MAGDALENE HC II",
                                "STATE HOUSE HC IV"="STATE HOUSE HC IV",
                                "NAWAMPITI HC III"="NAWAMPITI (BUKAMBA) HC III",
                                "KIRUHURA PRISONS HC II"="KIRUHURA PRISONS CLINIC",
                                "KAYONZA HC III (KANUNGU)"="KAYONZA HC III",
                                "KIMWANYI HC II"="KIMWANYI HC II",
                                "ENGEYE HC IV (LWENGO)"="ENGEYE HEALTH CLINIC HC III",
                                "RAILWAY HC III"="RAILWAY HC II",
                                "MARACHA HC IV"="MARACHA HC IV",
                                "KAKIIKA HC II"="KAKIIKA HC II",
                                "KYOMON HC III"="KYOMON HC III",
                                "NABBINGO PARISH HC III"="NABBINGO HC II",
                                "VICTORIA HOSPITAL (UMCH)"="VICTORIA HOSPITAL",
                                "ST KIZITO HC II (KIJABIJO)"="ST. KIZITO HEALTH CENTRE HC III",
                                "MORUITA HC II"="MORUITA HC II",
                                "BBIRA HC III (WAKISO)"="BBIRA NGO HC II",
                                "ST APOLLO HC III-NAMASUBA"="ST. APOLO HC III",
                                "MPIGI PRISONS HC II"="MPIGI PRISON HC II",
                                "BUSOTA HC III"="BUSOTA HC III",
                                "MAGANJO  HC II"="MAGANJO HC II",
                                "BUKAMBA HC III"="BUKAMBA HC II",
                                "KANGINIMA HOSPITAL"="KANGINIMA (BUTEBO) HOSPITAL",
                                "NAKATITI HC III"="NAKATITI HC II",
                                "TODORA HC III"="TODORA HC II",
                                "ARWOTCEK HC II"="ARWOTCEK HC II",
                                "AWEI HC III"="AWEI HC III",
                                "BIKO HC II"="BIKO HC II",
                                "OPOPONGO HC III"="OPOPONGO HC II",
                                "LACOR HC III (AMURU)"="AMURU LACOR HC III",
                                "ATUNGA HC III"="ATUNGA HC II",
                                "AYARA HC II"="AYARA HC II",
                                "ANYANGATIR HC III"="ANYANGATIR HC III",
                                "KAMDINI HC III"="KAMDINI HC II",
                                "KIDILANI HC III"="KIDILANI HC II",
                                "AYER HC III"="AYER HC III",
                                "ALIK HC III"="ALIK HC II",
                                "OWINY HC III"="OWINY HC II",
                                "LIRA UNIVERSITY HOSPITAL"="LIRA UNIVERSITY HOSPITAL",
                                "KITWARA HC II"="KITWARA HC II",
                                "NYAKIMASA HC III"="NYAKIMASA HC II",
                                "OLI HC IV"="RIVER OLI HC IV",
                                "LANGOL HC II"="LANGOL HC II",
                                "NAMA WELLNESS CENTRE"="NAMA WELLNESS COMMUNITY HC III",
                                "KIMAKA HC III"="KIMAKA HC II",
                                "MRC/UVRI-ENTEBBE"="MRC/UVRI-ENTEBBE",
                                "SINGO HC III"="SINGO HC III",
                                "KITOKOLO HC II"="KITOKOLO HC II",
                                "BUKERERE HC II"="BUKERERE HC II",
                                "ST JAMES MASIRIBA HC III"="ST. JAMES MASIRIBA COU HC III",
                                "KASHEREGYENYI HC III"="KASHEREGYENYI HC III",
                                "FORTPORTAL PRISONS HC III"="FORTPORTAL PRISONS HC III",
                                "OBERABIC HC II"="OBERABIC HC II",
                                "OYAM MAIN PRISON"="OYAM MAIN PRISONS HC II",
                                "KATUNGU MISSION HC III"="KATUNGU MISSION HC III",
                                "NKOKONGERU HC II GOV'T (BUIKWE)"="NKOKONJERU HC II",
                                "KAGULU HC III N/A"="KAGULU HC III",
                                "CHEGERE HC II"="CHEGERE HC II",
                                "NJOVU ISLAMIC MEDICAL CEMTER"="NJOVU MEDICAL CENTER HC III",
                                "OJE MISSION HC III"="OJE MISSION HC III",
                                "ST PETERS BUSIBO HC III"="ST. PETER,BUSIBO",
                                "NAKITOKOLO HC III"="NAKITOKOLO NAMAYUMBA HC II",
                                "MIGADDE HC II"="MIGADDE HC II",
                                "OFUA HC III"="OFUA (OFUA) HC III",
                                "WILELA HC III"="WILELA HC II",
                                "BUKIMBI HC II"="BUKIMBI HC II",
                                "ABOKE MISSION  HC III"="ABOKE MISSION HC III",
                                "ABELLA HC III"="ABELA HC II",
                                "LORO HC III"="LORO HC III",
                                "MUTUFU HC III"="MUTUFU HC II",
                                "BULAAGO HC III"="BULAAGO HC II",
                                "NYARUHANDAGAZI HC III"="NYARUHANDAGAZI HC III",
                                "ALWOROCENG HC II"="ALWOROCENG HC II",
                                "OKOLE HC III"="OKOLE HC II",
                                "MALERE HC II"="MALERE HC II",
                                "NYAKASHOZI HC II"="NYAKASHOZI HC II",
                                "OMUGO EXTENTION HC III"="WIDI(OMUGO EXTENTION) HC III",
                                "OCIA HC III"="OCIA HC III",
                                "SOP SOP HC III"="SOP SOP HC III",
                                "ST MARYS  HC IV (KAKUMIRO)"="KAKUMIRO HC IV",
                                "LUBBE HC II"="LUBBE HC II",
                                "KITANYATA HC III"="KITANYATA HC II",
                                "SENTEMA PRISON HC II"="SENTEMA PRISONS HC II",
                                "KIZIGO HC LL"="KIZIGO HC II",
                                "ALLUSTIN HC LLL"="",
                                "ARUA PRISON HC III"="ARUA MAIN PRISONS HC III",
                                "BUSIME HC III"="BUSIME HC II",
                                "MILNE MEDICAL CENTRE"="MILNE MEDICAL CENTRE HC II",
                                "MIHEMBERO HC II"="MIHEMBERO HC II",
                                "BUKAMBA HC II"="BUKAMBA HC II",
                                "KASUBI COG HC III"="KASUBI HC III",
                                "ST KLAUS HC III"="ST. KLAUS HC III",
                                "CURUBE HC III"="CURUBE HC III",
                                "BURONDO HC III"="BURONDO HC III",
                                "NAMUSAALE HC III"="NAMUSAALE HC II",
                                "KASHEKYE HC III"="KASHESHE HC III",
                                "MUBENDE TOWN COUNCIL"="MUBENDE TOWN COUNCIL HC II",
                                "DAVID FAGERLEES MEDICAL CENTRE"="DAVID FAGERLEE'S MEDICAL CENTRE",
                                "NABIKAKALA HC III"="NABIKAKALA HC II",
                                "ISINDE HC III"="ISINDE HC II",
                                "SHEEMA COMMUNITY HC"="SHEEMA COMMUNITY HC III",
                                "KASOZO HCII"="KASOOZO HC II",
                                "KISALA HC II"="KISAALA HC II",
                                "AKALI HC III"="AKALI HC II",
                                "LUTEETE HC II"="LUTEETE HC II",
                                "MUNTEME HC III"="MUNTEME HC III",
                                "KITOJO HC III"="KISOJO HC III",
                                "MABAARE HC III"="MABAARE HC III",
                                "ST NOAH BUYAMBI"="ST. NOAH BUYAMBI HC III",
                                "ONYWAKO HC III"="ONYWAKO HC II",
                                "OGWEYE HC III"="OGWETE HC III",
                                "MEETING POINT KITGUM"="MEETING POINT HC II",
                                "BUNG HC II"="BUNG HC II",
                                "ANYACOTO HC II"="ANYACOTO HC II",
                                "BUBIRO HC III"="BUBIRO HC II",
                                "OMBI HC III"="",
                                "LUBIMBIRI HC II"="LUBIMBIRI HC II",
                                "HAKISHENYI HC II"="HAKISHENYI HC II",
                                "OUR LADY OF THE SICK KAMBAALA HC III"="KAMBAALA HC III",
                                "RWESHANDE HC III"="RWENSHANDE HC III",
                                "ALOO HC II"="ALONI HC II",
                                "NABUMALI HC III"="NABUMALI HC III",
                                "BUHUNGIRO HC II"="BUHUNGIRO HC II",
                                "KYUNGU HC III"="KYUNGU HC II",
                                "DIOCESE OF KITGUM HC II"="DIOCESE OF KITGUM HC II",
                                "LULYANGO HC II"="LULYANGO HC II",
                                "KAARA HC II"="KAARA HC II",
                                "IBANDA MISSION HC III"="IBANDA MISSION HC III",
                                "MAGOGGO HC II"="MAGOGGO HC II",
                                "MUZINDA KATEREKE HC"="MUZINDA KATEREKE HC II",
                                "KASHEESHE HC III"="KASHESHE HC III",
                                "BUNAMWAYA HC II"="BUNAMWAYA HC II",
                                "KIIGYA HC II"="KIIGYA HC II",
                                "DEI HC II"="DEI HC II",
                                "MITYANA PRISON HC"="MITYANA PRISONS HC II",
                                "MUGALIKE HC III"="MUGALIKE HC II",
                                "BACAYAYA HC II"="BACAYAYA HC II",
                                "AWER HC II"="AWER HC II",
                                "CHAKULIA HC III"="CHAKULIA HC III",
                                "KIRIGIME HC III"="KIRIGIME HC III",
                                "KUNGU HC III"="KUNGU HC II",
                                "GOD'S CARE MEDICAL CENTER"="GOD'S CARE MEDICAL CENTRE HC III",
                                "CENTRAL 1"="",
                                "KIKANDWA HC III"="KIKANDWA HC III",
                                "AMWOMA HC III"="AMWOMA HC II",
                                "KARUJANGA HC III"="KARUJANGA HC II",
                                "ST. CHARLES LWANGA BUIKWE HOSPITAL"="BUIKWE ST. CHARLES LWANGA HOSPITAL",
                                "BAKALUBE HC II"="BUKATUBE HC III",
                                "KAMULI POLICE HC III"="KAMULI POLICE HC II",
                                "KYAKABADIIMA HC III"="KYAKABADIIMA HC II",
                                "ST NOA BUYAMBA HC III"="ST. NOAH BUYAMBI HC III",
                                "KORO HC III"="KORO HEALTH CENTRE HC III",
                                "ST STEPHENS COMMUNITY  HC III"="ST. STEPHEN HC II",
                                "CPHL"="",
                                "JOKASY MEDICAL CENTER"="JOSKA MEDICAL CENTRE CLINIC",
                                "LWEMIKOMAGO HC III"="LWEMIKOMAGO HC III",
                                "KIHANI HC III"="KIHANI HC II",
                                "MUBANDA HC III"="MUBANDA HC III",
                                "KANYOGOGA HC III"="KANYOGOGA HC II",
                                "PROF WAMUKOTA MMC"="PROF WAMUKOTA MEMORIAL HC III",
                                "NAKATOVU HC III"="NAKATOVU HC II",
                                "KYAKIDDU HC II"="KYAKIDDU HC II",
                                "BUTOHA HC III"="BUTOHA HC III",
                                "RWABARATA HC III"="RWABARATA HC III",
                                "BUTOVE HC II"="BUTOVE HC II",
                                "KABOWA HC II"="KABOWA HC II",
                                "MALEMBO HC III"="MALEMBO HC II",
                                "LINGIRA HEALTH CENTER"="LINGIRA (YWAM) HEALTH CENTRE HC II",
                                "KISABAGWA HC III"="KISABAGWA HC II",
                                "KICHWABUGINGO HC II"="KICHWABUGINGO HC II",
                                "CARDINAL NSUBUGA MEMORIAL HEALTH CENTER"="CARDINAL NSUBUGA MEMORIAL HC III",
                                "OSEERA HC II"="OSEERA HC II",
                                "WAMBABYA HC II"="WAMBABYA HC II",
                                "OLOK HC III"="OLOK HC III",
                                "DIICUNYI HC III"="",
                                "KITGUM PRISON HC III"="KITGUM PRISON HC II",
                                "OMOT HC II"="OMOT HC II",
                                "KIZOOLE HC"="KIZOOLE HC",
                                "BUKUBA HC III"="BUKUBA HC II",
                                "MAKHAI HC III"="MAKHAI HC II",
                                "KITOOMA HC III"="KITOOMA HC III",
                                "KABALE HC II"="KABALA HC II",
                                "AYA HC III"="AYA HC III",
                                "KYETUME HC III"="KYETUME HC III",
                                "NAMUYENJE HC II"="NAMUYENJE HC II",
                                "KANUNGU PRISONS HC II"="KANUNGU PRISON  HC II",
                                "KAGAMBA HC II"="KAGAMBA HC II",
                                "GOOD HOPE MEDICAL CENTER"="GOOD HOPE MEDICAL CENTRE HC II",
                                "KIMA HC III"="KIRA HC III",
                                "CRANE SURVEY MASAKA"="",
                                "CRANE SURVEY BUVUMA"="",
                                "KASOOZO HC III"="KASOOZO HC II",
                                "ST MARYS KAKINDO HC III"="ST. MARY'S KAKINDO HC III",
                                "CRANE SURVEY TORORO"="",
                                "BUYENGO HC III"="BUYENGO HC II",
                                "CRANE SURVEY MBARARA"="",
                                "CRANE SURVEY GULU"="",
                                "ST DENIS KYANGO HC III"="KYANGO HC III",
                                "CRANE SURVEY BUSIA"="",
                                "ALYECMEDA HC III"="ALYECMEDA HC II",
                                "SEMBABULE PRISON HC II"="SEMBABULE PRISON HC II",
                                "CRANE SURVEY KAMPALA"="",
                                "CRANE SURVEY JINJA"="",
                                "CRANE SURVEY ARUA"="",
                                "CHEPTAPOYO HC II"="CHEPTAPOYO HC II",
                                "NAMPANGA HC II"="NAMPANGA HC II",
                                "BURORA HC III"="BURORA HC II",
                                "NAKYESSA HC II"="NAKYESA HC II",
                                "CRANE SURVEY MBALE"="",
                                "CRANE SURVEY LIRA"="",
                                "ISINGIRO PRISON HC III"="ISINGIRO PRISON HC II",
                                "KASAMBA HEALTH CENTER"="",
                                "CRANE SURVEY FORT PORTAL"="",
                                "IGANGA PRISON HC III"="IGANGA PRISONS HC II",
                                "MJAP (ISS CLINIC) MULAGO"="MULAGO NRH - MJAP ISS CLINIC",
                                "KATABI MILITARY HOSPITAL"="KATABI MILITARY HC III",
                                "TASO MULAGO"="TASO MULAGO SPECIAL CLINIC",
                                "KAWAALA HC III"="KAWAALA HC IV",
                                "KISWA HC IV"="KISWA HC III",
                                "WATTUBA HC III"="WATUBBA HC III",
                                "EPI CENTRE HC III (WAKISO)"="WAKISO EPI CENTRE HC III",
                                "KICWAMBA HC III (KABAROLE)"="KICWAMBA HC III",
                                "TASO MASINDI"="TASO MASINDI SPECIAL CLINIC",
                                "KAWEMPE NATIONAL REFERRAL HOSPITAL"="MULAGO NRH - OBS&GYN PMTCT CLINIC",
                                "KIZIBA HC III (WAKISO)"="KIZIBA HC III",
                                "IKI  IKI HC III"="IKI IKI HC III",
                                "BOMBO MILITARY HOSPITAL"="BOMBO GENERAL MILITARY HOSPITAL",
                                "NAANYWA HC III"="NANYWA HC III",
                                "JINJA R R HOSPITAL"="JINJA REGIONAL REFERRAL HOSPITAL",
                                "KATOVU HC III"="KATOVU COU HC III",
                                "KIBAALE  HC II( RAKAI)"="KIBAALE HC II",
                                "PALLISA MISSION KAUCHO HC III"="PALLISA MISSION HC III",
                                "ST JUDE ULEPPI HC III"="ST. JUDE ULEPPI HC II",
                                "LACOR HC III (PABBO)"="LACOR-PABBO HC III",
                                "TASO SOROTI"="TASO SOROTI SPECIAL CLINIC",
                                "KIHEFO HC III"="KIHEFO CLINIC",
                                "BULIISA GENERAL HOSPITAL"="BULIISA HOSPITAL",
                                "AMURIA GENERAL HOSPITAL"="AMURIA HOSPITAL",
                                "KANONI HC III (GOMBA)"="KANONI HC III",
                                "NAKASEKE  HOSPITAL"="NAKASEKE HOSPITAL",
                                "KYANAMUKAKA HC IV"="KYANAMUKAAKA HC IV",
                                "ST MATIA MULUMBA HC III (MUBENDE)"="ST. MATIA MULUMBA HC III",
                                "KAMBUGU HC III (KIBOGA)"="KAMBUGU HC III",
                                "MARINE MILITARY HC II (BUTIABA)"="MARINE MILITARY HC III",
                                "MPEEFU HC III"="MPEEFU B HC III",
                                "HOIMA R R HOSPITAL"="HOIMA REGIONAL REFERRAL HOSPITAL",
                                "KISIIZI HOSPITAL C.O.U (RUKUNGIRI)"="COU KISIIZI HOSPITAL",
                                "ST BERNARDS MANNYA HC III"="ST. BERNARD MANNYA HC III",
                                "MIREMBE HC III (GAYAZA)"="MIREMBE (BUKULULA) HC III",
                                "KISOKO HC III (TORORO)"="KISOKO HC III",
                                "KITEBI HC III"="KITEBI HEALTH CENTRE HC III",
                                "NSAMBYA HOME CARE"="NSAMBYA HOME CARE CLINIC",
                                "ST BENEDICT HEALTH CENTER"="ST. BENEDICT'S HC III",
                                "KARUNGU HC III (BUHWEJU)"="KARUNGU (BUHWEJU) HC III",
                                "AGWATA HC III"="AGWATTA HC III",
                                "MEETING POINT KAMPALA"="MEETING POINT KAMPALA HC III",
                                "ANGAL HOSPITAL (ST LUKE)"="ANGAL HOSPITAL",
                                "ST MARYS KIGUMBA HC III (NGO)"="ST. MARY'S KIGUMBA HC III",
                                "RUKUNYU GENERAL HOSPITAL"="RUKUNYU HOSPITAL",
                                "BIHARWE HC III"="BIHARWE (NYABUHAMA) HC III",
                                "KIBAALE HC IV (KIBAALE)"="KIBAALE HC IV",
                                "KABALE HC III (HOIMA)"="KABAALE HC III (HOIMA)",
                                "KITENGA HC III (MUBENDE)"="KITENGA HC III",
                                "MJAP-MMC  HC IV"="MBARARA MUNICIPAL COUNCIL HC IV",
                                "KABALE R R HOSPITAL"="KABALE REGIONAL REFERRAL HOSPITAL",
                                "RUSHOOKA HC II"="MOTHER FRANCISCA LECHNER HC IV",
                                "MADI OPEI HC IV"="MADI-OPEI HC IV",
                                "ICEME HC III"="ICEME HC III (ICEME SUBCOUNTY)",
                                "BUWUNGA HC III (BUGIRI)"="BUGIRI BUWUNGA HC III",
                                "AWACH HC II (ABIM)"="AWACH HC II",
                                "ST ASSUMPTA HC III"="ST. ASSUMPTA HC III",
                                "ORIAJIN HOSPITAL"="ORIAJINI HOSPITAL",
                                "AIDS INFORMATION CENTER(ARUA)"="AIDS INFORMATION CENTRE (ARUA) SPECIAL CLINIC",
                                "BUWENGE NGO"="BUWENGE NGO HOSPITAL",
                                "PABBO HC III (AMURU GOVT)"="PABBO (GOVT) HC III",
                                "KIBULI MUSLIM HOSPITAL"="KIBULI HOSPITAL",
                                "SOROTI R R HOSPITAL"="SOROTI REGIONAL REFERRAL HOSPITAL",
                                "UGANDA CARES (SOROTI)"="UGANDA CARES HC II",
                                "PALLISA GENERAL HOSPITAL"="PALLISA HOSPITAL",
                                "NGORA FREDA CAR HOSPITAL (NGO)"="NGORA FREDA CARR HOSPITAL",
                                "KANONI HC III (KIRUHURA)"="KIRUHURA KANONI HC III",
                                "RUSHOROZA HC III"="RUSHOROZA HOSPITAL",
                                "NYAKISENYI HC III"="NYAKISHENYI HC III",
                                "KAKOBA DIVISION HC III"="KAKOBA HC III",
                                "TASO ENTEBBE"="TASO ENTEBBE SPECIAL CLINIC",
                                "KABWOHE CLINICAL RESEARCH CENTRE"="KABWOHE CLINICAL RESEARCH CENTRE HC II",
                                "BUSHENYI MEDICAL CENTRE HC III"="BUSHENYI MEDICAL CENTER HC III",
                                "KARUNGU HC III (KIRYANDONGO)"="ST. JUDE THADDEOS KARUNGU HC III",
                                "BUTARE HC III (BUHWEJU)"="BUTARE HC III",
                                "PANYADOLI HILLS HC II"="PANYADOLI HILL HC III",
                                "LIRA R R HOSPITAL"="LIRA REGIONAL REFERRAL HOSPITAL",
                                "KARITA HC III (AMUDAT)"="KARITA HC IV",
                                "407 BRIGADE HC III"="MORUITA 407 BREGADE HC III",
                                "KIHIIHI HC IV"="KIHIHI HC IV",
                                "KAHONDO HC II"="KAHONDO HC II GOVT",
                                "KIBANDA HC II (KABALE)"="KIBANDA HC II",
                                "KATUNGURU HC III(RUBIRIZI)"="KATUNGURU HC III (RUBIRIZI)",
                                "ST LUKE BUJUNI HC III (KIBAALE)"="ST. LUKE BUJUNI (KIBAALE) HC III",
                                "BUYANJA HC III (RUKUNGIRI)"="BUYANJA HC III",
                                "MUGARAMA HC III (KIBAALE)"="MUGARAMA HC III",
                                "NORTH KIGEZI DIOCESE M C HC IV"="NORTH KIGEZI HC IV",
                                "DR AMBROSOLIC CLINIC (KALIRO)"="DR. AMBROSOLI HC III",
                                "RUBAYA HC IV (KABALE)"="RUBAYA HC IV",
                                "KITANGA HC III (RUKIGA)"="KITANGA HC II",
                                "KINONI HC IV (MBARARA)"="KINONI HC IV",
                                "ACHOL PII MILITARY HC IV (5TH)"="5TH MILITARY DIVISION HC IV",
                                "PAJULE HC  IV"="PAJULE HC IV",
                                "MAYIRYE HC III"="ARCH BISHOP KIWANUKA MAYIRYE HC III",
                                "BUKUYA HC III"="BUKUYA HC IV",
                                "BUSUNJU HC III"="BUSUNJU HC II",
                                "KAJOJI HC III"="KAJJOJI HC III",
                                "BUMUGUSHA HC III (BULAMBULI)"="BUMUGUSHA HC III",
                                "RWEBISENGO HC III"="RWEBISENGO HC IV",
                                "CDC-KIRUDDU"="KIRUDDU NRH - LAB",
                                "KISUBBA HC III"="KISUBA HC III",
                                "ST ELIZABETH HC III KIJJUKIZO"="KIJJUKIZO (ST. ELIZABETH) HC III",
                                "KARUGUTU HC IV(NTOROKO)"="KARUGUTU HC IV",
                                "NKUNGU HC III"="NKUNGU HC II",
                                "ST AUSTIN HC II(MBALE)"="ST. AUSTIN MBALE HC II",
                                "KYARUMBA PHC III"="KYARUMBA PHC HC III",
                                "STELLA MARIS HC III"="STELLA MARIS NTOROKO HC III",
                                "KASAMBYA HC III (KAKUMIRO)"="KASAMBYA (KAKUMIRO) HC III",
                                "IRIIRI HC III"="IRIRI HC III",
                                "KATIKAMU HC III"="KATIKAMU SDA DISPENSARY HC II",
                                "BIHANGA HC III (BUHWEJU)"="BIHANGA HC III",
                                "BUTABIKA  HOSPITAL"="BUTABIKA NATIONAL REFERRAL HOSPITAL",
                                "MUDUMA HC III"="MUDUUMA HC III",
                                "IDI MULAGO"="MULAGO NRH - INFECTIOUS DISEASE INSTITUTE",
                                "KYERE MISSION NGO"="KYERE MISSION",
                                "NSWANJERE HC III (ST JOSEPH)"="NSWANJERE HC III",
                                "FAMILY HEALTH RESOURCE CENTRE KIRUHURA"="FAMILY HEALTH RESOURCE CENTRE CLINIC",
                                "ST MONICA HC III (KATENDE)"="ST. MONICA KATENDE HC III",
                                "DONNA MEDICAL CENTRE"="DONA CARNEVALE MEDICAL CENTRE HC III",
                                "AKOBOI HC II (SERERE)"="AKOBOI (SERERE) HC II",
                                "KYATEREKERA HC III (KAGADI)"="KYATEREKERA HC III",
                                "ST KEVIN HC III"="ST. KEVIN TOROMA HC III",
                                "ENGAJU HCII"="ENGAJU HC III",
                                "UGANDA REPRODUCTIVE HEALTH BUREAU(BUGIRI)"="URHB MEDICAL CENTER CLINIC",
                                "BUGIRI TOWN COUNCIL HC II"="BUGIRI MC HC III",
                                "NYONDO HC III (MBALE)"="NYONDO HC III",
                                "APERKIRA HC III"="APERIKIRA HC III",
                                "CHINA UGANDA FRIENDSHIP HOSPITAL- NAGURU"="CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                                "AIDS INFORMATION CENTRE (SOROTI)"="AIDS INFORMATION CENTRE (SOROTI) HC II",
                                "NABITSIICHI HC III"="NABITSIKHI HC III",
                                "TOUCH NAMUWONGO (IHK)"="TOUCH CLINIC - NAMUWONGO HC II",
                                "KASOZI HC III (LUWEERO)"="KASOZI HC III",
                                "ALIVE MEDICAL SERVICES HC III"="ALIVE MEDICAL SERVICES SPECIAL CLINIC",
                                "BUTAWAATA HC III"="BUTAWATA HC III",
                                "ENTEBBE HOSPITAL MCH GENERAL"="ENTEBBE REGIONAL REFERRAL HOSPITAL",
                                "KASESE MUNICIPAL COUNCIL HC III (KMC)"="KASESE MUNICIPAL COUNCIL HC III",
                                "MPUMUDDE HC III (LYANTONDE)"="MPUMUDDE HC III",
                                "BUGINYANYA HC III (BULAMBULI)"="BUGINYANYA HC III",
                                "HOLY CROSS ORTHODOX HOSPITAL NAMUNGOONA"="HOLY CROSS ORTHODOX MISSION NAMUNGOONA HOSPITAL",
                                "ST PADRE PIO HC III BUSUNJU"="ST. PADRE PIO MIREMBE HC III",
                                "ST BALIKUDDEMBE HC III (KYANKWANZI)"="ST. BALIKUDEMBE HC III",
                                "ST LUKE HC III (KKONGE)"="KKONGE ST. LUKE HC III",
                                "KAWEMPE HOME CARE"="KAWEMPE HOME CARE CLINIC HC III",
                                "ONGUTOI HEALTH CENTER"="ONGUTOI HC III",
                                "KACHUMBALA MISSION DISPENSARY"="KACHUMBALA NGO HC II",
                                "ST MICHEAL HCF HC III (AMURIA)"="ST. MICHAEL-WERA HC III",
                                "USUK HC III"="ST. ANNE USUK HC III",
                                "MUDAKORI HC III"="MUDAKOR HC III",
                                "KYAMWINULA HC III"="KYAMWINULA HC II",
                                "FAMILY HOPE CENTRE JINJA"="FAMILY HOPE CENTER JINJA SPECIAL CLINIC",
                                "LYAKAJULA HC II (LYANTODE)"="LYAKAJURA HC III",
                                "DR CHARLES FARTHING MEMORIAL CLINIC"="DR.CHARLES FURTHING CLINIC",
                                "KALIRO HC III (LYANTONDE)"="KALIIRO HC III",
                                "MASAKA POLICE CLINIC"="MASAKA POLICE HC III",
                                "MASAKA R R HOSPITAL"="MASAKA REGIONAL REFERRAL HOSPITAL",
                                "BUWUNGA HC III (MASAKA)"="BUWUNGA HC III",
                                "NYAMARWA HC III (KIBAALE)"="NYAMARWA HC III",
                                "KIFAMBA HC III (RAKAI)"="KIFAMBA HC III",
                                "KIRUMBA HC III (RAKAI)"="KIRUMBA HC III",
                                "KABUWOKO HC III"="KABUWOKO GOVT HC III",
                                "ST CECILIA BUYAMBA HC III"="BUYAMBA DISP &MU HC III",
                                "KIBANDA HC III (RAKAI)"="KIBANDA HC III",
                                "PIDC (BAYLOR-MULAGO)"="MULAGO NRH - PIDC COE BAYLOR CLINIC",
                                "LWAMAGGWA HC III"="LWAMAGGWA GOVT HC III",
                                "RUBAYA HC III (MBARARA)"="RUBAYA HC III",
                                "ST LUCIA KAGAMBA HC III"="ST. LUCIA KAGAMBA HC III",
                                "SYANYONJA HC II"="SHANYONJA HC II",
                                "ST JACOBS LWAMAGGWA HC"="LWAMAGGWA NGO HC II",
                                "OMAGORO HC III"="OMAGORO HC II",
                                "KIZIBA HC III (RAKAI)"="RAKAI KIZIBA HC II",
                                "JOY MEDICAL CENTER (MBALE)"="JOY MEDICAL HC II",
                                "NURTURE AFRICA HC III"="NURTURE AFRICA II SPECIAL CLINIC",
                                "NGORA DISTRICT MATERNITY HC III"="NGORA DISTRICT MATERNITY UNIT HC III",
                                "MBUYE HC III"="MBUYE HC II",
                                "EMESCO HC IV"="EMESCO HC III",
                                "KIBALE HC III(PALISA)"="KIBALE HC III",
                                "FAMILY HOPE CENTRE KAMPALA"="FAMILY HOPE CENTRE KAMPALA SPECIAL CLINIC",
                                "BUYAMBA HC III-RAKAI"="BUYAMBA HC III",
                                "KARAMBI HC III (KASEESE)"="KARAMBI (KASESE) HC III",
                                "ST FRANCIS ACUMET"="ST. FRANCIS ACUMET HC III",
                                "NYAKIBALE HOSPITAL"="KAROLI LWANGA (NYAKIBALE) HOSPITAL",
                                "JCRC (WAKISO)"="JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                "KABIRA HC III (RAKAI)"="KABIRA (KYOTERA) HC III",
                                "KASANGA PHC III"="KASANGA PHC HC III",
                                "MUSYENENE HC III"="MUSHENENE HC III",
                                "BIIKIRA HC III"="ST. ANDREWS BIIKIRA MARIA HC III",
                                "GULU R R HOSPITAL"="GULU REGIONAL REFERRAL HOSPITAL",
                                "IRUHUURA HC III"="IRUHURA COU HC III",
                                "HOLY INNOCENTS HC III"="HOLY INNOCENT HC III",
                                "STD/MARPI CLINIC-MULAGO"="MULAGO NRH - MARPI STI PROJECT CLINIC",
                                "KANARA HC II"="KANARA HC III",
                                "COMMUNITY HEALTH PLAN-UGANDA"="COMMUNITY HEALTH CENTER (KIBUKU) HC III",
                                "PANYADOLI HC III"="PANYADOLI HC IV",
                                "RUKOKI HC III"="RUKOKI HC IV",
                                "KOBOKO GENERAL HOSPITAL"="KOBOKO HOSPITAL",
                                "KYAKUTEREKERA HC III (KAKUMIRO)"="KYAKUTEREKERA HC III",
                                "KAZINGA HC III (KYEGEGWA)"="KAZINGA HC III",
                                "ST RICHARD'S HC III"="ST. RICHARD HC III",
                                "ST PHILIPS HC II"="ST. PHILIP HC II",
                                "ST MAURITZ HC II"="ST. MAURITZ HC II",
                                "RWIMI PRISONS HC III"="RWIMI PRISON HC III",
                                "ST MARYS HC II KATOOSA"="ST. MARY'S KATOOSA HC II",
                                "AMACH HC IV (LIRA)"="AMACH HC IV",
                                "MWENGE HC III"="MWENGE CLINIC HC III",
                                "COMMUNITY HC PLAN UGANDA LUGOBA(KAWEMPE)"="COMMUNITY HEALTH PLAN UGANDA",
                                "KAGWARA HC II"="KAGWARA HC III",
                                "FR.BILBAO HC III"="FR. BILBAO HC III",
                                "BUGUNGU YP HC III"="BUGUNGU YP PRISON HC II",
                                "MUBENDE REHABILITATION CENTRE"="MUBENDE REHABILITATION CENTRE HC III",
                                "NAMUGONGO FUND FOR SPECIAL CHILDREN"="NAMUGONGO FUND FOR SPECIAL CHILDREN CLINIC",
                                "KISENYI HC IV (KAMPALA)"="KISENYI HC IV",
                                "NTOROKO HC III (NTOROKO)"="NTOROKO HC III",
                                "UG PRISONS HOIMA HC II"="HOIMA PRISONS HC II",
                                "MILITARY POLICE HC III (MAKINDYE)"="MAKINDYE POLICE HC III",
                                "MUHUIJU HC III"="MUHWIJU HC III",
                                "ST MARYS HC KASAALA"="ST. MARY'S KASAALA HC III",
                                "KAKASI C.O.U HC III"="KAKASI HC II",
                                "ST MATIA MULUMBA HC III (NAMAYINGO)"="BUSWALE ST. MATIA HC III",
                                "LUYITAAYITA HC III (NGO)"="LUYITAYITA HC III",
                                "PALABEK  GEM HC III"="PALABEK-GEM HC III",
                                "LOBO ROM HC III"="LOBOROM HC III",
                                "MITALA MARIA HC III"="MITALA-MARIA HC III",
                                "NEWLIFE HC III"="NEW LIFE HC III",
                                "KARWENYI HC II"="KARWENYI HC III",
                                "FIDUGA CLINIC"="FIDUGA HC III",
                                "REACHOUT (KINAWATAKA SITE)"="REACHOUT KINAWATAKA CLINIC HC II",
                                "MUWRP CLINIC"="MAKERERE UNIVERSITY WALTER REED CLINIC HC II",
                                "ADVENTIST MEDICAL CENTER"="ADVENTIST MEDICAL CENTRE HC III",
                                "MINISTRY OF DEFENCE CLINIC"="MBUYA MILITARY CLINIC (MOD GARRISON)",
                                "ST PAULS HC IV"="ST. PAUL (KASESE) HC IV",
                                "ISS CLINIC(MBARARA R R HOSPITAL)"="MBARARA REGIONAL REFERRAL HOSPITAL",
                                "LUGAZI MUSLIM HC II"="LUGAZI MUSLIM HC III",
                                "MENGO HOSPITAL COUNSELLING AND HOMECARE"="MENGO DOCTORS CLINIC HC II",
                                "NABISWERA HC III"="NABISWERA HC IV",
                                "MULAGO NATIONAL HOSPITAL- MJAP TB HIV CLINIC"="MULAGO NRH - MJAP TB HIV CLINIC",
                                "KACHANGA HC II"="KACHANGA ISLAND HC II",
                                "MJAP-MUH"="MAKERERE UNIVERSITY HOSPITAL",
                                "ST CLARE ORUNGO HC III"="ST. CLAIRE ORUNGO HC III",
                                "MUKWANO MEDICAL CENTRE"="MUKWANO MEDICAL CENTRE HC II",
                                "TESO SAFE MOTHERHOOD PROJECT"="TESO SAFE MOTHERHOOD HC III",
                                "KYENZAZA HC II"="KYENZAZA HC III",
                                "RUTOTO SDA HC II"="RUTOTO SDA DISPENSARY HC II",
                                "KAZINGA HC II (RUBIRIZI)"="KAZINGA (RUBIRIZI) HC II",
                                "KISHENYI HC II (RUBIRIZI)"="KISHENYI HC II",
                                "KYABAKARA HC II"="KYABAKARA HC III",
                                "NAMUGONGO HC III (KALIRO)"="NAMUGONGO HC III",
                                "KALIRO T C"="KALIRO TOWN COUNCIL HC II",
                                "COMMUNITY HC (KALIRO)"="COMMUNITY HC III",
                                "ST MATIA MULUMBA HC III (BUYENDE)"="IRUNDU ST. MATHIAS MULUMBA HC III",
                                "BUYANJA HC II (GOMBA)"="BUYANJA (GOMBA) HC II",
                                "ST FRANCIS HEALTH CARE SERVICES(NJERU-BUIKWE)"="ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                "KABAALE HC III(KALUNGU)"="KABAALE HC III (KALUNGU)",
                                "RUTI HC"="RUTI HC II",
                                "AOET HC III"="AOET HC II",
                                "MASOLYA HC II"="MASOLYA HC III",
                                "BUBAARE HC III (MBARARA)"="BUBAARE HC III",
                                "RUGASHALI HC III"="RUGASHARI HC III",
                                "EBENEZER SDA MEDICAL HC III"="EBENEZER SDA HC III",
                                "HOLY INNOCENT'S CHILDREN'S HOSPITAL"="HOLY INNOCENTS CHILDREN'S HOSPITAL",
                                "RWANGARA HC II"="RWANGARA HC III",
                                "WENTZ MEDICAL CENTER"="WENTZ MEDICAL CENTRE HC III",
                                "KICWAMBA HC III (KAMWENGE)"="KICWAMBA HC II",
                                "MABAALE HC III (KAGADI)"="MABAALE HC III",
                                "BUSALA HC II"="BUSAALA HC III",
                                "MAYUGE HC III (MAYUGE)"="MAYUGE HC IV",
                                "KASAMBIIKA HC III"="KASAMBIKA HC III",
                                "BUIKWE HOSPITAL (ST CHARLSE LWANGA)"="BUIKWE ST. CHARLES LWANGA HOSPITAL",
                                "KAGULU HC III"="KAGULU HC II",
                                "WESUNIRE HC III"="WESUNIRE CATHOLIC HC III",
                                "TONYA HC II"="TONYA HC III",
                                "KASONGA HC III"="KASONGA HC II",
                                "REACHOUT (BANDA SITE)"="REACH OUT - BANDA CLINIC",
                                "BWEEMA HC III"="BWEEMA HC II",
                                "MAGAMAGA ARMY HC III (MAYUGE)"="MAGAMAGA BARRACKS HC III",
                                "MUJUNZA HC II"="MUJUNZA HC III",
                                "HEALTH INITIATIVE FOR AFRICA- UGANDA"="HEALTH INITIATIVE FOR AFRICA HC II",
                                "BUMADADA HC III"="BUMADANDA HC III",
                                "CHERISH HC III"="CHERISH MEDICAL CENTRE HC III",
                                "KINGDOM LIFE MEDICAL MARTENITY HC II"="KINGDOM LIFE HEALTH CENTER CLINIC",
                                "ZAAM MEDICAL CENTRE"="ZAAM MEDICAL CENTRE KATWE HC III",
                                "ST FRANCIS OF ASSIS NADDANGIRA HC II"="NADANGIRA HC III",
                                "NAGGALAMA HOSPITAL"="ST. FRANCIS NAGGALAMA HOSPITAL",
                                "METHA HOSPITAL"="LUGAZI SCOUL HOSPITAL",
                                "MUZIZI HC II"="MUZIIZI TEA ESTATE HC II",
                                "SAIDINAH ABUBAKER HOSPITAL"="SAIDINA ABUBAKAR ISLAMIC HOSPITAL",
                                "KAMWOKYA CHRISTIAN CARING COMMUNITY"="KAMWOKYA CHRISTIAN CARING COMMUNITY HC III",
                                "HOPE MEDICAL CENTRE"="HOPE MEDICAL CENTER (MBALE) HC II - CBO",
                                "MALABA HC III (TORORO)"="MALABA HC III",
                                "KALAGALA HC II (BUIKWE)"="BUIKWE KALAGALA HC II",
                                "RAPHA MEDICAL CENTER(WAKISO)"="RAPHA MEDICAL CENTRE HC III",
                                "LUFUKA VALLEY HEALTH CENTRE III"="LUFUKA VALLEY HC III",
                                "NDEJJE HC IV (WAKISO)"="NDEJJE HC IV",
                                "MUHORO NGO HC III"="MUHORRO NGO HC III",
                                "KYANKARAMATA HC II"="KYANKARAMATA HC III",
                                "KATEETE HC II (MUKONO)"="KATEETE HC II",
                                "GALIRAAYA HC III"="GALIRAYA HC III",
                                "OMATENGA HC II"="OMATENGA HC III",
                                "KATWE HC III (KASESE)"="KATWE (KABATORO) HC III",
                                "KAKURE HC II"="KAKURE HC III",
                                "KANGULUMIRA INTERGRATED"="KANGULUMIRA I.H.P HC II",
                                "RWIBAALE HC III"="ST. THERESA LISIEUX RWIBAALE HC IV",
                                "AVOGERA HC II"="AVOGERA HC III",
                                "ZEU HC III (ZOMBO)"="ZEU HC III",
                                "BUTIABA HC II"="BUTIABA HC III",
                                "ST MARTHA MATERNITY HOME- BUKEDEA"="ST MARTHA MATERNITY HOME",
                                "KAMACHA HC III"="KAMACA HC III",
                                "SAS CLINIC BUGOLOBI"="SAS CLINIC - BUGOLOBI",
                                "AIDS INFORMATION CENTRE (KAMPALA)"="AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                "LIRA MILITARY BARRACKS HC III"="LIRA ARMY BARRACKS HC III",
                                "LWAJJE HC III"="LWAJJE HC II",
                                "KIGAMBO HC II"="KIGAMBO HC III",
                                "REACHOUT (MBUYA SITE)"="REACHOUT - MBUYA CLINIC HC II",
                                "NILE BREWERIES HC II"="NILE BREWERIES COMPANY CLINIC HC III",
                                "LUGAZI HC II"="LUGAZI II HC II",
                                "KITALYA PRISON HC II"="KITALYA PRISONS HC II",
                                "MARY LAND HC III"="MARYLAND KOCOA HC III",
                                "AYILO HC III"="AYILO I HC III",
                                "BREGMA MEDICAL CENTRE"="BREGMA HC II",
                                "OLIMAI  HC III"="OLIMAI HC III",
                                "KITGUM MATIDI HC III"="KITGUM-MATIDI HC III",
                                "ST ELIZABETH BUMANGI HC III"="ST. ELIZABETH BUMANGI HC III",
                                "KIHUNGYA II"="KIHUNGYA HC II",
                                "RWENZORI MOUNTAINEERING SERVICES"="RWENZORI MT. SERVICES HC III",
                                "ST PETER AND PAUL HC III"="ST. PETER AND PAUL HC III",
                                "KATADOBA HC III"="KATODOBA UMSC HC III",
                                "HIMA GOV'T HC III"="HIMA HC III",
                                "PABWO HC III (GULU)"="PABWO HC III",
                                "LUJJABWA HC II"="LUJJABWA ISLAND HC II",
                                "KITANTE MEDICAL CENTRE"="KITANTE MEDICAL CENTRE HC IV",
                                "LAMBU HC"="LAMBU HC II",
                                "MARATATU B HC III"="MARATATU HC III",
                                "KASAMBYA HC IV (MUBENDE)"="MUBENDE KASAMBYA HC III GOVT",
                                "KAJJANSI HC III"="KAJJANSI HC IV",
                                "MUHANGA COU HCII"="MUHANGA HC II",
                                "BUTOLOOGO HC II (C/O MADUDU HC III)"="BUTOLOOGO HC III",
                                "RUHAAMA HC III"="RUHAMA HC III",
                                "NAMATALE HC II"="NAMATALE HC III",
                                "WEKOMIIRE HC III (ST THEREZA)"="WEKOMIIRE ST. THEREZA HC III",
                                "KYEGEGWA HC IV"="KYEGEGWA HOSPITAL",
                                "KISOJJO HC II (BUKOMANSIMBI)"="KISOJJO (KIBINGE) HC II",
                                "KISOJO HC III (KYENJOJO)"="KISOJO HC III",
                                "KABAROLE HOSPITAL C.O.U"="KABAROLE COU HOSPITAL",
                                "BUGOGO HC III"="BUGOGO HC II",
                                "MABALE(KYENJOJO) HC II"="MABALE TEA FACTORY HC II",
                                "HOPE AGAIN MEDICAL CENTRE (KYENJOJO)"="HOPE AGAIN MEDICAL CENTRE HC III",
                                "VILLA MARIA - KAIHURA HC II"="VILLA MARIA (KAIHURA) HC II",
                                "KIFAMPA HC III (GOMBA)"="KIFAMPA HC III",
                                "LUKAYA HEALTH CARE CENTRE (UGANDA CARES)"="LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
                                "MAMBA HC II"="MAMBA HC III",
                                "ST ADOLF HC III (BUTIITI)"="ST. ADOLF HC II",
                                "KYAAYI HC III"="KYAYI HC III",
                                "BWEYOGERERE HC III MUSLIM (HASSAN TOURABI HC III)"="BWEYOGERERE HASSAN TURABI HC III",
                                "ROYAL VAN ZANTEEN"="ROYAL VAN ZANTEN HC II",
                                "TTAKAJUNGE HC III"="TAKAJJUNGE HC III",
                                "NAGOJJE HC III"="NAGGOJJE HC III",
                                "ST CHARLES KABUWOKO HC III"="KABUWOKO NGO HC II",
                                "BUSAWA MANZE HC III"="BUSAWAMANZE HC III",
                                "ST AMBROSE CHARITY HC III(KAGADI)"="ST. AMBROSE CHARITY HC IV",
                                "MASINDI PRISONS HC III"="MASINDI MAIN PRISON HC III",
                                "KINYARA HC III"="KINYARA SUGAR WORKS HC III",
                                "BUDINI HC III"="ST. FRANCIS BUDINI HC III",
                                "IBULANKU HC III"="IBULANKU COMMUNITY HEALTH CENTER HC III",
                                "FAD MILITARY HC IV (MMH)"="MASINDI MILITARY BARRACKS HC IV",
                                "ST STEPHENS HOSPITAL (MPERERWE)"="ST. STEPHEN'S MPERERWE HOSPITAL",
                                "NAMPUNGE CHURCH OF GOD HC III"="NAMPUNGE HC II",
                                "BANDA HC II (WAKISO)"="WAKISO BANDA HC II",
                                "SDA HC III (MBALE)"="SDA HC III",
                                "KIBENGO HC II (ISINGIRO)"="KIBENGO (ISINGIRO) HC III",
                                "AIDS INFORMATION CENTER(MBALE)"="AIDS INFORMATION CENTRE (MBALE) SPECIAL CLINIC",
                                "AHMADIYYA HC III"="AHAMADIYA HC IV",
                                "KIGARAMA HC III"="KIGARAMA (KIGARAMA) HC III",
                                "BUNGOKHO MUTOTO HC III"="BUNGOKHO-MUTOTO HC III",
                                "BIIRA HC III(NGO)"="BIRA HC III",
                                "BUHARA HC III (GOVT)"="BUHARA HC III",
                                "ST JOSEPH HOSPITAL  KITGUM"="ST. JOSEPH'S KITGUM HOSPITAL",
                                "BUSAANA HC III"="BUSANA HC III",
                                "KIBENGO HC III (LUWEERO)"="KIBENGO HC III",
                                "KATETE HC III (KANUNGU)"="KATETE HC III",
                                "NYAMITYOBORA HC III"="NYAMITYOBORA HC II",
                                "KITURA HC III"="KITURA HC II",
                                "NTUNGAMO PRISON HC II"="NTUNGAMO PRISONS HC II",
                                "IGAYAZA HC II"="IGAYAZA HC III",
                                "KABIRA HC III (MITOOMA)"="KABIRA (MITOOMA) HC III",
                                "MUBENDE R R HOSPITAL"="MUBENDE REGIONAL REFERRAL HOSPITAL",
                                "KICHWAMBA HC III (RUBIRIZI)"="KICWAMBA (RUBIRIZI) HC III",
                                "MAKONDO HC III"="MAKONDO HC II",
                                "MASAKA ARMOURED BRIGADE HC III (A/BDE)"="ARMOURED BRIGADE HC III",
                                "SIAAP BUGOMA CLINIC"="SSESE ISLANDS AFRICAN AIDS PROJECT (SIAAP) HC II",
                                "MASAKA PRISON HC III"="MASAKA PRISONS HC III",
                                "MT ST MARYS HOSPITAL"="MT. ST. MARY'S HOSPITAL-DOK",
                                "ST FRANCIS MBIRIIZI HC III"="MBIRIZI ST. FRANCIS HC III",
                                "INTERNATIONAL HOSPITAL KAMPALA"="INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                "KAKOMO HC III"="KAKOMO HC IV",
                                "KAKOMA HC III (ISINGIRO)"="KAKOMA HC III",
                                "AIDS INFORMATION CENTRE (KABALE)"="AIDS INFORMATION CENTRE (KABALE) SPECIAL CLINIC",
                                "BIISO HC III"="BIISO HC IV",
                                "KITOVU HOSPITAL"="MASAKA (KITOVU HOSPITAL) REGIONAL BLOOD BANK",
                                "GALILEE COM GEN HOSPITAL"="GALILEE COMMUNITY HOSPITAL",
                                "KINONI HC III (KIRUHURA)"="KIRUHURA KINONI HC III",
                                "ST MARYS HC III-KYEIBUZA"="KYEIBUZA (ST. MARY'S) HC III",
                                "LAKE MBURO HC III"="KANYARYERU (LAKE MBURO) HC III",
                                "KARAMBI HC III (KABAROLE)"="KARAMBI (KABAROLE) HC III",
                                "IGANGA ISLAMIC MEDICAL CENTER"="IGANGA ISLAMIC MEDICAL CENTRE HC III",
                                "KIYUNGA HC IV ( LUUKA)"="KIYUNGA HC IV",
                                "BUKIRO HC III"="BUKIIRO HC III",
                                "BANDA HC III (NAMAYINGO)"="BANDA HC III",
                                "BUGAANA HC II"="BUGANA HC II",
                                "BUSIRO HC III"="BUSIRO CHURCH OF GOD HC III",
                                "KAMULI DISTRICT GOVT HOSPITAL"="KAMULI HOSPITAL",
                                "NAMUNINGI HC II"="NAMUNYINGI HC II",
                                "TASO JINJA"="TASO JINJA SPECIAL CLINIC",
                                "JINJA POLICE HC III (MATERNITY)"="JINJA POLICE HC III",
                                "AIDS INFORMATION CENTER(JINJA)"="AIDS INFORMATION CENTRE (JINJA) SPECIAL CLINIC",
                                "KIGANGAZI HC II"="KIGANGAZZI HC II",
                                "KALAMBA HC II"="KALAMBA COMMUNITY HC II",
                                "ST ELIZABETH NTUUSI HC III (NGO)"="NTUUSI HC III",
                                "BUSEMBATIA HC III"="BUSEMBATYA HC III",
                                "IKUMBYA HC III (LUUKA)"="IKUMBYA HC III",
                                "TORORO DISTRICT HOSPITAL"="TORORO GENERAL HOSPITAL",
                                "SIIPI HC III"="SIPI HC III",
                                "MOROTO R R HOSPITAL"="MOROTO REGIONAL REFERRAL HOSPITAL",
                                "MOROTO ARMY HC IV"="ARMY BARRACKS HC III",
                                "LATANYA HC III"="LATANYA HC II",
                                "ST AGATHA LWEBITAKULI"="LWEBITAKULI NGO HC III",
                                "ACHOL PII HC III"="ACHOLPII HC III",
                                "ACHOLI BUR HC III"="ACHOLI-BUR HC III",
                                "KITAGATA  HOSPITAL"="KITAGATA HOSPITAL",
                                "KASESE COMMUNITY MEDICAL CENTRE(KCMC) HC III"="BISHOP MASEREKA CHRISTIAN FOUNDATION HC IV",
                                "ST STEPHENS DISP AND MATERNITY CENTER (LUZIRA)"="ST. STEVEN DISPENSARY AND MATERNITY HC III",
                                "JOY MEDICAL CENTRE(KAMPALA)"="JOY MEDICAL CENTRE HC III",
                                "DOUBLE CURE MEDICAL CENTRE"="DOUBLE CURE HC IV",
                                "OUR LADY OF MARIA ASSUMPTA"="MARIA ASSUMPTA HC III",
                                "EPI CENTRE HC IV (BUTAMBALA)"="EPI-CENTRE SENGE HC III",
                                "KYABADAAZA HC III"="KYABADAZA HC III",
                                "KOMGBE HC III"="KOMGBE HEALTH CENTRE HC III",
                                "OKUBANI HC III"="OKUBANI HEALTH CENTRE HC III",
                                "KYAMULIBWA MRC"="MRC KYAMULIBWA HC II",
                                "RAKAI HEALTH SCIENCES PROGRAM"="RAKAI HEALTH SCIENCES PROGRAM CLINIC",
                                "NYANTABOOMA HC III"="NYANTABOMA HC III",
                                "BUKUUKU HC IV"="BUKUKU HC IV",
                                "KITOVU MOBILE"="KITOVU MOBILE CLINIC",
                                "KASOZI HC III (WAKISO)"="WAKISO KASOZI HC III",
                                "KINYARUGUNJO HC III"="KINYARUGONJO HC III",
                                "MUNATHAMAT HC III"="MUNATHAMAT HC II",
                                "KASAANA HC III"="KASANA HC III",
                                "RURAMBIIRA HC II"="RURAMBIRA HC II",
                                "KAYONZA HC III (KANUNGU)"="KAYONZA HC III",
                                "ST FRANCIS HOSPITAL (MUTOLERE)"="MUTOLERE HOSPITAL",
                                "BIHANGA ARMY HC II"="BIHANGA UPDF BARRACKS HC II",
                                "MULAGO NATIONAL HOSPITAL- MUJHU CLINIC"="MULAGO NRH - MUJHU CLINIC",
                                "RWEMIGINA HC II"="RWEMIGYINA HC II",
                                "AIDS INFORMATION CENTRE (MBARARA)"="AIDS INFORMATION CENTRE (MBARARA) SPECIAL CLINIC",
                                "DDAMBA HC III"="DAMBA HC II",
                                "NOAHS ARK FAMILY CLINIC"="NOAH'S ARK HC III",
                                "UGANDA MARTYRS HOSPITAL (MBALALA)"="UGANDA MARTYRS MEDICAL CENTRE",
                                "NTUNGAMO HC III"="NTUNGAMO HC IV",
                                "RUBAARE HC IV (NTUNGAMO)"="RUBARE HC IV",
                                "KAYONZA HC III (NTUNGAMO)"="KAYONZA (NTUNGAMO) HC III",
                                "NYAMITANGA HC III"="NYAMITANGA GOVT HC III",
                                "JAGUZI HC II"="JAGUSI HC III",
                                "ENDIINZI HC III"="ENDIIZI HC III",
                                "BUGEMBE HC  IV"="BUGEMBE HC IV",
                                "MBARARA CENTRAL PRISON HC III"="MBARARA MAIN PRISONS HC III",
                                "MAGAMAGA HC III (JINJA)"="MAGAMAGA HC III",
                                "ISIBUKA HC III"="ISIBUKA NURSING HOME HC III",
                                "KAIROS HC IV"="KAIROS MEDICAL CENTRE HC IV",
                                "AARAPOO HC II"="AARAPOO HC III",
                                "KIDETOK MISSION HC III"="KIDETOK HC III",
                                "HAMA HC II"="HAAMA HC II",
                                "KASAMBYA HC III (KALUNGU)"="KASAMBYA (KALUNGU) HC III",
                                "LOBALANGIT HC II"="LOBALANGIT HC III",
                                "BIDIBIDI RECEPTION HC III"="BIDIBIDI HC III",
                                "LUZIRA HC III"="LUZIRA HEALTH CENTRE HC III",
                                "BANGATUTI HC III"="BANGATUTI HEALTH CENTRE HC III",
                                "IYETE HC III"="IYETE HEALTH CENTRE HC III",
                                "KAKIIKA PRISONS"="KAKIIKA PRISONS HC II",
                                "YIVU ABEA HC II"="YIVU ABEA HC III",
                                "ZIA ANGELINA"="ZIA ANGELINA HC II",
                                "BENEDICT MEDICAL CENTER"="BENEDICT HC IV",
                                "VINE MEDICARE CLINIC"="VINE MEDICARE CLINIC (OLD KAMPALA) HC II",
                                "KABUBBU HC III"="KABUBBU HC IV",
                                "GGWATIRO HOSPITAL"="GGWATIRO NURSING HOME HOSPITAL",
                                "ST ULRIKA HC III"="KIZIBA CATHOLIC/KIZIBA ST. ULRIKA HC III",
                                "KABAALE HC II (SSEMBABULE)"="SEMBABULE KABAALE HC II",
                                "MARACHA HOSPITAL (ST JOSEPH)"="ST. JOSEPHS MARACHA HOSPITAL",
                                "CASE HOSPITAL"="CASE MEDICAL CENTRE HOSPITAL",
                                "BUKASA HC II (MPIGI)"="BUKASA HC II",
                                "KITAASA HC III"="ST. MECHTILDA KITAASA HC III",
                                "ST JOSEPH'S BUYEGE HC III"="BUYEGE HC III",
                                "HEALTH INITIATIVES ASSOCIATION"="HEALTH INITIATIVE ASSOCIATION HC II",
                                "KYAALI HC III (MPIGI )"="NSAMU/KYALI HC III",
                                "NJERU T/C HC III"="NJERU TOWN COUNCIL HC III",
                                "ST LUKE NAMALIGA HC III"="ST. LUKE NAMALIGA HC III",
                                "NKOKONJERU HOSPITAL NGO (BUIKWE)"="NKOKONJERU HOSPITAL",
                                "GGOLI HC III (MPIGI)"="GGOLI - ST YAKOBO HC III",
                                "BUYAGA HC III(BULAMBULI)"="BUYAGA HC III",
                                "KWANYIYI HC III"="KWANYIY HC III",
                                "SSEGUKU HC II"="SEGUKU HC II",
                                "TRIAM MEDICAL CENTER"="TRIAM MEDICAL CENTRE HC II",
                                "GOMBE HC II"="GOMBE (WAKISO) HC II",
                                "MUKONO HOSPITAL C.O.U"="MUKONO COU HOSPITAL",
                                "KASAWO MISSION HC II"="KASAWO MISSION HC III",
                                "OUR LADY  OF LOURDES HC III (NAKASONGOLA)"="OUR LADY NAKASONGOLA HC III",
                                "ESERI DOMICILIARY CLINIC"="ESERI DOM CLINIC HC II",
                                "FRANCISCAN HC KAKOOGE"="ST. FRANCISCAN HC IV",
                                "KABERAMAIDO GENERAL HOSPITAL"="KABERAMAIDO HOSPITAL",
                                "KATUUGO HC II"="KATUUGO HC III",
                                "BISHOP CEASER ASILI HOSPITAL"="BISHOP ASCILI HOSPITAL",
                                "ST FRANCIS HC   III MIGYEERA"="ST. FRANCIS HC III",
                                "APAPAI HC II (KABERAMAIDO)"="APAPAI HC III",
                                "NAMUTUMBA HC  III (NAMUTUMBA)"="NAMUTUMBA HC III",
                                "NYENGA HOSPITAL"="ST. FRANCIS NYENGA HOSPITAL",
                                "TASO MBALE"="TASO MBALE SPECIAL CLINIC",
                                "HOLY CROSS HC III (KIKYUSA)"="HOLY CROSS - KIKYUSA HC III",
                                "TASO MBARARA"="TASO MBARARA SPECIAL CLINIC",
                                "MPUMUDDE HC IV (JINJA)"="MPUMUDDE HC IV",
                                "MYANZI HC III (MUBENDE)"="MYANZI HC III",
                                "KALUNGI HC III(KALUNGU)"="KALUNGI (LUKAYA) HC III",
                                "GADAFI HC III"="GADDAFI BARRACKS HC III",
                                "KYANGATTO HC II"="KYANGATO HC II",
                                "KAKIRA HC III (JINJA)"="KAKIRA HC III",
                                "ST MONICA BIRONGO HC III (KALUNGU)"="ST. MONICA BIRONGO HC III",
                                "KAKIRA SUGAR LTD HOSPITAL"="KAKIRA SUGAR WORKERS HOSPITAL",
                                "BULUBA HOSPITAL ST FRANCIS"="BULUBA HOSPITAL",
                                "HOPE MEDICAL CENTER (SHEEMA)"="HOPE MEDICAL CENTRE HC III",
                                "THE SURGERY"="THE SURGERY HC III",
                                "2ND DIV MILITARY HOSPITAL(MAKENKE)"="UPDF 2ND DIV. HC IV",
                                "GULU MILITARY HOSPITAL (4TH DIVISION)"="GULU MILITARY HOSPITAL",
                                "RWENSHAMA HC III"="RWENSHAMA (GOVT) HC III",
                                "NDEJJE UNIVERSITY HC III"="NDEJJE UNIVERSITY HC II",
                                "KAUGA PRISON HC III"="KAUGA PRISONS HC II",
                                "MAYUGE HC III (BUGIRI)"="BUGIRI MAYUGE HC III",
                                "KUMI HOSPITAL"="KUMI (ONGINO) HOSPITAL",
                                "TASO TORORO"="TASO TORORO SPECIAL CLINIC",
                                "LIRA PAG MISSION HOSPITAL"="PAG MISSION HOSPITAL",
                                "FOCREV HC III"="LUMINO FOC REV HC III",
                                "KOTIDO HC IV"="KOTIDO HOSPITAL",
                                "ST ELIZABETH HC IV-MAGALE"="MAGALE (UCMB) HC IV",
                                "BUTIRU CHRISCO HOSPITAL"="BUTIRU CHRISCO (UCMB) HC III",
                                "NABULOLA CMC HC III"="NABULOLA HC III",
                                "AIDS INFORMATION CENTRE(LIRA)"="AIDS INFORMATION CENTRE (LIRA) SPECIAL CLINIC",
                                "ALIMI HC II"="ACIMI HC II",
                                "KIYUNI HC III (MUBENDE)"="KIYUNI (MUBENDE) HC III",
                                "ATURA HC II"="ATURA HC III",
                                "LIRA MEDICAL CENTRE"="LIRA MEDICAL CENTRE LTD HC III",
                                "MINAKULU HC III (OYAM)"="MINAKULU (ACENO) HC II",
                                "KYEMBOGO HC III"="KYEMBOGO HOLY CROSS HC III",
                                "HOPE CLINIC LUKULI"="HOPE CLINIC LUKULI HC III",
                                "LACOR HOSPITAL"="ST. MARY'S HOSPITAL LACOR",
                                "TASO RUKUNGIRI"="TASO RUKUNGIRI SPECIAL CLINIC",
                                "KALANGAALO HC II"="KALANGALO HC III",
                                "NAMUTAMBA HC III  COU  (MITYANA)"="NAMUTAMBA HC III",
                                "KIKOLIMBO HC III"="KIKOLIMBO HC II",
                                "KITONGO HC III (MITYANA)"="KITONGO HC III",
                                "ARUA R R HOSPITAL"="ARUA REGIONAL REFERRAL HOSPITAL",
                                "MBALE HC II (KYENJOJO)"="MBALE HC II",
                                "ST GABRIEL MIREMBE MARIA HC III"="ST. GABRIEL MIREMBE MARIA HC III",
                                "ALL SAINTS HC II"="ALL SAINTS PURANGA HC II",
                                "ST LUKE KMD (KIYINDA )HC III (MITYANA)"="ST. LUKE KIYINDA MITYANA DIOCESE HC III",
                                "ST FRANCIS HC III OCODRI"="ST. FRANCIS (OCODRI) HC III",
                                "ST FRANCIS C HC III (HOSFA)- MITYANA"="ST. FRANCIS HC IV (MITIYANA)",
                                "KIYUNI HC III (KYANKWANZI)"="KIYUNI HC III",
                                "ST JACINTA HC III"="ST. JACINTA ZIGOTI HC III",
                                "NDORWA PRISON"="NDORWA PRISONS HC II",
                                "JINJA ISLAMIC HC"="JINJA ISLAMIC HC III",
                                "NKOKONJERU  HC III (KAYUNGA)"="NKOKONJERU HC III",
                                "KAWEWETA HC III"="KAWEWEETA HC III",
                                "BUKONTE HC III"="BUKONTE HC II",
                                "WELL SPRINGS CHILDRENS MEDICAL CETER - KAMUTUUZA HC III"="WELL SPRING HC III",
                                "YAYARI HC III"="YAYARI HEALTH CENTRE HC III",
                                "BOLOMONI HC III"="BOLOMONI HEALTH CENTRE HC III",
                                "YANGANI HC II"="YANGANI HEALTH CENTRE HC III",
                                "DRAMBA HC II"="DRAMBA HC III",
                                "NATTYOLE HC III"="ST. KIZITO NATYOLE HC III",
                                "TASO MASAKA"="TASO MASAKA SPECIAL CLINIC",
                                "KATWE HC III (KIBOGA)"="KATWE (DWANIRO) HC III",
                                "KYANAMUYOJO HC III"="KYANAMUYONJO HC III",
                                "MYERI HC II (KYENJOJO)"="MYERI HC III",
                                "NGOMA HC III (NTUNGAMO)"="NGOMA HC IV",
                                "HIMA CEMENT CLINIC"="HIMA STAR HC II",
                                "KATOJO HC III (PRISONS)"="KATOJO PRISONS HC III",
                                "AWACH HC IV (GULU)"="AWACH HC IV",
                                "FORT POLICE HC III"="POLICE CLINIC (KABAROLE) HC III",
                                "BUHARA HC III (NGO)"="BUHARA (NGO) HC III",
                                "ST MARTIN HC III (KYENJOJO)"="ST. MARTINS-MABIIRA HC III",
                                "BUTARE HC III (NTUNGAMO)"="BUTAARE HC III",
                                "AZUR CHRISTIAN HC IV"="AZUR HC IV",
                                "GOLI HC IV (NEBBI)"="GOLI HC IV",
                                "HOLY FAMILY HOSPITAL-NYAPEA"="NYAPEA HOSPITAL",
                                "KITGUM GENERAL HOSPITAL"="KITGUM HOSPITAL",
                                "LIRA KATO HC III (AGAGO)"="LIRA KATO HC III",
                                "BUHUKA HC III"="BUHUUKA HC III",
                                "CHILD AND FAMILY MEDICAL"="CHILD AND FAMILY MEDICAL CLINIC HC II",
                                "KANYAMWIRIMA HC III"="KANYAMWIRIMA ARMY HC III",
                                "JUMBO MEDICAL CLINIC"="JUMBO CORNER MEDICAL CLINIC HC II",
                                "KABAMBA HC III"="KABAMBA BARRACKS HC III",
                                "ZOMBO HC IV"="ZUMBO HC III",
                                "NAMITI HC II"="NAMITI HEALTH CENTRE HC II",
                                "ARIBA HC II (OYAM)"="ARIBA HC III",
                                "OPETA HC III (KOLE)"="OPETA HC II",
                                "BUWOOYA HC III"="BUWOOYA HC II",
                                "KYANYA SDA HC III"="KYANYA SDA HC II",
                                "BELLE HC IV"="BELLE HC III",
                                "BIDDABUGYA HC III"="BIDABUJA HC III",
                                "NAKASONGOLA PRISON HC III"="NAKASONGOLA PRISONS HC III",
                                "KAVULE HC II"="KAVULE (BUIKWE) HC II",
                                "AGULE COMMUNITY HC III (NGO)"="AGULE COMMUNITY HC III",
                                "SSANJE DOMICILIARY CLINIC"="SSANJE DOMICILIARY CLINIC HC III",
                                "ST LUKE HC III KATIYI"="ST. LUKE KATIYI HC III",
                                "PAJULU HC III (ARUA)"="PAJULU HC III",
                                "TASO GULU"="TASO GULU  SPECIAL CLINIC",
                                "MBIRIZI MOSLEM HC III"="MBIRIZI MUSLIM HC III",
                                "SWINGA HC III"="SWINGA HEALTH CENTRE HC III",
                                "LODONGA HC III"="LODONGA HC IV",
                                "MUKO HC III (NGO)"="MUKO NGO HC III",
                                "ST JOSEPH  H/ C III(MADUDU)"="ST. JOSEPH MADUDU HC III",
                                "MUINAINA  PRISON HC II"="MUINAINA PRISONS HC II",
                                "MITANDI HC III (NGO)"="MITANDI HC III",
                                "KAGOGGO HC III"="KAGOGGO HC II",
                                "SIRIMULA HC III"="SIRIMULA HC II",
                                "ATIPE HC II"="ATIPE HC III",
                                "GALIMAGI HC III"="GALIMAGI (BUTEBO) HC III",
                                "APAPAI HC IV (SERERE)"="APAPAI HC IV",
                                "KALUNGI HC III (NAKASONGOLA)"="KALUNGI (KALUNGI) HC III",
                                "NGOMANENE HC III(GOMBA)"="NGOMANENE HC III",
                                "MOROTO PRISONS HC III"="MOROTO PRISONS HC II",
                                "MOYO MISSION HC III"="MOYO MISSION HC IV",
                                "KIREWA HC III"="KIREWA COMMUNITY HC III",
                                "CHEMWON HC III"="CHEMWOM HC III",
                                "LIVING WATER HC"="LIVING WATER COMMUNITY MEDICAL CENTRE CLINIC",
                                "KALAWA HC II"="NAKILORO HC II",
                                "SENYI HC II"="SSENYI HC II",
                                "KABIZZI HC II"="KABIZI HC II",
                                "MIREMBE MEDICAL CENTRE"="MIREMBE (NANGABO) HC III",
                                "CHRIST THE KING MEDICAL CENTRE"="CHRIST THE KING HC II",
                                "ST ANTHONY HOSPITAL"="ST. ANTHONY'S TORORO HOSPITAL",
                                "KYAMBOGO UNIVERSITY MEDICAL CENTRE"="KYAMBOGO UNIVERSITY MEDICAL CENTRE HC II",
                                "INTERNATIONAL MEDICAL CENTRE"="KAMPALA INTERNATIONAL MEDICAL CENTRE HC II",
                                "KIBANGA HC III (ST ELISABETH OF THURINGEN )"="ST. ELIZABETH KIBANGA IHU HC III",
                                "NAKAYONZA HC III"="NAKAYONZA  HC III",
                                "OTUMBARI HC III"="OTUMBARI ST. LAWRENCE HC III",
                                "LUWEERO INDUSTRIES"="LUWERO INDUSTRIES LTD CLINIC",
                                "ORIENT MEDICAL CENTRE"="ORIENT MEDICAL CENTER HC III",
                                "NYAKITIBWA HC III"="NYAKITIIBWA HC III",
                                "KITOVU HC II (MAYUGE)"="KITOVU HC II",
                                "TWAJIJ HEALTH CENTRE III"="TWAJIJI HEALTH CENTRE HC III",
                                "BUGAYA HC III (BUVUMA)"="BUGAYA HC III",
                                "ST MARYS MNH BUKOMANSIMBI"="ST. MARY'S MATERNITY & NURSING HOME HC III",
                                "SOROTI PRISON HC III"="SOROTI MAIN PRISONS HC III",
                                "NSAMBYA HOSPITAL"="ST. FRANCIS NSAMBYA HOSPITAL",
                                "BUGOLOBI MEDICAL CENTRE"="BUGOLOBI MEDICAL CENTRE HC II",
                                "KATETA COU HC II"="KATETA NGO HC II",
                                "MARANATHA HC II"="MARANATHA HC III",
                                "RULONGO HC II"="RURONGO HC III",
                                "FAITH MULIRA HEALTH CARE CENTER"="FAITH MULIRA HEALTH CARE CENTRE HC III",
                                "ST ANTHONY HC II"="ST. ANTHONY HC II",
                                "NAMUNGO HC II"="NAMUNGO HC III",
                                "KAGUNGA HC II"="RUBANDA KAGUNGA HC II",
                                "KYARUMBA HC III (GOVERNMENT)"="KYARUMBA HC III",
                                "MUNOBWA HC II"="MUNOBWA-HIIMA TEA FACTORY HC II",
                                "ACET HC II"="ACET COMMUNITY ORIENTED CLINIC",
                                "KARWENSANGA HC II"="KARWESANGA HC II",
                                "IMVEPI HC III"="IMVEPI HC II",
                                "RESTORATION GATE WAY"="RESTORATION GATEWAY HOSPITAL",
                                "MBURAMIZI HC III"="MBURAMIZI ARMY BARRACKS HC III",
                                "ABII CLINIC"="ABII CLINIC HC IV",
                                "JURU HC II"="JURU HC III",
                                "APIRE HC II"="APIRE HC III",
                                "KOCHI HC III (YUMBE)"="KOCHI HC III",
                                "KIRYANDONGO PRISON HC II"="KIRYANDONGO PRISON CLINIC HC II",
                                "KARIN HC II"="KARIN MEDICAL CENTRE HC II",
                                "KABIGI HC III"="KABIGI MUSLIM HC III",
                                "BUZIIRASAGAMA HC II"="BUZIRASAGAMA HC II",
                                "KABERAMAIDO CATHOLIC HC III"="KABERAMAIDO CATHOLIC MISSION HC III",
                                "KAYONZA TEA FACTORY"="KAYONZA TEA FACTORY HC III",
                                "TORORO PRISON HC II"="TORORO MAIN PRISONS HC III",
                                "JOMOROGO HC III"="JOMOROGO HEALTH CENTRE HC III",
                                "KYOTERA MEDICAL CENTRE"="KYOTERA MED. CENTRE HC III",
                                "ST FRANCIS ASSISI KITABU (KASESE)"="ST. FRANCIS OF ASSIS-KITABU HC III",
                                "LIRA PRISON HC III"="LIRA PRISONS HC II",
                                "NYAMIRANGA HC III"="NYAMIRINGA HC III",
                                "BUGIRI HC III"="BUGIRI HC II (KANUNGU)",
                                "LOYORO HC II"="LOYORO HC III",
                                "MUNDADDE HC III"="MUNDADDE HC II",
                                "ST SABENA HC II"="ST. SABENA HC II",
                                "BUKATUBE HC II"="BUKATUBE HC III",
                                "KARUHEMBE HC II"="KARUHEMBE HC III",
                                "LAKWATOMER HC III"="LAKWATOMER HC II",
                                "ARAHMAH MEDICAL CENTER"="ARAHMAH MEDICAL CENTRE HC IV",
                                "ABIA HC II"="ABIA HC II (ABIA SUBCOUNTY)",
                                "KAYONZA HC II (RAKAI)"="KAYONZA KACHEERA HC II",
                                "UG PRISON RUKUNGIRI HC II"="RUKUNGIRI PRISONS HC II",
                                "ADUKU HC II (NGO)"="ADUKU MISSION HC II",
                                "ODOUBU HC III"="ODUOBU HC III",
                                "ST CLARET HC II (NYABWINA)"="ST. CLARET HC II",
                                "MULAGO NATIONAL HOSPITAL- MJAP (CDC) MULAGO"="KIRUDDU NRH - MJAP (CDC) MULAGO",
                                "VICTORIA MEDICAL CENTRE"="VICTORIA MEDICAL CENTRE HC II",
                                "KADIC CLINIC NAKULABYE"="KADIC CLINIC NAKULABYE HC II",
                                "ST ALOYSIOUS NAMABAALE HC III"="ST. ALOYSIUS NAMABAALE",
                                "DOCTORS HOSPITAL SSEGUKU"="DOCTORS HOSPITAL SEGUKU HOSPITAL",
                                "ALERE HC II"="ALERE (AMURIA) HC II",
                                "ST MARTINS HC III ( AMAKIO)"="ST. MARTINS AMAKIO",
                                "KARAMOJA DIOCESIAN DEVELOPMENT ORG"="KDDO HC III",
                                "ST FRANCIS HC II (AKIA)"="AKIA ST. FRANCIS HC II",
                                "NGOM-OROMO HC II"="NGOMOROMO HC II",
                                "ANAKA HC II (MADI OPEI)"="ANAKA HC II",
                                "AMURU HC III"="AMURU LACOR HC III",
                                "KIGO PRISONS HC III"="KIGO MAIN PRISONS HC III",
                                "KAMPALA MEDICAL CHAMBERS"="KAMPALA MEDICAL CHAMBERS CLINIC HC II",
                                "ST PIUS KIDEPO RUPA HC III"="ST. PIUS KIDEPO HC III",
                                "AKOBOI HC II (KATAKWI)"="AKOBOI HC II",
                                "ABALANG HC II"="ABALANG HC II (ANYARA SUBCOUNTY)",
                                "MENGO HOSPITAL-EMTCT"="MENGO HOSPITAL",
                                "RUSHESHE HC III"="RUSHEESHE HC III",
                                "NABUTONGWA HC III"="NABUTONGWA HC II",
                                "IGAMARA HC III"="IGAMARA HEALTH CENTRE HC III",
                                "MASAKA MUNICIPAL COUNCIL HC II"="MASAKA MUNICIPAL CLINIC HC II",
                                "BUKUNGA HC III"="BUKUNGA HC II"
                                
  ))
#####################################################################################
#### The numerator (Data source, hub module) ####
#### cleaning names and obtaining months and Quarters
df_track <- df_track %>% 
  mutate(
    Yr  =  year(`Visit Date`),
    Month =  month(`Visit Date`)
  ) 
# Add quarter, "Qtr"
df_track <- df_track %>% 
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))
#### rename the columns
df_track <- df_track %>% 
  rename(DVisit     =  `Visit Date`,
         HName      =   `Hub Name`,
         HFacility  =    `Facility Name`,
         RName      =    `Riders Name`)

##clean the HName column
df_track <- df_track %>% 
  mutate(HName = recode(HName,
                        # for reference: OLD = NEW
                        "Aber Hospital"            =     "Aber",
                        "Abim Hospital"            =     "Abim",
                        "Adjumani Hospital"        =     "Adjumani",
                        "Alebtong H/C IV"          =     "Alebtong",
                        "Amolatar H/C IV"          =     "Amolatar",
                        "Amuria H/C IV"            =      "Amuria",
                        "Anaka Hospital"           =      "Anaka",
                        "Apac General Hospital"            =      "Apac",
                        "Arua RRH"                 =      "Arua",
                        "ATIAK HC IV"              =      "Atiak",
                        "Atutur Hospital"          =      "Atutur",
                        "Budadiri H/C IV"          =      "Budadiri",
                        "Bududa Hospital"          =      "Bududa",
                        "Bugiri Hospital"          =      "Bugiri",
                        "Bundibugyo Hospital"      =      "Bundibugyo",
                        "Bushenyi H/C IV"          =      "Bushenyi",
                        "Busolwe Hospital"         =      "Busolwe",
                        "Butenga H/C IV"           =      "Masaka",
                        "Buvuma H/C IV"            =      "Buvuma",
                        "Buyinja H/C IV"           =      "Buyinja",
                        "Dokolo H/C IV"            =      "Dokolo",
                        "Entebbe Grade B Hospital" =      "Entebbe",
                        "FortPortal RRH"           =      "FortPortal",
                        "Gombe Hospital"           =      "Gombe",
                        "Hoima RRH"                =      "Hoima",
                        "Iganga Hospital"          =      "Iganga",
                        "Itojo Hospital"           =      "Itojo",
                        "Jinja RRH"                =      "Jinja",
                        "Kaabong"                  =      "Kaabong",
                        "Kaabong Hospital"         =      "Kaabong",
                        "Kabale RRH"               =      "Kabale",
                        "Kaberamaido H/C IV"       =      "Kaberamaido",
                        "Kagadi  Hospital"         =      "Kagadi",
                        "Kagando Hospital"         =      "Kagando",
                        "Kakindo H/C IV"           =      "Kakindo",
                        "Kalangala H/C IV"         =      "Kalangala",
                        "Kalongo Hospital"         =      "Kalongo",
                        "Kamuli District Govt Hospital"    =           "Kamuli",
                        "Kanungu H/C IV"          =       "Kanungu",
                        "Kapchorwa Hospital"      =       "Kapchorwa",
                        "Kassanda H/C IV"         =       "Kassanda",
                        "Kaswa"                   =       "FortPortal",
                        "Katakwi Hospital"        =       "Katakwi",
                        "Kawolo Hospital"         =       "Kawolo",
                        "Kayunga Hospital"        =       "Kayunga",
                        "Kazo H/C IV"             =       "Mbarara",
                        "Kiboga Hospital"         =       "Kiboga",
                        "Kidera H/C IV"           =       "Kidera",
                        "Kigandalo H/C IV"        =       "Kigandalo",
                        "Kilembe Mines Hospital"  =       "Kilembe Mines",
                        "Kiryandongo Hospital"    =       "Kiryandongo",
                        "Kisoro Hospital"         =        "Kisoro",
                        "Kitagata  Hospital"      =        "Kitagata",
                        "Kitgum General Hospital"         =  "Kitgum",
                        "Koboko H/C IV"           =        "Koboko",
                        "Kotido General Hospital"         =       "Kotido",
                        "Kyegegwa H/C IV"         =       "Kyegegwa",
                        "Kyenjojo Hospital"       =       "Kyenjojo",
                        "Lalogi  H/C IV"          =       "Lalogi",
                        "Lira RRH"                =       "Lira",
                        "Luwero H/C IV"           =       "Luweero",
                        "Luwero General Hospital" =       "Luweero",
                        "Lwengo H/C IV"           =       "Lwengo",
                        "Lyantonde Hospital"      =       "Lyantonde",
                        "Maddu H/C IV"            =        "Maddu",
                        "Madi Opei H/C IV"        =        "Madi Opei",
                        "Masafu Hospital"         =        "Masafu",
                        "Masaka RRH"              =        "Masaka",
                        "Masindi Hospital"        =       "Masindi",
                        "Matany Hospital"         =        "Matany",
                        "Mbale RRH"               =        "Mbale",
                        "Mbarara RRH"             =        "Mbarara",
                        "Mityana Hospital"        =       "Mityana",
                        "Moroto RRH"              =        "Moroto",
                        "Moyo Hospital"          =         "Moyo",
                        "Mpigi H/C IV"            =        "Mpigi",
                        "Mubende RRH"             =        "Mubende",
                        "Mugusu"                  =        "FortPortal",
                        "Mukono General Hospital"         =         "Mukono",
                        "Nakasongola H/C IV"      =         "Nakasongola",
                        "Nebbi Hospital"          =         "Nebbi",
                        "Ngoma H/C IV (Nakaseke)" =         "Ngoma",
                        "Ngora H/C IV"            =         "Ngora",
                        "Ntwetwe H/C IV"          =         "Ntwetwe",
                        "Nyakibale Hospital"      =         "Nyakibale",
                        "Pajule H/C  IV"          =         "Pajule",
                        "Pallisa General Hospital"        =         "Pallisa",
                        "Rakai Hospital"          =          "Rakai",
                        "Rhino Camp H/C IV"       =          "Rhino Camp",
                        "Rugazi H/C IV"           =          "Rugazi",
                        "Ruhoko H/C IV"           =          "Ruhoko",
                        "Rukunyu General Hospital"        =          "Rukunyu",
                        "Rushere Community Hospital"       =           "Rushere",
                        "Rwekubo H/C IV"          =          "Rwekubo",
                        "Serere H/C IV"           =           "Serere",
                        "Soroti RRH"              =           "Soroti",
                        "Ssembabule H/C IV"       =           "Ssembabule",
                        "Tokora H/C IV"           =           "Tokora",
                        "Tororo  Lab Hub"         =           "Tororo",
                        "UNHLS"                   =           "Kampala",
                        "Wakiso H/C IV"           =           "Wakiso",
                        "Warr H/C IV"             =           "Warr",
                        "Yumbe Hospital"          =           "Yumbe",
                        "Bumanya H/C IV"          =           "Bumanya"
        ))

# Capitalize the HName
df_track$HName <- toupper(df_track$HName)
df_track$HFacility  <-toupper(df_track$HFacility)

# R R Hospital
df_track$HName  <- gsub(" R R HOSPITAL","",df_track$HName)
df_track$HFacility  <- gsub("H/C", "HC", df_track$HFacility)
df_track$HFacility  <- gsub("R R HOSPITAL", "REGIONAL REFERRAL HOSPITAL", df_track$HFacility)
####################################################################################
#### Cleaning e-tracking health facility names df_track1 ####
df_track1    <-   df_track %>% 
  mutate(HFacility     =      recode(HFacility,
                                     "403 BRIGADE"="403 BRIGADE",
                                     "405 BRIGADE HC III"="405 BRIGADE HC III",
                                     "407 BRIGADE HC III"="MORUITA 407 BREGADE HC III",
                                     "509 BRIGADE HC III"="509 BRIGADE HC III",
                                     "AAKUM HC II"="AAKUM HC II",
                                     "AARAPOO HC II"="AARAPOO HC III",
                                     "ABAKO ELIM HC II"="ABAKO-ELIM HC II",
                                     "ABAKO HC III"="ABAKO HC III",
                                     "ABALA HCIII"="ABALA HC III",
                                     "ABALANG HC II (DOKOLO)"="ABALANG HC II (OKWALONGWEN SUBCOUNTY)",
                                     "ABANYA HC II"="ABANYA HC II",
                                     "ABARILELA HC III"="ABARILELA HC III",
                                     "ABEDOBER HC III"="ABEDOBER HC III",
                                     "ABEI"="ABEI HC II",
                                     "ABELLA HC III"="ABELA HC II",
                                     "ABER HC II"="ABER HC II",
                                     "ABER HOSPITAL"="ABER HOSPITAL",
                                     "ABI CLINIC"="ABII CLINIC HC IV",
                                     "ABIA HC II"="ABIA HC II (ABIA SUBCOUNTY)",
                                     "ABIM GENERAL HOSPITAL"="ABIM HOSPITAL",
                                     "ABIM HOSPITAL"="ABIM HOSPITAL",
                                     "ABOKE HC IV"="ABOKE HC IV",
                                     "ABOKE MISSION HC III"="ABOKE MISSION HC III",
                                     "ABONGOMOLA HC III"="ABONGOMOLA HC III",
                                     "ABWOCH HCII"="ABWOCH HC II",
                                     "ACET HCIII"="ACET HC II",
                                     "ACHERER"="ACHERER HC II",
                                     "ACHOL PII HC III"="ACHOLPII HC III",
                                     "ACHOL PII MILLITARY HC IV"="ACHOLPII HC III",
                                     "ACHOLI BUR HC III"="ACHOLI-BUR HC III",
                                     "ACII HC II"="ACII HC II",
                                     "ACIMI HC III"="ACIMI HC II",
                                     "ACOKARA HC II"="ACOKARA HC II",
                                     "ACOWA  HC  III"="ACOWA HC III",
                                     "ACUMET  HC   III"="ST. FRANCIS ACUMET HC III",
                                     "ACURU HCII"="ACURU HC II",
                                     "ACUT HC II"="ACUT HC II",
                                     "ADAGMON HC III"="ADAGMON HC II",
                                     "ADEA HC II"="ADEA HC II",
                                     "ADIGO HC II"="ADIGO HC II",
                                     "ADILANG HC III"="ADILANG HC III",
                                     "ADJUMANI HOSPITAL"="ADJUMANI HOSPITAL",
                                     "ADJUMANI MISSION HC III"="ADJUMANI MISSION HC III",
                                     "ADOK HC III"="ADOK HC II",
                                     "ADUKU HC IV"="ADUKU HC IV",
                                     "ADUKU MISSION HC II"="ADUKU MISSION HC II",
                                     "ADUMI HC IV"="ADUMI HC IV",
                                     "ADVENTIST MEDICAL CENTRE"="ADVENTIST MEDICAL CENTRE HC III",
                                     "ADWIR HC II"="ADWIR HC II",
                                     "ADYEGI HC II"="ADYEGI HC II",
                                     "ADYEL HCIII"="ADYEL HC III",
                                     "AGALI HC III"="AGALI HC III",
                                     "AGARIA HC II"="AGARIA HC II",
                                     "AGIERMACH HC III"="AGIERMACH HC III",
                                     "AGIRIGIROI HC II"="AGIRIGIROI HC II",
                                     "AGOJO HCII"="AGOJO HC II",
                                     "AGORO HC III"="AGORO HC III",
                                     "AGU HC III"="AGU HC III",
                                     "AGULE HC III"="AGULE HC III",
                                     "AGULURUDE HC III"="AGULURUDE HC III",
                                     "AGURUT HC II"="AGURUT HC II",
                                     "AGWATA HC III"="AGWATTA HC III",
                                     "AIDS INFORMATION CENTER (LIRA) HC/III"="AIDS INFORMATION CENTRE (LIRA) SPECIAL CLINIC",
                                     "AIDS INFORMATION CENTER(JINJA)"="AIDS INFORMATION CENTRE (JINJA) SPECIAL CLINIC",
                                     "AIDS INFORMATION CENTER(MBALE)"="AIDS INFORMATION CENTRE (MBALE) SPECIAL CLINIC",
                                     "AIDS INFORMATION CENTRE (ARUA)"="AIDS INFORMATION CENTRE (ARUA) SPECIAL CLINIC",
                                     "AIDS INFORMATION CENTRE (KAMPALA)"="AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                     "AIDS INFORMATION CENTRE (MBARARA)"="AIDS INFORMATION CENTRE (MBARARA) SPECIAL CLINIC",
                                     "AIDS INFORMATION CENTRE (SOROTI)"="AIDS INFORMATION CENTRE (SOROTI) HC II",
                                     "AJAGA HC III"="AJAGA HC III",
                                     "AJELUK HC III"="AJELUK HC III",
                                     "AJIA HC III"="AJIA HC III",
                                     "AKALI"="AKALI HC II",
                                     "AKALO HC III"="AKALO HC III",
                                     "AKETA HC III"="AKETA HC III",
                                     "AKIDE HC II"="AKIDE HC II",
                                     "AKILOK HC II"="AKILOK HC II",
                                     "AKOBOI HC II (KATAKWI)"="AKOBOI HC II",
                                     "AKOBOI HC II (SERERE)"="AKOBOI (SERERE) HC II",
                                     "AKOKORO HC III"="AKOKORO HC III",
                                     "AKUNA LABER HC III"="AKUNA LABER HC III",
                                     "AKUORO HCIII"="AKUORO HC III",
                                     "AKURA II"="AKURA HC II",
                                     "AKWANGI HC II"="AKWANGI HC II",
                                     "AKWORO HC II"="AKWORO HC II",
                                     "AKWORO HC III"="AKWORO HC III",
                                     "ALAKAS HC II"="ALAKAS HC II",
                                     "ALANGI HC III"="ALANGI HC III",
                                     "ALANYI HC III"="ALANYI HC III",
                                     "ALAO HC II"="ALAO HC II",
                                     "ALAPATA HC II (DOKOLO)"="ALAPATA HC II",
                                     "ALEBTONG HC IV"="ALEBTONG HC IV",
                                     "ALEMERE HC II"="ALEMERE MEDICAL AID HC II",
                                     "ALENGA HC III (MISSION)"="ALENGA HC III",
                                     "ALERE HC II"="ALERE HC II",
                                     "ALERE HCII"="ALERE HC II",
                                     "ALEREK HC III"="ALEREK HC III",
                                     "ALERO HC III"="ALERO HC III",
                                     "ALIBA HC II"="ALIBA HC III",
                                     "ALIBA HC III"="ALIBA HC III",
                                     "ALIK HCIII"="ALIK HC II",
                                     "ALIM HC II"="ALIM HC II",
                                     "ALIMUGONZA HC II"="ALIMUGONZA HC II",
                                     "ALIRA HC II"="ALIRA HC II",
                                     "ALITO HC III"="ALITO HC III",
                                     "ALIVE MEDICAL SERVICES HC III"="ALIVE MEDICAL SERVICES SPECIAL CLINIC",
                                     "ALIWANG HC III"="ALIWANG HC III",
                                     "ALL SAINTS BUCHANAGANDI HC III"="ALL SAINTS BUCHANAGANDI HC III",
                                     "ALL SAINTS HC II"="ALL SAINTS PURANGA HC II",
                                     "ALLUSTIN  HC III"="ALUSTIN HC III",
                                     "ALOI MISSION HC III"="ALOI MISSION HC III",
                                     "ALOKOLUM  HCIII"="ALOKOLUM HC II",
                                     "ALONI HC II"="ALONI HC II",
                                     "ALOP HC II"="ALOP HC II",
                                     "ALWI HC III"="ALWI HC III",
                                     "ALWOROCENG"="ALWOROCENG HC II",
                                     "ALYECMEDA HC II"="ALYECMEDA HC II",
                                     "AMACH HC IV"="AMACH HC IV",
                                     "AMAI COMMUNITY HOSPITAL"="AMAI COMMUNITY HOSPITAL",
                                     "AMEDEK HC II"="AMEDEK HC II",
                                     "AMITA PRISONS HC II"="AMITA PRISON HC II",
                                     "AMOLATAR HC IV"="AMOLATAR HC IV",
                                     "AMUCA SDA HC III"="AMUCA SDA HC III",
                                     "AMUCU HC III"="AMUCU HC III",
                                     "AMUDA HC II"="AMUDA HC II",
                                     "AMUDAT HOSPITAL"="AMUDAT HOSPITAL",
                                     "AMUGU HC III"="AMUGU HC III",
                                     "AMURIA HC IV"="AMURIA HOSPITAL",
                                     "AMUSUS HC II"="AMUSUS HC III",
                                     "AMWA HC II"="AMWA HC II",
                                     "AMWOMA HC III"="AMWOMA HC II",
                                     "AMYEL HCII"="AMYEL HC II",
                                     "ANAKA HC II"="ANAKA HC II",
                                     "ANAKA HOSPITAL"="ANAKA HOSPITAL",
                                     "ANAMWANY HC II"="ANAMWANY HC II",
                                     "ANARA HCIII"="ANARA HC II",
                                     "ANGAGURA HC III"="ANGAGURA HC III",
                                     "ANGAL HOSPITAL (ST LUKE)"="ANGAL HOSPITAL",
                                     "ANGETTA HC III"="ANGETTA HC III",
                                     "ANKOLE TEA HC II"="ANKOLE TEA FACTORY HC II",
                                     "ANYACOTO HC II"="ANYACOTO HC II",
                                     "ANYANGATIR HC III"="ANYANGATIR HC III",
                                     "ANYARA HC III"="ANYARA HC III",
                                     "ANYEKE HC IV"="ANYEKE HC IV",
                                     "ANYIRIBU HC III"="ANYIRIBU HC III",
                                     "AOET HC III"="AOET HC II",
                                     "APAC GENERAL HOSPITAL"="APAC HOSPITAL",
                                     "APALA BAR OWOO HC III"="APALABAROWO HC III",
                                     "APALA HC III"="APALA HC III",
                                     "APALOPAMA HC II"="APA LOPAMA HC II",
                                     "APALOPUS HC II"="APA LOPUS HC II",
                                     "APAPAI HC II"="APAPAI HC III",
                                     "APAPAI HC IV (SERERE)"="APAPAI HC IV",
                                     "APARANGA HC II"="APARANGA HC II",
                                     "APEITOLIM HC II"="APEITOLIM HC II",
                                     "APERKIRA HCIII"="APERIKIRA HC III",
                                     "APIRE HC III"="APIRE HC III",
                                     "APO HC III"="APO HC III",
                                     "APODORWA HC II"="APODORWA HC II",
                                     "APOI HC III"="APOI HC III",
                                     "APOPONG  HC  III"="APOPONG HC III",
                                     "APUCE HC II"="APUCE HC II",
                                     "APUTI HC III"="APUTI HC III",
                                     "APWORI HC III"="APWORI HC III",
                                     "APYETA HC II"="APYETA HC II",
                                     "ARABAKA HC II"="ARABAKA HC II",
                                     "ARAHMAH MEDICAL CENTRE IV"="ARAHMAH MEDICAL CENTRE HC IV",
                                     "ARAPAI HC II"="ARAPAI HC II",
                                     "AREMBWOLA HC II"="AREMBWOLA HC II",
                                     "ARINYAPI HCIII"="ARINYAPI HC III",
                                     "ARIPEA HC III"="ARIPEA HC III",
                                     "ARIWA HC III"="ARIWA HC III",
                                     "AROI HC III"="AROI HC III",
                                     "AROMO HC III"="AROMO HC III",
                                     "ARUA POLICE HC III"="ARUA POLICE HC III",
                                     "ARUA REGIONAL REFERRAL HOSPITAL"="ARUA REGIONAL REFERRAL HOSPITAL",
                                     "ARWOTCEK HC II"="ARWOTCEK HC II",
                                     "ASAMUK HC III"="ASAMUK HC III",
                                     "ASTU HC V"="ASTU HC IV",
                                     "ASURET HC III"="ASURET HC III",
                                     "ATABU HC II"="ATABU HC II",
                                     "ATANGA HC III"="ATANGA HC III",
                                     "ATANGI HC III"="ATANGI HC III",
                                     "ATANGWATA HC III"="ATANGWATA HC III",
                                     "ATAR"="ATAR HC II",
                                     "ATARI HC II"="ATARI HC II",
                                     "ATIAK HC IV"="ATIAK HC IV",
                                     "ATIIRA HC III"="ATIIRA HC III",
                                     "ATIPE HC II"="ATIPE HC III",
                                     "ATOOT HC II"="ATOOT HC II",
                                     "ATUNGA HC II"="ATUNGA HC II",
                                     "ATURA HC II"="ATURA HC III",
                                     "ATUTUR HOSPITAL"="ATUTUR HOSPITAL",
                                     "AUKOT HC II"="AUKOT HC II",
                                     "AVOGERA HC III"="AVOGERA HC III",
                                     "AWACH HC II"="AWACH HC II",
                                     "AWALIWAL HC II"="AWALIWAL HC II",
                                     "AWEI HC III"="AWEI HC III",
                                     "AWELO HC III"="AWELO HC II",
                                     "AWER HC II"="AWER HC II",
                                     "AWERE HC III"="AWERE HC III",
                                     "AWICH HCII"="AWICH HC II",
                                     "AWIRI HC II"="AWIRI HC II",
                                     "AWONANGIRO HC II"="AWONANGIRO HC II",
                                     "AWOO HC II"="AWOO HC II",
                                     "AYA"="AYA HC III",
                                     "AYAGO HC II (APAC)"="AYAGO HC II",
                                     "AYAGO HC III"="AYAGO HC III",
                                     "AYARA HCII"="AYARA HC II",
                                     "AYER HC III"="AYER HC III",
                                     "AYILO HC III"="AYILO I HC III",
                                     "AYIPE HC III"="AYIPE HC III",
                                     "AYIRA HEALTH SERVICES"="AYIRA HEALTH SERVICES",
                                     "AYIRI HC III"="AYIRI HC III",
                                     "AYIVU HC III"="AYIVU HEALTH CENTRE HC III",
                                     "AYIVUNI HC"="AYIVUNI HC III",
                                     "AZUR CHRISTIAN HC IV"="AZUR HC IV",
                                     "BACAYAYA"="BACAYAYA HC II",
                                     "BAITAMBOGWE HC 3"="BAITAMBOGWE HC III",
                                     "BALA HC III"="BALA HC III",
                                     "BALAWOLI HC III"="BALAWOLI HC III",
                                     "BAMUGOLODDE HC III"="BAMUGOLODDE HC III",
                                     "BAMUNANIKA HC III"="BAMUNANIKA HC III",
                                     "BAMURE HC II"="BAMURE HC II",
                                     "BANANYWA HC"="BANANYWA HC II",
                                     "BANDA HC II (WAKISO)"="WAKISO BANDA HC II",
                                     "BANDA HC III (NAMAYINGO)"="BANDA HC III",
                                     "BANGATUTI HC III"="BANGATUTI HEALTH CENTRE HC III",
                                     "BARAKALA HC IV"="BARAKALA HC III",
                                     "BARAPWO HC III"="BARAPWO HC III",
                                     "BARDYANG HC II"="BARDYANG HC II",
                                     "BARJOBI HCIII"="BARJOBI HC III",
                                     "BARR HC III"="BARR HC III",
                                     "BATA HC III"="BATA HC III",
                                     "BBAALE HC IV"="BBAALE HC IV",
                                     "BBIRA HC III"="BBIRA NGO HC II",
                                     "BBIRA HCII"="BBIRA HC II",
                                     "BEATRICE TIERNEY HC II"="BEATRICE TIERNEY HC II",
                                     "BEATRICE TIERNEY HC III"="BEATRICE TIERNEY HC II",
                                     "BELAMELING"="BELAMELING HC II",
                                     "BELLE"="BELLE HC III",
                                     "BENEDICT MEDICAL CENTRE"="BENEDICT HC IV",
                                     "BENET HC II"="BENET HC III",
                                     "BESIA HC III"="BESIA HC III",
                                     "BIBIA HC III"="BIBIA HC III",
                                     "BIDDABUGYA HC III"="BIDABUJA HC III",
                                     "BIDIBIDI HC III"="BIDIBIDI HC III",
                                     "BIGASA HC III"="BIGASA HC III",
                                     "BIGODI HC III"="BIGODI HC III",
                                     "BIGULI  HC III"="BIGULI HC III",
                                     "BIHANGA ARMY HC II"="BIHANGA HC II",
                                     "BIHANGA HC II (KAMWENGE)"="BIHANGA UPDF BARRACKS HC II",
                                     "BIHARWE HC III"="BIHARWE (NYABUHAMA) HC III",
                                     "BIISO HC IV"="BIISO HC IV",
                                     "BIKO HC II"="BIKO HC II",
                                     "BIKURUNGU HC III"="BIKURUNGU HC III",
                                     "BILEAFE HC III"="BILEAFE HC III",
                                     "BINYA HC II"="BINYA HC II",
                                     "BINYINY HC III"="BINYINY HC III",
                                     "BIRA HC III"="BIRA HC III",
                                     "BIREMBO"="BIREMBO HC III",
                                     "BISHESHE HC III"="BISHESHE HC III",
                                     "BISHOP CEASER ASILI HOSPITAL"="BISHOP ASCILI HOSPITAL",
                                     "BISINA HC II"="BISINA HC II",
                                     "BISON HC III"="BISON HC III",
                                     "BISOZI HCIV"="BISOZI HC IV",
                                     "BITOOMA HC III"="BITOOMA HC III",
                                     "BIWIHI HC II"="BIWIHI HC II",
                                     "BOBI HC III"="BOBI HC III",
                                     "BOLO HC II"="BOLO HC II",
                                     "BOLOMONI  HC III"="BOLOMONI HEALTH CENTRE HC III",
                                     "BOMBO HC III"="BOMBO HC III",
                                     "BOMBO MILITARY HOSPITAL"="BOMBO GENERAL MILITARY HOSPITAL",
                                     "BONDO HC III"="BONDO HC III",
                                     "BONDO MILITARY"="BONDO MILITARY HC III",
                                     "BOROBORO HC III"="BOROBORO HC III",
                                     "BOWA HC III"="BOWA HC III",
                                     "BREGMA MEDICAL CENTRE"="BREGMA HC II",
                                     "BRIM HC II"="BRIM HC II",
                                     "BUBAGO"="BUBAGO HC II",
                                     "BUBALYA HC III"="BUBALYA HC III",
                                     "BUBANGO HC III"="BUBANGO HC II",
                                     "BUBEKE HC III"="BUBEKE HC III",
                                     "BUBIRO HC II"="BUBIRO HC II",
                                     "BUBOOLO HC II"="BUBOOLO HC II",
                                     "BUBUKWANGA HC III"="BUBUKWANGA HC III",
                                     "BUBULO HC IV"="BUBULO HC IV",
                                     "BUBUNGI HC III"="BUBUNGI HC III",
                                     "BUBUTU HC III"="BUBUTU HC III",
                                     "BUCHANAGANDI HC III"="ALL SAINTS BUCHANAGANDI HC III",
                                     "BUDADIRI HC IV"="BUDADIRI HC IV",
                                     "BUDAKA HC IV"="BUDAKA HC IV",
                                     "BUDIMA HC III"="BUDIMA HC III",
                                     "BUDONDO HC IV"="BUDONDO HC IV",
                                     "BUDONGO HC II"="BUDONGO HC II",
                                     "BUDUDA HOSPITAL"="BUDUDA HOSPITAL",
                                     "BUDUMBA HC III"="BUDUMBA HC III",
                                     "BUDWALE HC III"="BUDWALE HC III",
                                     "BUFUMA HC III"="BUFUMA HC III",
                                     "BUFUMBO HC IV"="BUFUMBO HC IV",
                                     "BUFUMIRA HC III"="BUFUMIRA HC III",
                                     "BUFUNDA HC III"="BUFUNDA HC III",
                                     "BUFUNJO HC III"="BUFUNJO HC III",
                                     "BUGAANA HC II"="BUGANA HC II",
                                     "BUGALO HC III"="BUGALO HC III",
                                     "BUGAMBA HC IV"="BUGAMBA HC IV",
                                     "BUGAMBE HC II (HOIMA)"="BUGAMBE HC III",
                                     "BUGAMBE TEA HC III (HOIMA)"="BUGAMBE TEA HC III",
                                     "BUGAMBO HC II"="BUGAMBO HC II",
                                     "BUGANA HC III"="BUGANA HC III",
                                     "BUGANGARI HC IV"="BUGANGARI HC IV",
                                     "BUGAYA"="BUYENDE BUGAYA HC III",
                                     "BUGAYA HC III (BUVUMA)"="BUGAYA HC III",
                                     "BUGEMA UNIVERSITY HC III"="BUGEMA UNIVERSITY HC III",
                                     "BUGEMBE HC  IV"="BUGEMBE HC IV",
                                     "BUGEYWA HC II"="BUGEYWA HC II",
                                     "BUGINYANYA HC III"="BUGINYANYA HC III",
                                     "BUGIRI HC II (KANUNGU)"="BUGIRI HC II (KANUNGU)",
                                     "BUGIRI HOSPITAL"="BUGIRI HOSPITAL",
                                     "BUGIRI TOWN COUNCIL HC II"="BUGIRI HC II",
                                     "BUGITIMWA HC III"="BUGITIMWA GOVT HC III",
                                     "BUGOBERO HC IV"="BUGOBERO HC IV",
                                     "BUGOGO HC III"="BUGOGO HC II",
                                     "BUGOIGO HC II"="BUGOIGO HC II",
                                     "BUGONDO HC III"="BUGONDO HC III",
                                     "BUGONGI HC III"="BUGONGI HC III",
                                     "BUGOTO HC II"="BUGOTO HC II",
                                     "BUGOYE HC III"="BUGOYE HC III",
                                     "BUGULU HCII"="BUGULU HC II",
                                     "BUGULUMBYA HC III"="BUGULUMBYA HC III",
                                     "BUGUNGU HC III"="BUGUNGU HC II",
                                     "BUHAGHURA HC III"="BUHAGHURA HC III",
                                     "BUHANDA HC II"="BUHANDA HC II",
                                     "BUHANIKA HC III"="BUHANIKA HC III",
                                     "BUHEHE HC III"="BUHEHE HC III",
                                     "BUHIMBA HC III"="BUHIMBA HC III",
                                     "BUHUGU HC III"="BUHUGU HC III",
                                     "BUHUKA"="BUHUUKA HC III",
                                     "BUHUNGA HC IV"="BUHUNGA HC IV",
                                     "BUIKWE HC III"="BUIKWE HC III",
                                     "BUIKWE HOSPITAL (ST CHARLSE LWANGA)"="BUIKWE ST. CHARLES LWANGA HOSPITAL",
                                     "BUJALYA HC III"="BUJALYA HC III",
                                     "BUJUBULI HC III"="BUJUBULI HC III",
                                     "BUJUGU HC III"="BUJUGU HC III",
                                     "BUJUMBURA HC III"="BUJUMBURA HC III",
                                     "BUJUUKO HC III"="BUJUUKO HC III",
                                     "BUJWANGA HC II"="BUJWANGA HC II",
                                     "BUKAKATA HC III"="BUKAKATA HC III",
                                     "BUKALAGI HC III"="BUKALAGI HC III",
                                     "BUKALASA HC III"="BUKALASA HC III",
                                     "BUKALASI HC III"="BUKALASI HC III",
                                     "BUKAMBA HC II"="BUKAMBA HC II",
                                     "BUKANGAMA HC III"="BUKANGAMA HC III",
                                     "BUKASA HC II (MPIGI)"="BUKASA HC II",
                                     "BUKASA HC IV"="BUKASA HC IV",
                                     "BUKASAKYA HC III"="BUKASAKYA HC III",
                                     "BUKATUBE HC III"="BUKATUBE HC III",
                                     "BUKAYA HC II"="BUKAYA HC II",
                                     "BUKEDEA HC IV"="BUKEDEA HC IV",
                                     "BUKEDEA PRISON"="BUKEDEA PRISON HC II",
                                     "BUKEERI HC III"="BUKEERI HC III",
                                     "BUKEWA HC III"="BUKEWA HC III",
                                     "BUKHABUSI HC III"="BUKHABUSI HC III",
                                     "BUKHALU HC III"="BUKHALU HC III",
                                     "BUKIBOKOLO HC III"="BUKIBOKOLO HC III",
                                     "BUKIGAI HC III"="BUKIGAI HC III",
                                     "BUKIMBI HC II"="BUKIMBI HC II",
                                     "BUKIMBIRI HC III"="BUKIMBIRI HC III",
                                     "BUKOMERO HC IV"="BUKOMERO HC IV",
                                     "BUKOOVA HC III"="BUKOOVA HC III",
                                     "BUKOTO HC II"="BUKOTO HEALTH CENTER HC II",
                                     "BUKOTO HC III"="BUKOTO HC III",
                                     "BUKULULA HC IV"="BUKULULA HC IV",
                                     "BUKURUNGU HCII"="BUKURUNGU HC II",
                                     "BUKUUKU HC IV"="BUKUKU HC IV",
                                     "BUKUYA HC III"="BUKUYA HC IV",
                                     "BUKWIRI COU"="BUKWIRI COU HC II",
                                     "BUKWO GENERAL HOSPITAL"="BUKWO GENERAL HOSPITAL",
                                     "BUKWO HC IV"="BUKWO HC IV",
                                     "BUKYIMANAYI HC III"="BUKIMANAYI HC III",
                                     "BULAGA"="BULAGA HC II",
                                     "BULAGO HC II"="BULAAGO HC II",
                                     "BULANGE HC III"="BULANGE HC III",
                                     "BULANGIRA HC III"="BULANGIRA HC III",
                                     "BULEGENI HC II"="BULEGENI HC III",
                                     "BULERA HC III"="BULERA HC III",
                                     "BULESA HC III"="BULESA HC III",
                                     "BULIDHA HC III"="BULIDHA HC III",
                                     "BULIISA  GENERAL HOSPITAL"="BULIISA HOSPITAL",
                                     "BULIISA HC IV"="BULIISA HC IV",
                                     "BULO HC III"="BULO HC III",
                                     "BULONDO HC III"="BULONDO HC III",
                                     "BULOPA HC III"="BULOPA HC III",
                                     "BULUBA HOSPITAL ST FRANCIS"="BULUBA HOSPITAL",
                                     "BULUCHEKE HC III"="BULUCHEKE HC III",
                                     "BULUGANYA HC III"="BULUGANYA HC III",
                                     "BULUGUYI HC III"="BULUGUYI HC III",
                                     "BULUJEWA HC III"="BULUJEWA HC III",
                                     "BULULU HC III"="BULULU HC III",
                                     "BULUMBI HC III"="BULUMBI HC III",
                                     "BULUYA HC II"="BULUYA HC II",
                                     "BULWADDA HC II"="BULWADDA HC II",
                                     "BULWALA HC III"="BULWALA HC III",
                                     "BULYAMBWA HC II"="BULYAMBWA HC II",
                                     "BUMADANDA HC III"="BUMADANDA HC III",
                                     "BUMANGI HC II"="ST. ELIZABETH BUMANGI HC III",
                                     "BUMANYA HC IV"="BUMANYA HC IV",
                                     "BUMASIKYE"="BUMASIKYE HC III",
                                     "BUMASOBO HC III"="BUMASOBO HC III",
                                     "BUMBO HC III"="BUMBO HC III",
                                     "BUMOOLI HC III"="BUMOOLI HC III",
                                     "BUMUGIBOLE HC II"="BUMUGIBOLE HC III",
                                     "BUMUGUSHA HC III"="BUMUGUSHA HC III",
                                     "BUMULISHA HC III"="BUMULISHA HC III",
                                     "BUMUMULO HC III"="BUMUMULO HC III",
                                     "BUMUNJI HCIII"="BUMUNJI HC II",
                                     "BUMUSI HC III"="BUMUSI HC II",
                                     "BUMWAMBU HC III"="BUMWAMBU HC III",
                                     "BUMWONI HC III"="BUMWONI HC III",
                                     "BUNAGAMI HC III"="BUNAGAMI HC III",
                                     "BUNAMBUTYE  RESETTLEMENT HC III"="BUNAMBUTYE RESETTLEMENT HC III",
                                     "BUNAMONO HC III"="BUNAMONO HC II",
                                     "BUNAMWAYA HC II"="BUNAMWAYA HC II",
                                     "BUNANGAKHA HC II"="BUNANGAKA HC III",
                                     "BUNAPONGO HC III"="BUNAPONGO HC III",
                                     "BUNASEKE HC III"="BUNASEKE HC III",
                                     "BUNDEGE"="BUNDEGE HC II",
                                     "BUNDIBUGYO HOSPITAL"="BUNDIBUGYO HOSPITAL",
                                     "BUNDIGOMA HC III"="BUNDINGOMA HC II",
                                     "BUNDIMULANGYA HC II"="BUNDIMULANGYA HC II",
                                     "BUNG HC II"="BUNG HC II",
                                     "BUNGOKHO MUTOTO HC III"="BUNGOKHO-MUTOTO HC III",
                                     "BUNJAKO HC III"="BUNJAKO HC III",
                                     "BUNOGA HC III"="BUNOGA HC III",
                                     "BUPADHENGO HC III"="BUPADHENGO HC III",
                                     "BUPOMPOLI HC II"="BUPOMBOLI HC III",
                                     "BUPOTO HC III"="BUPOTO HC III",
                                     "BURARU HC III"="BURARU HC III",
                                     "BUREMBA"="BUREMBA HC III",
                                     "BURONDO HC II"="BURONDO HC III",
                                     "BURORA HC II"="BURORA HC II",
                                     "BURUNGA HC III"="BURUNGA HC III",
                                     "BUSAALE HC II"="BUSAALE HC II",
                                     "BUSAANA HC III"="PARENTS BUSAANA HC II",
                                     "BUSABA HC III"="BUSABA HC III",
                                     "BUSABAGA HC III"="BUSABAGA HC III",
                                     "BUSABI HC III"="BUSABI HC III",
                                     "BUSALA HC III"="BUSAALA HC III",
                                     "BUSAMAGA HC III"="BUSAMAGA HC III",
                                     "BUSAMUZI"="BUSAMUZI HC III",
                                     "BUSANO HC III"="BUSANO HC III",
                                     "BUSANZA HC IV"="BUSANZA HC IV",
                                     "BUSARU HC IV"="BUSARU HC IV",
                                     "BUSAWA MANZE HC III"="BUSAWAMANZE HC III",
                                     "BUSEDDE HC III"="BUSEDDE HC III",
                                     "BUSEREGENYU HCIII"="BUSEREGENYU HC III",
                                     "BUSERUKA HC III"="BUSERUKA HC III",
                                     "BUSESA  MEDICAL CENTRE"="BUSEESA MEDICAL CENTRE HC III",
                                     "BUSETA HC III"="BUSETA HC III",
                                     "BUSHEEKA"="BUSHEKA (SEMBABULE) HC II",
                                     "BUSHENYI HC IV"="BUSHENYI HC IV",
                                     "BUSHENYI MEDICAL CENTRE HC III"="BUSHENYI MEDICAL CENTER HC III",
                                     "BUSHENYI UGANDA PRISONS HC II"="BUSHENYI UGANDA PRISONS HC III",
                                     "BUSHIKA HC III"="BUSHIKA HC III",
                                     "BUSHIKORI HC III"="BUSHIKORI HC III",
                                     "BUSHIYI HC III"="BUSHIYI HC III",
                                     "BUSIA HC IV"="BUSIA HC IV",
                                     "BUSIIME HCII"="BUSIME HC II",
                                     "BUSIRA HCII"="BUSIRA HC II",
                                     "BUSIRIBA HC II"="BUSIRIBA HC II",
                                     "BUSIRO HC III"="BUSIIRO HC II",
                                     "BUSITEMA HC III"="BUSITEMA HC III",
                                     "BUSIU HC IV"="BUSIU HC IV",
                                     "BUSOGA HC II"="BUSOGA HC II",
                                     "BUSOLWE HOSPITAL"="BUSOLWE HOSPITAL",
                                     "BUSORU HC II"="BUSORU HC II",
                                     "BUSOTA"="BUSOTA HC III",
                                     "BUSSI HC III"="BUSSI HC III",
                                     "BUSUNGA HC II"="BUSUNGA HC II",
                                     "BUSUNJU HC II"="BUSUNJU HC II",
                                     "BUSUYI HC II"="BUSUYI HC II",
                                     "BUTAAKA HC III"="BUTAAKA HC III",
                                     "BUTABIKA  HOSPITAL"="BUTABIKA NATIONAL REFERRAL HOSPITAL",
                                     "BUTAGAYA HC III"="BUTAGAYA HC III",
                                     "BUTALANGU HC III"="BUTALANGU HC III",
                                     "BUTALEJA HC III"="BUTALEJA HC III",
                                     "BUTAMA HC III"="BUTAMA HC III",
                                     "BUTANDIGA HC III"="BUTANDIGA HC III",
                                     "BUTANSI HC III"="BUTANSI HC III",
                                     "BUTARE HC III (NTUNGAMO )"="BUTARE HC III",
                                     "BUTAWAATA HC III"="BUTAWATA HC III",
                                     "BUTEBA HC III"="BUTEBA HC III",
                                     "BUTEBO HC IV"="BUTEBO HC IV",
                                     "BUTEMA HC III"="BUTEMA HC III",
                                     "BUTEMBA HC III"="BUTEMBA HC III",
                                     "BUTENDE HC III"="BUTENDE HC III",
                                     "BUTENGA HC IV"="BUTENGA HC IV",
                                     "BUTEZA HC III"="BUTEZA HC III",
                                     "BUTIABA HC III"="BUTIABA HC III",
                                     "BUTIITI HC III"="BUTIITI HC III",
                                     "BUTIRU CHRISCO HOSPITAL"="BUTIRU CHRISCO (UCMB) HC III",
                                     "BUTIRU HC III"="BUTIRU HC III",
                                     "BUTOGOTA HC II"="BUTOGOTA HC II",
                                     "BUTOHA"="BUTOHA HC III",
                                     "BUTOLOOGO HC II (C/O MADUDU HC III)"="BUTOLOOGO HC III",
                                     "BUTOORO HC III"="BUTOOLO HC III",
                                     "BUTUNDUZI HC III"="BUTUNDUZI HC III",
                                     "BUTUNTUMULA HC III"="BUTUNTUMULA HC III",
                                     "BUVUMA HC IV"="BUVUMA HC IV",
                                     "BUWAATA HC II"="BUZAAYA HC II",
                                     "BUWABWALA HC III"="BUWABWALA HC III",
                                     "BUWAGAJJO HC III"="BUWAGAJJO HC III",
                                     "BUWAISWA HC III"="BUWAISWA HC III",
                                     "BUWALASI HC III"="BUWALASI HC III",
                                     "BUWAMA HC III"="BUWAMA HC III",
                                     "BUWANGWA HC III"="BUWANGWA HC III",
                                     "BUWASA HC IV"="BUWASA HC IV",
                                     "BUWEMBE"="BUWEMBE HC II",
                                     "BUWEMBE HC III"="BUWEMBE HC II",
                                     "BUWENGE GENERAL HOSPITAL"="BUWENGE GENERAL HOSPITAL",
                                     "BUWENGE HC IV"="BUWENGE HC IV",
                                     "BUWENGE HOSPITAL"="BUWENGE GENERAL HOSPITAL",
                                     "BUWOOYA"="BUWOOYA HC II",
                                     "BUWUMBA HC III"="BUDUMBA HC III",
                                     "BUWUNGA HC III (BUGIRI)"="BUGIRI BUWUNGA HC III",
                                     "BUWUNGA HC III (MASAKA)"="BUWUNGA HC III",
                                     "BUWUNI HC II"="BUWUNI HC II",
                                     "BUYAGA HC III"="BUYAGA HC III",
                                     "BUYAMBA HC III-RAKAI"="BUYAMBA HC III",
                                     "BUYAMBI HC II- (ST NOA) MITYANA"="ST. NOAH BUYAMBI HC III",
                                     "BUYANJA HC III (RUKUNGIRI)"="BUYANJA HC III",
                                     "BUYANJA HCII"="BUYANGA HC II",
                                     "BUYIGA HC III"="BUYIGA HC III",
                                     "BUYINJA HC IV"="BUYINJA HC IV",
                                     "BUYOBO HC III"="BUYOBO HC II",
                                     "BUYOGA HC III"="BUYOGA HC III",
                                     "BUYUGU HC III"="BUYUGU HC II",
                                     "BUZIBWERA HC IV"="BWIZIBWERA HC IV",
                                     "BUZIIKA HC II"="BUZIIKA HC II",
                                     "BUZIIRASAGAMA HC II"="BUZIRASAGAMA HC II",
                                     "BWAMBARA HC III"="BWAMBARA HC III",
                                     "BWEEMA"="BWEEMA HC II",
                                     "BWENDERO HC III"="BWENDERO HC III",
                                     "BWERA HOSPITAL"="BWERA HOSPITAL",
                                     "BWEYALE KITARA MEDICAL CENTRE"="BWEYALE KITARA MEDICAL CENTRE",
                                     "BWEYONGEDDE HC II"="BWEYONGEDDE HC II",
                                     "BWIJANGA HC IV"="BWIJANGA HC IV",
                                     "BWIKARA HC III"="BWIKARA HC III",
                                     "BWIKHONGE HC II"="BWIKHONGE HC II",
                                     "BWINDI COMMUNITY HOSPITAL"="BWINDI COMMUNITY HOSPITAL",
                                     "BWIZI HC III"="BWIZI HC III",
                                     "BWONDHA HC II"="BWONDHA HC II",
                                     "BWONGYERA HC III"="BWONGYERA HC III",
                                     "BYAKABANDA HC III"="BYAKABANDA HC III",
                                     "BYERIMA"="BYERIMA HC II",
                                     "CARDINAL NSUBUGA HC III"="CARDINAL NSUBUGA MEMORIAL HC III",
                                     "CASE HOSPITAL"="CASE MEDICAL CENTRE HOSPITAL",
                                     "CHAHAFI HC IV"="CHAHAFI HC IV",
                                     "CHAKULIA HC III"="CHAKULIA HC III",
                                     "CHARIS HC III"="CHARIS HC III",
                                     "CHAWENTE HC III"="CHAWENTE HC III",
                                     "CHEBONET HC III"="CHEBONET HC III",
                                     "CHEGERE HC II"="CHEGERE HC II",
                                     "CHEMOSONG HC II"="CHEMOSONG HC II",
                                     "CHEMWON HC III"="CHEMWOM HC III",
                                     "CHEPKWASTA HC II"="CHEPKWASTA HC II",
                                     "CHEPTAPOYO HC II"="CHEPTAPOYO HC II",
                                     "CHEPTUYA HC II"="CHEPTUYA HC III",
                                     "CHEPTUYA HC III"="CHEPTUYA HC III",
                                     "CHERISH HC III"="CHERISH MEDICAL CENTRE HC III",
                                     "CHESOWER HC III"="CHESOWER HC III",
                                     "CHILD AND FAMILY MEDICAL"="CHILD AND FAMILY MEDICAL CLINIC HC II",
                                     "CHINA UGANDA FRIENDSHIP HOSPITAL- NAGURU"="CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                                     "CHOICE MEDICAL CENTRE"="CHOICE MEDICAL CLINIC HC II",
                                     "CIFORO HC III"="CIFORO HC III",
                                     "CILIO HC III"="CILIO HC III",
                                     "COMBONI HOSPITAL"="COMBONI HOSPITAL",
                                     "COOROM HC II(KOCH)"="KOCH HC II",
                                     "DABANI HOSIPITAL"="DABANI HOSPITAL",
                                     "DAKABELA HC III"="DAKABELA HC III",
                                     "DAVID FRAGELI MEDICAL CENTER HCIII"="DAVID FAGERLEE'S MEDICAL CENTRE",
                                     "DDAMBA HC III"="DAMBA HC II",
                                     "DIBOLYEC HC II"="DIBOLYEC HC II",
                                     "DIIKA HC II"="DIIKA HC II",
                                     "DIIMA HC III"="DIIMA HC III",
                                     "DINO HC II"="DINO HC II",
                                     "DIOCESE OF KITGUM HC II"="DIOCESE OF KITGUM HC II",
                                     "DOKOLO HC IV"="DOKOLO HC IV",
                                     "DONNA CARNEVALE MEDICAL CENTRE"="DONA CARNEVALE MEDICAL CENTRE HC III",
                                     "DOUBLE CURE MEDICAL CENTRE"="DOUBLE CURE HC IV",
                                     "DR CHARLES FARTHING MEMORIAL CLINIC"="DR.CHARLES FURTHING CLINIC",
                                     "DRAMBA HC III"="DRAMBA HC III",
                                     "DRANYA HC III"="DRANYA HC III",
                                     "DREAM CENTRE UGANDA"="DREAM CENTRE HC III",
                                     "DRICILE HC III"="DRICILE HC III",
                                     "DUFILE HC III"="DUFILE HC III",
                                     "DUNGI HC II"="DDUNGI HC II",
                                     "DWOLI HC III"="DWOLI HC III",
                                     "DZAIPI HC III"="DZAIPI HC III",
                                     "EASTERN DIVISION HC III"="EASTERN DIVISION HC III",
                                     "EBENEZER SDA MEDICAL HC III"="EBENEZER SDA HC III",
                                     "EDIOFE HC III"="EDIOFE HC III",
                                     "ELEMA HC II"="ELEMA HC II",
                                     "EMESCO HC IV"="EMESCO HC III",
                                     "ENDIINZI HC III"="ENDIIZI HC III",
                                     "ENGEYE HC II"="ENGEYE HC II",
                                     "ENTEBBE GRADE B HOSPITAL"="ENTEBBE HOSPITAL MCH GENERAL",
                                     "EPI CENTRE HC II (MPIGI)"="KIRINGENTE EPI CENTRE HC II",
                                     "EPI CENTRE HC III (BUTAMBALA)"="EPI-CENTRE SENGE HC III",
                                     "EPICENTRE NAMAYUMBA"="NAMAYUMBA EPI CENTRE HC III",
                                     "EREMI HC III"="EREMI HC III",
                                     "EREPI"="EREPI HC II",
                                     "ERIA HC III"="ERIA HC III",
                                     "ESERI DOM. CLINIC"="ESERI DOM CLINIC HC II",
                                     "ETAM HC III"="ETAM HC III",
                                     "EWANGA HC III"="EWANGA HC III",
                                     "FAD MILITARY HOSPITAL"="MASINDI MILITARY BARRACKS HC IV",
                                     "FAMILY HEALTH RESOURCE CENTRE KIRUHURA"="FAMILY HEALTH RESOURCE CENTRE CLINIC",
                                     "FAMILY HOPE CENTRE JINJA"="FAMILY HOPE CENTER JINJA SPECIAL CLINIC",
                                     "FAMILY HOPE CENTRE KAMPALA"="FAMILY HOPE CENTRE KAMPALA SPECIAL CLINIC",
                                     "FASTLINE MEDICAL CENTRE"="FAST LINE CINIC CLINIC",
                                     "FIDUGA HC III"="FIDUGA HC III",
                                     "FIMRC BUSHIKA HC III"="BUSHIKA HC III",
                                     "FLAMA NWOYA"="FLAMA HC II",
                                     "FLORENCE NIGHTINGALE HOSPITAL"="FLORENCE NIGHTINGALE HOSPITAL",
                                     "FOCREV HC III"="LUMINO FOC REV HC III",
                                     "FORT PORTAL POLICE"="BUNDIBUGYO POLICE HC II",
                                     "FORTPORTAL REGIONAL REFERRAL HOSPITAL"="FORT PORTAL REGIONAL REFERRAL HOSPITAL",
                                     "FR.BILBAO HC III"="FR. BILBAO HC III",
                                     "FRANCISICAN HC IV"="ST. FRANCISCAN HC IV",
                                     "GADAFI HC III"="GADDAFI BARRACKS HC III",
                                     "GALIBOLEKA HC II"="GALIBOLEKA HC II",
                                     "GALILEE HOSPITAL"="GALILEE COMMUNITY HOSPITAL",
                                     "GALIMAGI HC III"="GALIMAGI (BUTEBO) HC III",
                                     "GALIRAAYA HC III"="GALIRAYA HC III",
                                     "GAMAGOO HC III"="GAMOGO HC III",
                                     "GAMATIMBEYI HC III"="GAMATIMBEI HC III",
                                     "GAMOGO HC III"="GAMOGO HC III",
                                     "GANGMING HC II"="GANGMING HC II",
                                     "GARRY HOLMES"="RUTH GAYLORD HOSPITAL",
                                     "GASOVU HC III"="GASOVU HC III",
                                     "GATERITERI HC III"="GATERITERI HC III",
                                     "GAYAZA HCII"="GAYAZA HC II",
                                     "GBOROKOLONGO HC III"="GBOROKOLONGO HC III",
                                     "GGOLI HC III (MPIGI)"="GGOLO HC III",
                                     "GGOLO HC III"="GGOLO HC III",
                                     "GGWATIRO HOSPITAL"="GGWATIRO NURSING HOME HOSPITAL",
                                     "GOD'S CARE MEDICAL CENTER"="GOD'S CARE MEDICAL CENTRE HC III",
                                     "GOD'S MERCY M/C"="GOD'S MERCY",
                                     "GOGONYO HC III"="GOGONYO HC III",
                                     "GOLI"="GOLI HC IV",
                                     "GOMA HC II"="GOMA HC III",
                                     "GOMBE HC II"="GOMBE (WAKISO) HC II",
                                     "GOMBE HOSPITAL"="GOMBE HOSPITAL",
                                     "GOOD SHEPHERD"="GOOD SHEPHERD MEDICAL CLINIC",
                                     "GOOPI HC II"="GOOPI HC II",
                                     "GULU REGIONAL REFERRAL HOSPITAL"="GULU REGIONAL REFERRAL HOSPITAL",
                                     "GURUGURU HC II"="GURUGURU HC II",
                                     "GWENG-COO HC II"="GWENG-COO HC II",
                                     "GWERE"="GWERE HC II",
                                     "GWERI HC III"="GWERI HC III",
                                     "HAAMA HC II"="HAAMA HC II",
                                     "HAMUKUNGU HC II"="HAMUKUNGU HC II",
                                     "HAMURWA HC IV"="HAMURWA HC IV",
                                     "HAPUUYO HC III"="HAPUUYO HC III",
                                     "HEALTH INITIATIVE FOR AFRICA (BMC)"="HEALTH INITIATIVE FOR AFRICA HC II",
                                     "HEALTH INITIATIVES ASSOCIATION"="HEALTH INITIATIVE ASSOCIATION HC II",
                                     "HIMA  GOVT HC III"="HIMA HC III",
                                     "HIMA CEMENT CLINIC"="HIMA HC III",
                                     "HIS GRACE HEALTHCARE"="HIS GRACE MEDICAL CENTRE HC II",
                                     "HOIMA POLICE HC II"="HOIMA POLICE HC III",
                                     "HOIMA PRISONS"="HOIMA PRISONS HC II",
                                     "HOIMA REGIONAL REFERRAL HOSPITAL"="HOIMA REGIONAL REFERRAL HOSPITAL",
                                     "HOLY CROSS HC III (KIKYUSA)"="HOLY CROSS - KIKYUSA HC III",
                                     "HOLY CROSS ORTHODOX (NAMUNGOONA) HOSPITAL"="HOLY CROSS ORTHODOX MISSION NAMUNGOONA HOSPITAL",
                                     "HOLY FAMILY HOSPITAL-NYAPEA"="NYAPEA HOSPITAL",
                                     "HOLY INNOCENT CHILDRENS HOSPITAL"="HOLY INNOCENTS CHILDREN'S HOSPITAL",
                                     "HOLY INNOCENTS HC III (BUKEDEA)"="HOLY INNOCENT HC III",
                                     "HOPE AGAIN MEDICAL CENTRE (KYENJOJO)"="HOPE AGAIN MEDICAL CENTRE HC III",
                                     "HOPE CLINIC LUKULI"="HOPE CLINIC LUKULI HC III",
                                     "HOPE MEDICAL CENTRE"="HOPE MEDICAL CENTRE",
                                     "HOPE MEDICAL CENTRE (AMG)(SHEEMA)"="HOPE MEDICAL CENTRE HC III",
                                     "HUKESEHO HC III"="HUKESEHO HC III",
                                     "IBAHWE"="IBAKWE HC II",
                                     "IBANDA HOSPITAL"="IBANDA HOSPITAL",
                                     "IBANDA MISSION HC III"="IBANDA MISSION HC III",
                                     "IBOA HC II"="IBOA HC II",
                                     "IBUJE HC III"="IBUJE HC III",
                                     "ICEME (GOVT) HC III"="ICEME HC III (ICEME TOWN COUNCIL)",
                                     "ICEME GOVT HC III"="ICEME HC III (ICEME SUBCOUNTY)",
                                     "ICEME HC III"="ICEME HC III (ICEME SUBCOUNTY)",
                                     "IDI"="MULAGO NRH - INFECTIOUS DISEASE INSTITUTE",
                                     "IDIWA"="IDIWA HC III",
                                     "IGAMARA HC III"="IGAMARA HEALTH CENTRE HC III",
                                     "IGANGA HOSPITAL"="IGANGA HOSPITAL",
                                     "IGAYAZA HC II"="IGAYAZA HC III",
                                     "IHANDIRO HC III"="IHANDIRO HC III",
                                     "IKI  IKI HC III"="IKI IKI HC III",
                                     "IKOBA HC III"="IKOBA HC III",
                                     "IKUMBYA HC III (LUUKA)"="IKUMBYA HC III",
                                     "IMVEPI HC III"="IMVEPI HC II",
                                     "INDE HC III"="INDE HC III",
                                     "INDILINGA HC II"="INDILINGA HC II",
                                     "INOMO HC III"="INOMO HC III",
                                     "INTERNATIONAL MEDICAL CENTRE"="INTERNATIONAL MEDICAL CENTRE CLINIC",
                                     "INUULA HC II"="INUULA HC II",
                                     "IRIIRI HC III"="IRIRI HC III",
                                     "IRUHUURA HC III"="IRUHURA COU HC III",
                                     "ISHAKA ADVENTIST HOSPITAL"="ISHAKA ADVENTIST HOSPITAL",
                                     "ISHONGORORO HC IV"="ISHONGORORO HC IV",
                                     "ISIBUKA HC III"="ISIBUKA NURSING HOME HC III",
                                     "ISINDE HC II"="ISINDE HC II",
                                     "ISULE HC III"="ISULE HC III",
                                     "ISUNGA HC III"="ISUNGA HC III",
                                     "ITOJO HOSPITAL"="ITOJO HOSPITAL",
                                     "ITULA HC III"="ITULA HC III",
                                     "IWEMBA HC III"="IWEMBA HC III",
                                     "IYETE HC III"="IYETE HEALTH CENTRE HC III",
                                     "IYOLWA HC III"="IYOLWA HC III",
                                     "JAANA HC II"="JAANA HC II",
                                     "JAGUZI HC II"="JAGUSI HC III",
                                     "JANGOKORO HC III"="JANGOKORO HC III",
                                     "JARO HOSPITAL"="JARO HOSPITAL",
                                     "JCRC FORT PORTAL"="JCRC FORT PORTAL",
                                     "JCRC LUBOWA(WAKISO)"="JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                     "JEWA"="JEWA HC III",
                                     "JINJA CENTRAL HC III"="JINJA CENTRAL HC III",
                                     "JINJA ISLAMIC HEALTH CENTRE III"="JINJA ISLAMIC HC III",
                                     "JINJA MAIN PRISON HC III"="JINJA MAIN PRISON HC III",
                                     "JINJA POLICE HC III (MATERNITY)"="JINJA POLICE HC III",
                                     "JINJA REGIONAL REFERRAL HOSPITAL"="JINJA REGIONAL REFERRAL HOSPITAL",
                                     "JOKASY"="JOKASY CLINIC HC II",
                                     "JOY HOSPICE"="JOY HOSPICE HC II",
                                     "JOY MEDICAL CENTER (MBALE  )"="JOY MEDICAL HC II",
                                     "JOY MEDICAL CENTRE"="JOY MEDICAL CENTRE HC III",
                                     "JUMBO CLINIC"="JUMBO CLINIC",
                                     "JUPANZIRI HC III"="JUPANZIRI HC III",
                                     "JURU HC II"="JURU HC III",
                                     "KAABONG HOSPITAL"="KAABONG HOSPITAL",
                                     "KAABONG MISSION HC III"="KAABONG MISSION HC III",
                                     "KABAALE HC II"="SEMBABULE KABAALE HC II",
                                     "KABAHANGO"="KABAHANGO HC III",
                                     "KABALE HC III (HOIMA)"="KABAALE HC III (HOIMA)",
                                     "KABALE HC III (KALUNGU)"="KABAALE HC III (KALUNGU)",
                                     "KABALE REGIONAL REFERRAL HOSPITAL"="KABALE REGIONAL REFERRAL HOSPITAL",
                                     "KABALUNGI HCII"="KABALUNGI HC II",
                                     "KABAMBA HC III"="KABAMBA HC III",
                                     "KABAMBA MILITARY HOSPITAL"="KABAMBA BARRACKS HC III",
                                     "KABAMBIRO HC III"="KABAMBIRO HC III",
                                     "KABANGA HC III"="KABANGA HC III",
                                     "KABAROLE HOSPITAL C.O.U"="KABAROLE COU HOSPITAL",
                                     "KABARWA   HC  III"="KABARWA HC III",
                                     "KABASA MEMORIAL HOSPITAL"="KABASA MEMORIAL HOSPITAL",
                                     "KABASEKENDE"="KABASEKENDE HC III",
                                     "KABATEMA HC II"="KABATEMA HC II",
                                     "KABATUNDA HC III"="KABATUNDA HC III",
                                     "KABAYANDA HC II"="KABAYANDA HC II",
                                     "KABBO HC II"="KABBO HC II",
                                     "KABELYO HC II"="KABELYO HC II",
                                     "KABENDE HC II"="KABENDE HC III",
                                     "KABERAMAIDO CATHOLIC HC III"="KABERAMAIDO CATHOLIC MISSION HC III",
                                     "KABERAMAIDO HC IV"="KABERAMAIDO HOSPITAL",
                                     "KABEYWA HC III"="KABEYWA HC III",
                                     "KABIGI MUSLIM HC II"="KABIGI MUSLIM HC III",
                                     "KABINGO HC II"="KIBINGO HC II",
                                     "KABIRA HC III (RAKAI)"="KABIRA (KYOTERA) HC III",
                                     "KABIZZI HC II"="KABIZI HC II",
                                     "KABOGWE"="ST. THERESA KABOGWE HC II",
                                     "KABOLOI HC III"="KABOLOI HC III",
                                     "KABONERO HC III"="KABONERO HC III",
                                     "KABOWA HCII"="KABOWA HC II",
                                     "KABUBWA HC II"="KABUBWA HC II",
                                     "KABUGA HC III"="KABUGA HC III",
                                     "KABUGA HCIII"="KABUGA HC III",
                                     "KABULA"="KABULA PRISON HC II",
                                     "KABULE HC III"="KABULE HC III",
                                     "KABUNGO HC III"="KABUNGO HC III",
                                     "KABUSHAHO HC III"="KABUSHAHO HC III",
                                     "KABUWOKO HC III"="KABUWOKO GOVT HC III",
                                     "KABUYANDA HC IV"="KABUYANDA HC IV",
                                     "KABWANGASI HC III"="KABWANGASI HC III",
                                     "KABWERI HC II"="KABWERI HC III",
                                     "KABWOHE CLINICAL RESEARCH CENTRE"="KABWOHE CLINICAL RESEARCH CENTRE HC II",
                                     "KABWOHE HC IV"="KABWOHE HC IV",
                                     "KABWOYA HC III"="KABWOYA HC III",
                                     "KABYUMA HC II"="KABYUMA HC II",
                                     "KACHANGA HC II"="KACHANGA ISLAND HC II",
                                     "KACHEERA HC III"="KACHEERA HC III",
                                     "KACHERI HC III"="KACHERI HC III",
                                     "KACHONGA HC III"="KACHONGA HC III",
                                     "KACHUMBALA HC III"="KACHUMBALA HC III",
                                     "KACHUMBALA MISSION DISPENSARY"="KACHUMBALA NGO HC II",
                                     "KACHUNG HC II"="KACHUNG HC II",
                                     "KADAMA HC III"="KADAMA HC III",
                                     "KADERUNA HC III"="KADERUNA HC III",
                                     "KADUKU HC II"="KADUKU HC II",
                                     "KADUNGULU HC III"="KADUNGULU HC III",
                                     "KAGADI  HOSPITAL"="KAGADI HOSPITAL",
                                     "KAGANDO HOSPITAL"="KAGANDO HOSPITAL",
                                     "KAGANO  HC   III"="KAGANO HC III",
                                     "KAGEZI HC III"="KAGEZI HC III",
                                     "KAGOGGO HC II"="KAGOGGO HC II",
                                     "KAGOROGORO HC II"="KAGOROGORO DISPENSARY HC II",
                                     "KAGOTE HC III"="KAGOTE HC III",
                                     "KAGUMBA"="KAGUMBA HC III",
                                     "KAGUMU HC III"="KAGUMU HC III",
                                     "KAGWARA HC II"="KAGWARA HC III",
                                     "KAHOKYA HC II"="KAHOKYA HC II",
                                     "KAHUNDE HC II"="KAHUNDE HC II",
                                     "KAIHURA VILLA MARIA HC II"="VILLA MARIA (KAIHURA) HC II",
                                     "KAIMESE HC II"="KAIMESE HC III",
                                     "KAJJANSI HC III"="KAJJANSI CLAYS HC II",
                                     "KAJOJI HCII"="KASOLI HC II",
                                     "KAKABARA HC III"="KAKABARA HC III",
                                     "KAKAIRE HC III"="KAKAIRE HC III",
                                     "KAKAMAR HC II"="KAKAMAR HC II",
                                     "KAKANJU HC III"="KAKANJU HC III",
                                     "KAKASI C.O.U HC III"="KAKASI COU HC II",
                                     "KAKINDO HC IV"="KAKINDO HC IV",
                                     "KAKINGA HC III"="KAKINGA HC III",
                                     "KAKINGOL"="KAKINGOL HC III",
                                     "KAKIRA HC III (JINJA)"="KAKIRA HC III",
                                     "KAKIRA SUGAR WORKS HOSPITAL"="KAKIRA SUGAR WORKERS HOSPITAL",
                                     "KAKIRI HC III"="KAKIRI HC III",
                                     "KAKO HCIII"="KAKO HC III",
                                     "KAKOBA DIVISION HC III"="KAKOBA HC III",
                                     "KAKOMA HC III"="KAKOMA HC III",
                                     "KAKOOGE HC III"="KAKOOGE HC III",
                                     "KAKOOLA HC III"="KAKOOLA HC II (NAKASONGOLA)",
                                     "KAKORO HC III"="KAKORO HC III",
                                     "KAKUKA HC III"="KAKUKA HC III",
                                     "KAKUMIRO HC IV"="KAKUMIRO HC IV",
                                     "KAKURE HC II"="KAKURES HC II",
                                     "KAKUUTO HC IV"="KAKUUTO HC IV",
                                     "KALADIMA HC III"="KALADIMA HC III",
                                     "KALAGALA"="KALAGALA HC IV",
                                     "KALAGALA HC IV"="KALAGALA HC IV",
                                     "KALAKI HC III"="KALAKI HC III",
                                     "KALANGAALO HC II"="KALANGALO HC III",
                                     "KALANGALA HC IV"="KALANGALA HC IV",
                                     "KALAPATA HC III"="KALAPATA HC III",
                                     "KALAWA HCII"="KALAMA HC II",
                                     "KALEGE"="KALEGE HC II",
                                     "KALEMUNGOLE HC II"="KALEMUNGOLE HC II",
                                     "KALI"="KALI HC II",
                                     "KALIBU HC III"="KALIBU HC III",
                                     "KALIMON HC II"="KALIMON HC II",
                                     "KALIRO HC III (LYANTONDE)"="KALIIRO HC III",
                                     "KALISIZO HOSPITAL"="KALISIZO HOSPITAL",
                                     "KALOKENGEL HC II"="KALOKENGEL HC II",
                                     "KALONGA HC III"="KALONGA HC III",
                                     "KALONGO HOSPITAL"="KALONGO AMBROSOLI MEMORIAL HOSPITAL",
                                     "KALOWANG HC III"="KALOWANG HC III",
                                     "KALUNGI HC III (KALUNGU)"="KALUNGI (LUKAYA) HC III",
                                     "KALUNGI HC III (NAKASONGOLA)"="KALUNGI (KALUNGI) HC III",
                                     "KALUNGU HC III"="KALUNGU HC III",
                                     "KALUUBA HCII"="KALUBA HC II",
                                     "KAMACHA HC III"="KAMACA HC III",
                                     "KAMBAALA HC III"="KAMBAALA HC III",
                                     "KAMBUGA HOSPITAL"="KAMBUGA HOSPITAL",
                                     "KAMBUGU HC III (KIBOGA)"="KAMBUGU HC III",
                                     "KAMDINI HC II"="KAMDINI HC II",
                                     "KAMEKE HC III"="KAMEKE HC III",
                                     "KAMERUKA HC III"="KAMERUKA HC III",
                                     "KAMION HC II"="KAMION HC II",
                                     "KAMIRA HC III"="KAMIRA HC III",
                                     "KAMOD HC II"="KAMOD HC II",
                                     "KAMONKOLI HC III"="KAMONKOLI HC III",
                                     "KAMORU HCII"="KAMORU HC II",
                                     "KAMPALA MEDICAL CHAMBERS"="KAMPALA MEDICAL CHAMBERS CLINIC HC II",
                                     "KAMPIRINGISA HC III"="KAMPIRINGISA HC III",
                                     "KAMUBEIZI HC II"="KAMUBEIZI HC II",
                                     "KAMUDA HC III"="KAMUDA HC III",
                                     "KAMUGE HC III"="KAMUGE HC III",
                                     "KAMUKUZI HC III"="KAMUKUZI DIVISION HC II",
                                     "KAMULEGU HC III"="KAMULEGU HC III",
                                     "KAMULI DISTRICT GOVT HOSPITAL"="KAMULI HOSPITAL",
                                     "KAMULI HC III"="KIMULI HC III",
                                     "KAMULI MISSION HOSPITAL"="KAMULI MISSION HOSPITAL",
                                     "KAMWENGE HC III"="KAMWENGE HC III",
                                     "KAMWENGE PRISONS HCIII"="KAMWENGE PRISON  HC II",
                                     "KAMWOKYA CHRISTIAN CARING COMMUNITY"="KAMWOKYA CHRISTIAN CARING COMMUNITY HC III",
                                     "KANAMBA HC III"="KANAMBA HC III",
                                     "KANARA HC II"="KAARA HC II",
                                     "KANAWAT HC III"="KANAWAT HC III",
                                     "KANGAI HC III"="KANGAI HC III",
                                     "KANGALABA HC III"="KANGALABA HC III",
                                     "KANGINIMA HOSPITAL"="KANGINIMA (BUTEBO) HOSPITAL",
                                     "KANGO HC III"="KANGO HC III",
                                     "KANGOLE HC III"="KANGOLE HC III",
                                     "KANGULUMIRA HC IV"="KANGULUMIRA HC IV",
                                     "KANGULUMIRA INTERGRATED"="KANGULUMIRA I.H.P HC II",
                                     "KANONI"="KANONI HC III",
                                     "KANONI HC III (GOMBA)"="KANONI HC III",
                                     "KANSAMBYA HC II"="KANSAMBYA HC II",
                                     "KANSEERA HC II"="KANSEERA HC II",
                                     "KANU HC II"="KANU HC II",
                                     "KANUNGU HC IV"="KANUNGU HC IV",
                                     "KANYAMWIRIMA HC III"="KANYAMWIRIMA ARMY HC III",
                                     "KANYANTOROGO HC III"="KANYANTOROGO HC III",
                                     "KANYATSI HC II"="KANYATSI HC II",
                                     "KANYUM HC III"="KANYUM HC III",
                                     "KANYUMU HC II (PALLISA)"="KANYUMU HC II",
                                     "KANYWAMAIZI HC III"="KANYWAMAIZI HC III",
                                     "KANYWAMBOGO HC III"="KANYWAMBOGO HC III",
                                     "KAPAPI"="KAPAPI HC III",
                                     "KAPCHORWA HOSPITAL"="KAPCHORWA HOSPITAL",
                                     "KAPCHORWA PRISONS HC II"="KAPCHORWA PRISON HC II",
                                     "KAPEDO HC III"="KAPEDO HC III",
                                     "KAPEEKA HC III"="KAPEEKA HC III",
                                     "KAPELEBYONG HC IV"="KAPELEBYONG HC IV",
                                     "KAPETA HC II"="KAPETA HC II",
                                     "KAPIR HC III"="KAPIR HC III",
                                     "KAPKOLOSWO HC III"="KAPKOLOSWO HC III",
                                     "KAPKOROS HC II"="KAPKOROS HC II",
                                     "KAPRORON HC IV"="KAPRORON HC IV",
                                     "KAPTUM HC II"="KAPTUM HC III",
                                     "KAPUJAN HC III"="KAPUJAN HC III",
                                     "KAPWAI HC III"="KAPUWAI HC III",
                                     "KAPWAI PACODAT HC III"="KAPUWAI HC III",
                                     "KARAMBI  HC  III"="KARAMBI (KASESE) HC III",
                                     "KARAMBI HC III (KABAROLE)"="KARAMBI (KABAROLE) HC III",
                                     "KARAMBI HC III (KASESE)"="KARAMBI (KASESE) HC III",
                                     "KARENGA HC IV"="KARENGA HC IV",
                                     "KARINGA HCI II"="KARINGA HC II",
                                     "KARITA HC III (AMUDAT)"="KARITA HC IV",
                                     "KARONGO HC III"="KARONGO HC III",
                                     "KARUGUTU HC IV"="KARUGUTU HC IV",
                                     "KARUNGU HC III"="ST. JUDE THADDEOS KARUNGU HC III",
                                     "KARUSANDARA HC III"="KARUSANDARA HC III",
                                     "KARWENYI HC II"="KARWENYI HC III",
                                     "KASAALI  HC III"="KASAALI HC III",
                                     "KASAGAMA HC III"="KASAGAMA HC III",
                                     "KASAMBIRA"="KASAMBIRA HC II",
                                     "KASAMBYA HC II (GOMBA)"="KASAMBYA (GOMBA) HC II",
                                     "KASAMBYA HC III (KALUNGU)"="KASAMBYA (KALUNGU) HC III",
                                     "KASAMBYA HC III (KIBAALE)"="KASAMBYA (KAKUMIRO) HC III",
                                     "KASAMBYA HC IV (MUBENDE)"="MUBENDE KASAMBYA HC III GOVT",
                                     "KASANGA PHC III"="KASANGA PHC HC III",
                                     "KASANJE HC III"="KASANJE HC III",
                                     "KASASA HC III"="KASASA HC III",
                                     "KASASIRA HC III"="KASASIRA HC III",
                                     "KASAWO HC III"="KASAWO HC III",
                                     "KASAWO MISSION"="KASAWO MISSION HC III",
                                     "KASEETA HC III"="KASEETA HC III",
                                     "KASENDA HC III"="KASENDA HC III",
                                     "KASENSERO HC II"="KASENSERO HC II",
                                     "KASENYI HC II"="KASENYI HC II",
                                     "KASEREM  HC III"="KASEREM HC III",
                                     "KASEREM HC III"="KASEREM HC III",
                                     "KASESE COMMUNITY MEDICAL CENTRE HC III"="KASESE COMMUNITY HEALTH CENTER HC III",
                                     "KASESE TOWN COUNCIL HC III"="KASESE MUNICIPAL COUNCIL HC III",
                                     "KASHAGAZI HC II"="KISHAGAZI HC II",
                                     "KASHAKA HC II"="KASHAKA HC II",
                                     "KASHAMBYA HC III"="KASHAMBYA HC III",
                                     "KASHONGI HC III"="KASHONGI HC III",
                                     "KASHUMBA HC III"="KASHUMBA HC III",
                                     "KASODO HC III"="KASODO HC III",
                                     "KASOKWE HC III"="KASOKWE HC II",
                                     "KASOLWE HC II"="KASOLWE HC II",
                                     "KASONGA"="KASONGA HC II",
                                     "KASOZI HC III (LUWERO)"="KASOZI HC III",
                                     "KASOZO HC II"="KASOOZO HC II",
                                     "KASSANDA HC IV"="KASSANDA HC IV",
                                     "KASSANDA HCIV"="KASSANDA HC IV",
                                     "KASUBI HC III"="KASUBI HC III",
                                     "KASULE HC III"="KASULE HC III",
                                     "KASULENGA HC II"="KASULENGE HC II",
                                     "KASUNGANYANJA HC III"="KASUNGANYANJA HC III",
                                     "KASUSU HC III"="KASUSU HC III",
                                     "KASWA HC III"="KASWA HC III",
                                     "KATABOK HC II"="KATABOK HC III",
                                     "KATADOBA HC III"="KATODOBA UMSC HC III",
                                     "KATAKWI HOSPITAL"="KATAKWI HOSPITAL",
                                     "KATARAKA HC IV"="KATARAKA HC IV",
                                     "KATARAZA HCIII"="KATARAZA HC III",
                                     "KATASENYWA HC III"="KATASENYWA HC III",
                                     "KATEETE HC II"="KATEETE HC II",
                                     "KATERERA HC III"="KATERERA HC III",
                                     "KATHILE HC III"="KATHILE HC III",
                                     "KATIKAMU HC III"="KATIKAMU HC III",
                                     "KATIMBA HC III"="KATIMBA HC III",
                                     "KATIRA HC III (BUDAKA)"="KATIRA HC III",
                                     "KATOJO HC III"="KATOJO PRISONS HC III",
                                     "KATOMOR HC III"="KOTOMOR HC III",
                                     "KATOOGO HC III"="KATOOGO HC III",
                                     "KATOOKE HC III"="KATOOKE HC III",
                                     "KATOVU HC III"="KATOVU HC II",
                                     "KATULIKIRE HC III"="KATULIKIRE HC III",
                                     "KATUM HC II"="KATUM HC II",
                                     "KATUNGURU HC III (KASESE)"="KATUNGURU HC II (KASESE)",
                                     "KATUNGURU HC III(RUBIRIZI)"="KATUNGURU HC III (RUBIRIZI)",
                                     "KATUUGO HC II"="KATUUGO HC III",
                                     "KATWE HC III (KASESE)"="KATWE (KABATORO) HC III",
                                     "KATWE HC III (KIBOGA)"="KATWE (DWANIRO) HC III",
                                     "KAUCHO MISSION HCIII"="KAUCHO MISSION HC III ",
                                     "KAUKURA HC II"="KAUKURA HC II",
                                     "KAVERA HC III"="KASESE 307 BDE UPDF (KAVERA) HC III",
                                     "KAVULE HC II"="KAVULE CLINIC HC II",
                                     "KAWAALA HC III"="KAWAALA HC IV",
                                     "KAWANDA HC III"="KAWANDA HC III",
                                     "KAWEMPE HOME CARE"="KAWEMPE HOME CARE CLINIC HC III",
                                     "KAWEMPE NATIONAL REFERRAL HOSPITAL"="KAWEMPE NATIONAL REFERRAL HOSPITAL",
                                     "KAWERI PRISONS HCIII"="KAWEERI PRISONS HC II",
                                     "KAWEWETA HC III"="KAWEWEETA HC III",
                                     "KAWOKO MUSLIM HC III"="KAWOKO MUSLIM HC III",
                                     "KAWOLO HOSPITAL"="KAWOLO HOSPITAL",
                                     "KAWONGO HC III"="KAWONGO HC III",
                                     "KAYANGO HC III"="KAYANGO HC III",
                                     "KAYANJA HC II"="KAYANJA HC II",
                                     "KAYANZI"="KAYANZI HC II",
                                     "KAYEBE HCIII"="KAYEBE HC II",
                                     "KAYENJE HC II"="KAYENJE HC II",
                                     "KAYOGERA HC II"="KAYOGERA HC II",
                                     "KAYONZA HC III (KANUNGU)"="KAYONZA HC III",
                                     "KAYONZA HC III (NTUNGAMO)"="KAYONZA (NTUNGAMO) HC III",
                                     "KAYUNGA HOSPITAL"="KAYUNGA REGIONAL REFERRAL HOSPITAL",
                                     "KAZINGA HC II (RUBIRIZI)"="KAZINGA (RUBIRIZI) HC II",
                                     "KAZINGA HC III"="KAZINGA HC III",
                                     "KAZO HC IV"="KAZO HC IV",
                                     "KAZWAMA HC II"="KAZWAMA HC II",
                                     "KDDO HC III"="KDDO HC III",
                                     "KEBISONI HC IV"="KEBISONI HC IV",
                                     "KEI HC III"="KEI HC III",
                                     "KENKEBU HC II"="KENKEBU HC II",
                                     "KEREKERENE HC III"="KEREKERENE HC III",
                                     "KERWA HC III"="KERWA HC III",
                                     "KIBAALE  HC II( RAKAI)"="KIBAALE HC II",
                                     "KIBAALE HC IV"="KIBAALE HC IV",
                                     "KIBAALE PRISONS HC11"="KIBAALE PRISON HC II",
                                     "KIBAATE"="KIBATE HC III",
                                     "KIBAIRE HC II"="KIBAIRE HC II",
                                     "KIBALE HC III(PALISA)"="KIBALE HC III",
                                     "KIBALINGA HC III"="KIBALINGA HC III",
                                     "KIBANDA HC III (RAKAI)"="KIBANDA HC III",
                                     "KIBANGA HC III (ST ELISABETH OF THURINGEN )"="KIHANGA HC III",
                                     "KIBAR HC II"="KIBAR HC II",
                                     "KIBAZI HC III"="KIBAZI HC III",
                                     "KIBENGO HC II"="KIBENGO HC III",
                                     "KIBENGO HC III"="KIBENGO HC III",
                                     "KIBIBI NURSING HOME HC III"="KIBIBI NURSING HOME HC III",
                                     "KIBIITO HC IV"="KIBIITO HC IV",
                                     "KIBIRIZI HC III"="KIBIRIZI HC III",
                                     "KIBOGA HOSPITAL"="KIBOGA HOSPITAL",
                                     "KIBOTA HCIII"="KIBOTA HC II",
                                     "KIBUGGA HC III"="KIBUGGA HC II",
                                     "KIBUKU COMMUNITY HC III"="COMMUNITY HEALTH CENTER (KIBUKU) HC III",
                                     "KIBUKU HC IV"="KIBUKU HC IV",
                                     "KIBUKU(COMMUNITY HC)"="COMMUNITY HEALTH CENTER (KIBUKU) HC III",
                                     "KIBULI MUSLIM HOSPITAL"="KIBULI HOSPITAL",
                                     "KIBUYE HC II"="KIBUYE HC II",
                                     "KICHECHE HC III"="KICHECHE HC III",
                                     "KICHINJAJI  HC  III"="KICHINJAJI HC III",
                                     "KICHWABUGINGO HC II"="KICHWABUGINGO HC II",
                                     "KICHWAMBA HC II (KAMWENGE)"="KICHWAMBA HC II",
                                     "KICHWAMBA HC III (RUBIRIZI)"="KICWAMBA (RUBIRIZI) HC III",
                                     "KICWAMBA HC III (KABAROLE)"="KICWAMBA HC III",
                                     "KIDA HOSPITAL"="KIDA HOSPITAL",
                                     "KIDEPO HC II"="KIDEPO HC II",
                                     "KIDERA HC IV"="KIDERA HC IV",
                                     "KIDETOK MISSION HC III"="KIDETOK HC III",
                                     "KIDILANI"="KIDILANI HC II",
                                     "KIDONGOLE HC III"="KIDONGOLE HC III",
                                     "KIDUBULI HCIII"="KIDUBULI HC III",
                                     "KIFAMBA HC III (RAKAI)"="KIFAMBA HC III",
                                     "KIFAMPA HC III (GOMBA)"="KIFAMPA HC III",
                                     "KIGAMBO HC II"="KIGAMBO HC III",
                                     "KIGANDA HC IV"="KIGANDA HC IV",
                                     "KIGANDALO HC IV"="KIGANDALO HC IV",
                                     "KIGANDO"="KIGANDO HC III",
                                     "KIGARAALE HC III"="KIGARAALE HC III",
                                     "KIGARAMA HC III"="KIGARAMA HC II",
                                     "KIGEZI HC II"="KIGEZI HC II",
                                     "KIGO PRISONS HC III"="KIGO MAIN PRISONS HC III",
                                     "KIGOROBYA HC IV"="KIGOROBYA HC IV",
                                     "KIGOYERA HC II"="KIGOYERA HC II",
                                     "KIGULU HC II"="KIGULU HC II",
                                     "KIGUMBA HC III"="KIGUMBA HC III",
                                     "KIGUMBA PRISON HC III"="KIGUMBA PRISONS HC II",
                                     "KIGUNGU HC III"="KIGUNGU HC III",
                                     "KIGWERA HC III"="KIGWERA HC III",
                                     "KIHANI HC III"="KIHANI HC II",
                                     "KIHIIHI HC IV"="KIHIHI HC IV",
                                     "KIHUNDA HC III"="KIHUNDA HC III",
                                     "KIHUNGYA HC II"="KIHUNGYA HC II",
                                     "KIHUUKYA HCIII"="KIHUUKYA HC II",
                                     "KIIGE HC II"="KIIGE HC II",
                                     "KIIGYA HC III"="KIIGYA HC II",
                                     "KIJUMBA HCII"="KIRUMBA HC II",
                                     "KIJUNJUBWA HC III"="KIJUNJUBWA HC III",
                                     "KIJURA HC III"="KIJURA HC III",
                                     "KIKAGATE HC III"="KIKAGATE HC III",
                                     "KIKAMULO HC III"="KIKAMULO HC III",
                                     "KIKANDWA  HC  III"="KIKANDWA HC III",
                                     "KIKANDWA HC II"="KIKANDWA HC III",
                                     "KIKATSI HC III"="KIKATSI HC III",
                                     "KIKO HC III"="KIKO HC III",
                                     "KIKOKWA  HC   III"="KIKOKWA HC III",
                                     "KIKOLIMBO HC III"="KIKOLIMBO HC II",
                                     "KIKOMA HC III"="KIKOMA HC III",
                                     "KIKOMA HCII"="KIKOMA HC II",
                                     "KIKONDA HC III"="KIKONDA HC III",
                                     "KIKUBYA HC"="KIKUBYA HC II",
                                     "KIKURURA"="KIKURURA HC II",
                                     "KIKUUBE HC IV"="KIKUUBE HC IV",
                                     "KIKWAYI HC II"="KIKWAYI HC II",
                                     "KIKYENKYE HC III"="KIKYENKYE HC III",
                                     "KIKYO HC IV"="KIKYO HC IV",
                                     "KILAK HC III"="KILAK HC III",
                                     "KILEMBE MINES HOSPITAL"="KILEMBE MINES HOSPITAL",
                                     "KIMENGO HC III"="KIMENGO HC III",
                                     "KIMENYEDDE HC II"="KIMENYEDDE HC II",
                                     "KIMULI HC III"="KIMULI HC III",
                                     "KIMULIKIDONGO HC II"="KIMULIKIDONGO HC II",
                                     "KIMWANYI HC III"="KIMWANYI HC III",
                                     "KINANIRA HC III"="KINANIRA HC III",
                                     "KINGDOM LIFE MEDICAL MARTENITY HC II"="KINGDOM LIFE HEALTH CENTER CLINIC",
                                     "KINONI HC III (KIRUHURA)"="KIRUHURA KINONI HC III",
                                     "KINONI HC III (LWENGO)"="LWENGO KINONI GOVT HC III",
                                     "KINONI HC IV (MBARARA)"="KINONI HC IV",
                                     "KINUUKA HC III"="KINUUKA HC III",
                                     "KINYABWAMBA HC III"="KINYABWAMBA HC III",
                                     "KINYAMASEKE HC III"="KINYAMASEKE HC III",
                                     "KINYARA HC III"="KINYARA SUGAR WORKS HC III",
                                     "KINYARUGUNJO HC III"="KINYARUGONJO HC III",
                                     "KINYOGOGA HC III"="KINYOGOGA HC III",
                                     "KIRAGGA HC III"="KIRAGGA HC III",
                                     "KIRALAMBA  HC III"="KIRALAMBA HC III",
                                     "KIREMA HC III"="KIREMA HC III",
                                     "KIREWA COMMUNITY HC III"="KIREWA COMMUNITY HC III",
                                     "KIRIKA HC III"="KIRIKA HC III",
                                     "KIRIKI HC III"="KIRIKI HC III",
                                     "KIRIMA HC III"="KIRIMA HC III",
                                     "KIROKO HC II"="KIROKO HC II",
                                     "KIRU HC II"="KIRU HC II",
                                     "KIRUDDU HOSPITAL"="KIRUDDU NATIONAL REFERRAL HOSPITAL",
                                     "KIRUHURA HC IV"="KIRUHURA HC IV",
                                     "KIRUMBA HC III (RAKAI)"="KIRUMBA HC III",
                                     "KIRYANDONGO HOSPITAL"="KIRYANDONGO HOSPITAL",
                                     "KIRYANDONGO PRISON HC II"="KIRYANDONGO PRISON CLINIC HC II",
                                     "KIRYANGA HC III"="KIRYANGA HC III",
                                     "KIRYANGA HCIII"="KIRYANGA HC III",
                                     "KISAARU TEA HC II"="KISAARU TEA HC II",
                                     "KISAASI COU HC III"="KISAASI COU HC III",
                                     "KISABAGWA"="KISABAGWA HC II",
                                     "KISALA HC"="KISAALA HC II",
                                     "KISENGWE"="KISENGWE HC III",
                                     "KISENYI HC IV (KAMPALA)"="KISENYI HC IV",
                                     "KISHENYI HC II (RUBIRIZI)"="KISHENYI HC II",
                                     "KISIITA HC III"="KISIITA HC III",
                                     "KISIIZI  HC III"="KISIIZI HC III",
                                     "KISIIZI HOSPITAL C.O.U (RUKUNGIRI)"="COU KISIIZI HOSPITAL",
                                     "KISOJJO HC II (BUKOMANSIMBI)"="KISOJJO (KIBINGE) HC II",
                                     "KISOJO HC II (KASESE)"="MUBUKU KISOJO HC II",
                                     "KISOJO HC III (KYENJOJO)"="KISOJO HC III",
                                     "KISOKO HC III"="KISOKO HC III",
                                     "KISOMORO HC III"="KISOMORO HC III",
                                     "KISORO HOSPITAL"="KISORO HOSPITAL",
                                     "KISOZI HC III"="KISOZI HC III",
                                     "KISUBBA HC III"="KISUBA HC III",
                                     "KISUGU HC III"="KISUGU HC III",
                                     "KISULE HC III"="KATIKAMU KISUULE (THE GOOD SAMARITAN) HC III",
                                     "KISWA HC IV"="KISWA HC III",
                                     "KITAASA HC III"="ST. MECHTILDA KITAASA HC III",
                                     "KITAGATA  HOSPITAL"="KITAGATA HOSPITAL",
                                     "KITAIHUKA"="KITAIHUKA HC III",
                                     "KITALA HC II"="KITALA HC II",
                                     "KITALYA III"="KITALYA PRISONS HC II",
                                     "KITANDA HC III"="KITANDA HC III",
                                     "KITANTE MEDICAL CENTRE"="KITANTE MEDICAL CENTRE HC IV",
                                     "KITANYATA HC II"="KITANYATA HC II",
                                     "KITAYUNJWA HC III"="KITAYUNJWA HC III",
                                     "KITEBI HC III"="KITEBI HEALTH CENTRE HC III",
                                     "KITENGA HC III"="KITENGA HC III",
                                     "KITGUM GENERAL HOSPITAL"="KITGUM HOSPITAL",
                                     "KITGUM MATIDI HC III"="KITGUM-MATIDI HC III",
                                     "KITGUM PRISON"="KITGUM PRISON HC II",
                                     "KITHOLHU HC III"="KITHOLHU HC III",
                                     "KITI HC III"="KITI HC III",
                                     "KITIMBA HC III"="KITIMBA HC III",
                                     "KITOKOLO HC II"="KITOKOLO HC II",
                                     "KITONDO HC III"="KITONDO HC III",
                                     "KITONGO HC III"="KITONGO HC III",
                                     "KITOOLE HC II"="KITOOLE HC II",
                                     "KITOVU HCII"="KITOVU HC II",
                                     "KITOVU HOSPITAL"="KITOVU HOSPITAL",
                                     "KITOVU MOBILE"="KITOVU MOBILE CLINIC",
                                     "KITSWAMBA HC III"="KITSWAMBA HC III",
                                     "KITULE HCII"="KITULE HC II",
                                     "KITUNTU HC III"="KITUNTU HC III",
                                     "KITURA HC III"="KITURA HC II",
                                     "KITWARA HC II"="KITWARA HC II",
                                     "KITWE HC II"="KITWE HC II",
                                     "KITWE HC IV"="KITWE HC IV",
                                     "KITYERERA HC IV"="KITYERERA HC IV",
                                     "KIU TEACHING HOSPITAL"="KIU TEACHING HOSPITAL",
                                     "KIWANGALA HC IV"="KIWANGALA HC IV",
                                     "KIWOKO HOSPITAL"="KIWOKO HOSPITAL",
                                     "KIYAGARA HC II"="KIYAGARA HC II",
                                     "KIYEYI HC III"="KIYEYI HC III",
                                     "KIYOMBYA HC III"="KIYOMBYA HC III",
                                     "KIYOOLA HC II"="KIYOOLA HC II",
                                     "KIYUMBA HC IV"="KIYUMBA HC IV",
                                     "KIYUNGA"="KIYUNGA HC II",
                                     "KIYUNI HC III (KYANKWANZI)"="KIYUNI HC III",
                                     "KIYUNI HC III (MUBENDE)"="KIYUNI (MUBENDE) HC III",
                                     "KIZIBA HC III (RAKAI)"="RAKAI KIZIBA HC II",
                                     "KIZIBA HC III (WAKISO)"="KIZIBA HC III",
                                     "KIZIIKO HC II"="KIZIIKO HC II",
                                     "KKONGE HC II"="KKONGE ST. LUKE HC III",
                                     "KOBOKO HC IV"="KOBOKO HOSPITAL",
                                     "KOBOKO MISSION HC III"="KOBOKO MISSION HC III",
                                     "KOBULUBULU HC III"="KOBULUBULU HC III",
                                     "KOBWIN HC III"="KOBWIN HC III",
                                     "KOCH GOMA HC III"="KOCH GOMA HC III",
                                     "KOCH LII HC III"="KOCH LII HC III",
                                     "KOCHEKA HCIII"="KOCHEKA HC II",
                                     "KOCHI HC III"="KOCHI HC III",
                                     "KOCHOLO HC II"="KOCHOLO HC II",
                                     "KODONYO HC II"="KODONYO HC II",
                                     "KOJJA  HEALTH CENTER"="KOJJA HC IV",
                                     "KOLE PRISON HCII"="KOLE PRISON HC II",
                                     "KOLIR HC III"="KOLIR HC III",
                                     "KOLONYI HOSPITAL"="KOLONYI HC IV",
                                     "KOMAMBOGA HC III"="KOMAMBOGA HC III",
                                     "KOMGBE HC III"="KOMGBE HEALTH CENTRE HC III",
                                     "KONGTA HC II"="KONGTA HC II",
                                     "KOOKI COMMUNITY HOSPITAL"="ST. ANDREA KAHWA KOOKI COMMUNITY HOSPITAL",
                                     "KOPOTH HC II"="KOPOTH HC III",
                                     "KORO ABILI HC II"="KORO ABILI HC II",
                                     "KORO HC III"="KORO HEALTH CENTRE HC III",
                                     "KORTEK HC III"="KORTEK HC III",
                                     "KOSIKE HC II"="KOSIKE HC III",
                                     "KOSIROI HC II"="KOSIROI HC II",
                                     "KOTIDO GENERAL HOSPITAL"="KOTIDO HOSPITAL",
                                     "KOTIDO POLICE HC II"="KOTIDO POLICE HC II",
                                     "KOTIDO PRISON"="KOTIDO PRISONS HC II",
                                     "KOYA HC II"="KOYA HC II",
                                     "KRISTINA HC II"="KRISTINA HC III",
                                     "KUCWINY HC III"="KUCWINY HC III",
                                     "KULIKULINGA HC III"="KULIKULINGA HC III",
                                     "KULUBA HC II"="KALUBA HC II",
                                     "KULUVA HOSPITAL"="KULUVA HOSPITAL",
                                     "KUMI HC IV"="KUMI HC IV",
                                     "KUMI HOSPITAL"="KUMI (ONGINO) HOSPITAL",
                                     "KUNGU HC III"="KUNGU HC II",
                                     "KUREKU HC II"="KUREKU HC II",
                                     "KWANYINY HC III"="KWANYIY HC III",
                                     "KWAPA HC III"="KWAPA HC III",
                                     "KWERA HC III"="KWERA HC III",
                                     "KWIRWOT HC II"="KWIRWOT HC II",
                                     "KWONKIC HCII"="KWONKIC HC II",
                                     "KWOTI HC II"="KWOTI HC II",
                                     "KYABADAAZA HC III"="KYABADAZA HC III",
                                     "KYABAKARA HC II"="KYABAKARA HC III",
                                     "KYABASAIJA HC III"="KYABASAIJA HC III",
                                     "KYABASALA HC II"="KYABASARA HC II",
                                     "KYABAZAALA  HC III"="KYABAZAALA HC III",
                                     "KYABENDA HC III"="KYABENDA HC III",
                                     "KYABIRUKWA HC III"="KYABIRUKWA HC III",
                                     "KYABUGIMBI HC IV"="KYABUGIMBI HC IV",
                                     "KYADONDO MEDICAL CENTRE"="KYADONDO MEDICAL CENTRE HC IV",
                                     "KYAKABADIIMA HC II"="KYAKABADIIMA HC II",
                                     "KYAKARAFA HCII"="KYAKARAFA HC II",
                                     "KYAKATARA HC III"="KYAKATARA HC III",
                                     "KYAKIDDU HC II"="KYAKIDDU HC II",
                                     "KYAKUTEREKERA"="KYAKUTEREKERA HC II",
                                     "KYAKUTEREKERA HC III"="KYAKUTEREKERA HC III",
                                     "KYALUGONDO HC III"="KYALUGONDO HC III",
                                     "KYALULANGIRA HC III"="KYALULANGIRA HC III",
                                     "KYAMAGANDA HCIII"="KYAMAGANDA HC III",
                                     "KYAMBOGO UNIVERSITY MEDICAL CENTRE"="KYAMBOGO UNIVERSITY MEDICAL CENTRE HC II",
                                     "KYAMPISI HC III"="KYAMPISI HC III",
                                     "KYAMUHUNGA HC III"="KYAMUHUNGA HC III",
                                     "KYAMULIBWA HC III"="KYAMULIBWA HC III",
                                     "KYAMULIBWA HC IV"="ST. JOSEPH OF THE GOOD SHEPHERD KYAMULIBWA HC IV",
                                     "KYAMULIIBWA MRC"="MRC KYAMULIBWA HC II",
                                     "KYAMUSISI HC III"="KYAMUSISI HC III",
                                     "KYAMWINULA HC III"="KYAMWINULA HC II",
                                     "KYANAMUGERA HC II"="KYANAMUGERA HC II",
                                     "KYANAMUKAKA HC IV"="KYANAMUKAAKA HC IV",
                                     "KYANAMUYOJO HC III"="KYANAMUYONJO HC III",
                                     "KYANGATO HC II"="KYANGATO HC II",
                                     "KYANGWALI HC IV"="KYANGWALI HC IV",
                                     "KYANGYENYI HC III"="KYANGYENYI HC III",
                                     "KYANKARAMATA HC II"="KYANKARAMATA HC III",
                                     "KYANKWANZI HC III"="KYANKWANZI HC III",
                                     "KYANTUNGO HC IV"="KYANTUNGO HC IV",
                                     "KYANYA HC III"="KYANYA SDA HC II",
                                     "KYARUMBA GOVT HC III"="KYARUMBA HC III",
                                     "KYARUMBA HC III (GOVERNMENT)"="KYARUMBA HC III",
                                     "KYARUMBA PHC III"="KYARUMBA PHC HC III",
                                     "KYARUSOZI HC IV"="KYARUSOZI HC IV",
                                     "KYASANSUWA HCIII"="KYASANSUWA HC II",
                                     "KYATEREKERA HC III"="KYATEREKERA HC III",
                                     "KYATIRI HC III"="KYATIRI HC III",
                                     "KYATO HC II"="KYATO HC II",
                                     "KYAYI HC III"="KYAYI HC III",
                                     "KYAZANGA HC IV"="KYAZANGA HC IV",
                                     "KYEBANDO HC III"="KYEBANDO HC III",
                                     "KYEBE HC III"="KYEBE HC III",
                                     "KYEEYA HC111"="KYEEYA HC II",
                                     "KYEGEGWA HC IV"="KYEGEGWA HOSPITAL",
                                     "KYEHORO HC III"="KYEHORO HC III",
                                     "KYEIRUMBA HC III"="KYEIRUMBA HC III",
                                     "KYEIZOOBA HC III"="KYEIZOOBA HC III",
                                     "KYEMBOGO HC III"="KYEMBOGO HOLY CROSS HC III",
                                     "KYEMPANGO HCIII"="KYEMPANGO HC III",
                                     "KYENGANDO"="KYENGANDO HC II",
                                     "KYENGERA HC II"="KYENGERA HC III",
                                     "KYENGEZA HC II"="KYENGEZA HC II",
                                     "KYENJOJO HOSPITAL"="KYENJOJO HOSPITAL",
                                     "KYENJOJO PRISON HC"="KYENJOJO PRISON HC II",
                                     "KYENZAZA HC II"="KYENZAZA HC III",
                                     "KYERE HC III"="KYERE HC III",
                                     "KYERE MISSION HC III"="KYERE HC III",
                                     "KYETUME COMMUNITY BASED HC III (MUKONO)"="KYETUME CBHC HC IV",
                                     "KYETUME HC III (LWENGO)"="KYETUME HC III",
                                     "KYETUME KATOSI HC III (MUKONO)"="KYETUME CBHC HC IV",
                                     "KYOGA HCII"="KYOGA HC II",
                                     "KYONDO HC III"="KYONDO HC III",
                                     "KYOTERA MEDICAL CENTRE"="KYOTERA MED. CENTRE HC III",
                                     "LABONGOGALI HC III"="LABONGOGALI HC III",
                                     "LACOR HC III (AMURU)"="AMURU LACOR HC III",
                                     "LACOR HC III( PABBO)"="LACOR-PABBO HC III",
                                     "LACOR HEALTH CENTRE III OPIT"="LACOR OPIT HC III",
                                     "LAGOT HC II"="LAGOT HC II",
                                     "LAGUTI HC III"="LAGUTI HC III",
                                     "LAKE MBURO HC III"="KANYARYERU (LAKE MBURO) HC III",
                                     "LAKONY MEDICAL CENTER"="LAKONY MEDCAL CENTRE",
                                     "LAKWATOMER HC II"="LAKWATOMER HC II",
                                     "LALLE HC II"="LALLE HC II",
                                     "LALOGI  HC IV"="LALOGI HC IV",
                                     "LAMA"="LAMA HC III",
                                     "LAMBU HC II"="LAMBU HC II",
                                     "LAMEZIA"="LAMEZIA HC III",
                                     "LAMIYO HC III"="LAMIYO HC II",
                                     "LAMWO PRISON HCII"="LAMWO PRISON HC II",
                                     "LANENEOBER HC III"="LANENOBER HC III",
                                     "LANGOL HC II"="LANGOL HC II",
                                     "LAPAINAT HC III"="LAPAINAT HC III",
                                     "LAPIRIN HC III"="LAPIRIN HC III",
                                     "LAPUL HC III"="LAPUL HC III",
                                     "LAROPI HC III"="LAROPI HC III",
                                     "LASANGA HEALTH CENTRE III"="LASANGA HC III",
                                     "LATANYA HC III"="LATANYA HC II",
                                     "LATORO HC II (NWOYA)"="LATORO HC II",
                                     "LAYITA HCIII"="LAYITA HC II",
                                     "LEFORI HC III"="LEFORI HC III",
                                     "LELAOBARO HC II"="LELA-OBARO HC II",
                                     "LEMUSUII HC III"="LEMUSUI HC III",
                                     "LIGILIGI HCII"="LIGILIGI HC II",
                                     "LIMOTO HC III"="LIMOTO HC II",
                                     "LIRA  KATO HC III (  AGAGO )"="LIRA KATO HC III",
                                     "LIRA MEDICAL CENTRE"="LIRA MEDICAL CENTRE LTD HC III",
                                     "LIRA MILITARY BARRACKS HC III"="LIRA MILITARY BARRACKS HC III",
                                     "LIRA PALWO HC III"="LIRA PALWO HC III",
                                     "LIRA REGIONAL REFERRAL HOSPITAL"="LIRA REGIONAL REFERRAL HOSPITAL",
                                     "LIRA UNIVERSITY HOSPITAL"="LIRA UNIVERSITY HOSPITAL",
                                     "LIVING WATER HC"="LIVING WATER COMMUNITY MEDICAL CENTRE CLINIC",
                                     "LOBALANGIT HC II"="LOBALANGIT HC III",
                                     "LOBO  ROM HC III"="LOBOROM HC III",
                                     "LOBULE HC III"="LOBULE HC III",
                                     "LOCHOM HC II"="LOCHOM HC II",
                                     "LODONGA HC III"="LODONGA HC IV",
                                     "LOGIRI HC III"="LOGIRI HC III",
                                     "LOGOBA HC III"="LOGOBA HC III",
                                     "LOKALES HC II"="LOKALES HC II",
                                     "LOKANAYONA HC II"="LOKANAYONA HC II",
                                     "LOKERUI HC II"="LOKERUI HC II",
                                     "LOKIDING HC/II"="LOKIDING HC II",
                                     "LOKITEDED HC II"="LOKITEDED HC II",
                                     "LOKITELAEBU HC III"="LOKITELAEBU HC III",
                                     "LOKOLIA HC II"="LOKOLIA HC III",
                                     "LOKOPO HC III"="LOKOPO HC III",
                                     "LOKORI HC II"="LOKORI HC II",
                                     "LOKUNG HC III"="LOKUNG HC III",
                                     "LOKWAKARAMOE HC II"="LOKWAKARAMOE HC II",
                                     "LOLACHAT HC III"="LOLACHAT HC III",
                                     "LOMERIS HC II"="LOMERIS HC II",
                                     "LOMODOCH HC II"="LOMODOCH HC II",
                                     "LOMORUNYANGAE HCII"="LOMORUNYANGAE HC II",
                                     "LOMUNGA HC II"="LOMUNGA HC II",
                                     "LOOKOROK HC II"="LOOKOROK HC II",
                                     "LOPEEI HC III"="LOPEEI HC III",
                                     "LOPELIPEL"="LOPELIPEL HC II",
                                     "LOPUTUK HC III"="LOPUTUK HC III",
                                     "LOPUYO HCII"="LOPUYO HC II",
                                     "LORENGECHORA HC III"="LORENGECHORA HC III",
                                     "LORENGEDWAT HC III"="LORENGEDWAT HC III",
                                     "LORO HC III"="LORO HC III",
                                     "LORO PRISON  HC II"="LORO PRISON HC II",
                                     "LOROO HC III"="LOROO HC III",
                                     "LOSAKUCHA HCII"="LOSAKUCHA HC II",
                                     "LOSILANG HC II"="LOSILANG HC II",
                                     "LOTIM HC II"="LOTIM HC II",
                                     "LOTIRIR"="LOTIRIR HC II",
                                     "LOTOME HC III"="LOTOME HC III",
                                     "LOYO AJONGA HC III"="LOYOAJONGA HC III",
                                     "LOYORO HC II"="LOYORO HC III",
                                     "LUBAGA HOSPITAL"="LUBAGA HOSPITAL",
                                     "LUBYA"="LUBYA HC II",
                                     "LUDARA HC III"="LUDARA HC III",
                                     "LUFUKA VALLEY HEALTH CENTRE III"="LUFUKA VALLEY HC III",
                                     "LUGASA HC III"="LUGASA HC III",
                                     "LUGAZI  HC II"="LUGAZI II HC II",
                                     "LUGAZI MUSLIM HC II"="LUGAZI MUSLIM HC III",
                                     "LUJJABWA HC II"="LUJJABWA ISLAND HC II",
                                     "LUJORONGOLE HC II"="LUJORONGOLE HC II",
                                     "LUKAALE"="LUKALE HC II",
                                     "LUKAYA HC III"="LUKAYA HC III",
                                     "LUKAYA HEALTH CARE CENTRE"="LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
                                     "LUKOLE HC III"="LUKOLE HC III",
                                     "LUKOLO HC III"="LUKOLO HC III",
                                     "LUKWOR HCII"="LUKWOR HC II",
                                     "LULAGALA HC III"="LULAGALA HC III",
                                     "LULAMBA HC III"="LULAMBA HC III",
                                     "LULYAMBUZI HC III"="LULYAMBUZI HC III",
                                     "LULYANGO HC II"="LULYANGO HC II",
                                     "LUMINO HC III"="LUMINO HC III",
                                     "LUNYO HC III"="LUNYO HC III",
                                     "LURU"="LURU HC III",
                                     "LURUJO HC II"="LURUJO HC III",
                                     "LUSANGYA HC II"="ST. MATIA LUSANJA HC II",
                                     "LUTEETE HC III (LUWERO)"="LUTEETE HC II",
                                     "LUWERO GENERAL HOSPITAL"="LUWERO HOSPITAL",
                                     "LUWERO INDUSTRIES HC III"="LUWERO INDUSTRIES LTD CLINIC",
                                     "LUWUNGA HC III"="LUWUNGA BARRACKS HC III",
                                     "LUYITAAYITA HC III (NGO)"="LUYITAYITA HC III",
                                     "LUZIRA HC III"="LUZIRA HEALTH CENTRE HC III",
                                     "LWAKALOOLO HC II"="LWAKALOLO HC II",
                                     "LWALA HOSPITAL"="LWALA HOSPITAL",
                                     "LWAMAGGWA HC III"="LWAMAGGWA GOVT HC III",
                                     "LWAMATA HC III"="LWAMATA HC III",
                                     "LWAMPANGA HC III"="LWAMPANGA HC III",
                                     "LWANDA HC III"="LWANDA HC III",
                                     "LWANGOLI HC III"="LWANGOLI HC III",
                                     "LWANJUSI HC III"="LWANJUSI HC III",
                                     "LWANKONI HC III"="LWANKONI HC III",
                                     "LWEBITAKULI HC III"="LWEBITAKULI NGO HC III",
                                     "LWEMIKOMAGO HCIII"="LWEMIKOMAGO HC III",
                                     "LWEMIYAGA HC III"="LWEMIYAGA HC III",
                                     "LWENGO HC IV"="LWENGO HC IV",
                                     "LYAKAJULA HC III (LYANTODE)"="LYAKAJURA HC III",
                                     "LYAMA HC III"="LYAMA HC III",
                                     "LYANTONDE HOSPITAL"="LYANTONDE HOSPITAL",
                                     "LYANTONDE MUSLIM HC III"="LYANTONDE MUSLIM HC III",
                                     "MAAJI A HC II"="MAAJI A HC II",
                                     "MAAJI C HCII"="MAAJI C HC II",
                                     "MAAMA  MARIA"="MAAMA MARIA MATERNITY CENTRE HC III",
                                     "MAANYI HC III"="MAANYI HC III",
                                     "MABAALE HC III"="MABAALE HC III",
                                     "MABALE(KYENJOJO) HC II"="MABALE TEA FACTORY HC II",
                                     "MABONA HC III"="MABONA HC III",
                                     "MADDU HC IV"="MADDU HC IV",
                                     "MADI KILOC HCII"="MADI KILOCH HC II",
                                     "MADI OPEI HC IV"="MADI-OPEI HC IV",
                                     "MADUDU HC III"="MADUDU HC III",
                                     "MAGADA HCII"="MAGADA HC II",
                                     "MAGALA HC III"="MAGALA HC III",
                                     "MAGAMAGA ARMY HC III (MAYUGE)"="MAGAMAGA BARRACKS HC III",
                                     "MAGAMAGA HC III"="MAGAMAGA HC III",
                                     "MAGANJO"="MAGANJO HC II",
                                     "MAGOGGO HC II"="MAGOGGO HC II",
                                     "MAGORO HC III"="MAGORO HC III",
                                     "MAHANGO HC III"="MAHANGO HC III",
                                     "MAHEGA HCII"="MAHEGA HC II",
                                     "MAHYORO HC III"="MAHYORO HC III",
                                     "MAINA HCII"="MAINA HC II",
                                     "MAISUKA HC III"="MAISUKA HC III",
                                     "MAJANJI HCIII"="MAJANJI HC II",
                                     "MAKENKE HC IV (UPDF)"="UPDF 2ND DIV. HC IV",
                                     "MAKHONJE HC III"="MAKHONJE HC III",
                                     "MAKINDU HC III"="MAKINDU HC III",
                                     "MAKOKOTO HCIII"="MAKOKOTO HC II",
                                     "MAKONDO HC II"="MAKONDO HC II",
                                     "MAKONGE HC III"="MAKONGE HC III",
                                     "MAKONZI HC III"="MAKONZI HC II",
                                     "MAKOOLE"="MAKOOLE HC II",
                                     "MAKUKUULU HC III"="MAKUKUULU HC III",
                                     "MAKULUBITA HC III"="MAKULUBITA HC III",
                                     "MALABA HC III"="MALABA HC III",
                                     "MALANGALA HC III"="MALANGALA HC III",
                                     "MALERA HC III"="MALERA HC III",
                                     "MALERE HCII"="MALERE HC II",
                                     "MALIBA HC III"="MALIBA HC III",
                                     "MALONGO HC III"="MALONGO HC III",
                                     "MALUKHU HC III"="MALUKHU HC III",
                                     "MAMBA HC II"="MAMBA HC III",
                                     "MANTOROBA HC II"="MANTOROBA HC II",
                                     "MARACHA HC IV"="MARACHA HC IV",
                                     "MARACHA HOSPITAL (ST JOSEPH)"="ST. JOSEPHS MARACHA HOSPITAL",
                                     "MARANATHA HC II"="MARANATHA HC III",
                                     "MARATATU B"="MARATATU HC III",
                                     "MARIA ASSUMPTA"="MARIA ASSUMPTA HC III",
                                     "MARINE MILITARY HC II"="MARINE MILITARY HC III",
                                     "MARY IMMACULATE HC II"="MARY IMMACULATE HC II",
                                     "MARY LAND HC III"="MARYLAND KOCOA HC III",
                                     "MASAFU HOSPITAL"="MASAFU HOSPITAL",
                                     "MASAFU HOSPITAL HUB"="MASAFU HOSPITAL",
                                     "MASAKA ARMOURED BRIGADE HC III (A/BDE)"="ARMOURED BRIGADE HC III",
                                     "MASAKA HC III"="KAKUMIRO - MASAKA HC III",
                                     "MASAKA POLICE CLINIC"="MASAKA POLICE HC III",
                                     "MASAKA PRISION HC III"="MASAKA PRISONS HC III",
                                     "MASAKA REGIONAL REFERRAL HOSPITAL"="MASAKA REGIONAL REFERRAL HOSPITAL",
                                     "MASINDI HOSPITAL"="MASINDI HOSPITAL",
                                     "MASINDI KITARA HOSPITAL"="MASINDI KITARA MED.  CENTRE HC IV",
                                     "MASINDI PORT HC III"="MASINDI PORT HC III",
                                     "MASINDI PRISON HC III"="MASINDI MAIN PRISON HC III",
                                     "MASIRA HC III"="MASIRA HC III",
                                     "MASIYOMPO HC III"="MASIYOMPO HC III",
                                     "MASOLYA HC/III"="MASOLYA HC III",
                                     "MATALE"="MATALE HC III",
                                     "MATANDA HC III"="MATANDA HC III",
                                     "MATANY HOSPITAL"="MATANY HOSPITAL",
                                     "MATEETE HC III"="MATEETE HC III",
                                     "MATUGA HCII"="MATUGA HC II",
                                     "MATUMA HC III"="MATUMA HC III",
                                     "MAWUJJO HC II"="MAWUJJO HC II",
                                     "MAWUKI HC II"="MAWUKI HC II",
                                     "MAYIRYE HC III"="ARCH BISHOP KIWANUKA MAYIRYE HC III",
                                     "MAYUGE HCIII"="BUGIRI MAYUGE HC III",
                                     "MAZINGA HC III"="MAZINGA HC III",
                                     "MAZIRIGA HC II"="MAZIRIGA HC II",
                                     "MBAARE HC III"="MBAARE HC III",
                                     "MBALE HCII (KYENJOJO)"="MBALE HC II",
                                     "MBALE MAIN PRISONS HC III"="MBALE MAIN PRISONS HC III",
                                     "MBALE MUNICIPAL HCII"="MBALE MUNICIPAL HC II",
                                     "MBALE PEOPLES HOSPITAL"="MBALE PEOPLE'S CLINIC",
                                     "MBALE POLICE HC III"="MBALE POLICE HC III",
                                     "MBALE REGIONAL REFERRAL HOSPITAL"="MBALE REGIONAL REFERRAL HOSPITAL",
                                     "MBARARA POLICE HC III"="MBARARA POLICE HC III",
                                     "MBARARA REGIONAL REFERRAL HOSPITAL"="MBARARA REGIONAL REFERRAL HOSPITAL",
                                     "MBAYA HC III"="MBAYA HC III",
                                     "MBEHENYI HC III"="MBEHENYI HC III",
                                     "MBIRIZI MOSLEM HC III"="MBIRIZI MUSLIM HC III",
                                     "MBULAMUTI HC III"="MBULAMUTI HC III",
                                     "MBUYE HC III"="MBUYE HC II",
                                     "MEETING POINT KAMPALA"="MEETING POINT KAMPALA HC III",
                                     "MELLA HC III"="MELLA HC III",
                                     "MENDE HC III"="MENDE HC III",
                                     "MENGO HOSPITAL-EMTCT"="MENGO HOSPITAL",
                                     "MERIKIT HC III"="MERIKIT HC III",
                                     "METHA HOSPITAL"="MEHTA HOSPITAL SCOUL",
                                     "METU HC III"="METU HC III",
                                     "MIDIGO HC IV"="MIDIGO HC IV",
                                     "MIGADDE HCII"="MIGADDE HC II",
                                     "MIGAMBA HC II"="MIGAMBA HC II",
                                     "MIGONGWE HC II"="MIGONGWE HC II",
                                     "MIHENBERO HC II"="MIHEMBERO HC II",
                                     "MILLITARY POLICE HC III (MAKINDYE)"="MAKINDYE POLICE HC III",
                                     "MINAKULU HC II"="MINAKULU HC III",
                                     "MINAKULU HC III (OYAM)"="MINAKULU HC III (OYAM)",
                                     "MINISTRY OF DEFENCE CLINIC(MBUYA)"="MBUYA MILITARY CLINIC (MOD GARRISON)",
                                     "MIRAMBI HC III"="MIRAMBI HC III",
                                     "MITALA MARIA HC III"="MITALA-MARIA HC III",
                                     "MITANDI HC III (NGO)"="MITANDI HC III",
                                     "MITOOMA HC IV"="MITOOMA HC IV",
                                     "MITUKULA HC III"="MITUKULA HC III",
                                     "MITYANA HC III (UMSC)"="MITYANA UMSC CLINIC HC III",
                                     "MITYANA HOSPITAL"="MITYANA HOSPITAL",
                                     "MJAP-MMC  HC IV"="MULAGO NRH - MJAP ISS CLINIC",
                                     "MJAP-MULAGO"="KIRUDDU NRH - MJAP (CDC) MULAGO",
                                     "MOCHA HC III"="MOCHA HC III",
                                     "MOLO HC III"="MOLO HC III",
                                     "MOROTO ARMY HC IV"="ARMY BARRACKS HC III",
                                     "MOROTO POLICE HC II"="MOROTO POLICE HC II",
                                     "MOROTO PRISONS"="MOROTO PRISONS HC II",
                                     "MOROTO REGIONAL REFERRAL HOSPITAL"="MOROTO REGIONAL REFERRAL HOSPITAL",
                                     "MORUITA HC III"="MORUITA HC II",
                                     "MORULEM HC III"="MORULEM HC III",
                                     "MORULINGA HCII"="MORULINGA HC II",
                                     "MORUNGATUNY HC III"="MORUNGATUNY HC III",
                                     "MOTHER ANGIOLETTA HCII"="MOTHER ANGIOLETTA MEMORIAL CLINIC",
                                     "MOTHER INTERNATIONAL YALAYALA - ATIAK"="MOTHER HEALTH INTERNATIONAL HC II",
                                     "MOYO GENERAL HOSPITAL"="MOYO HOSPITAL",
                                     "MOYO HOSPITAL"="MOYO HOSPITAL",
                                     "MOYO MISSION HC III"="MOYO MISSION HC IV",
                                     "MPAMBWE HC III"="MPAMBWA HC III",
                                     "MPARA HC III"="MPARA HC III",
                                     "MPARANGASI HC III"="MPARANGASI HC III",
                                     "MPASAANA HC II"="MPASAANA HC II",
                                     "MPEEFU HC III"="MPEEFU B HC III",
                                     "MPENJA HC III"="MPENJA HC III",
                                     "MPIGI HC IV"="MPIGI HC IV",
                                     "MPONGI HC III"="MPONGI HC III",
                                     "MPONGO HC II"="MPONGO HC II",
                                     "MPUGWE HC III"="MPUGWE HC III",
                                     "MPUMUDDE HC III (LYANTONDE)"="MPUMUDDE HC III",
                                     "MPUMUDDE HC IV (JINJA)"="MPUMUDDE HC IV",
                                     "MPUMWE HC II"="MPUMWE HC II",
                                     "MPUNGE HC III"="MPUNGE HC III",
                                     "MPUNGU HC III (KANUNGU)"="MPUNGU HC III",
                                     "MT.ST.MARY'S HOSPITAL"="MT. ST. MARY'S HOSPITAL-DOK",
                                     "MUBANDA HC111"="MUBANDA HC III",
                                     "MUBENDE REGIONAL REFERRAL HOSPITAL"="MUBENDE REGIONAL REFERRAL HOSPITAL",
                                     "MUBENDE REHABILITATION CENTRE"="MUBENDE REHABILITATION CENTRE HC III",
                                     "MUBENDE TOWN COUNCIL HCII"="MUBENDE TOWN COUNCIL HC II",
                                     "MUBUKU PRISON HCII"="MUBUKU PRISON HC II",
                                     "MUCWA HC II"="MUCWA HC III",
                                     "MUCWINI HC III"="MUCWINI HC III",
                                     "MUDAKORI HC III"="MUDAKOR HC III",
                                     "MUDUUMA HC III"="MUDUUMA HC III",
                                     "MUGALIKE HC II"="MUGALIKE HC II",
                                     "MUGALIKE HC III"="MUGALIKE HC II",
                                     "MUGARAMA"="MUGARAMA HC III",
                                     "MUGGI HCII"="MUGGI HC II",
                                     "MUGITI HC III"="MUGITI HC III",
                                     "MUGOYE HC III"="MUGOYE HC III",
                                     "MUGUSU HC III"="MUGUSU HC III",
                                     "MUHOKYA HC III"="MUHOKYA HC III",
                                     "MUHORO HC III"="MUHORRO NGO HC III",
                                     "MUHORRO GOV'T"="MUHORRO GVT HC II",
                                     "MUHOTI MILITARY HC III"="MUHOOTI BARRACCKS HC III",
                                     "MUHWIJU HC III"="MUHWIJU HC III",
                                     "MUJUNZA HC II"="MUJUNZA HC III",
                                     "MUKABARA HC III"="MUKABARA HC III",
                                     "MUKATHI HC III"="MUKATHI HC III",
                                     "MUKONDO HC II"="MUKONDO HC II",
                                     "MUKONGORO HC III"="MUKONGORO HC III",
                                     "MUKONO C.O.U HOSPITAL"="MUKONO COU HOSPITAL",
                                     "MUKONO GENERAL HOSPITAL"="MUKONO GENERAL HOSPITAL",
                                     "MUKUJU HC IV"="MUKUJU HC IV",
                                     "MUKURA HC III"="MUKURA HC III",
                                     "MULABANA HC II"="MULABANA HC II",
                                     "MULAGI HC III"="MULAGI HC III",
                                     "MULANDA HC IV"="MULANDA HC IV",
                                     "MULTICARE MEDICAL CENTRE"="MULTICARE MEDICAL CENTRE HC II",
                                     "MUNATHAMATI"="MUNATHAMAT HC II",
                                     "MUNDADDE HC III"="MUNDADDE HC II",
                                     "MUNGULA HC IV"="MUNGULA HC IV",
                                     "MUNOBWA CLINIC"="MUNOBWA-HIIMA TEA FACTORY HC II",
                                     "MUNYONYI"="MUNYONYI HC III",
                                     "MURAMBA HC III"="MURAMBA HC III",
                                     "MURCHISON BAY HOSPITAL"="MURCHISION BAY MAIN HOSPITAL",
                                     "MURUBA"="MURUBA HC II",
                                     "MUSANDAMA HC II"="MUSANDAMA HC II",
                                     "MUSHANGA HC III"="MUSHANGA HC III",
                                     "MUSHUMBA HC II"="MUSHUMBA HC II",
                                     "MUSOZI HC III"="MUSOZI HC III",
                                     "MUSYENENE HC III"="MUSHENENE HC III",
                                     "MUTARA HC III"="MUTARA HC III",
                                     "MUTERERE HCIII"="MUTERERE HC III",
                                     "MUTUFU HC III"="MUTUFU HC II",
                                     "MUTUKULA HC III"="MUTUKULA HC III",
                                     "MUTUMBA HC III"="MUTUMBA HC III",
                                     "MUTUNDA HC III"="MUTUNDA HC III",
                                     "MUTUNGO HC II"="MUTUNGO HC II",
                                     "MUTUSHET HC II"="MUTUSHET HC II",
                                     "MUWANGA HC III"="MUWANGA HC III",
                                     "MUWRP CLINIC"="MAKERERE UNIVERSITY WALTER REED CLINIC HC II",
                                     "MUWUMBA HC III"="MUWUMBA HC III",
                                     "MUYEMBE  HC IV"="MUYEMBE HC IV",
                                     "MUZIIZI HC II"="MUZIIZI TEA ESTATE HC II",
                                     "MWENGE ESTATE CLINIC"="MWENGE CLINIC HC III",
                                     "MWENGE HC III"="MWENGE CLINIC HC III",
                                     "MWERA HC IV"="MWERA HC IV",
                                     "MWITANZINGE"="MWITANZIGE HC III",
                                     "MWIZI HC III"="MWIZI HC III",
                                     "MYANZI HC III (MUBENDE)"="MYANZI HC III",
                                     "MYERI HC II (KYENJOJO)"="MYERI HC III",
                                     "NAAMA HC III"="NAAMA HC III",
                                     "NAANYWA HC III"="NANYWA HC III",
                                     "NABALANGA HC III"="NABALANGA HC III",
                                     "NABIGANDA HC III"="NABIGANDA HC IV",
                                     "NABIGASA HC III"="NABIGASA HC III",
                                     "NABIKAKALA HCII"="NABIKAKALA HC II",
                                     "NABILATUK HC IV"="NABILATUK HC IV",
                                     "NABILATUK MISSION HC II"="NABILATUK MISSION HC II",
                                     "NABINGO HC II"="NABBINGO HC II",
                                     "NABINGOOLA HC III"="NABINGOOLA HC III",
                                     "NABIRAMA HC II"="NABIRAMA HC II",
                                     "NABIRUMBA HC III"="NABIRUMBA HC III",
                                     "NABISWERA HC III"="NABISWERA HC IV",
                                     "NABITSIKHI HC III"="NABITSIKHI HC III",
                                     "NABOA HC III"="NABOA HC III",
                                     "NABUKALU HC III"="NABUKALU HC III",
                                     "NABULENGER HC II"="NABULENGER HC II",
                                     "NABULEZI HC II"="NABULEZI HC II",
                                     "NABULI HC III"="NABULI HC III",
                                     "NABULOLA CMC HC III"="NABULOLA HC III",
                                     "NABUMALI HCII"="NABUMALI HC III",
                                     "NABUMALI HCIII"="NABUMALI HC III",
                                     "NABUTONGWA HC II"="NABUTONGWA HC II",
                                     "NABWAL HC II"="NABWAL HC II",
                                     "NABWENDO HC III"="NABWENDO HC III",
                                     "NABYEWANGA HC II"="NABYEWANGA HC II",
                                     "NADUNGET HC III"="NADUNGET HC III",
                                     "NAGOJJE HC III"="NAGGOJJE HC III",
                                     "NAGONGERA HC IV"="NAGONGERA HC IV",
                                     "NAGURU POLICE HC IV"="NAGURU POLICE HC IV",
                                     "NAGWERE HC III"="NAGWERE HC III",
                                     "NAIKU HEALTH CENTRE III"="NAIKU HC III",
                                     "NAJJEMBE HC III"="NAJJEMBE HC III",
                                     "NAKAALE HC II"="NAKAALE HC II",
                                     "NAKALOKE HC III"="NAKALOKE HC III",
                                     "NAKAPELIMEN HC II"="NAKAPELIMEN HC II",
                                     "NAKAPELIMORU HC III"="NAKAPELIMORU HC III",
                                     "NAKAPIRIPIRIT FIELD PRISON"="NAKAPIRIPIRIT PRISON HC II",
                                     "NAKAPIRIPIRIT HC III"="NAKAPIRIPIRIT HC III",
                                     "NAKASEETA HC II"="NAKASEETA (KASANGOMBE) HC II",
                                     "NAKASEKE  HOSPITAL"="NAKASEKE HOSPITAL",
                                     "NAKASOJJO HC II"="NAKASOJJO HC II",
                                     "NAKASONGOLA"="NAKASONGOLA HC IV",
                                     "NAKASONGOLA MILITARY HOSPITAL"="NAKASONGOLA MILITARY HOSPITAL",
                                     "NAKASONGOLA PRISON  HC III"="NAKASONGOLA PRISONS HC III",
                                     "NAKATITI"="NAKATITI HC II",
                                     "NAKATITI HC III"="NAKATITI HC II",
                                     "NAKATONYA HC III"="NAKATONYA HC III",
                                     "NAKATOVU HC II"="NAKATOVU HC II",
                                     "NAKAWUKA HC III"="NAKAWUKA HC III",
                                     "NAKAYONZA HC III"="NAKAYONZA  HC III",
                                     "NAKICHUMET HC II"="NAKICHUMET HC II",
                                     "NAKIFUMA HC III"="NAKIFUMA HC III",
                                     "NAKILORO HC II"="NAKILORO HC II",
                                     "NAKITEMBE HC II"="NAKITEMBE HC II",
                                     "NAKITOKOLO HC III"="NYAKITOKOLI HC II",
                                     "NAKITOMA HC III"="NAKITOMA HC III",
                                     "NAKIVALE HC III"="NAKIVALE HC III",
                                     "NAKWAKWA HCII"="NAKWAKWA HC II",
                                     "NAKWASI HC III"="NAKWASI HC III",
                                     "NALINYA NDAGIRE HC III"="NALINYA NDAGIRE HC III",
                                     "NALUTUNTU HCIII"="NALUTUNTU HC III",
                                     "NALWEYO HC III"="NALWEYO HC III",
                                     "NAMABEYA HC II"="NAMABEYA HC II",
                                     "NAMAGGWA CLINIC"="NAMAGGWA MATERNITY CLINIC HC II",
                                     "NAMAITSU HC II"="NAMAITSU HC II",
                                     "NAMAKWEKWE HC III"="NAMAKWEKWE HC III",
                                     "NAMALU HC III"="NAMALU HC III",
                                     "NAMALU PRISON HC II"="NAMALU PRISONS HC II",
                                     "NAMANYONYI HC III"="NAMANYONYI HC III",
                                     "NAMASAGALI HC III"="NAMASAGALI HC III",
                                     "NAMASALE HC III"="NAMASALE HC III",
                                     "NAMATALA HC IV"="NAMATALA HC IV",
                                     "NAMATALE"="NAMATALE HC III",
                                     "NAMAWANGA HC III"="NAMAWANGA HC III",
                                     "NAMAYUMBA HC IV"="NAMAYUMBA HC IV",
                                     "NAMBIESO HC III"="NAMBIESO HC III",
                                     "NAMENDERA HC II"="NAMENDERA HC II",
                                     "NAMENGO HC III"="NAMENGO HC III",
                                     "NAMINAGE HC III"="NAMINAGE HC II",
                                     "NAMINYA HC II"="NAMINYA HC II",
                                     "NAMISAMBYA"="NAMISAMBYA HC II",
                                     "NAM-OKORA HC IV"="NAMOKORA HC IV",
                                     "NAMPANGA HC II"="NAMPANGA HC II",
                                     "NAMPUNGE CHURCH OF GOD HC III"="NAMPUNGE HC II",
                                     "NAMUGANGA HC II"="NAMUGANGA HC III",
                                     "NAMUGONGO FUND FOR SPECIAL CHILDREN"="NAMUGONGO FUND FOR SPECIAL CHILDREN CLINIC",
                                     "NAMUNGO HC II"="NAMUNGO HC III",
                                     "NAMUNINGI HC II"="NAMUNYINGI HC II",
                                     "NAMUSITA"="NAMUSITA (NAMUTUMBA) HC II",
                                     "NAMUTAMBA HC III  COU  (MITYANA)"="NAMUTAMBA HC III",
                                     "NAMUUSALE HC II"="NAMUNSALA HC II",
                                     "NAMWENDWA HC IV"="NAMWENDWA HC IV",
                                     "NANKANDULO HC IV"="NANKANDULO HC IV",
                                     "NANKOMA HC IV"="NANKOMA HC IV",
                                     "NANSANGA"="NANSANGA HC III",
                                     "NANYONIA ANGIKALIO HC III"="NAYONAI ANGIKALIO HC II",
                                     "NAPUMPUM HC III"="NAPUMPUM HC II",
                                     "NARENGEPAK H//C II"="NARENGEPAK HC II",
                                     "NASSOLO WAMALA"="NASSOLO WAMALA HC II",
                                     "NATIRAE HC II"="NATIRAE HC II",
                                     "NATTYOLE HC III"="ST. KIZITO NATYOLE HC III",
                                     "NATURUMRUM HC II"="NATURUMRUM HC II",
                                     "NAWANYAGO HC III"="NAWANYAGO HC III",
                                     "NAWEYO HC III"="NAWEYO HC III",
                                     "NAZIGO HC III"="NAZIGO HC III",
                                     "NAZIGO MISSION HC II"="NAZIGO MISSION HC II",
                                     "NDAIGA"="NDAIGA HC II",
                                     "NDANGARO"="NDAGARO HC III",
                                     "NDEIJA HC III"="NDEIJA HC III",
                                     "NDEJJE HC IV (WAKISO)"="NDEJJE HC IV",
                                     "NDEJJE UNIVERSITY HC III"="NDEJJE UNIVERSITY HC II",
                                     "NEBBI HOSPITAL"="NEBBI HOSPITAL",
                                     "NEWLIFE HC III"="NEW LIFE HC III",
                                     "NGAI HC III"="NGAI HC III",
                                     "NGANDO HC III"="NGANDO HC III",
                                     "NGANGATA HC II"="NGANGATA HC II",
                                     "NGARAMA HC III"="NGARAMA HC III",
                                     "NGARIAM HC III"="NGARIAM HC III",
                                     "NGENGE HC II"="NGENGE HC III",
                                     "NGENGE HC III"="NGENGE HC III",
                                     "NGERIBALYA HC II"="NGERIBALYA HC II",
                                     "NGETTA HC III"="NGETTA HC III",
                                     "NGOGWE HC III"="NGOGWE HC III",
                                     "NGOLERIET HC II"="NGOLERIET HC II",
                                     "NGOMA HC III (NTUNGAMO)"="NGOMA HC II",
                                     "NGOMA HC IV (NAKASEKE)"="NGOMA HC IV",
                                     "NGOMANENE HC III"="NGOMANENE HC III",
                                     "NGOM-OROMO HC II"="NGOMOROMO HC II",
                                     "NGORA FREDA CAR HOSPITAL (NGO)"="NGORA FREDA CARR HOSPITAL",
                                     "NGORA HC IV"="NGORA HC IV",
                                     "NGORA MATERNITY UNIT HC III"="NGORA DISTRICT MATERNITY UNIT HC III",
                                     "NGORA PRISONS HC II"="NGORA PRISON HC II",
                                     "NGURWE"="NGURWE HC II",
                                     "NGURWE HC II"="NGURWE HC II",
                                     "NILE BREWERIES HC II"="NILE BREWERIES COMPANY CLINIC HC III",
                                     "NINDYE HC III"="NINDYE HC III",
                                     "NJERU T/C HC III"="NJERU TOWN COUNCIL HC III",
                                     "NKATA"="NKATA HC II",
                                     "NKOKONJERU  HC III (KAYUNGA)"="NKOKONJERU HC III",
                                     "NKOKONJERU HOSPITAL (BUIKWE)"="NKOKONJERU HOSPITAL",
                                     "NKOMBE HCIII"="NKOMBE (IMANYIRO) HC III",
                                     "NKONI HEALTH CENTER III"="NKONI HC III",
                                     "NKOOKO HC III"="NKOOKO HC III",
                                     "NKOZI HOSPITAL"="NKOZI HOSPITAL",
                                     "NKURUBA HC  III"="NKURUBA HC III",
                                     "NOAH'S ARK FAMILY"="NOAH'S ARK HC III",
                                     "NSAGGU  HC II"="NSAGGU HC II",
                                     "NSAMBYA HOME CARE"="NSAMBYA HOME CARE CLINIC",
                                     "NSAMBYA HOSPITAL"="ST. FRANCIS NSAMBYA HOSPITAL",
                                     "NSAMBYA POLICE HC IV"="NSAMBYA POLICE HC IV",
                                     "NSAMIZI III"="NZAMIZI HC III",
                                     "NSANGI HC III"="NSANGI HC III",
                                     "NSAWO HC III"="NSAWO HC III",
                                     "NSHUNGYEZI HC III"="NSHUNGYEZI HC III",
                                     "NSHWERE HC III"="NSHWERE HC III",
                                     "NSIIKA HC IV"="NSIIKA HC IV",
                                     "NSOZI HC III"="NSOZI HC III",
                                     "NSWANJERE HC III (ST JOSEPH)"="NSWANJERE HC III",
                                     "NTANDI HC III"="NTANDI HC III",
                                     "NTARA HC IV"="NTARA HC IV",
                                     "NTEETE HC II"="NTETE HC II",
                                     "NTEKO HC III"="NTEKO HC III",
                                     "NTENJERU HC III"="NTENJERU HC III",
                                     "NTENUGI HC II"="NTENUNGI HC II",
                                     "NTINKALU HCII"="NTINKALU HC II",
                                     "NTONWA HC II"="NTONWA HC II",
                                     "NTOOMA HC II"="NTOOMA HC II",
                                     "NTOROKO HC III (NTOROKO)"="NTOROKO HC III",
                                     "NTUNGAMO HC III"="NTUNGAMO HC II",
                                     "NTUNGU HC II"="NTUNGU HC II",
                                     "NTUUSI HC IV"="NTUUSI HC IV",
                                     "NTWETWE HC IV"="NTWETWE HC IV",
                                     "NYABBANI HC III"="NYABBANI HC III",
                                     "NYABIHUNIKO HC III"="NYABIHUNIKO HC III",
                                     "NYABIRONGO HC III"="NYABIRONGO HC III",
                                     "NYABUBARE HC III"="NYABUBARE HC III",
                                     "NYABUGANDO HC III"="NYABUGANDO HC III",
                                     "NYABUSWA HC III"="NYABUSWA HC III",
                                     "NYADRI HC III"="NYADRI HC III",
                                     "NYAHUKA HC IV"="NYAHUKA HC IV",
                                     "NYAKABANDE HC II"="NYAKABANDE HC II",
                                     "NYAKADOT HC III"="NYAKADOTI HC III",
                                     "NYAKAGYEME HC III"="NYAKAGYEME HC III",
                                     "NYAKARONGO  HC   II"="NYAKARONGO HC II",
                                     "NYAKASHASHARA HC III"="NYAKASHASHARA HC III",
                                     "NYAKAYOJO HC III"="NYAKAYOJO HC III",
                                     "NYAKIBALE HOSPITAL"="KAROLI LWANGA (NYAKIBALE) HOSPITAL",
                                     "NYAKIMASA"="NYAKIMASA HC II",
                                     "NYAKINAMA HC III"="NYAKINAMA HC III",
                                     "NYAKISHENYI HC III"="NYAKISHENYI HC III",
                                     "NYAKITIBWA HC III"="NYAKITIIBWA HC III",
                                     "NYAKITOKOLI HC III"="NYAKITOKOLI HC II",
                                     "NYAKITUNDA HC III"="NYAKITUNDA HC III",
                                     "NYAKWAE HC III"="NYAKWAE HC III",
                                     "NYAKYERA HC III"="NYAKYERA HC III",
                                     "NYAMABUGA HC III"="NYAMABUGA HC III",
                                     "NYAMAREBE HC III"="NYAMAREBE HC III",
                                     "NYAMARWA HC III (KIBAALE)"="NYAMARWA HC III",
                                     "NYAMIGISA HC II"="NYAMIGISA HC II",
                                     "NYAMIRAMI"="NYAMIRAMI HC IV",
                                     "NYAMIRANGA HC III"="NYAMIRINGA HC III",
                                     "NYAMITANGA HC III"="NYAMIRINGA HC III",
                                     "NYAMITYOBORA HC III"="NYAMITYOBORA HC II",
                                     "NYAMUYANJA HC IV"="NYAMUYANJA HC IV",
                                     "NYAMWEGABIRA HC II"="NYAMWEGABIRA HC III",
                                     "NYANKWANZI HC III"="NYANKWANZI HC III",
                                     "NYANTABOMA HC III"="NYANTABOMA HC III",
                                     "NYANTONZI HC III"="NYANTONZI HC III",
                                     "NYARAVUR HC III"="NYARAVUR HC III",
                                     "NYARUBUNGO HC III"="NYARUBUNGO HC III",
                                     "NYARUBUYE HC III"="NYARUBUYE HC III",
                                     "NYARUGOOTE HC II"="NYARUGOTE HC II",
                                     "NYARUHANDAGAZI HC IV"="NYARUHANDAGAZI HC III",
                                     "NYARUSIZA HC III"="NYARUSIZA HC III",
                                     "NYENDO HC II"="NYENDO HC II",
                                     "NYENGA HOSPITAL"="ST. FRANCIS NYENGA HOSPITAL",
                                     "NYERO HC III"="NYERO HC III",
                                     "NYIMBWA HC IV"="NYIMBWA HC IV",
                                     "NYONDO HC III"="NYONDO HC III",
                                     "NYUMANZI HC II"="NYUMANZI HC III",
                                     "NYUMANZI RECEPTION CENTER"="NYUMANZI HC III",
                                     "OBALANGA HC III"="OBALANGA HC III",
                                     "OBER HC III"="OBER HC III",
                                     "OBERABIC HC II"="OBERABIC HC II",
                                     "OBIM HC"="OBIM HC II",
                                     "OBOLOKOME HC II"="OBOLOKOME (LIRA) HC II",
                                     "OBONGI HC IV"="OBONGI HC IV",
                                     "OBURIN HC II"="OBURIN HC II",
                                     "OBUTETE HC II"="OBUTETE HC II",
                                     "OBYEN HC II"="OBYEN HC II",
                                     "OCEA HC II"="OCEA HC II",
                                     "OCHERO HC III"="OCHERO HC III",
                                     "OCOKICAN HC II"="OCOKICAN HC II",
                                     "ODEK HC III"="ODEK HC III",
                                     "ODOUBU HC III"="ODUOBU HC III",
                                     "ODUPI HC III"="ODUPI HC III",
                                     "OFFAKA HC III"="OFFAKA HC III",
                                     "OFUA HC III"="OFUA (OFUA) HC III",
                                     "OFUA MSF HC III"="OFUA (URIAMA) HC III",
                                     "OGAKO HC II"="OGAKO HC II",
                                     "OGOKO HC II"="OGOKO HC II",
                                     "OGOM HC III"="OGOM HC III",
                                     "OGONYO HC III"="OGONYO HC III",
                                     "OGUR HC IV"="OGUR HC IV",
                                     "OGWETE HC III"="OGWETE HC III",
                                     "OJE (MISSION) HC III"="OJE MISSION HC III",
                                     "OJOM HC II"="OJOM HC II",
                                     "OKIDI HC III"="OKIDI HC III",
                                     "OKINGA HC II"="OKINGA HC II",
                                     "OKOCHO HC II"="OKOOCHO HC II",
                                     "OKOL HCII"="OKOL HC II",
                                     "OKOLE HC III"="OKOLE HC II",
                                     "OKOLLO HC III"="OKOLLO HC III",
                                     "OKORITOK HC II"="KORITOK HC II",
                                     "OKUBANI HC III"="OKUBANI HEALTH CENTRE HC III",
                                     "OKWANG HC III"="OKWANG HC III",
                                     "OKWONGO HC III"="OKWONGO HC III",
                                     "OLADOT HC III"="OLADOT HCII",
                                     "OLD KAMPALA HOSPITAL"="OLD KAMPALA HOSPITAL",
                                     "OLEBA HC III"="OLEBA HC III",
                                     "OLEPEK HC II"="OLELPEK HC III",
                                     "OLI HC IV"="RIVER OLI HC IV",
                                     "OLILIM HC III"="OLILIM HC III",
                                     "OLIMAI COMMUNITY HC III"="OLIMAI HC III",
                                     "OLIVU HC III"="OLIVU HC II",
                                     "OLOKO HC III"="OLOK HC III",
                                     "OLUJOBO HC III"="OLUJOBO HC III",
                                     "OLUKO SOLIDALE HC III"="OLUKO SOLIDALE HC III",
                                     "OLUVU HC III"="OLUVU HC III",
                                     "OLWAL HC III"="OLWAL HC III",
                                     "OMAGORO HC III"="OMAGORO HC II",
                                     "OMARARI HC II"="OMARARI HC II",
                                     "OMATENGA HC II"="OMATENGA HC III",
                                     "OMBIDRI ONDREA HC III"="OMBIDRIONDREA HC III",
                                     "OMEL HC II"="OMEL HC II",
                                     "OMIITO HC II"="OMIITO HC II",
                                     "OMIYA ANYIMA HC III"="OMIYA ANYIMA HC III",
                                     "OMODOI HC II"="OMODOI HC II",
                                     "OMORO HC III"="OMORO HC III",
                                     "OMOT HCIII"="OMOT HC II",
                                     "OMUGO HC IV"="OMUGO HC IV",
                                     "ONGAKO HC III"="ONGAKO HC III",
                                     "ONGICA HC III"="ONGICA HC III",
                                     "ONGINO HC III"="ONGINO HC III",
                                     "ONGONGOJA HC II"="ONGONGOJA HC II",
                                     "OPENZINZI HC III"="OPENZINZI HC III",
                                     "OPETA HC III (KOLE)"="OPETA HC II",
                                     "OPIA HC III"="OPIA HC III",
                                     "OPOPONGO HC II"="OPOPONGO HC II",
                                     "OPOT HC II"="OPOT HC II",
                                     "OPYELO HCIII"="OPYELO HC III",
                                     "ORABA HC II"="ORABA HC III",
                                     "ORETA HC II"="ORETA HC II",
                                     "ORIAJIN HOSPITAL"="ORIAJINI HOSPITAL",
                                     "ORIVU HC II"="ORIVU HC III",
                                     "OROM HC III"="OROM HC III",
                                     "ORUKA HC III"="ORUKA HC III",
                                     "ORUM HC IV"="ORUM HC IV",
                                     "ORUNGO HC III"="ORUNGO HC III",
                                     "ORUSSI HC III"="ORUSSI HC III",
                                     "ORWAMUGE HC III"="ORWAMUGE HC III",
                                     "ORYANG HCII"="ORYANG HC II",
                                     "OSEERA HC11"="OSEERA HC II",
                                     "OSUKURU HC III"="OSUKURU HC III",
                                     "OTENO HC II"="OTENO HC II",
                                     "OTONG HC II"="OTONG HC II",
                                     "OTUBOI HC III"="OTUBOI HC III",
                                     "OTUMBARI HC III"="OTUMBARI ST. LAWRENCE HC III",
                                     "OTWAL HC III"="OTWAL HC III",
                                     "OTWEE HC III"="OTWEE HC III",
                                     "OUR LADY  OF LOURDES HC III (NAKASONGOLA)"="OUR LADY OF LOURDES HC II",
                                     "OUR LADY OF MARIA ASSUMPTA"="MARIA ASSUMPTA HC III",
                                     "OVUJO HC III"="OVUJO HC III",
                                     "OWINY HC II"="OWINY HC II",
                                     "OYAM PRISON HC III"="OYAM MAIN PRISONS HC II",
                                     "OYIMA"="OYIMA HC III",
                                     "PABBO HC III (AMURU GOVT)"="PABBO (GOVT) HC III",
                                     "PACER HCIII"="PACER HC II",
                                     "PACHARA HCII"="PACHARA HC II",
                                     "PACILO HC II"="PACILO HC II",
                                     "PADER HC III"="PADER HC III",
                                     "PADER MATERNITY"="PADER MATERNITY HOME CLINIC",
                                     "PADIBE HC IV"="PADIBE HC IV",
                                     "PADIBE WEST HC III"="PADIBE WEST HC III",
                                     "PADRE PIO HC III"="PADRE PIO HC III",
                                     "PADWOT MIDYERE HC III"="PADWOT MIDYERE HC III",
                                     "PAG HC IV"="PAG MISSION HOSPITAL",
                                     "PAGIRINYA HC III"="PAGIRINYA HC III",
                                     "PAGIRINYA QUARANTINE CENTER"="PAGIRINYA HEALTH POST HC II",
                                     "PAIDHA HC III"="PAIDHA HC III",
                                     "PAIMOL HC III"="PAIMOL HC III",
                                     "PAJIMO HC III"="PAJIMO HC III",
                                     "PAJIMO MILITARY HC III"="PAJIMO MILITARY HC II",
                                     "PAJULE HC  IV"="PAJULE HC IV",
                                     "PAJULU HC III (ARUA)"="PAJULU HC III",
                                     "PAKADHA HC III"="PAKADHA HC III",
                                     "PAKANYI HC III"="PAKANYI HC III",
                                     "PAKELE HC III"="PAKELE HC III",
                                     "PAKIA HC III"="PAKIA HC III",
                                     "PAKOR HC II"="PAKOR HC II",
                                     "PAKWACH HC IV"="PAKWACH HC IV",
                                     "PAKWACH MISSION"="PAKWACH MISSION HC III",
                                     "PALABEK  GEM HC III"="PALABEK-GEM HC III",
                                     "PALABEK OGILI HC III"="PALABEK OGILI HC III",
                                     "PALABEK-KAL HC III"="PALABEK-KAL HC III",
                                     "PALENGA HC II"="PALENGA HC II",
                                     "PALLISA GENERAL HOSPITAL"="PALLISA HOSPITAL",
                                     "PALLISA HC III"="PALLISA MISSION HC III",
                                     "PALOGA HC III"="PALOGA HC III",
                                     "PALORINYA HC III"="PALORINYA HC III",
                                     "PALUDA HCIII"="PALUDA HC III",
                                     "PALWAK  MEDICAL CENTRE ATIAK"="PALWAK HC II",
                                     "PALWAK MEDICAL CENTER"="PALWAK HC II",
                                     "PAMINYA HC III"="PAMINYA HC III",
                                     "PAMODO HC II"="PAMODO HC II",
                                     "PANDWONG HC III"="PANDWONG HC III",
                                     "PANGIRA HC II"="PANGIRA HC II",
                                     "PANOKRACH HC II"="PANOKRACH HC II",
                                     "PANYADOLI HC IV"="PANYADOLI HC IV",
                                     "PANYADOLI HILLS HC III"="PANYADOLI HILL HC III",
                                     "PANYANGA"="PANYANGA HC II",
                                     "PANYANGARA HC III"="PANYANGARA HC III",
                                     "PANYANGASI HC III"="PANYANGASI HC III",
                                     "PANYIGORO HC III"="PANYIGORO HC III",
                                     "PANYIMUR HC III"="PANYIMUR HC III",
                                     "PARAA HC II"="PARAA HC II",
                                     "PARABONGO HC II"="PARABONGO HC II",
                                     "PAROMBO HC III"="PAROMBO HC III",
                                     "PATONGO HC III"="PATONGO HC III",
                                     "PATUDA HC II"="PATUDA HC II",
                                     "PAUMA HCII"="PAUMA HC II",
                                     "PAWACH HCII"="PAWACH HC II",
                                     "PAWEL HC II"="PAWEL HC II",
                                     "PAWOR HC III"="PAWOR HC III",
                                     "PAYA HC III"="PAYA HC III",
                                     "PETTA HC III"="PETTA HC III",
                                     "PIDC (BAYLOR-MULAGO)"="MULAGO NRH - PIDC COE BAYLOR CLINIC",
                                     "PIJOKE HCIII"="PIJOKE HC III",
                                     "PINGIRE HC III"="PINGIRE HC III",
                                     "PIRE HC II"="PIRE HC II",
                                     "POGO HC II"="POGO HC II",
                                     "POKWERO HC III"="POKWERO HC III",
                                     "POTIKA HC II"="POTIKA HC II",
                                     "POYAMERI HC III"="POYAMERI HC III",
                                     "PRINCESS DIANA HC IV"="PRINCESS DIANA HC IV",
                                     "PROF WOMUKOTA"="PROF WAMUKOTA MEMORIAL HC III",
                                     "PUPUKAMUYA HC II"="PUPU KAMUYA HC II",
                                     "PURANGA HC III"="PURANGA HC III",
                                     "PURONGO HC III"="PURONGO HC III",
                                     "RACKOKO HC III"="RACKOKO HC III",
                                     "RAILWAY"="RAILWAY HC II",
                                     "RAKAI HEALTH SCIENCES PROGRAM"="RAKAI HEALTH SCIENCES PROGRAM CLINIC",
                                     "RAKAI HOSPITAL"="RAKAI HOSPITAL",
                                     "RAMBIA HC III"="RAMBIA HC III",
                                     "RAMOGI"="RAMOGI HC II",
                                     "RAPHA MEDICAL CENTRE"="RAPHA MEDICAL CENTRE",
                                     "RAPHA MEDICAL CENTRE(WAKISO)"="RAPHA MEDICAL CENTRE",
                                     "REACHOUT (  KINAWATAKA   SITE)"="REACHOUT KINAWATAKA CLINIC HC II",
                                     "REACHOUT (BANDA SITE)"="REACH OUT - BANDA CLINIC",
                                     "REACHOUT (MBUYA SITE)"="REACHOUT - MBUYA CLINIC HC II",
                                     "RENGEN HC III"="RENGEN HC III",
                                     "REPRODUCTIVE HEALTH MEDICAL CENTER II"="REPRODUCTIVE HEALTH UGANDA (KAPCHORWA)",
                                     "REPRODUCTIVE HEALTH UGANDA (APAC)"="REPRODUCTIVE HEALTH UGANDA (APAC) HC II",
                                     "REPRODUCTIVE HEALTH UGANDA (LIRA)"="REPRODUCTIVE HEALTH UGANDA (LIRA) HC III",
                                     "RESTORATION GATEWAY HOSPITAL"="RESTORATION GATEWAY HOSPITAL",
                                     "RHINO CAMP HC IV"="RHINO CAMP HC IV",
                                     "RIKI HC III"="RIKI HC III",
                                     "RIKITAE HCII"="RIKITAE HC II",
                                     "ROBIDIRE HC III"="ROBIDIRE HC III",
                                     "ROYAL VAN ZANTEEN"="ROYAL VAN ZANTEN HC II",
                                     "RUBAARE HC IV"="RUBARE HC IV",
                                     "RUBAYA HC IV (KABALE)"="RUBAYA HC IV",
                                     "RUBONA HC II"="RUBONA HC II",
                                     "RUBONDO HC II"="RUBONDO HC II",
                                     "RUBONGI MILITARY HOSPITAL"="RUBONGI MILITARY HOSPITAL",
                                     "RUBOROGOTA HC III"="RUBOROGOTA HC III",
                                     "RUBUGURI HC IV"="RUBUGURI HC IV",
                                     "RUGAAGA HC IV"="RUGAAGA HC IV",
                                     "RUGARAMA HC III"="RUGARAMA HC III",
                                     "RUGASHALI HC III"="RUGASHARI HC III",
                                     "RUGAZI HC IV"="RUGAZI HC IV",
                                     "RUGAZI MISION"="RUGAZI MISSION HC II",
                                     "RUGYEYO HC III"="RUGYEYO HC III",
                                     "RUHAAMA HC III"="RUHAMA HC III",
                                     "RUHANGIRE HC II"="RUHANGIRE HC II",
                                     "RUHARO MISSION HOSPITAL"="RUHARO MISSION HOSPITAL",
                                     "RUHIIRA HC III"="RUHIIRA HC III",
                                     "RUHINDA HC III"="RUHINDA HC III",
                                     "RUHOKO HC IV"="RUHOKO HC IV",
                                     "RUHUMURO HC III"="RUHUMURO HC III",
                                     "RUKIRI HC III"="RUKIRI HC III",
                                     "RUKOKI HC III"="RUKOKI HC IV",
                                     "RUKONI HC III"="RUKONI HC III",
                                     "RUKUNGIRI HC IV"="RUKUNGIRI HC IV",
                                     "RUKUNGIRI POLICE HC III"="RUKUNGIRI POLICE HC III",
                                     "RUKUNYU GENERAL HOSPITAL"="RUKUNYU HOSPITAL",
                                     "RUMURI"="RUMURI HC II",
                                     "RUPA HC II"="RUPA HC II",
                                     "RURAMBIIRA HC II"="RURAMBIRA HC II",
                                     "RURONGO HC II"="RURONGO HC III",
                                     "RUSHERE COMMUNITY HOSPITAL"="RUSHERE COMMUNITY HOSPITAL",
                                     "RUSHOOKA HC II"="MOTHER FRANCISCA LECHNER HC IV",
                                     "RUTAKA HC III"="RUTAKA HC III",
                                     "RUTEETE HC III (FORTPORTAL)"="RUTEETE HC III",
                                     "RUTENGA HC III"="RUTENGA HC III",
                                     "RUTI HC"="RUTI HC II",
                                     "RUTOTO SDA HC  11"="RUTOTO SDA DISPENSARY HC II",
                                     "RWABARATA HC II"="RWABARATA HC III",
                                     "RWABIGYEMANO HC III"="RWABIGYEMANO HC III",
                                     "RWAGIMBA HCIII"="RWAGIMBA HC III",
                                     "RWAITENGYA HC II"="RWAITENGYA HC II",
                                     "RWAKABENGO HC III"="RWAKABENGO HC III",
                                     "RWAMWANJA HC III"="RWAMWANJA HC III",
                                     "RWANGARA HC II"="RWANGARA HC III",
                                     "RWASHAMAIRE HC IV"="RWASHAMAIRE HC IV",
                                     "RWEBISENGO HC III"="RWEBISENGO HC IV",
                                     "RWEIKINIRO HC III"="RWEIKINIRO HC III",
                                     "RWEKUBO HC IV"="RWEKUBO HC IV",
                                     "RWEMIGINA HC II"="RWEMIGYINA HC II",
                                     "RWENGIRI  HC III"="RWENGIRI HC III",
                                     "RWENJAZA HC II"="RWENJAZA HC II",
                                     "RWENKOBWA HC III"="RWENKOBWA HC III",
                                     "RWENSHAMA HC III"="RWENSHAMA (GOVT) HC III",
                                     "RWENYAWAWA HC III"="RWENYAWAWA HC III",
                                     "RWENZORI MOUNTAINEERING SERVICES"="RWENZORI MT. SERVICES HC III",
                                     "RWESANDE HC IV"="RWESANDE HC IV",
                                     "RWESHANDE HC III"="RWENSHANDE HC III",
                                     "RWETAMU"="RWETAMU HC III",
                                     "RWIBALE HC IV"="ST. THERESA LISIEUX RWIBAALE HC IV",
                                     "RWIMI HC III"="RWIMI HC III",
                                     "RWIMI PRISONS"="RWIMI PRISON HC III",
                                     "RYEISHE HC III"="RYEISHE HC III",
                                     "SACRED HEART YALA YALA"="SACRED HEART HC II (ATIAK SUBCOUNTY)",
                                     "SAFE MOTHERHOOD NAMAYEMBA"="NAMAYEMBA SAFE MOTHERHOOD HC II",
                                     "SAGITU HC II"="SAGITU HC II",
                                     "SAIDINAH ABUBAKER HOSPITAL"="SAIDINA ABUBAKAR ISLAMIC HOSPITAL",
                                     "SANGA HC III"="SANGA HC III",
                                     "SAPIRI HC III"="SAPIRI HC III",
                                     "SDA HC II"="SDA HC III",
                                     "SEBIGORO HC III"="SEBIGORO HC III",
                                     "SEETA NAZIGO HC III"="SEETA NAZIGO HC III",
                                     "SEGUKU HC II"="SEGUKU HC II",
                                     "SEKIWUNGA HC III"="SEKIWUNGA HC III",
                                     "SEMUTO HC IV"="SEMUTO HC IV",
                                     "SENYI HC II"="SSENYI HC II",
                                     "SERERE HC IV"="SERERE HC IV",
                                     "SHARED BLESSINGS HC III"="SHARED BLESSINGS HC III",
                                     "SHUKU HC IV"="SHUUKU HC IV",
                                     "SIAAP BUGOMA CLINIC"="SSESE ISLANDS AFRICAN AIDS PROJECT (SIAAP) HC II",
                                     "SIGULU HC III"="SIGULU HC III",
                                     "SIIRA"="SIIRA HC III",
                                     "SIKUDA HCIII"="SIKUDA HC II",
                                     "SIMU PONDO HCIII"="SIMU PONDO HC II",
                                     "SINGILA HC II"="SINGILA HC II",
                                     "SIRIMULA HC III"="SIRIMULA HC II",
                                     "SIRIPI HC III"="SIRIPI HC III",
                                     "SIRONKO HC III"="SIRONKO HC III",
                                     "SOP SOP HC III"="SOP SOP HC III",
                                     "SOROTI HC III"="SOROTI HC III",
                                     "SOROTI PRISON HC111"="SOROTI MAIN PRISONS HC III",
                                     "SOROTI REGIONAL REFERRAL HOSPITAL"="SOROTI REGIONAL REFERRAL HOSPITAL",
                                     "SSANJE DOMICILIARY CLINIC"="SSANJE DOMICILIARY CLINIC HC III",
                                     "SSEKAMULI HC III"="SSEKAMULI HC III",
                                     "SSEKANYONYI HC IV"="SSEKANYONYI HC IV",
                                     "SSEMBABULE HC IV"="SSEMBABULE HC IV",
                                     "SSI HC III"="SSI HC III",
                                     "SSUNGA HC III"="SSUNGA HC III",
                                     "ST ADOLF HC III (BUTIITI)"="ST. ADOLF HC II",
                                     "ST AMBROSE CHARITY HC III"="ST. AMBROSE CHARITY HC IV",
                                     "ST ANDREW ANAKA"="ST. ANDREW HC II",
                                     "ST ANDREW HC II"="ST. ANDREW HC II",
                                     "ST ANDREWS NYARUSHANJE HCIII"="NYARUSHANJE HC III",
                                     "ST ANTHONY HC II"="ST. ANTHONY HC II",
                                     "ST ANTHONY HOSPITAL"="ST. ANTHONY'S TORORO HOSPITAL",
                                     "ST APOLLO HC III-NAMASUBA"="ST. APOLO HC III",
                                     "ST ASSUMPTA HC III"="ST. ASSUMPTA HC III",
                                     "ST AUSTIN HC II(MBALE)"="ST. AUSTIN MBALE HC II",
                                     "ST BALIKUDDEMBE HC III"="ST. BALIKUDEMBE HC III",
                                     "ST BALIKUDDEMBE MARKET/UGANDA CARES  AHF"="ST. BALIKUDEMBE UGANDA CARES HC II",
                                     "ST BENEDICT HEALTH CENTER"="BENEDICT HC IV",
                                     "ST BERNARDS MANNYA HC III"="ST. BERNARD MANNYA HC III",
                                     "ST CHARLES KABUWOKO HC III"="KABUWOKO GOVT HC III",
                                     "ST CLARE ORUNGO HC III"="ST. CLAIRE ORUNGO HC III",
                                     "ST DENNIS KYANGO HC III"="KYANGO HC III",
                                     "ST ELIZABETH HC III KIJJUKIZO"="KIJJUKIZO (ST. ELIZABETH) HC III",
                                     "ST ELIZABETH HC IV-MAGALE"="MAGALE (UCMB) HC IV",
                                     "ST FRANCIS ASSIS KITABU (KASESE)"="ST. FRANCIS OF ASSIS-KITABU HC III",
                                     "ST FRANCIS C HC III - MIGYERA"="ST FRANCIS HC III - MIGYERA",
                                     "ST FRANCIS C HC III (HOSFA)- MITYANA"="ST FRANCIS COMMUNITY HC III",
                                     "ST FRANCIS HC II"="ST. FRANCIS HC II",
                                     "ST FRANCIS HC II-AKIA"="AKIA ST. FRANCIS HC II",
                                     "ST FRANCIS HC III OCODRI"="ST. FRANCIS (OCODRI) HC III",
                                     "ST FRANCIS HEALTH CARE SERVICES"="ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                     "ST FRANCIS HOSPITAL (MUTOLERE)"="MUTOLERE HOSPITAL",
                                     "ST FRANCIS OF ASSIS- KITABU"="ST. FRANCIS OF ASSIS-KITABU HC III",
                                     "ST FRANCIS OF ASSIS NADDANGIRA HC III"="NADANGIRA HC III",
                                     "ST FRANSIS MBIRIZI HC III"="MBIRIZI ST. FRANCIS HC III",
                                     "ST JACINTA HC III"="ST. JACINTA ZIGOTI HC III",
                                     "ST JACOB HEALTH CENTER(LWAMAGGWA)"="LWAMAGGWA NGO HC II",
                                     "ST JAMES MASIRIBA"="ST. JAMES MASIRIBA COU HC III",
                                     "ST JOSEPH  H/ C III(MADUDU)"="ST. JOSEPH MADUDU HC III",
                                     "ST JOSEPH BUKUUMI"="ST. JOSEPH BUKUUMI HC II",
                                     "ST JOSEPH HOSPITAL  KITGUM"="ST. JOSEPH'S KITGUM HOSPITAL",
                                     "ST JUDE (KAPEDO) HC II"="ST. JUDE KAPEDO HC II",
                                     "ST JUDE THADDEOUS KARUNGU HC III"="ST. JUDE THADDEOS KARUNGU HC III",
                                     "ST JUDE UCEPPI HC III"="ST. JUDE ULEPPI HC II",
                                     "ST KEVIN HC III"="ST. KEVIN TOROMA HC III",
                                     "ST KIZITO BWAISE CLINIC"="ST. KIZITO BWAYIISE CLINIC HC II",
                                     "ST KIZITO KIYUNGA HC II"="ST. KIZITO KIYUNGA HC II",
                                     "ST KLAUS"="ST. KLAUS HC III",
                                     "ST LUCIA KAGAMBA HC III"="ST. LUCIA KAGAMBA HC III",
                                     "ST LUKE BUJUNI HC III (KIBAALE)"="ST. LUKE BUJUNI (KIBAALE) HC III",
                                     "ST LUKE HC II (LALOGI)"="ST LUKE HC II (LALOGI)",
                                     "ST LUKE HC III KATIYI"="ST. LUKE KATIYI HC III",
                                     "ST LUKE KMD (KIYINDA )HC III (MITYANA)"="ST. LUKE KIYINDA MITYANA DIOCESE HC III",
                                     "ST LUKE MEDICAL CENTRE"="ST. LUKE MEDICAL CENTRE",
                                     "ST LUKE NAMALIGA HC III"="ST. LUKE NAMALIGA HC III",
                                     "ST MAGDALENE HC II"="ST. MAGDALENE HC II",
                                     "ST MARTHA MATERNITY HOME- BUKEDEA"="ST MARTHA MATERNITY HOME",
                                     "ST MARTIN HC III -MABIRA (KYENJOJO)"="ST. MARTINS-MABIIRA HC III",
                                     "ST MARTINS HC"="ST. MARTINS DOMICILIARY HC II",
                                     "ST MARTINS HC III ( AMAKIO)"="ST. MARTINS AMAKIO",
                                     "ST MARYS"="ST. MARY'S CLINIC",
                                     "ST MARYS HC II KATOOSA (KYENJOJO)"="ST. MARY'S KATOOSA HC II",
                                     "ST MARYS HC III-KYEIBUZA"="KYEIBUZA (ST. MARY'S) HC III",
                                     "ST MARYS HC KASAALA"="ST. MARY'S KASAALA HC III",
                                     "ST MARYS KIGUMBA HC III (NGO)"="ST. MARY'S KIGUMBA HC III",
                                     "ST MARYS MNH BUKOMANSIMBI"="ST. MARY'S MATERNITY & NURSING HOME HC III",
                                     "ST MATHIAS AMALER HC III"="AMALER HC III",
                                     "ST MATIA MULUMBA HC III (MUBENDE)"="ST. MATIA MULUMBA HC III",
                                     "ST MATIA MULUMBA HC III (NAMAYINGO)"="ST MATIA MULUMBA HC III (NAMAYINGO)",
                                     "ST MICHEAL HCF HC III (AMURIA)"="ST MICHEAL HCF HC III (AMURIA)",
                                     "ST MONICA BIRONGO HC III (KALUNGU)"="ST. MONICA BIRONGO HC III",
                                     "ST MONICA HC III (KATENDE)"="ST. MONICA KATENDE HC III",
                                     "ST PADRE PIO HC III BUSUNJU"="ST. PADRE PIO MIREMBE HC III",
                                     "ST PAULS HC IV"="ST. PAUL (KASESE) HC IV",
                                     "ST PETER AND PAUL HC III"="ST. PETER AND PAUL HC III",
                                     "ST PETERS BUSIBO HC III"="ST. PETER,BUSIBO",
                                     "ST PETERS HC II"="ST. PETER'S HC II",
                                     "ST PIUS KIDEPO RUPA HC III"="ST. PIUS KIDEPO HC III",
                                     "ST RICHARD MEDICARE CENTRE"="ST. RICHARD HC III",
                                     "ST SABENA HC II"="ST. SABENA HC II",
                                     "ST STEPHENS HOSPITAL (MPERERWE)"="ST. STEPHEN'S MPERERWE HOSPITAL",
                                     "ST URIKA-KIZIBA"="KIZIBA CATHOLIC/KIZIBA ST. ULRIKA HC III",
                                     "ST. CECILIA BUYAMBA"="BUYAMBA HC III",
                                     "ST. FRANCIS NAGGALAMA HOSPITAL"="ST. FRANCIS NAGGALAMA HOSPITAL",
                                     "ST. GABRIEL MIREMBE MARIA HCIII"="ST. GABRIEL MIREMBE MARIA HC III",
                                     "ST.JOSEPH'S BUYEGE HC III"="BUYEGE HC III",
                                     "STEFANO CAMPANGOLO"="ST. STEFANO CAMPAGNOLO MEMORIAL HC II",
                                     "SWAZI HC III"="SWAZI HC II",
                                     "SWINGA HC III"="SWINGA HEALTH CENTRE HC III",
                                     "SYANYONJA HC II"="SHANYONJA HC II",
                                     "TAPAC HC III"="TAPAC HC III",
                                     "TAQWA HC III"="TAQWA HC III",
                                     "TARA HC III"="TARA HC III",
                                     "TASO JINJA"="TASO JINJA SPECIAL CLINIC",
                                     "TASO MASINDI"="TASO MASINDI SPECIAL CLINIC",
                                     "TASO MBARARA"="TASO MBARARA SPECIAL CLINIC",
                                     "TASO MULAGO"="TASO MULAGO SPECIAL CLINIC",
                                     "TASO RUKUNGIRI"="TASO RUKUNGIRI SPECIAL CLINIC",
                                     "TASO SOROTI"="TASO SOROTI SPECIAL CLINIC",
                                     "TASO TORORO"="TASO TORORO SPECIAL CLINIC",
                                     "TEBOKE HC III"="TEBOKE HC III",
                                     "TEBOKE MISIION HC III"="TEBOKE HC III",
                                     "TECWA HC II"="TECHWA HC II",
                                     "TEGERES HC III"="TEGERES HC III",
                                     "TEGOT HC II"="TEGOT HC II",
                                     "TEKULU HC II"="TEKULU HC II",
                                     "TERENBOY HC II"="TEREN-BOY HC III",
                                     "TESO SAFE MOTHERHOOD PROJECT"="TESO SAFE MOTHERHOOD HC III",
                                     "THE SURGERY NAGURU"="THE SURGERY HC III",
                                     "TIRINYI HC III"="TIRINYI HC III",
                                     "TIRIRI HC IV"="TIRIRI HC IV",
                                     "TODORA HC III"="TODORA HC II",
                                     "TOKORA HC IV"="TOKORA HC IV",
                                     "TONGOLO HC II"="TONGOLO HC II",
                                     "TONYA HC II"="TONYA HC III",
                                     "TORO  KAHUNA HC III"="TORO KAHUNA HC III",
                                     "TOROMA HC IV"="TOROMA HC IV",
                                     "TOROMA HCII"="TOROMA HC II",
                                     "TORORO  LAB HUB"="TORORO GENERAL HOSPITAL",
                                     "TORORO POLICE HC III"="TORORO POLICE HC III",
                                     "TORORO PRISONS"="TORORO MAIN PRISONS HC III",
                                     "TOUCH NAMUWONGO (IHK)"="TOUCH CLINIC - NAMUWONGO HC II",
                                     "TTAKAJUNGE  HC II"="TAKAJJUNGE HC III",
                                     "TTIKALU HC III"="TTIKALU HC III",
                                     "TUBUR HC III"="TUBUR HC III",
                                     "TUMBOBOI HC II"="TUMBOBOI HC III",
                                     "TWAJIJI HC III"="TWAJIJI HEALTH CENTRE HC III",
                                     "UGANDA CARES (SOROTI)"="UGANDA CARES HC II",
                                     "UGANDA MARTYRS' HOSPITAL (MBALALA)"="UGANDA MARTYRS HOSPITAL",
                                     "UGANDA MATYRS HC II"="UGANDA MARTYRS HC II",
                                     "UKUSIJONI HC III"="UKUSIJONI HC III",
                                     "URIAMA HC II"="OFUA (URIAMA) HC III",
                                     "USUK HC III"="ST. ANNE USUK HC III",
                                     "UVRI"="ENTEBBE UVRI HC II",
                                     "VICTORIA HOSPITAL (UMC)"="UMC VICTORIA (NAGURU) HOSPITAL",
                                     "VILLA MARIA HOSPITAL"="VILLA MARIA HOSPITAL",
                                     "VILLAGE OF EDEN MEDICAL CENTRE"="EDEN MEDICAL CENTRE",
                                     "VIRIKA HOSPITAL"="VIRIKA HOSPITAL",
                                     "VURRA HC III"="VURRA HC III",
                                     "WABIGALO HC III"="WABIGALO HC III",
                                     "WABULUNGU HC III"="WABULUNGU HC III",
                                     "WABUSANA HC III"="WABUSANA HC III",
                                     "WABWOKO HC III"="WABWOKO HC III",
                                     "WADELAI HC III"="WADELAI HC III",
                                     "WAGAGAI HC IV"="WAGAGAI HC IV",
                                     "WAKAWAKA HC II"="WAKAWAKA HC II",
                                     "WAKISI HC III"="WAKISI HC III",
                                     "WAKISO HC IV"="WAKISO HC IV",
                                     "WAKITAKA HC III"="WAKITAKA HC III",
                                     "WAKYATO HC III"="WAKYATO HC III",
                                     "WALELA HCII"="WALELA HC II",
                                     "WALUKUBA HC IV"="WALUKUBA HC IV",
                                     "WAMBABYA"="WAMBABYA HC II",
                                     "WANALE HC III"="WANALE HC III",
                                     "WANDEGEYA HC II"="WANDEGEYA HC II",
                                     "WANDI HC III"="WANDI HC III",
                                     "WARR HC IV"="WARR HC IV",
                                     "WATOTO HC III"="WATOTO HC II",
                                     "WATTUBA HC III"="WATUBBA HC III",
                                     "WEKOMIIRE HC III (ST THEREZA)"="WEKOMIIRE ST. THEREZA HC III",
                                     "WENTZ MEDICAL CENTER"="WENTZ MEDICAL CENTRE HC III",
                                     "WERA HC III"="WERA HC III",
                                     "WESTERN DIVISION HC III"="WESTERN DIVISION HC III",
                                     "WII ANAKA HC II"="WIIANAKA HC II",
                                     "WILELA HC II"="WILELA HC II",
                                     "WOL HC III"="WOL HC III",
                                     "YABWENGI HC II"="YABWENG HC II",
                                     "YANGANI HC III"="YANGANI HEALTH CENTRE HC III",
                                     "YAWE MEDICAL CENTRE"="YAWE HC II",
                                     "YAYARI HC III"="YAYARI HEALTH CENTRE HC III",
                                     "YERYA HC III"="YERYA HC III",
                                     "YINGA HC III"="YINGA HC III",
                                     "YIVU ABEA HC II"="YIVU ABEA HC III",
                                     "YOTKOM MEDICAL CENTRE"="YOT KOM HC III",
                                     "YOYO HC III"="YOYO HC III",
                                     "YUMBE HC IV"="YUMBE HC IV",
                                     "YUMBE HOSPITAL"="YUMBE REGIONAL REFERRAL HOSPITAL",
                                     "ZAAM MEDICAL CENTRE"="ZAM MEDICAL CENTRE",
                                     "ZAMBIA HC II"="ZAMBIA HC II",
                                     "ZEU HC III"="ZEU HC III",
                                     "ZIROBWE HC III"="ZIROBWE HC III",
                                     "ZOMBO HC III"="ZUMBO HC III"
  ))
################################################################################
#### Preparing the tracking data for comparison with VL data set ####
# Data frame with duplicates on each day removed
df6 <- df_track1 %>%
  distinct(DVisit, HFacility, .keep_all = TRUE)       
# obtaining the week number to be used to during scheduled analysis
df6 <- df6 %>%                                       
  mutate (Wnum = lubridate::isoweek(DVisit))           
df6 <- df6 %>% 
  mutate(Wnum = as.character(Wnum))  

##Developing table indicating number of times each site is visited by week
df7 <- df6 %>%                           
  group_by(Qtr, HFacility, Wnum) %>% 
  summarise(
    n_visits = n(),
    .groups = "drop")
#### calculating the overall no. of visits, and adding column of Expected visits in period
df7a <- df7 %>%                          
  group_by(HFacility,Qtr) %>% 
  summarise(
    n_visits = n(), .groups = "drop")

#### re arranging the data set
df7aPvt  <-  df7a %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = n_visits
  ) %>% 
  select(HFacility, `Oct-Dec`, `Jan-Mar`)

#### Calculating the expected visits
df6b  <-  df6 %>% 
  group_by(Qtr) %>%
  summarise(EVisit = 2 * n_distinct(Wnum[!is.na(Wnum)])) %>%
  ungroup() 

#### re-arrange this for ease of reference during analyis
df6bPvt   <-  df6b %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = EVisit
  )

##### considering the numerator; developing table for number of visits per schedule per facility
df8 <- df7aPvt %>% 
  rowwise() %>% 
  mutate(
    Oct_Dec = round((`Oct-Dec`/df6bPvt$`Oct-Dec`)*100, 1),
    Jan_Mar = round((`Jan-Mar`/df6bPvt$`Oct-Dec`)*100, 1)
    )

###################################################################################
#### Report - %age scheduled visit by HF  ####
df_joined <-   df4 %>% 
  left_join(df8, by = "HFacility")

# Selecting required columns
df_joined    <-  df_joined %>% 
  select(RRH,District,HName,HFacility,`Oct-Dec`, `Jan-Mar`, Oct_Dec, Jan_Mar)

# function to color the columns
siteVsts   <-  function(column_data){
  ifelse(column_data >= 75, "green",
         ifelse(column_data >= 50 & column_data < 75, "yellow","red"))
}


# The table
tble_df_joined    <-   flextable(df_joined)


#### Format the table
tble_df_joined <-  tble_df_joined %>%
  add_header_row(values = c(
    RRH    =    "RRH",
    District  = "District",
    HName =      "Hub Name",
    HFacility  =   " Health Facility",
    "No. of visits by Qtr","",
    "%age of visits by Qtr",""
    )
  )

tble_df_joined <-  tble_df_joined %>%
  set_header_labels(
    RRH    =    "RRH",
    District  = "District",
    HName =      "Hub Name",
    HFacility  =   "Health Facility",
    `Oct-Dec`    =   "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar",
     Oct_Dec      =   "Oct-Dec",
     Jan_Mar      =   "Jan-Mar"
      )

#### color code the results
### List Columns to Apply Coloring ###
columns <- c("Oct_Dec", "Jan_Mar")

### Apply Background Color ###
for (col in columns) {
  tble_df_joined <-  tble_df_joined %>%
    bg(j = col, bg = siteVsts(df_joined[[col]]), part = "body")
}


# Merging columns 
tble_df_joined <-  tble_df_joined %>%
  merge_at(i = 1, j = 5:6, part = "header") %>% 
  merge_at(i = 1, j = 7:8, part = "header")

# Merging vertically for the first five columns in the header
tble_df_joined <-  tble_df_joined %>%
  merge_v(j = 1:3, part = "header") 

# Adding vertical lines to improve readability
tble_df_joined <-  tble_df_joined %>%
  vline(j = c(1, 2, 3, 4, 6, 8), part = "all")


tble_df_joined <-  tble_df_joined %>%
  add_header_row(values="Health Facility line list of percentage of scheduled visits", 
                 colwidths = ncol(df_joined))

tble_df_joined


#### Report - national %age of scheduled visits honored ####
sche_visitsPvt   <-  df_joined %>% 
  pivot_longer(
    cols = c(Oct_Dec,Jan_Mar),
    names_to = "Qtr",
    values_to = "Value"
  )
#### calculate the number of sites with >75% sites visits honored
sche_visits   <-  sche_visitsPvt %>% 
    group_by(Qtr) %>% 
  summarise(
    No_HFs   = n_distinct(HFacility),
    `No.>=75%` = sum(Value >= 75, na.rm = TRUE),
    `No.Btn50%-74%`  =  sum(Value >= 50 & Value < 75, na.rm = T),
    `No.<50%`  =  sum(Value >= 0 & Value < 50, na.rm = T),
    .groups = "drop"
  ) %>% 
  mutate(
    `%with>=75%` = round((`No.>=75%` / No_HFs) * 100, 0),
    `%Btn50%-74%` = round((`No.Btn50%-74%` / No_HFs) * 100, 0),
    `%with<50%` = round((`No.<50%` / No_HFs) * 100, 0)
  )%>% 
  mutate(
    Qtr = factor(Qtr, levels = c("Oct_Dec", "Jan_Mar"))
  ) %>% 
  arrange(Qtr)

#### the table
tbl_sche_visits   <-  flextable(sche_visits)

tbl_sche_visits   <-  tbl_sche_visits %>% 
  add_header_row(values = c(
    Qtr    =  "Qtr",
    No_HFs   =  "# of health facilities with samples received at CPHL reference lab",
    "# of health facilities with site visits honored to the respective proportions","","",
    "% of health facilities with site visits honored to the respective proportions","",""
  ))

tbl_sche_visits   <-  tbl_sche_visits %>% 
  set_header_labels(
    Qtr    =  "Qtr",
    No_HFs   =  "# of health facilities with samples received at CPHL reference lab",
    `No.>=75%`    =  "≥75%",
    `No.Btn50%-74%`   =  "50%-74%",
    `No.<50%`        =  "<50%",
    `%with>=75%`    =  "≥75%",
    `%Btn50%-74%`   =  "50%-74%",
    `%with<50%`        =  "<50%"
      )

tbl_sche_visits   <-  tbl_sche_visits %>% 
  # Merging columns 
  merge_at(i = 1, j = 3:5, part = "header") %>% 
  merge_at(i = 1, j = 6:8, part = "header")

# Merging vertically for the first five columns in the header
tbl_sche_visits   <-  tbl_sche_visits %>%
  merge_v(j = 1:2, part = "header") 

# Adding vertical lines to improve readability
tbl_sche_visits   <-  tbl_sche_visits %>%
  vline(j = c(1, 2, 5, 8), part = "all")

tbl_sche_visits   <-  tbl_sche_visits %>%
  bold(part = "header") %>%
  bold(j = ~ Qtr, bold = TRUE) %>% 
  bg(j = ~ Qtr, bg = "#D9E1F2") %>%
  bold(j = ~ `%with>=75%`, bold = TRUE) %>% 
  bg(j = ~ `%with>=75%`, bg = "green") %>%
  bold(j = ~ `%Btn50%-74%`, bold = TRUE) %>% 
  bg(j = ~ `%Btn50%-74%`, bg = "yellow") %>%
  bold(j = ~ `%with<50%`, bold = TRUE) %>% 
  bg(j = ~ `%with<50%`, bg = "#F4CCCC") %>%
  autofit() 


tbl_sche_visits   <-  tbl_sche_visits %>%
  add_header_row(values="% of sites with >75% of scheduled site visits honored", 
                 colwidths = ncol(sche_visits))

tbl_sche_visits


#### Report - Line list of health facilities with low (<25%) peripheral site visits e-tracked ####
HF_site_visit_25 <- df_joined %>% 
  filter((!is.na(Oct_Dec) & Oct_Dec <26) | (!is.na(Jan_Mar) & Jan_Mar < 26))
  
# Arrange in descending order
HF_site_visit_25  <-  HF_site_visit_25 %>% 
  group_by(RRH) %>% 
  arrange(Jan_Mar, .by_group = TRUE)

# function to color the columns
siteVsts   <-  function(column_data){
  ifelse(column_data >= 75, "green",
         ifelse(column_data >= 50 & column_data < 75, "yellow","red"))
}
# The table
Table_HF_site_visit_25 <- flextable(HF_site_visit_25)

#### Format the table
Table_HF_site_visit_25 <-  Table_HF_site_visit_25 %>%
  add_header_row(values = c(
    RRH    =    "RRH",
    District  = "District",
    HName =      "Hub Name",
    HFacility  =   " Health Facility",
    "No. of visits by Qtr","",
    "%age of visits by Qtr",""
  )
  )

Table_HF_site_visit_25 <-  Table_HF_site_visit_25 %>%
  set_header_labels(
    RRH    =    "RRH",
    District  = "District",
    HName =      "Hub Name",
    HFacility  =   "Health Facility",
    `Oct-Dec`    =   "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar",
    Oct_Dec      =   "Oct-Dec",
    Jan_Mar      =   "Jan-Mar"
  )

#### color code the results
### List Columns to Apply Coloring ###
columns <- c("Oct_Dec", "Jan_Mar")

### Apply Background Color ###
for (col in columns) {
  tble_df_joined <-  tble_df_joined %>%
    bg(j = col, bg = siteVsts(df_joined[[col]]), part = "body")
}


# Merging columns 
Table_HF_site_visit_25 <-  Table_HF_site_visit_25 %>%
  merge_at(i = 1, j = 5:6, part = "header") %>% 
  merge_at(i = 1, j = 7:8, part = "header")

# Merging vertically for the first five columns in the header
Table_HF_site_visit_25 <-  Table_HF_site_visit_25 %>%
  merge_v(j = 1:3, part = "header") 

# Adding vertical lines to improve readability
Table_HF_site_visit_25 <-  Table_HF_site_visit_25 %>%
  vline(j = c(1, 2, 3, 4, 6, 8), part = "all")


Table_HF_site_visit_25 <-  Table_HF_site_visit_25 %>%
  add_header_row(values="Line list of health facilities with <25% of scheduled visits honoured", 
                 colwidths = ncol(HF_site_visit_25))

Table_HF_site_visit_25

##### Report - %age of site visits honored by RRH #####
#### re-organising the data frame
RRHdf_joined    <-   df_joined %>% 
  pivot_longer(
    cols = c("Oct_Dec", "Jan_Mar"),
    names_to = "Qtr",
    values_to = "Value"
  )
#### obtaining proportions by RRH
Reg_sche <- RRHdf_joined %>%
  filter(! is.na(RRH)) %>%
  group_by(RRH, Qtr) %>% 
   summarise(
    No_HFs   = n_distinct(HFacility),
    `>=75%` = sum(Value >= 75, na.rm = TRUE),
    `Btn50%-74%`  =  sum(Value >= 50 & Value < 75, na.rm = T),
    `<50%`  =  sum(Value >= 0 & Value < 50, na.rm = T),
    .groups = "drop"
    )
#### re-organising the data frame for the report
Reg_schePvt   <-  Reg_sche %>% 
  pivot_longer(
    cols = c(">=75%", "Btn50%-74%", "<50%"),
    names_to = "Proportions",
    values_to = "Pct"
  )

#### re-arrange again
Reg_schePvt2  <-   Reg_schePvt %>% 
  pivot_wider(
    names_from = c(Qtr, Proportions),
    values_from = Pct
  ) %>% 
  select(RRH,No_HFs, starts_with("Oct_Dec"), starts_with("Jan_Mar")
   )

# The table
Table_Reg_schePvt2 <- flextable(Reg_schePvt2)


#### Format the table
Table_Reg_schePvt2 <-  Table_Reg_schePvt2 %>%
  add_header_row(values = c(
    RRH   =  "RRH",
    No_HFs    =    "No of Sites in region with a VL sample received at CPHL",
    "No. of sites who are visited by the sample transporter with the indicated proportions of the targeted schedules in Oct-Dec","","",
    "No. of sites who are visited by the sample transporter with the indicated proportions of the targeted schedules in Jan-Mar","",""
     )
  )

Table_Reg_schePvt2 <-  Table_Reg_schePvt2 %>%
  set_header_labels(
    RRH   =  "RRH",
    No_HFs    =    "No of Sites in region with a VL sample received at CPHL",
    `Oct_Dec_>=75%`    =   "≥75%",
    `Oct_Dec_Btn50%-74%`    =   "Btn50%-74%",
    `Oct_Dec_<50%`      =   "<50%",
    `Jan_Mar_>=75%`    =   "≥75%",
    `Jan_Mar_Btn50%-74%`    =   "Btn50%-74%",
    `Jan_Mar_<50%`      =   "<50%"
  )

# Merging columns 
Table_Reg_schePvt2 <-  Table_Reg_schePvt2 %>%
  merge_at(i = 1, j = 3:5, part = "header") %>% 
  merge_at(i = 1, j = 6:8, part = "header")

# Merging vertically for the first five columns in the header
Table_Reg_schePvt2 <-  Table_Reg_schePvt2 %>%
  merge_v(j = 1:2, part = "header") 

# Adding vertical lines to improve readability
Table_Reg_schePvt2 <-  Table_Reg_schePvt2 %>%
  vline(j = c(1, 2, 5, 8), part = "all")


Table_Reg_schePvt2 <-  Table_Reg_schePvt2 %>%
  add_header_row(values="No. of sites who are visited by the sample transporter with the indicated proportions of the targeted schedules, by RRH", 
                 colwidths = ncol(Reg_schePvt2))

Table_Reg_schePvt2


#### %age of sites with 75% visits honored by Hub####
#### obtaining proportions by Hub
Hub_sche <- RRHdf_joined %>%
  filter(! is.na(RRH)) %>%
  group_by(RRH, HName, Qtr) %>% 
  summarise(
    No_HFs   = n_distinct(HFacility),
    `>=75%` = sum(Value >= 75, na.rm = TRUE),
    `Btn50%-74%`  =  sum(Value >= 50 & Value < 75, na.rm = T),
    `<50%`  =  sum(Value >= 0 & Value < 50, na.rm = T),
    .groups = "drop"
  )
#### re-organising the data frame for the report
Hub_schePvt   <-  Hub_sche %>% 
  pivot_longer(
    cols = c(">=75%", "Btn50%-74%", "<50%"),
    names_to = "Proportions",
    values_to = "Pct"
  )

#### re-arrange again
Hub_schePvt2  <-   Hub_schePvt %>% 
  pivot_wider(
    names_from = c(Qtr, Proportions),
    values_from = Pct
  ) %>% 
  select(RRH, HName, No_HFs, starts_with("Oct_Dec"), starts_with("Jan_Mar")
  )

# The table
Hub_Pct_visit <- flextable(Hub_schePvt2)


#### Format the table
Hub_Pct_visit <-  Hub_Pct_visit %>%
  add_header_row(values = c(
    RRH   =  "RRH",
    HName   =   "Hub Name",
    No_HFs    =    "No of Sites in region with a VL sample received at CPHL",
    "No. of sites who are visited by the sample transporter with the indicated proportions of the targeted schedules in Oct-Dec","","",
    "No. of sites who are visited by the sample transporter with the indicated proportions of the targeted schedules in Jan-Mar","",""
  )
  )

Hub_Pct_visit <-  Hub_Pct_visit %>%
  set_header_labels(
    RRH   =  "RRH",
    HName   =   "Hub Name",
    No_HFs    =    "No of Sites in region with a VL sample received at CPHL",
    `Oct_Dec_>=75%`    =   "≥75%",
    `Oct_Dec_Btn50%-74%`    =   "Btn50%-74%",
    `Oct_Dec_<50%`      =   "<50%",
    `Jan_Mar_>=75%`    =   "≥75%",
    `Jan_Mar_Btn50%-74%`    =   "Btn50%-74%",
    `Jan_Mar_<50%`      =   "<50%"
  )

# Merging columns 
Hub_Pct_visit <-  Hub_Pct_visit %>%
  merge_at(i = 1, j = 4:6, part = "header") %>% 
  merge_at(i = 1, j = 7:9, part = "header")

# Merging vertically for the first five columns in the header
Hub_Pct_visit <-  Hub_Pct_visit %>%
  merge_v(j = 1:3, part = "header") 

# Adding vertical lines to improve readability
Hub_Pct_visit <-  Hub_Pct_visit %>%
  vline(j = c(1, 2, 3, 4, 6, 9), part = "all")


Hub_Pct_visit <-  Hub_Pct_visit %>%
  add_header_row(values="No. of sites who are visited by the sample transporter with the indicated proportions of the targeted schedules, by Hub", 
                 colwidths = ncol(Hub_schePvt2))

Hub_Pct_visit



#########################################################################################
#.
########################################################################################
#### sample tracking reports ####
##########################################################################
#### cleaning the e-tracking data e-tracking ####
## Through hub to CPHL
track_type <- e_tracked %>% 
  rename(
    "Collection_site"  = "Collection Point Facility",
    "Test_Type"        = "Test Type",
    "No_samples"       = "No. Samples",
    "Time_picked"      = "Time Picked",
    "T_last_seen"      = "Time Last Seen",
    "fac_last_seen"    = "Facility Last seen",
    "Date_delivered"   = "Delivered on",
    "Date_received"    =  "Received at"
  )

track_type$Collection_site <- toupper(track_type$Collection_site)
track_type$Test_Type       <- toupper(track_type$Test_Type)

## capitalize each name column
track_type$Collection_site    <-  toupper(track_type$Collection_site)
track_type$District           <-  toupper(track_type$District)
track_type$Hub                <-  toupper(track_type$Hub)
track_type$Destination        <-  toupper(track_type$Destination)
track_type$fac_last_seen      <-  toupper(track_type$fac_last_seen)

#### change date type
track_type$Time_picked   <-  as.Date(track_type$Time_picked)
track_type$Date_delivered  <-  as.Date(track_type$Date_delivered)
track_type$Date_received   <-  as.Date(track_type$Date_received)



#### add quarter and months
track_type <- track_type %>% 
  mutate(
    Yr  =  year(Time_picked),
    Month =  month(Time_picked)
  ) 
# Add quarter, "Qtr"
track_type <- track_type %>% 
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))

##clean the HName column

#### Joining e-tracking database to the VL LIMS ####

# Create a data frame with sample tracking codes for ease of analysis
samples_tracked <-  track_type %>% 
  left_join(VLRaw, by = c("Package ID" = "tracking_code"))


#### change the date of collection format 
VLRaw$date_collected  <-  as.Date(VLRaw$date_collected)

### select the rows with data 
samples_tracked   <-   samples_tracked %>% 
  filter(!is.na(form_number)) %>% 
  select("Package ID","Collection_site","District.x","Hub","Destination","Test_Type",              
         "No_samples","Time_picked","Status","T_last_seen","fac_last_seen","Date_delivered",         
         "Date_received","TAT","form_number","HFacility","date_collected",          
         "sample_type","picked_from_facility_on", "Qtr.y")
# Rename
samples_tracked   <-   samples_tracked %>% 
  rename("District"  =  "District.x",
         "PID"       =  "Package ID",
         "Qtr"      =  "Qtr.y")

# add RRH column
samples_tracked   <-   samples_tracked %>% 
  left_join(Regions, by = "District")

#########################################################################
##########################################################################
#### Report - TAT from e-tracking database ####
# calculating TAT      
TAT_e_tracking <- TAT_e_tracking %>%                                      # Created TAT Columns                               
  mutate(
    ePick_TAT  = Time_picked - date_collected,
    eTrp_TAT = Date_delivered - Time_picked,
    eRctTAT = Date_received - date_collected,      #  Date collected from VLIMS
    eCPHL_TAT = Date_received   - Date_delivered) 

####################################################################################
#### Report - UNHLS TAT Report based on e-tracked ####
### TAT for VL and EID Samples and for CPHL
eTAT_VL <- TAT_e_tracking %>%
  filter(Test_Type %in% c("VL DBS","VL PLASMA","HIV DR","EID HIV"),
         Destination =="UNHLS")

## The calculation
TAT_report <- eTAT_VL %>%
  group_by(Destination, Qtr) %>%
  summarise(
    N        = sum(No_samples, na.rm = TRUE),
    No_Pick  = sum(No_samples[!is.na(ePick_TAT)], na.rm = TRUE),
    No_MvT   = sum(No_samples[!is.na(eTrp_TAT)], na.rm = TRUE),
    No_Rct   = sum(No_samples[!is.na(eRctTAT)], na.rm = TRUE ),
    No_lab   = sum(No_samples[!is.na(eCPHL_TAT)], na.rm = TRUE),
    TAT_Pck   =  median(ePick_TAT[ePick_TAT >= 0], na.rm = TRUE),
    TAT_Mvt   = median(eTrp_TAT, na.rm = TRUE),
    TATRct = median(eRctTAT, na.rm = TRUE),
    TATLab   = median(eCPHL_TAT, na.rm = TRUE),
    .groups = "drop"
  ) 


TATR_report <-  data.frame(
  Section     = c("Total","Collection to Pick","Pick to Deliver (Mov't)","Pick to Receipt at CPHL","Delivered to Received by CPHL"),
  No._samples = c(TAT_report$N,TAT_report$No_Pick,TAT_report$No_MvT,TAT_report$No_Rct,TAT_report$No_lab),
  TAT         = c(TAT_report$N,TAT_report$TAT_Pck,TAT_report$TAT_Mvt,TAT_report$TATRct,TAT_report$TATLab)
)

# the table 
Table_TATR_report <- flextable(TATR_report)

Table_TATR_report <- Table_TATR_report %>% 
  add_header_row(values = "Summary of EID and VL samples e-tracked and TAT for at each level",
                 colwidths = ncol(TATR_report))

Table_TATR_report

#### Report - TAT receipt at CPHL by RRH ####
### TAT for VL and EID Samples and for CPHL
eTAT_VLRRH <- TAT_e_tracking %>%
  filter(Test_Type %in% c("VL DBS","VL PLASMA","HIV DR","EID HIV"),
         Destination ==  "UNHLS")

eTAT_VLRRH   <-  eTAT_VLRRH %>%    # changing to numeric, removing strings
  mutate(
    ePick_TAT = round(as.numeric(gsub(" days", "", ePick_TAT))),
    eTrp_TAT = round(as.numeric(gsub(" days", "", eTrp_TAT))),
    eRctTAT = round(as.numeric(gsub(" days", "", eRctTAT))),
    eCPHL_TAT = round(as.numeric(gsub(" days", "",eCPHL_TAT)))
  )

## The calculation  of TAT and number of samples
RRH_TAT_report <- eTAT_VLRRH %>%
  group_by(RRH) %>%
  summarise(
    N        = sum(No_samples),
    No_Pick  = sum(No_samples[!is.na(ePick_TAT)], na.rm = TRUE),
    No_MvT   = sum(No_samples[!is.na(eTrp_TAT)], na.rm = TRUE),
    No_Rct   = sum(No_samples[!is.na(eRctTAT)], na.rm = TRUE ),
    No_lab   = sum(No_samples[!is.na(eCPHL_TAT)], na.rm = TRUE),
    TAT_Pck   =  median(ePick_TAT[ePick_TAT >= 0], na.rm = TRUE),
    TAT_Mvt   = median(eTrp_TAT, na.rm = TRUE),
    TATRct = median(eRctTAT, na.rm = TRUE)
  ) 

# the table 
Table_RRH_TAT_report <- flextable(RRH_TAT_report)

Table_RRH_TAT_report   <-  Table_RRH_TAT_report %>% 
  bg(j = "TAT_Pck", bg = ifelse(RRH_TAT_report$TAT_Pck <= 3, "green",
                                ifelse(RRH_TAT_report$TAT_Pck > 3 
                                       & RRH_TAT_report$TAT_Pck <= 5, "yellow", "red"))) %>% 
  bg(j = "TAT_Mvt", bg = ifelse(RRH_TAT_report$TAT_Mvt <= 3, "green",
                                ifelse(RRH_TAT_report$TAT_Mvt > 3 
                                       & RRH_TAT_report$TAT_Mvt <= 5, "yellow", "red"))) %>% 
  
  bg(j = "TATRct", bg = ifelse(RRH_TAT_report$TATRct <= 7, "green",
                               ifelse(RRH_TAT_report$TATRct > 7 
                                      & RRH_TAT_report$TATRct <= 14, "yellow", "red"))
  )


Table_RRH_TAT_report   <-   Table_RRH_TAT_report %>% 
  add_header_row(values = "TAT for EID,VL samples e-tracked, by RRH", colwidths = 
                   ncol(RRH_TAT_report))

Table_RRH_TAT_report

#### Report - samples from Non-Hub, and TAT ####
HubTAT    <-   TAT_e_tracking %>% 
  filter(Test_Type %in% c("VL DBS","VL PLASMA","HIV DR","EID HIV")) %>% 
  mutate(
    Testing_site  =  ifelse(Collection_site == Hub, "1","2")) # selecting tests to be sent to CPHL, not tested at Hub

# correct the TAT data type
HubTAT$ePick_TAT <-  round(as.numeric(gsub(" days", "", HubTAT$ePick_TAT)))

# Analyze RRH status
HubTAT_all <- HubTAT %>% 
  filter(!is.na(RRH) & Destination != "UNHLS" & Testing_site == "2") %>% # the collection site is not the hub
  group_by(RRH) %>% 
  summarise(
    N   =  sum(No_samples),
    No_sending_sites  =  n_distinct(Collection_site),
    No_Pick  = sum(No_samples[!is.na(ePick_TAT)], na.rm = TRUE),
    TAT_Pck = median(ePick_TAT[ePick_TAT >= 0], na.rm = TRUE)
  ) %>% 
  select(RRH,N,No_sending_sites,TAT_Pck)

# The table
tble_HubTAT_all   <-  flextable(HubTAT_all)

tble_HubTAT_all   <-  tble_HubTAT_all %>% 
  add_header_row(values = "Sample Pick TAT where collection site is not a hub (EID,VL samples",
                 colwidths = ncol(HubTAT_all))
tble_HubTAT_all

#### Report - Percentage of samples delivered at hub within 3 days ####
Pct_del <- HubTAT %>% 
  filter(!is.na(RRH) & Destination != "UNHLS" & Testing_site == "2") %>% # the collection site is not the hub
  group_by(RRH) %>% 
  summarise(
    No_packages = sum(No_samples, na.rm = TRUE), # Summing No_samples across rows
    No_less3 = sum((ePick_TAT <= 3) * No_samples, na.rm = TRUE) # Weighted sum for <= 3 days
  ) %>% 
  mutate(
    Pct3Days = round((No_less3 / No_packages) * 100, 0) # Calculate percentage
  )

# the table
tbl_Pct_del   <-  flextable(Pct_del)

tbl_Pct_del  <-  tbl_Pct_del %>% 
  add_header_row(values = "Pct of samples delivered from peripheral sites to in 3 days at hub",
                 colwidths = ncol(Pct_del))

tbl_Pct_del
#######################################################################
#########################################################################
#### Report - Hub numbers and TAT ####
# Analyze RRH status
HubTAT_allr <- HubTAT %>% 
  filter(!is.na(RRH) & Destination != "UNHLS" & Testing_site == "1") %>% # the collection site is not the hub
  group_by(RRH) %>% 
  summarise(
    N   =  sum(No_samples),
    No_Hubs  =  n_distinct(Hub),
    No_Pick  = sum(No_samples[!is.na(ePick_TAT)], na.rm = TRUE),
    TAT_Pck = median(ePick_TAT[ePick_TAT >= 0], na.rm = TRUE)
  ) %>% 
  select(RRH,N,No_Hubs,TAT_Pck)

# The table
tble_HubTAT_allr   <-  flextable(HubTAT_allr)

tble_HubTAT_allr   <-  tble_HubTAT_allr %>% 
  add_header_row(values = "Sample Pick TAT where collection site is a hub (EID,VL samples",
                 colwidths = ncol(HubTAT_all))
tble_HubTAT_allr


#### Report - Percentage of samples delivered at CPHL within 3 days ####
Pct_delr <- HubTAT %>% 
  filter(!is.na(RRH) & Destination != "UNHLS" & Testing_site == "1") %>% # the collection site is not the hub
  group_by(RRH) %>% 
  summarise(
    No_packages = sum(No_samples, na.rm = TRUE), # Summing No_samples across rows
    No_less3 = sum((ePick_TAT <= 3) * No_samples, na.rm = TRUE) # Weighted sum for <= 3 days
  ) %>% 
  mutate(
    Pct3Days = round((No_less3 / No_packages) * 100, 0) # Calculate percentage
  )

# the table
tbl_Pct_delr   <-  flextable(Pct_delr)

tbl_Pct_delr  <-  tbl_Pct_delr %>% 
  add_header_row(values = "Pct of samples delivered from Hub to CPHL in 3 days",
                 colwidths = ncol(Pct_delr))

tbl_Pct_delr

#### data preparation for extent of utilization of e-codes ####
# number of samples by tracking code
no_sample_track_code <- join1 %>% 
  group_by(tracking_code) %>% 
  summarise(
    N = n()
  )
# comparing with number samples tracked data base
sam_track <- samples_tracked %>%                         # to ease joining. Only select number
  select("PID","No_samples")

# Joining to tracking codes
no_sample_track_code <- no_sample_track_code %>% 
  left_join(sam_track, by = c("tracking_code" = "PID"))

# similarity between number recorded on package and number in the system
no_sample_track_code <- no_sample_track_code %>%
  mutate(similarity = case_when(
    N == No_samples ~ "Y",
    TRUE ~ "N"
  ))
#####################################################################
############################################################################
#### Report - Similarity in number of samples recorded on package Vs those recorded in LIMS ####
# Summarizing similarities
similar <- no_sample_track_code %>% 
  summarise(
    Similar = sum(similarity == "Y"),
    Not     = sum(similarity == "N")
  )

# Adding totals and percentages
sim <- similar %>% 
  mutate(
    Total       = Similar + Not,
    Pct_similar = round((Similar / Total) * 100, 0),
    Pct_Not     = round((Not / Total) * 100, 0)
  )

# Creating the table with flextable
tble_similar <- flextable(sim)

tble_similar
########################################################################
#############################################################################
#### Report - No. of samples e-tracked by sample type, and coverage (no facilities) ####

# Filter out rows with empty No_samples (NA or zero)
filtered_track_type <- track_type %>%
  filter(!is.na(No_samples) & No_samples != 0)

filtered_track_type <- filtered_track_type %>% 
  filter(No_samples < 100000)


# Summarize the data
track_type_summary <- filtered_track_type %>%
  group_by(Test_Type) %>%
  summarise(
    No.samples    = sum(No_samples, na.rm = TRUE),
    No.facilities = n_distinct(Collection_site, na.rm = TRUE)
  ) %>% 
  arrange(desc(No.samples))

# the table
track <- flextable(track_type_summary)

autofit(track)
track

########################################################################
#########################################################################
#### General NSRTN report ####
# create a document
NSRTNRpt <- read_docx()

# Overall EID/VL TAT across the NSRTN sections
NSRTNRpt <- NSRTNRpt %>% 
  body_add_flextable(tbl_TATReport)

# NSRTN sectional TAT by Lab Test
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(tble_Labcombined)

# EID TAT Rct, by RRH
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(ERRH_RTAT)

# VL TAT Rct, by RRH
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(VLRRH_RTAT)

# %age of VL samples received at CPHL within 7 days, by RRH
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(tble_timelyRct)

# %age of EID samples received at CPHL within 7 days, by RRH
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(Etble_timelyRct)


# % of VL samples received at CPHL within targeted TAT by Hub
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(tble_timelyDLD)
	
# % age of EID samples downloaded within 3 days, by RRH
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(Etble_timelyDLD)


# EID TAT Rct, by Hub
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(Hub_VLTATR)


# EID TAT Rct, by Hub
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(Hub_EIDTATR)


# EID TAT Download, by Hub
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(Hub_VLTATDRpt)

# Add a blank line or heading
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(Hub_EIDTATDRpt) 

# %age of VL samples downloaded within 3 days, by Hub
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(tble_timelyHubDLD) 

# %age of VL samples downloaded within 3 days, by Hub
NSRTNRpt <- NSRTNRpt %>%
  body_add_flextable(tble_EtimelyDLD) 

# Save the document
print(NSRTNRpt, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrn.docx")



############################################
#### Annex 1 ####
# create a document
NSRTNFebAnnex <- read_docx()

# Sites with >7 Day VL sample receipt TAT
NSRTNFebAnnex <- NSRTNFebAnnex %>% 
  body_add_flextable(Hf_VLTATR)

# Sites with >7 Day EID Sample Receipt TAT
NSRTNFebAnnex <- NSRTNFebAnnex %>% 
  body_add_flextable(EHf_VLTATR)

# %age of VL samples received at CPHL within 7 days, by Hub
NSRTNFebAnnex <- NSRTNFebAnnex %>% 
  body_add_flextable(tble_HtimelyRct)

# %age of EID samples received at CPHL within 7 days, by Hub
NSRTNFebAnnex <- NSRTNFebAnnex %>%  
  body_add_flextable(Etble_HtimelyRct)

# Line list of sites with <50% of VL samples received at CPHL within 7 D
NSRTNFebAnnex <- NSRTNFebAnnex %>%   
  body_add_flextable(tble_Hf_VL7DaysPvt)


# Save the document
print(NSRTNFebAnnex, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex.docx")


####  Annex 2  ####
# create a document
NSRTNFebAnnex1 <- read_docx()

# Line list of sites with <50% of EID samples received at CPHL within 7 
NSRTNFebAnnex1 <- NSRTNFebAnnex1 %>% 
  body_add_flextable(Etble_Hf_VL7DaysPvt)


# Sites with >3 Day VL sample download TAT
NSRTNFebAnnex1 <- NSRTNFebAnnex1 %>% 
  body_add_flextable(Hf_VLTATD)


# Save the document
print(NSRTNFebAnnex1, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex1.docx")


####  Annex 3  ####
# create a document
NSRTNFebAnnex2 <- read_docx()

# Sites with >3 Day EID sample download TAT
NSRTNFebAnnex2 <- NSRTNFebAnnex2 %>%
  body_add_flextable(Hf_EIDTATD)

# Linelist of sites with < 50% of samples downloaded within 3 days
NSRTNFebAnnex2 <- NSRTNFebAnnex2 %>% 
  body_add_flextable(tble_VLhf_3DaysRpt)


# Linelist of sites with < 50% of EID samples downloaded within 3 days
NSRTNFebAnnex2 <- NSRTNFebAnnex2 %>% 
  body_add_flextable(tble_EIDPct_3DaysRpt)

# Save the document
print(NSRTNFebAnnex2, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex2.docx")


###############################################
#### create a document graphs ####
Q4graphs <- read_docx()

# Graphs

# % samples downloaded at CPHL by RRH
Q4graphs <- Q4graphs %>% 
  body_add_img(src = "%age national VL Samples recieved at CPHL within 7 days.png", width = 6, height = 4)

# %age of samples received within 7 days, by RRH
Q4graphs <- Q4graphs %>%  
  body_add_img(src = "%age VL Samples received at CPHL within 7 days.png", width = 6, height = 4)

# %age of samples received within 7 days, by RRH
Q4graphs <- Q4graphs %>%  
  body_add_img(src = "%age EID Samples received at CPHL within 7 days.png", width = 6, height = 4)

# VL TAT cascade
Q4graphs <- Q4graphs %>%  
  body_add_img(src = "VL TAT across the NSRTN Cascade.png", width = 6, height = 4)

# EID TAT cascade
Q4graphs <- Q4graphs %>%  
  body_add_img(src = "EID TAT across the NSRTN Cascade.png", width = 6, height = 4)

# %age VL downloaded
Q4graphs <- Q4graphs %>%  
  body_add_img(src = "%age VL Samples downloaded within 3 days.png", width = 6, height = 4)

# %age of eid samples downloaded within 3 days, by RRH
Q4graphs <- Q4graphs %>%  
  body_add_img(src = "%age EID Samples downloaded within 3 days.png", width = 6, height = 4)

# %age of site visits honored by RRH
Q4graphs <- Q4graphs %>%  
  body_add_img(src = "%age of site visits honored by RRH.png", width = 6, height = 4)


# Save the document
print(Q4graphs, target = "C:/Users/HP/Desktop/q4Graphs.docx")



#### tracking report ####
# create a document
Q1NSRTNadd1 <- read_docx()

# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%  
  body_add_flextable(RRH_Pct_visit)


# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%  
  body_add_flextable(Table_TATR_report)


# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%    
  body_add_flextable(Table_RRH_TAT_report)


# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%  
  body_add_flextable(tble_HubTAT_all)

# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%  
  body_add_flextable(tbl_Pct_del)


# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%  
  body_add_flextable(tble_similar)

# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%  
  body_add_flextable(track)

# Receipt TAT by RRH
Q1NSRTNadd1 <- Q1NSRTNadd1 %>%  
  body_add_flextable(tble_HubTAT_allr)

# Save the document
print(Q1NSRTNadd1, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex3.docx")


### annex2 - check to delete ####
# create a document
monthly_annex2 <- read_docx()
# % of eid samples received at CPHL within targeted TAT by HF
monthly_annex2 <- monthly_annex2 %>% 
  body_add_flextable(tble_EID_Fcs)
# Save the document
print(monthly_annex2, target = "C:/Users/HP/Desktop/annex2.docx")



#### Annex3 -  check to delete####
# create a document
monthly_annex3 <- read_docx()
# %age VL samples downloaded with targeted TAT, by health facility
monthly_annex3 <- monthly_annex3 %>% 
  body_add_flextable(tble_dfd_hf_pivot)
# Save the document
print(monthly_annex3, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex5.docx")


### Annex 4    check to delete#### 
# %age EID samples downloaded with targeted TAT, by health facility
# create a document
monthly_annex4 <- read_docx()

monthly_annex4 <- monthly_annex4 %>% 
  body_add_flextable(tble_Edfd_hf_pivot)
# Save the document
print(monthly_annex4, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex10.docx")



### Annex5
# create a document
monthly_annex5 <- read_docx()
# %age VL samples downloaded with targeted TAT, by health facility
monthly_annex5 <- monthly_annex5 %>% 
  body_add_flextable(EHF_DTAT)
# Save the document
print(monthly_annex5, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex3.docx")


### Annex 6 - possible delete ####
# %age EID samples downloaded with targeted TAT, by health facility
# create a document
monthly_annex6 <- read_docx()

monthly_annex6 <- monthly_annex6 %>% 
  body_add_flextable(HF_DTAT)
# Save the document
print(monthly_annex6, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex11.docx")



#### Annex 7 ####
# %age HFs visited as scheduled
# create a document
monthly_annex7 <- read_docx()

monthly_annex7 <- monthly_annex7 %>% 
  body_add_flextable(tble_df_joined)


monthly_annex7 <- monthly_annex7 %>% 
  body_add_flextable(Table_HF_site_visit_25)


monthly_annex7 <- monthly_annex7 %>% 
  body_add_flextable(Table_Reg_schePvt2)

monthly_annex7 <- monthly_annex7 %>% 
  body_add_flextable(Hub_Pct_visit)


# Save the document
print(monthly_annex7, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/NSRTN/R report/nstrnAnnex12.docx")


