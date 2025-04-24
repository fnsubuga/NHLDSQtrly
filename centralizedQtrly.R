#### Lab and results from centralized testing ####
#### Packages####
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
  sf,
  pandoc,
  openxlsx)

#########################################################
#### download data sets####
## Databases
Regions   <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/Regions.xlsx")
Hubs_RRH  <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/NSRTN/RRH_Hubs.xlsx")
Master    <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/Health Facility Master File.xlsx")
## Lab Data
#### VL
VLRaw1     <-read_excel("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/CQI and research/Lab_NSTRN/VL/VLOct-Dec24.xlsx",
                        col_types = "text")
VLRaw2     <- read_excel("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/VL/VLJan_Mar.xlsx",
                         col_types = "text")

#### returning to orginal dates
VLRaw1  <-  type.convert(VLRaw1, as.is = TRUE)
VLRaw2 <-  type.convert(VLRaw2, as.is = TRUE)

#### changing collection dates
#### Collection dates
VLRaw2$date_collected <- as.Date(VLRaw2$date_collected, origin = "1899-12-30")
VLRaw1$date_collected <- as.Date(VLRaw1$date_collected, origin = "1899-12-30")


#### removing un-needed parameters
#### initiation dates
VLRaw2$treatment_initiation_date <- as.Date(as.numeric(VLRaw2$treatment_initiation_date), origin = "1899-12-30")
VLRaw2$treatment_initiation_date <- as.Date(VLRaw2$treatment_initiation_date, format = "%Y-%m-%d")


VLRaw1$treatment_initiation_date <- as.Date(as.numeric(VLRaw1$treatment_initiation_date), origin = "1899-12-30")
VLRaw1$treatment_initiation_date <- as.Date(VLRaw1$treatment_initiation_date, format = "%Y-%m-%d")


#### EID
EIDRaw     <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/EID/EIDOct24-Mar25.xlsx")

###########################################################################
###########################################################################
## Clean and select VL data set parameters (VLRaw)####
#### Combining the VL data set
VLRaw   <-  bind_rows(VLRaw1,VLRaw2)

#### convert to system suggested data types
VLRaw  <- type.convert(VLRaw, as.is = TRUE)

#### convert to date type
VLRaw$date_collected  <-  as.Date(VLRaw$date_collected, origin = "1899-12-30")
VLRaw$date_received   <-  as.Date(VLRaw$date_received, origin = "1899-12-30")
VLRaw$released_at     <-  as.Date(VLRaw$released_at,origin = "1899-12-30")
VLRaw$date_downloaded  <-  as.Date(VLRaw$date_downloaded,origin = "1899-12-30")
VLRaw$test_date        <-  as.Date(VLRaw$test_date,origin = "1899-12-30")

#### selecting columns of interest
VLRaw <- VLRaw %>% 
   select(form_number,tracking_code,facility,district,hub,date_collected,date_received,date_created,sample_type,test_date,released_at,date_downloaded,status,rejection_reason_id,rejection_reason,delivered_at,picked_from_facility_on,suppressed
   )
 # Cleaning column names and extracking working columns
 VLRaw <- VLRaw %>% 
   rename(# New            #Old
     HFacility   =      facility,
     HName       =      hub,
     District    =      district,
     Status      =      status,
     Reject_reason =    rejection_reason)
 
 VLRaw$HName <- gsub(" Hub", "", as.character(VLRaw$HName))     #  Removing word Hub from hub name
 
 VLRaw <- VLRaw %>% 
   mutate(District  =        recode(District,
                                    # Old               # New
                                    "Luweero"           =      "Luwero",
                                    "Maracha-Terego"    =       "Maracha",
                                    "Ssembabule"         =       "Sembabule"))
 
 VLRaw$District <- toupper(VLRaw$District)
 
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
 
## Create TAT columns in VL data set (df)
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
          TATD = if_else(TATD < 0 | TATD > 90, NA_real_, TATD))

###################################################################
##################################################################
#### Cleaning EID conventional data set####
EIDRaw <- EIDRaw %>% 
   select(No,`Infant Name`,`EXP Number`,Sex,`Facility Name`,District,Hub,`Age in Months`,`Entry Point`,`Date Collected`,`Date Received`,`Date Tested`,`Date dispatched to cphl`,`Date Dispatched`,`Printed at`,Status,`Rejection Reason`,`PCR 1st/2nd`,Result,`Infant Name`
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
     "HName"             =     "Hub",
     "IName"             =  "Infant Name")
 
 EIDRaw$HName <- gsub(" Hub", "", as.character(EIDRaw$HName))     #  Removing word Hub from hub name
 
 EIDRaw <- EIDRaw %>% 
   mutate(District  =        recode(District,
                                    # Old               # New
                                    "Luweero"           =      "Luwero",
                                    "Maracha-Terego"    =       "Maracha",
                                    "Ssembabule"         =       "Sembabule"))
 
 EIDRaw$District  <- toupper(EIDRaw$District)
 
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

 ETATData <- ETconvert %>%                                              # Created TAT Columns                               
   mutate(
     TATR = date_received - date_collected,
     TATL = released_at   - date_received,
     TATD = date_downloaded - released_at,
     TATO = date_downloaded - date_collected) %>% 
   mutate_at(c("TATR", "TATL", "TATD", "TATO"), as.numeric) 
 
 Edf<- ETATData %>%                                                      # EID working data frame
   mutate(TATR = if_else(TATR < 0 | TATR > 90, NA_real_, TATR),
          TATL = if_else(TATL < 0 | TATL > 90, NA_real_, TATL),
          TATD = if_else(TATD < 0 | TATD > 90, NA_real_, TATD),
          TATO = if_else(TATO < 0 | TATO > 90, NA_real_, TATO),
          )
 
###################################################################
###################################################################
#### Report - Number of EID/VL Samples tested ####
 # total no of VL samples
 Total_VLsamples <- VLRaw %>% 
   filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>% 
   group_by(Qtr) %>% 
   summarise(
     no_VL = n(),
     No_facilities = n_distinct(HFacility),
     .groups = "drop"
   ) 
 
 # re-arrange the table
 Total_VLsamplesPvt   <-   Total_VLsamples %>% 
   pivot_longer(
     cols = c("no_VL","No_facilities"),
     names_to = "Sample Type",
     values_to = "Number"
   )
 
 # re-arrange the table
 VLPvt   <-   Total_VLsamplesPvt %>% 
   pivot_wider(
      names_from = Qtr,
      values_from = Number
   )
 

#### total Plasma
 Plasma_VLsamples   <-  VLRaw %>% 
   filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
   group_by(Qtr) %>% 
   filter(sample_type == "P") %>% 
   summarise(
     no_Plasma = n(),
     No_facilities     = n_distinct(HFacility)
   )
 # re-arrange the table
 Plasma_VLsamplesPvt   <-   Plasma_VLsamples %>% 
   pivot_longer(
     cols = c("no_Plasma","No_facilities"),
     names_to = "Sample Type",
     values_to = "Number"
   )
  # re-arrange the table
 PlasmaPvt   <-   Plasma_VLsamplesPvt %>% 
   pivot_wider(
     names_from = Qtr,
     values_from = Number
   )


#### total DBS 
  DBS_VLsamples   <-  VLRaw %>% 
    filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
    group_by(Qtr) %>% 
    filter(sample_type == "D") %>% 
   summarise(
     no_DBS  =  n(),
     No_facilities     = n_distinct(HFacility)
   )
  # re-arrange the table
  DBS_VLsamplesPvt   <-  DBS_VLsamples %>% 
    pivot_longer(
      cols = c("no_DBS","No_facilities"),
      names_to = "Sample Type",
      values_to = "Number"
    )
  # re-arrange the table
  DBSPvt   <-   DBS_VLsamplesPvt %>% 
    pivot_wider(
      names_from = Qtr,
      values_from = Number
    )


#### total sample type not mentioned
  NA_VLsamples   <-  VLRaw %>% 
    filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
    group_by(Qtr) %>% 
    filter(is.na(sample_type)) %>% 
    summarise(
      N_A  =  n(),
      No_facilities     = n_distinct(HFacility)
    )
  # re-arrange the table
  NA_VLsamplesPvt   <-  NA_VLsamples %>% 
    pivot_longer(
      cols = c("N_A","No_facilities"),
      names_to = "Sample Type",
      values_to = "Number"
    )
# re-arrange the table
  NAPvt   <-   NA_VLsamplesPvt %>% 
    pivot_wider(
      names_from = Qtr,
      values_from = Number
    )  

  
  
#### EID samples
no_EIDsamples   <-    EIDRaw %>% 
   filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
   group_by(Qtr) %>% 
   summarise(
     no_EID   =  n(),
     No_facilities     = n_distinct(HFacility)
   )
 # re-arrange the table
no_EIDsamplesPvt   <-  no_EIDsamples %>% 
   pivot_longer(
     cols = c("no_EID","No_facilities"),
     names_to = "Sample Type",
     values_to = "Number"
   )
 # re-arrange the table
 EIDPvt   <-   no_EIDsamplesPvt %>% 
   pivot_wider(
     names_from = Qtr,
     values_from = Number
   )  
 
#### Create the the report

# Combine the data frames
Summary_testDone  <-  rbind(VLPvt,PlasmaPvt,DBSPvt,NAPvt,EIDPvt)

# create No tests done data
TDone <- Summary_testDone %>% 
   filter(str_starts(`Sample Type`, "no_") | `Sample Type` =="N_A")
 
# create no. of facilities 
FacSource <- Summary_testDone %>% 
  filter(str_starts(`Sample Type`, "No")) %>% 
      rename(
        "Facilities"   =  "Sample Type",
        "Oct-Dec_1"   =   "Oct-Dec",  
        "Jan-Mar_1"   =  "Jan-Mar"
      )

#### combine the columns
TestDone  <-  cbind(TDone,FacSource) 

### select columns of interest and re-arrange periods
TestDone <- TestDone %>% 
  select(`Sample Type`,`Oct-Dec`,`Jan-Mar`,`Oct-Dec_1`,`Jan-Mar_1`)  

#### rename the sample type column name
TestDone <- TestDone %>% 
  mutate(`Sample Type`  = recode(`Sample Type`,
              "no_VL"   =  "VL",
              "no_Plasma"   =  "Plasma",
              "no_DBS"      =  "DBS",
              "N_A"    =  "NA",
              "no_EID"  =  "EID"))

#### the table
tbl_TestDone  <-  flextable(TestDone)

### format the table
tbl_TestDone   <-   tbl_TestDone %>% 
   add_header_row(values = c(
     `Sample Type`  =  "Sample Type",
      "No. of samples", "",
     "No. of originating facilities",""
  ))
## set the labels
tbl_TestDone   <-   tbl_TestDone %>% 
  set_header_labels(
    `Sample Type`  =  "Sample Type",
    `Oct-Dec`    =    "Oct-Dec",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec_1`    =    "Oct-Dec",
    `Jan-Mar_1`    =   "Jan-Mar"
      )
# Merging columns
tbl_TestDone   <-   tbl_TestDone %>%
   merge_at(i = 1, j = 2:3, part = "header") %>% 
   merge_at(i = 1, j = 4:5, part = "header")

# Merging vertically for the first five columns in the header
tbl_TestDone   <-   tbl_TestDone %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_TestDone   <-   tbl_TestDone %>%
  vline(j = c(1, 3, 5), part = "all")


tbl_TestDone   <-   tbl_TestDone %>%
  add_header_row(values="Number of samples received at CPHL, with, number of originating health facilities", 
                 colwidths = ncol(TestDone))

tbl_TestDone

###################################################################
###################################################################
#### Overall TAT ####
#### Report VL and EID Quarterly Overall TAT####
VL_TATO   <-  df %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
  group_by(Qtr) %>% 
  summarise(
    VL  =   median(TATO, na.rm = TRUE)
  )

EID_TATO   <-  Edf %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
  group_by(Qtr) %>% 
  summarise(
    EID   =   median(TATO, na.rm = TRUE)
  )
#### create the combined EID,VL data frame
OverallTAT  =  bind_cols(VL_TATO,EID_TATO)

#### re-arrange the table
OverallTATRpt   <-  OverallTAT %>% 
  select(`Qtr...1`,VL,EID)

### pivot
OverallTATPvt   <-   OverallTATRpt %>% 
  pivot_longer(
    cols = c("VL","EID"),
    names_to = "Sample Type",
    values_to = "TAT"
  )
# final arrangement
OvllTAT  <- OverallTATPvt %>% 
  pivot_wider(
    names_from = Qtr...1,
    values_from = TAT
  ) %>% 
  select(`Sample Type`,`Oct-Dec`,`Jan-Mar`)

#### The table
tble_OvllTAT  <-  flextable(OvllTAT)

### format the table
tble_OvllTAT  <- tble_OvllTAT %>% 
  add_header_row(values = c(
    `Sample Type`  =  "Sample Type",
    "Reporting Period", ""
  ))
## set the labels
tble_OvllTAT  <- tble_OvllTAT %>% 
  set_header_labels(
    `Sample Type`  =  "Sample Type",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )

tble_OvllTAT  <- tble_OvllTAT %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
tble_OvllTAT  <- tble_OvllTAT %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_OvllTAT  <- tble_OvllTAT %>% 
  vline(j = c(1, 2, 3), part = "all")


tble_OvllTAT  <- tble_OvllTAT %>% 
  add_header_row(values="EID/VL Overall TAT", colwidths = ncol(OvllTAT))

  tble_OvllTAT

#### Report - EID Overall Weekly TAT  ####
ETAT_Wk_Ovall <- Edf %>% 
  mutate (Wnum = lubridate::isoweek(date_received))           

ETAT_Wk_Ovall <- ETAT_Wk_Ovall %>% 
  mutate(Wnum = as.character(Wnum))           # convert to character to allow manipulation

# Step 2: Filter and arrange the data
ETAT_Wk_Ovall <- ETAT_Wk_Ovall %>% 
  filter(TATO >= 0) %>% 
  filter(Wnum %in% c(40:52,1:24)) %>%
  select(Wnum, TATO) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATO)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
ETAT_Wk_overall_Pvt11 <- ETAT_Wk_Ovall %>%
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATO,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

# correct weeks flow, and the final Ms Excel sheet
ETAT_Wk_overall_Pvt11  <-  ETAT_Wk_overall_Pvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))

#### The code to download the data for graph prism graphing
#### write.xlsx(ETAT_Wk_overall_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/EID/EIDOverall.xls")

#### the r graph



#### creating the r graph
ETAT_Wk_Ovall <- ETAT_Wk_Ovall %>%
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:14)),
                    ordered = TRUE))

# The graph
ggplot(ETAT_Wk_Ovall, aes(x = as.factor(w), y = TATO, fill = w)) +
  geom_boxplot() +
  geom_hline(yintercept = 14, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATO") +
  labs(title = "Centralized EID Test Result by Week") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Report - VL Overall Weekly TAT ####
TAT_Wk_Ovall <- df %>% 
  mutate (Wnum = lubridate::isoweek(date_received))           

TAT_Wk_Ovall <- TAT_Wk_Ovall %>% 
  mutate(Wnum = as.character(Wnum))           # convert to character to allow manipulation

# Step 2: Filter and arrange the data
TAT_Wk_Ovall <- TAT_Wk_Ovall %>% 
  filter(Wnum %in% c(40:52, 1:14)) %>%
  select(Wnum, TATO) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATO)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
TAT_Wk_overall_Pvt11 <- TAT_Wk_Ovall %>%
  filter(TATO >= 0 & TATO < 90) %>% 
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATO,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

# correct flow and the Ms Excel download sheet
TAT_Wk_overall_Pvt11  <-  TAT_Wk_overall_Pvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))


#### The code to download the data for graph prism graphing
#### write.xlsx(ETAT_Wk_overall_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/EID/EIDOverall.xls")

#### the r graph

#### creating the r graph
TAT_Wk_OvallGrh <- TAT_Wk_Ovall %>%
  filter(TATO >= 0 & TATO < 90) %>% 
  filter(TATO < 90) %>% 
  filter(!is.na("Wnum")) %>% 
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:14)),
                    ordered = TRUE))

# The graph
ggplot(TAT_Wk_OvallGrh, aes(x = as.factor(w), y = TATO, fill = w)) +
  geom_boxplot() +
  geom_hline(yintercept = 14, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATO") +
  labs(title = "Centralized VL Test Result Overall TAT by Week") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



##################################################################
##################################################################
#### Lab TAT ####
#### Report VL/EID Quarterly Lab TAT####
VL_TATL   <-  df %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
  group_by(Qtr) %>% 
   summarise(
     VL  =   median(TATL, na.rm = TRUE)
   )
 
 EID_TATL   <-  Edf %>% 
   filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
   group_by(Qtr) %>% 
   summarise(
     EID   =   median(TATL, na.rm = TRUE)
   )
#### create the combined EID,VL data frame
 LabTAT  =  bind_cols(VL_TATL,EID_TATL)

#### re-arrange the table
LabTATRpt   <-  LabTAT %>% 
   select(`Qtr...1`,VL,EID)

### pivot
LabTATPvt   <-   LabTATRpt %>% 
  pivot_longer(
    cols = c("VL","EID"),
    names_to = "Sample Type",
    values_to = "TAT"
  )
# final arrangement
LabsTAT  <- LabTATPvt %>% 
  pivot_wider(
    names_from = Qtr...1,
    values_from = TAT
  ) %>% 
  select(`Sample Type`,`Oct-Dec`,`Jan-Mar`)

#### The table
tble_LabTAT  <-  flextable(LabsTAT)

### format the table
tble_LabTAT  <- tble_LabTAT %>% 
  add_header_row(values = c(
    `Sample Type`  =  "Sample Type",
    "Reporting Period", ""
  ))
## set the labels
tble_LabTAT  <- tble_LabTAT %>% 
  set_header_labels(
    `Sample Type`  =  "Sample Type",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )

tble_LabTAT  <- tble_LabTAT %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
tble_LabTAT  <- tble_LabTAT %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_LabTAT  <- tble_LabTAT %>% 
  vline(j = c(1, 2, 3), part = "all")


tble_LabTAT  <- tble_LabTAT %>% 
  add_header_row(values="EID/VL Lab TAT", colwidths = ncol(LabsTAT))

tble_LabTAT

#### Report - VL Lab Weekly TAT ####
 # obtaining the week number
 # Step 1: Create a week number column
TAT_Wk_Lab <- df %>% 
    mutate(Wnum = isoweek(date_received)) %>% 
    mutate(Wnum = as.character(Wnum))  # Convert week number to character

# Step 2: Filter and arrange the data
TAT_Wk_Lab <- TAT_Wk_Lab %>% 
  filter(Wnum %in% c(40:52,1:14)) %>%
  select(Wnum, TATL) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATL)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
TAT_Wk_Lab_Pvt11 <- TAT_Wk_Lab %>%
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATL,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

#### Correcting the order of week numbers for Graph prism graph
TAT_Wk_Lab_Pvt11   <-   TAT_Wk_Lab_Pvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))

#### The graph prism download; write.xlsx(ETAT_Wk_overall_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/VL/VLWKLYlab.xls") 
  
#### creating the r graph
TAT_Wk_Lab <- TAT_Wk_Lab %>%
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:14)),
                    ordered = TRUE))

# The graph
ggplot(TAT_Wk_Lab, aes(x = as.factor(w), y = TATL, fill = w)) +
  geom_boxplot() +
  geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATL") +
  labs(title = "Box Plot of VL Lab TAT by Week Number") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Report - EID Lab TAT by week ####
ETAT_Wk_Lab <- Edf %>% 
  mutate (Wnum = lubridate::isoweek(date_received))           

ETAT_Wk_Lab <- ETAT_Wk_Lab %>% 
  mutate(Wnum = as.character(Wnum))           # convert to character to allow manipulation

# Step 2: Filter and arrange the data
ETAT_Wk_Lab <- ETAT_Wk_Lab %>% 
  filter(Wnum %in% c(40:52,1:14)) %>%
  select(Wnum, TATL) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATL)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
ETAT_Wk_Lab_Pvt11 <- ETAT_Wk_Lab %>%
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATL,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

# correct flow
ETAT_Wk_Lab_Pvt11  <-  ETAT_Wk_Lab_Pvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))


#### creating the r graph
ETAT_Wk_Labdf11 <- ETAT_Wk_Lab %>%
  filter(!is.na(Wnum)) %>% 
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:14)),
                    ordered = TRUE))


# The graph
plot2 <- ggplot(ETAT_Wk_Labdf11, aes(x = as.factor(Wnum), y = TATL, fill = Wnum)) +
  geom_boxplot() +
  geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATL") +
  labs(title = "Box Plot of EID Lab TAT by Week Number") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# creating data frame for lab weekly TAT
ETAT_Wk_Lab <- ETAT_Wk_Lab %>% 
  select(Wnum,TATL) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATL))

######################################################################
######################################################################
#### Sample Transport TAT ####
#### Report - Quarterly Sample Transport TAT ####
VL_TATR   <-  df %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
  group_by(Qtr) %>% 
  summarise(
    VL  =   median(TATR, na.rm = TRUE)
  )

EID_TATR   <-  Edf %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
  group_by(Qtr) %>% 
  summarise(
    EID   =   median(TATR, na.rm = TRUE)
  )
#### create the combined EID,VL data frame
TrpTAT  =  bind_cols(VL_TATR,EID_TATR)

#### re-arrange the table
TrpTATRpt   <-  TrpTAT %>% 
  select(`Qtr...1`,VL,EID)

### pivot
TrpTATPvt   <-   TrpTATRpt %>% 
  pivot_longer(
    cols = c("VL","EID"),
    names_to = "Sample Type",
    values_to = "TAT"
  )
# final arrangement
TransportTAT  <- TrpTATPvt %>% 
  pivot_wider(
    names_from = Qtr...1,
    values_from = TAT
  ) %>% 
  select(`Sample Type`,`Oct-Dec`,`Jan-Mar`)

#### The table
tble_trpTAT  <-  flextable(TransportTAT)

### format the table
tble_trpTAT  <- tble_trpTAT %>% 
  add_header_row(values = c(
    `Sample Type`  =  "Sample Type",
    "Reporting Period", ""
  ))
## set the labels
tble_trpTAT  <- tble_trpTAT %>% 
  set_header_labels(
    `Sample Type`  =  "Sample Type",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )

tble_trpTAT  <- tble_trpTAT %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
tble_trpTAT  <- tble_trpTAT %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_trpTAT  <- tble_trpTAT %>% 
  vline(j = c(1, 2, 3), part = "all")


tble_trpTAT  <- tble_trpTAT %>% 
  add_header_row(values="EID/VL Sample Transport TAT", 
                 colwidths = ncol(TransportTAT))

tble_trpTAT 
#### Report VL Transport TAT by week ####
TAT_Wk_Trp <- df %>% 
  mutate (Wnum = lubridate::isoweek(date_received))           

TAT_Wk_Trp <- TAT_Wk_Trp %>% 
  mutate(Wnum = as.character(Wnum))           # convert to character to allow manipulation

# Step 2: Filter and arrange the data
TAT_Wk_Trp <- TAT_Wk_Trp %>% 
  filter(Wnum %in% c(40:52,1:14)) %>%
  select(Wnum, TATR) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATR)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
TAT_Wk_Trp_Pvt11 <- TAT_Wk_Trp %>%
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATR,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

# correct flow
TAT_Wk_Trp_Pvt11  <-  TAT_Wk_Trp_Pvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))

#### The graph prism download; write.xlsx(TAT_Wk_Trp_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/VL/VLWKLYlab.xls") 

#### The r graph
#### creating the r graph
TAT_Wk_Trpdf1 <- TAT_Wk_Trp %>%
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:14)),
                    ordered = TRUE))
# The graph
ggplot(TAT_Wk_Trpdf1, aes(x = as.factor(Wnum), y = TATR, fill = Wnum)) +
  geom_boxplot() +
  geom_hline(yintercept = 4, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATR") +
  labs(title = "VL Sample Receipt at CPHL TAT by Week Number") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Report EID Transport TAT by week ####
ETAT_Wk_Trp <- Edf %>% 
  mutate (Wnum = lubridate::isoweek(date_received))           

ETAT_Wk_Trp <- ETAT_Wk_Trp %>% 
  mutate(Wnum = as.character(Wnum))           # convert to character to allow manipulation

# Step 2: Filter and arrange the data
ETAT_Wk_Trp <- ETAT_Wk_Trp %>% 
  filter(Wnum %in% c(40:52, 1:14)) %>%
  select(Wnum, TATR) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATR)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
ETAT_Wk_Trp_Pvt11 <- ETAT_Wk_Trp %>%
  filter(TATR >= 0 & TATR < 90) %>% 
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATR,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

# correct flow AND final Ms Excel sheet
ETAT_Wk_Trp_Pvt11  <-  ETAT_Wk_Trp_Pvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))

#### The code to download the data for graph prism graphing
#### write.xlsx(ETAT_Wk_overall_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/EID/EIDOverall.xls")

#### the r graph

#### creating the r graph
ETAT_Wk_TrpGrh <- ETAT_Wk_Trp %>%
  filter(!is.na("Wnum")) %>% 
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:14)),
                    ordered = TRUE))

# The graph
ggplot(ETAT_Wk_TrpGrh, aes(x = as.factor(w), y = TATR, fill = w)) +
  geom_boxplot() +
  geom_hline(yintercept = 7, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATO") +
  labs(title = "Weekly EID sample transport") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


######################################################################
######################################################################
#### Download TAT ####
#### Report - Quarterly Download TAT ####
VL_TATD   <-  df %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
  group_by(Qtr) %>% 
  summarise(
    VL  =   median(TATD, na.rm = TRUE)
  )

EID_TATD   <-  Edf %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>%
  group_by(Qtr) %>% 
  summarise(
    EID   =   median(TATD, na.rm = TRUE)
  )
#### create the combined EID,VL data frame
DldTAT  =  bind_cols(VL_TATD,EID_TATD)

#### re-arrange the table
DldTATRpt   <-  DldTAT %>% 
  select(`Qtr...1`,VL,EID)

### pivot
DldTATPvt   <-   DldTATRpt %>% 
  pivot_longer(
    cols = c("VL","EID"),
    names_to = "Sample Type",
    values_to = "TAT"
  )
# final arrangement
DwnLodTAT  <- DldTATPvt %>% 
  pivot_wider(
    names_from = Qtr...1,
    values_from = TAT
  ) %>% 
  select(`Sample Type`,`Oct-Dec`,`Jan-Mar`)

#### The table
tble_dldTAT  <-  flextable(DwnLodTAT)

### format the table
tble_dldTAT  <- tble_dldTAT %>% 
  add_header_row(values = c(
    `Sample Type`  =  "Sample Type",
    "Reporting Period", ""
  ))
## set the labels
tble_dldTAT  <- tble_dldTAT %>% 
  set_header_labels(
    `Sample Type`  =  "Sample Type",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )

tble_dldTAT  <- tble_dldTAT %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
tble_dldTAT  <- tble_dldTAT %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_dldTAT  <- tble_dldTAT %>% 
  vline(j = c(1, 2, 3), part = "all")


tble_dldTAT  <- tble_dldTAT %>% 
  add_header_row(values="EID/VL Download TAT", 
                 colwidths = ncol(DwnLodTAT))

tble_dldTAT
#### Report VL Download TAT by week ####
TAT_Wk_dld <- df %>% 
  mutate (Wnum = lubridate::isoweek(date_received))           

TAT_Wk_dld <- TAT_Wk_dld %>% 
  mutate(Wnum = as.character(Wnum))           # convert to character to allow manipulation

# Step 2: Filter and arrange the data
TAT_Wk_dld <- TAT_Wk_dld %>% 
  filter(Wnum %in% c(40:52,1:14)) %>%
  select(Wnum, TATD) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATD)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
TAT_Wk_dldPvt11 <- TAT_Wk_dld %>%
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATD,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

# correct weeks flow, and the final Ms Excel sheet
TAT_Wk_dldPvt11  <-  TAT_Wk_dldPvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))

#### The code to download the data for graph prism graphing
#### write.xlsx(TAT_Wk_dldPvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/EID/EIDOverall.xls")

#### the r graph

#### creating the r graph
TAT_Wk_dld <- TAT_Wk_dld %>%
  filter(TATD < 90) %>% 
  filter(!is.na(w)) %>% 
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:13)),
                    ordered = TRUE))

# The graph
ggplot(TAT_Wk_dld, aes(x = as.factor(w), y = TATD, fill = w)) +
  geom_boxplot() +
  geom_hline(yintercept = 3, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATD") +
  labs(title = "Sample Results Download by Week") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Report EID Download TAT by week ####
ETAT_Wk_dld <- Edf %>% 
  mutate (Wnum = lubridate::isoweek(date_received))           

ETAT_Wk_dld  <- ETAT_Wk_dld  %>% 
  mutate(Wnum = as.character(Wnum))           # convert to character to allow manipulation

# Step 2: Filter and arrange the data
ETAT_Wk_dld  <- ETAT_Wk_dld  %>% 
  filter(Wnum %in% c(40:52, 1:14)) %>%
  select(Wnum, TATD) %>% 
  arrange(desc(Wnum)) %>% 
  filter(!is.na(TATD)) %>% 
  ungroup() %>% 
  mutate(Wnum = paste0("w",Wnum))

## Step 3; creating the data frame
ETAT_Wk_dld_Pvt11 <- ETAT_Wk_dld  %>%
  filter(TATD >= 0 & TATD < 90) %>% 
  group_by(Wnum) %>% 
  mutate(row_id = row_number()) %>% # Create a unique row identifier for each Wnum
  ungroup() %>%
  pivot_wider(
    names_from = Wnum,
    values_from = TATD,
    values_fill = NA, # Fill missing values with NA
    values_fn = list # Use list to handle duplicates
  ) %>%
  unnest(cols = everything()) %>%  # Expand list-columns into individual rows
  select(-row_id)

# correct flow AND final Ms Excel sheet
ETAT_Wk_dld_Pvt11  <-  ETAT_Wk_dld_Pvt11 %>% 
  select(num_range("w", c(40:52, 1:14)))

#### The code to download the data for graph prism graphing
#### write.xlsx(ETAT_Wk_overall_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/EID/EIDOverall.xls")

#### the r graph

#### creating the r graph
ETAT_Wk_dld <- ETAT_Wk_dld %>%
  filter(!is.na(Wnum)) %>% 
  mutate(w = factor(Wnum, levels = c(paste0("w", 40:52), paste0("w", 1:14)),
                    ordered = TRUE))

# The graph
ggplot(ETAT_Wk_dld, aes(x = as.factor(w), y = TATD, fill = w)) +
  geom_boxplot() +
  geom_hline(yintercept = 3, color = "red", linetype = "dashed", size = 1) +
  xlab("Week Number") +
  ylab("TATD") +
  labs(title = "Weekly EID Download TAT") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
######################################################################
######################################################################
#### %age VL Lab results released within 4 Days TAT ####
Pct_4DTAT <- df %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    N      = sum(!is.na(TATL)),
    N_4D   = sum(TATL >= 0 & TATL  <= 4, na.rm = TRUE),
    VL = round((N_4D/N)*100,0)
  )
# re-arrange the table
Pct_4DTATPvt   <-   Pct_4DTAT %>% 
  select(Qtr,VL) %>% 
  pivot_longer(
    cols = "VL",
    names_to = "Sample Type",
    values_to = "%age samples"
  )

#### %age EID Lab results released within 4 Days TAT ####
EPct_4DTAT <- Edf %>% 
  filter(Qtr  %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    N      = sum(!is.na(TATL)),
    N_4D   = sum(TATL >= 0 & TATL  <= 4, na.rm = TRUE),
    EID = round((N_4D/N)*100,0)
  )
# re-arrange the table
EPct_4DTATPvt   <-   EPct_4DTAT %>% 
  select(Qtr,EID) %>% 
  pivot_longer(
    cols = "EID",
    names_to = "Sample Type",
    values_to = "%age samples"
  )

#### Report - Overall %age Lab results released within 4 Day TAT ####
PctRsts  <-   bind_cols(EPct_4DTAT,Pct_4DTAT)

#### the calculation
PctRstsdf <- PctRsts %>% 
  mutate(
    No.samples = rowSums(across(starts_with("N.")), na.rm = TRUE),
    No.timely  = rowSums(across(starts_with("N_4D")), na.rm = TRUE)
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
tbl_PctRstsdf   <-  flextable(PctRstsdf)


tbl_PctRstsdf  <-  tbl_PctRstsdf %>% 
  add_header_row(values = "%age samples results released by CPHL within 4 day TAT",
                 colwidths = ncol(PctTrpdf))

tbl_PctRstsdf

#### Report - Percentage of EID and VL(separate) samples downloaded within 4 days #### 
Pct_4D_release <- rbind(Pct_4DTATPvt,EPct_4DTATPvt)


Pct_4D_release$Qtr <- factor(Pct_4D_release$Qtr, 
                                        levels = c("Oct-Dec", "Jan-Mar"), 
                                        ordered = TRUE)
#### the graph
ggplot(Pct_4D_release, aes(x = `Sample Type`, y = `%age samples`, fill = Qtr)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = `%age samples`), vjust = -0.3, size = 3.5, position = position_dodge(width = 0.9)) +  # Ensure text aligns with bars
  geom_hline(yintercept = 75, color = "#950606", linetype = "dashed") +  
  annotate("text", x = 2, y = 75, label = "Target (75%)", vjust = -0.5, color = "#950606") +  
  theme_minimal() +
  labs(
    title = "% of EID/VL sample results released within 4 Day TAT",
    x = "Sample Type",
    y = "TAT (Days)"
  )

######################################################################
######################################################################
#### Report - EID at 0 - 2 months ####
`1stPcr` <- Edf %>% 
  filter(PCR == "1")

# total number
FirstPcr_all <- `1stPcr` %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_HEI = n()) %>% 
      filter(!is.na(RRH)) 

#### re-arrange the data frame
PcrallPvt   <-  FirstPcr_all %>%
  filter(Qtr %in% c("Oct-Dec", "Jan-Mar")) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_HEI
  )
  
#### number with timely EID 
FirstPcr2M <- `1stPcr` %>%
  filter(!is.na(RRH)) %>% 
  filter(Qtr %in% c("Oct-Dec", "Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_timely = sum(age >=0 & age < 2.1),
    .groups = "drop")
      
#### re arrange the table
FirstPcr2MPvt   <-  FirstPcr2M %>% 
  filter(Qtr %in% c("Oct-Dec", "Jan-Mar")) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_timely
  )

#### combine the two data frames columns
PCR2m   <-  PcrallPvt %>% 
  left_join(FirstPcr2MPvt, by = "RRH")

#### the report
nat.2m   <-  PCR2m %>%
  ungroup() %>%  # Ensure any previous grouping is removed
  summarise(
    RRH = "NATIONAL", # Create a row called "NATIONAL"
    across(where(is.numeric), sum, na.rm = TRUE), # Sum numeric columns
    .groups = "drop" # Remove grouping
  )

### combine the data frames
PCR2m  <-  rbind(PCR2m,nat.2m)

#### calculate the proportions
PCR2m   <-   PCR2m %>% 
  mutate(
    across(ends_with(".y"),
           ~ round(.x/get(sub(".y$",".x",cur_column()))*100,0),
           .names = "Pct_{.col}"
  ))

#### select columns of interest
PCR2mRpt   <-   PCR2m %>% 
  select(RRH,`Pct_Oct-Dec.y`,`Pct_Jan-Mar.y`)

# function to color the columns
timely_ConEID   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the tables
`1stPCR2m`  <- flextable(PCR2mRpt)


# format the table
`1stPCR2m`   <-  `1stPCR2m` %>% 
  add_header_row(
    values = c(
      RRH  =  "RRH",
      "%age of HEIs accesing timely 1st PCR in the respective Qtrs", ""
    ))

## set table labels
`1stPCR2m`   <-  `1stPCR2m` %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Pct_Jan-Mar.y`   =  "Jan-Mar",
    `Pct_Oct-Dec.y`   =  "Oct-Dec"
  )

### List Columns to Apply Coloring ###
columns <- c("Pct_Jan-Mar.y", "Pct_Oct-Dec.y")

### Apply Background Color ###
for (col in columns) {
  `1stPCR2m`   <-  `1stPCR2m` %>% 
    bg(j = col, bg = timely_ConEID(PCR2mRpt[[col]]), part = "body")
}

`1stPCR2m`   <-  `1stPCR2m` %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
`1stPCR2m`   <-  `1stPCR2m` %>% 
  merge_v(j = 1, part = "header")  

# Adding vertical lines to improve readability
`1stPCR2m`   <-  `1stPCR2m` %>% 
  vline(j = c(1,3), part = "all")


`1stPCR2m`   <-  `1stPCR2m` %>% 
  add_header_row(values = "%age of HEIs accessing timely EID through centralized lab testing",
                 colwidths = ncol(PCR2mRpt))

`1stPCR2m`

########################################################################
########################################################################
#### Report - Percentage of 2-12 months ####
#### number of HEIs at 2-12 
FirstPcr_2_12 <- `1stPcr` %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_timely = sum(age >= 2.1 & age <= 12)) %>% 
  filter(!is.na(RRH))


#### re-arrange the data frame
Pcr_2_12Pvt   <-  FirstPcr_2_12 %>%
  filter(Qtr %in% c("Oct-Dec", "Jan-Mar")) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_timely
  )

#### combine the two data frames columns
PCR12m   <-  PcrallPvt %>%   # PcrallPvt picked from above KPI
  left_join(Pcr_2_12Pvt, by = "RRH")

#### the report
nat.12m   <-  PCR12m %>%
  ungroup() %>%  # Ensure any previous grouping is removed
  summarise(
    RRH = "NATIONAL", # Create a row called "NATIONAL"
    across(where(is.numeric), sum, na.rm = TRUE), # Sum numeric columns
    .groups = "drop" # Remove grouping
  )

### combine the data frames
PCR12m  <-  rbind(PCR12m,nat.12m)

#### calculate the proportions
PCR12m   <-   PCR12m %>% 
  mutate(
    across(ends_with(".y"),
           ~ round(.x/get(sub(".y$",".x",cur_column()))*100,0),
           .names = "Pct_{.col}"
    ))

#### select columns of interest
PCR12mRpt   <-   PCR12m %>% 
  select(RRH,`Pct_Oct-Dec.y`,`Pct_Jan-Mar.y`)


# the tables
`1stPCR12m`  <- flextable(PCR12mRpt)


#### format the table
`1stPCR12m`   <-  `1stPCR12m` %>% 
  add_header_row(
    values = c(
      RRH  =  "RRH",
      "%age of HEIs accesing timely 1st PCR at 2-12 in the respective Qtrs", ""
    ))

## set table labels
`1stPCR12m`   <-  `1stPCR12m` %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Pct_Jan-Mar.y`   =  "Jan-Mar",
    `Pct_Oct-Dec.y`   =  "Oct-Dec"
  )


`1stPCR12m`   <-  `1stPCR12m` %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
`1stPCR12m`   <-  `1stPCR12m` %>% 
  merge_v(j = 1, part = "header")  

# Adding vertical lines to improve readability
`1stPCR12m`   <-  `1stPCR12m` %>% 
  vline(j = c(1,3), part = "all")


`1stPCR12m`   <-  `1stPCR12m` %>% 
  add_header_row(values = "%age of HEIs accessing timely EID through centralized lab testing at 2-12Months",
                 colwidths = ncol(PCR2mRpt))

`1stPCR12m`

#####################################################################
######################################################################
#### Positivity rate ####

# de duplicate the infants
all_infant   <-   Edf %>% 
distinct(IName, Result,Sex, `EXP Number`, .keep_all = TRUE)

## calculating the positivity rate
Pos   <-   all_infant %>% 
  filter(Qtr %in%  c("Oct-Dec", "Jan-Mar")) %>% 
  group_by(Qtr) %>% 
  summarise(
    No.tested  =  sum(Result %in% c("Negative","Positive"), na.rm = TRUE),
    No.Pos     = sum(Result == "Positive", na.rm = TRUE)
  )%>% 
  mutate(
    Con_Pos   =  round((No.Pos/No.tested)*100,1)
)
# table  
posty  <-  flextable(Pos)


posty   <-  posty %>% 
  add_header_row(values = "EID Positivity Rate",
                 colwidths = ncol(Pos))

posty

#### Report - Positivity by RRH ####
Pos_RRH  <- all_infant %>%                        # number of infants positive (duplicates removed)
  group_by(RRH,Qtr) %>% 
  summarise(
    No.tested  =  sum(Result %in% c("Negative","Positive"), na.rm = TRUE),
    No.Pos     = sum(Result == "Positive", na.rm = TRUE)
  )%>% 
  mutate(
    Con_Pos   =  round((No.Pos/No.tested)*100,1)
  )
#### create the report
Pos_RRHRpt   <-  Pos_RRH %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
  filter(!is.na(RRH)) %>% 
  select(RRH,Qtr,Con_Pos) %>% 
    pivot_wider(
      names_from = Qtr,
      values_from = Con_Pos
    )

#### re-arrange the table for correct Qtr flow
Pos_RRHRpt  <-  Pos_RRHRpt %>% 
  select(RRH,`Oct-Dec`, `Jan-Mar`)


#### the table
Tab_Pos_RRHRpt <- flextable(Pos_RRHRpt)

#### format the table
Tab_Pos_RRHRpt  <-   Tab_Pos_RRHRpt %>% 
  add_header_row(
    values = c(
      RRH  =  "RRH",
      "Positivity rate in the respective Qtrs", ""
    ))

## set table labels
Tab_Pos_RRHRpt  <-   Tab_Pos_RRHRpt %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Oct-Dec`   =  "Oct-Dec",
    `Jan-Mar`   =  "Jan-Mar"
  )


Tab_Pos_RRHRpt  <-   Tab_Pos_RRHRpt %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
Tab_Pos_RRHRpt  <-   Tab_Pos_RRHRpt %>% 
  merge_v(j = 1, part = "header")  

# Adding vertical lines to improve readability
Tab_Pos_RRHRpt  <-   Tab_Pos_RRHRpt %>% 
  vline(j = c(1,3), part = "all")


Tab_Pos_RRHRpt  <-   Tab_Pos_RRHRpt %>% 
  add_header_row(values = "EID Positivity rate by RRH",
                 colwidths = ncol(Pos_RRHRpt))
Tab_Pos_RRHRpt

##########################################################################
###########################################################################
#### INCOMPLETE - Number identified positive at different age groups ####
No_ve <-   all_infant %>% 
  mutate(PCR = ifelse(is.na(PCR),"Not Indicated",PCR))

pos_age <- No_ve %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(Result == "Positive") %>% 
  group_by(Qtr,PCR) %>% 
  summarise(
    No_2m      = sum(age >= 0 &  age <= 2),
    No_2_12    = sum(age >2 & age <=12),
    No_above12 = sum(age > 12)
  )

# add to obtain the total - dnominator
pos_agePvt   <-  pos_age %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = c(No_2m,No_2_12,No_above12)
  )

#### re-arrange the data set
pos_agePvt   <-  pos_agePvt %>% 
  select(PCR,contains("Oct-Dec"), contains("Jan-Mar")
  )

# Create the graph: percentage of infants identified at respective age brackets
ggplot(PCR_PosRpt, aes(x = PCR, y = N, fill = Age_brac)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Pct_2m" = "#006400", "Pct_2_12" = "#F1C40F", "Pct12" = "red")) +
  labs(x = "PCR", y = "%age of HEIs", fill = "Age Bracket", title = "%ge of HEIs identified +Ve at respective age groups") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold")) +
  geom_text(aes(label = N), position = position_stack(vjust = 0.5), size = 3, color = "white"
  )



#### Report - Identified positives at each PCR level ####
#### calculate the number positives for each quarter
PCR_Pos <-  all_infant %>%
 filter(Qtr %in%  c("Oct-Dec", "Jan-Mar")) %>% 
  group_by(Qtr,PCR) %>% 
  summarise(
    No.tested  =  sum(Result %in% c("Negative","Positive"), na.rm = TRUE),
    No.Pos     = sum(Result == "Positive", na.rm = TRUE)
  )%>% 
  mutate(
    Con_Pos   =  round((No.Pos/No.tested)*100,1)
  )

#### select columns of interest
PCR_Pos$PCR  <-  as.character(PCR_Pos$PCR)

PCR_PosRpt   <-  PCR_Pos %>% 
  select(Qtr, PCR, Con_Pos) %>% 
  mutate(PCR = replace_na(PCR, "Not Indicated"))

#### re-arrange the data
PCR_PosPvt   <-  PCR_PosRpt %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Con_Pos
  )

PCR_PosPvt  <-  PCR_PosPvt %>% 
  select(PCR,`Oct-Dec`,`Jan-Mar`)

#### the table

tbl_PCR_PosPvt   <-  flextable(PCR_PosPvt)
tbl_PCR_PosPvt
 
##################################################################################
#####################################################################################
#### Rejection Rates ####
#### Report - EID/VL Rejection Rate ####
 
#### VL Rejection rate
VL_rejec  <- df %>% 
   filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
   filter(!is.na(status)) %>% 
   group_by(Qtr,status) %>% 
    summarise(
      N = n(),.groups = "drop"
      )
#### correct data type
VL_rejec$status  <-  as.character(VL_rejec$status)

# Raname 0 and 1
VL_rejec <- VL_rejec %>% 
  mutate(status   =   recode(status,
     "1"  = "Accepted",
     "0"  =  "Rejected"
  ))

#### re-arrange the data set
VL_rejecPvt   <-  VL_rejec %>% 
  pivot_wider(
    names_from = status,
    values_from = N
  )

#### EID Rejection Rate
EID_rejec  <- Edf %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(!is.na(Status)) %>% 
  group_by(Qtr,Status) %>% 
  summarise(
    N = n(),
    .groups = "drop"
  )

#### re-arrange the data set
EID_rejecPvt   <-  EID_rejec %>% 
  pivot_wider(
    names_from = Status,
    values_from = N
  )

#### Join the table
retntble   <-  bind_cols(VL_rejecPvt, EID_rejecPvt)

#### the calculation
retntbledf <- retntble %>% 
  mutate(
    No.Accepted = rowSums(across(starts_with("Accepted")), na.rm = TRUE),
    No.rejtns  = rowSums(across(starts_with("Rejected")), na.rm = TRUE)
  ) %>% 
  mutate(
    Total  =  No.Accepted + No.rejtns
  ) %>% 
  mutate(
    Overall = round((No.rejtns / Total) * 100, 1)
  ) %>% 
  select(`Qtr...1`, Overall) %>% 
  
  rename(
    Qtr = `Qtr...1`
  ) %>% 
  
  mutate(
    Qtr = factor(Qtr, levels = c("Oct-Dec", "Jan-Mar"))
  ) %>% 
  arrange(Qtr)

#### the table
tbl_retntbledf  <-  flextable(retntbledf)

tbl_retntbledf  <-  tbl_retntbledf %>% 
  add_header_row(values = "Overall EID/VL Rejection Rate",
                 colwidths = ncol(retntbledf))

tbl_retntbledf


#### Report - EID and VL sample rejection rate ####
#### calculate the rejection rate
#### VL
VL_rejecPvt1   <-  VL_rejecPvt %>%
  rowwise() %>% 
  mutate(
  Total  =  Accepted+Rejected, 
  VL =  round((Rejected/Total)*100,1)
  ) %>% 
  select(Qtr,VL)


#### EID
#### calculate the rejection rate
EID_rejecPvt1   <-  EID_rejecPvt %>%
  rowwise() %>% 
  mutate(
    Total  =  Accepted+Rejected, 
    EID =  round((Rejected/Total)*100,1)
  ) %>% 
  select(Qtr,EID)

#### Combine the tables
R_Rate  <- VL_rejecPvt1 %>% 
  left_join(EID_rejecPvt1, by = "Qtr") %>% 
  left_join(retntbledf, by = "Qtr")

#### re-arrange the table
R_RatePvt  <-   R_Rate %>% 
  pivot_longer(
    cols = c("EID","VL", "Overall"),
    names_to = "Sample Type",
    values_to = "Rejection_Rate"
  )


#### Ensure correct display order for Qtr and Sample Type
R_RatePvt <- R_RatePvt %>%
  mutate(
    Qtr = factor(Qtr, levels = c("Oct-Dec", "Jan-Mar")),
    `Sample Type` = factor(`Sample Type`, levels = c("EID", "VL", "Overall"))
  )


#### the graph
ggplot(R_RatePvt, aes(x = `Sample Type`, y = Rejection_Rate, fill = Qtr)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = Rejection_Rate),
    vjust = -0.3,
    size = 3.5,
    position = position_dodge(width = 0.9)
  ) +
  geom_hline(yintercept = 1, color = "#950606", linetype = "dashed") +
  annotate("text", x = 2, y = 1, label = "Target (<1%)", vjust = -0.5, color = "#950606") +
  theme_minimal() +
  labs(
    title = "EID/VL Rejection Rate",
    x = "Sample Type",
    y = "Rejection Rate (%)"
  )

#### Report - EID/VL Rejection Rate by RRH ####
#### Report - VL rejection
RRH_VL_reject <- df %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(!is.na(Status)) %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    N = n(),
    N_reject = sum(Status == "0")
  ) %>% 
  mutate(
    VL  =  round((N_reject/N)*100,1)
  )
#### select columns of interest
VLRRH_rjt  <-   RRH_VL_reject %>%
  filter(!is.na(RRH)) %>% 
  select(RRH,Qtr,VL) %>% 
    pivot_wider(
      names_from = Qtr,
      values_from = VL
    ) %>% 
      select(RRH,`Oct-Dec`,`Jan-Mar`)

#### Report - EID rejection
RRH_EID_reject <- Edf %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(!is.na(Status)) %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    N = n(),
    N_reject = sum(Status == "Rejected")
  ) %>% 
  mutate(
    EID  =  round((N_reject/N)*100,1)
  )
#### select columns of interest
EIDRRH_rjt  <-   RRH_EID_reject %>%
  filter(!is.na(RRH)) %>% 
  select(RRH,Qtr,EID) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = EID
  ) %>% 
  select(RRH,`Oct-Dec`,`Jan-Mar`)

#### Combine the quarters
RRH_Rejects   <-   VLRRH_rjt %>% 
  left_join(EIDRRH_rjt, by = "RRH")

#### color coding function
RRate   <-  function(column_data){
  ifelse(column_data <= 1, "green",
         ifelse(column_data > 1 & column_data <= 2, "yellow","red"))
}


#### The table
tbl_RRH_Rejects  <-  flextable(RRH_Rejects) 
### format the table
tbl_RRH_Rejects  <-  tbl_RRH_Rejects %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "VL Rejection Rate by Qtr", "",
    "EID Rejection Rate by Qtr", ""
  ))
## set the labels
tbl_RRH_Rejects  <-  tbl_RRH_Rejects %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Oct-Dec.x`    =   "Oct-Dec",
    `Jan-Mar.x`    =    "Jan-Mar",
    `Oct-Dec.y`    =   "Oct-Dec",
    `Jan-Mar.y`    =    "Jan-Mar"
  )


### List Columns to Apply Coloring
columns <- c("Oct-Dec.x", "Jan-Mar.x","Oct-Dec.y","Jan-Mar.y")

### Apply Background Color ###
for (col in columns) {
  tbl_RRH_Rejects  <-  tbl_RRH_Rejects %>%
    bg(j = col, bg = RRate(RRH_Rejects[[col]]), part = "body")
}



tbl_RRH_Rejects  <-  tbl_RRH_Rejects %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertically for the first five columns in the header
tbl_RRH_Rejects  <-  tbl_RRH_Rejects %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_RRH_Rejects  <-  tbl_RRH_Rejects %>%
  vline(j = c(1, 3, 5), part = "all")


tbl_RRH_Rejects  <-  tbl_RRH_Rejects %>%
  add_header_row(values="EID/VL Rejection Rate", 
                 colwidths = ncol(RRH_Rejects))

tbl_RRH_Rejects

#### Report - %age of HFs with EID/VL <= 1% rejection rates ####
#### VL Health facility rejection rate
HF_VL_Rate <- df %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(!is.na(status)) %>%
  group_by(RRH,Qtr,District,HName,HFacility) %>% 
  summarise(
    N = n(),
    N_reject = sum(status == "0"),
    .groups = "drop")

### calculate the rejection rate
HF_VL_Rate <- HF_VL_Rate %>% 
  mutate(VL = round((N_reject/N)*100,1)
  ) %>% 
    select(RRH,Qtr,District,HName,HFacility, N,VL)

#### re-arrange the data frame
HF_VL_RatePvt  <-  HF_VL_Rate %>% 
  pivot_wider(
    id_cols = c(RRH,District,HName,HFacility),
    names_from = Qtr,
    values_from = VL
  )
#### HFs with high VL rejection rate
HF_high_VLReject <- HF_VL_RatePvt %>%
  summarise(
    `Sample Type`  =  "VL",
    N_Oct_Dec = sum(!is.na(`Oct-Dec`)),  # Count non-NA values in Oct-Dec
    N_Jan_Mar = sum(!is.na(`Jan-Mar`)),  # Count non-NA values in Jan-Mar
    `Oct-Dec` = sum(`Oct-Dec` <= 1.0, na.rm = TRUE),  # Count where Oct-Dec ≤ 1.0
    `Jan-Mar` = sum(`Jan-Mar` <= 1.0, na.rm = TRUE)   # Count where Jan-Mar ≤ 1.0
  ) %>% 
  
    mutate(
      `VLOct-Dec`  =  round((`Oct-Dec`/`N_Oct_Dec`)*100,0),
      `VLJan-Mar`  =  round((`Jan-Mar`/`N_Jan_Mar`)*100,0)
    ) %>% 
      select(`Sample Type`, starts_with("VL"))

#### rename the columns
HF_high_VLReject   <-  HF_high_VLReject %>% 
rename(
  "Oct-Dec"  =  "VLOct-Dec",
  "Jan-Mar"  =   "VLJan-Mar"  
)

#### EID Health facility rejection rate
HF_EID_Rate <- Edf %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(!is.na(Status)) %>%
  group_by(RRH,Qtr,District,HName,HFacility) %>% 
  summarise(
    N = n(),
    N_reject = sum(Status == "Rejected"),
    .groups = "drop")
### calculate the rejection rate
HF_EID_Rate <- HF_EID_Rate %>% 
  mutate(EID = round((N_reject/N)*100,1)
  ) %>% 
  select(RRH,Qtr,District,HName,HFacility, N,EID)

#### re-arrange the data frame
HF_EID_RatePvt  <-  HF_EID_Rate %>% 
  pivot_wider(
    id_cols = c(RRH,District,HName,HFacility),
    names_from = Qtr,
    values_from = EID
  )

#### HFs with high EID rejection rate
HF_high_EIDReject <- HF_EID_RatePvt  %>%
  summarise(
    `Sample Type`  =  "EID",
    N_Oct_Dec = sum(!is.na(`Oct-Dec`)),  # Count non-NA values in Oct-Dec
    N_Jan_Mar = sum(!is.na(`Jan-Mar`)),  # Count non-NA values in Jan-Mar
    `Oct-Dec` = sum(`Oct-Dec` <= 1.0, na.rm = TRUE),  # Count where Oct-Dec ≤ 1.0
    `Jan-Mar` = sum(`Jan-Mar` <= 1.0, na.rm = TRUE)   # Count where Jan-Mar ≤ 1.0
  ) %>% 
  
  mutate(
    `EOct-Dec`  =  round((`Oct-Dec`/N_Oct_Dec)*100,0),
    `EJan-Mar`  =  round((`Jan-Mar`/N_Jan_Mar)*100,0)
  ) %>% 
  select(`Sample Type`, starts_with("E"))

#### rename the columns
HF_high_EIDReject  <-  HF_high_EIDReject %>% 
  rename(
    "Oct-Dec"  =  "EOct-Dec",
    "Jan-Mar"  =   "EJan-Mar"  
  )

#### cOMBINE THE TABLE
Hf_RRate  <-  rbind(HF_high_VLReject,HF_high_EIDReject)

Hf_RRatePvt  <-  Hf_RRate %>% 
  pivot_longer(
    cols = c("Oct-Dec","Jan-Mar"),
    names_to = "Qtr",
    values_to = "Proportion"
  )

#### ensure correct Period dispaly in graph
Hf_RRatePvt$Qtr  <-   factor(Hf_RRatePvt$Qtr, levels = c("Oct-Dec","Jan-Mar"))

#### the graph
ggplot(Hf_RRatePvt, aes(x = `Sample Type`, y = Proportion, fill = Qtr)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Proportion), vjust = -0.3, size = 3.5, position = position_dodge(width = 0.9)) +  # Ensure text aligns with bars
  geom_hline(yintercept = 85, color = "#950606", linetype = "dashed") +  
  annotate("text", x = 2, y = 85, label = "Target (85%)", vjust = -1.5, color = "#950606") +  
  theme_minimal() +
  labs(
    title = "%age of health facilties with <=1% EID/VL rejection rate",
    x = "Sample Type",
    y = "Proportion"
  )
###############################################################
###############################################################
#### Report -  VL Rejection reasons ####
VL_rej_reason <- df %>% 
  filter(!is.na(rejection_reason)) %>% 
  filter(Qtr %in%  c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr,rejection_reason) %>% 
    summarise(
      N = n(),
      .groups = "drop"
    ) 

# Summarizing reasons for sample rejection
summary_VL_rej_reason <- VL_rej_reason %>% 
  mutate(
    rejection_reason = recode(
      rejection_reason,
      "ART Number details are shared by different patients from the same facility." = "Inconclusive patient specimen identifier",
      "Mismatching ART number between sample and form" = "Inconclusive patient specimen identifier",
      "Patient's dispatch form sent without patient's identifications(ART number or Other number)" = "Inconclusive patient specimen identifier",
      "Two forms were sent for the same sample(only dispatched results for one form)" = "Inconclusive patient specimen identifier",
      
      "All Dry Blood Spots on the card were less than the required size i.e not filling the perforated area" = "Poor sample quality, integrity/Wrong sample",
      "DBS card with presence of serum rings i.e improper drying(affecting the specimen integrity)" = "Poor sample quality, integrity/Wrong sample",
      "DBS sample was sent on non-perforated card(not able to get the right spot size during sample processing)" = "Poor sample quality, integrity/Wrong sample",
      "DBS sample was wet and or had moulds thus compromising with the sample integrity" = "Poor sample quality, integrity/Wrong sample",
      "Dry Blood sample sent with less than 2 spots" = "Poor sample quality, integrity/Wrong sample",
      "Recieved an empty DBS card" = "Poor sample quality, integrity/Wrong sample",
      "Sample was delivered under wrong storage container thus compromising the sample integrity" = "Poor sample quality, integrity/Wrong sample",
      "Specimen sent to CPHL Laboratory was  less than 0.75ml" = "Poor sample quality, integrity/Wrong sample",
      "Well labeled DBS card without any blood spots" = "Poor sample quality, integrity/Wrong sample",
      "Wrong sample type sent to CPHL-Laboratories,Only Plasma and Dried Blood Spots(DBS) are required" = "Poor sample quality, integrity/Wrong sample",
      "Wrong sample type was sent to CPHL-Laboratories,Only Plasma and Dried Blood Spots(DBS) are required" = "Poor sample quality, integrity/Wrong sample",
      
      "DBS Card without ART and form number details(DBS card cannot be linked to request form)" = "Missing,mismatching sample identifier",
      "Mismatching Specimen identifiers on request form and sample" = "Missing,mismatching sample identifier",
      "Plasma sample container without ART and form number details (sample cannot be linked to request form)" = "Missing,mismatching sample identifier",
      
      "Sample not recieved at CPHL Lab(only the request form was recieved at the testing Lab)" = "Sample Missing",
      
      "Specimen sent to CPHL was haemolysed" = "Haemolysed Sample"
    )
  )


summary_VL_rej_reason  <- summary_VL_rej_reason %>% 
  group_by(Qtr,Reject_reason) %>% 
    summarise(
      No_samples = sum(N),
      .groups = "drop"
    )

#### re-arrange the data
summary_VL_rejPvt   <-  summary_VL_rej_reason %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_samples
  )

#### obtain the totals
JanMar_Total  <- sum(summary_VL_rejPvt$`Jan-Mar`,na.rm = TRUE)
OctDec_Total  <- sum (summary_VL_rejPvt$`Oct-Dec`, na.rm = TRUE)

#### Create a table for the number of rejections
No_rejects <- data.frame(JanMar_Total, OctDec_Total) %>%
  mutate(`No. of rejected samples` = JanMar_Total + OctDec_Total) %>%
  select(`No. of rejected samples`, everything())  # Moves the new column to the first position

#### the table
tble_No_rejects  <-  flextable(No_rejects)

tble_No_rejects   <-  tble_No_rejects %>% 
  add_header_row(values = "Number of rejected samples",
                 colwidths = ncol(No_rejects))

tble_No_rejects

# add a colum  of the proportions (Denominator)
summary_VL_rejt_Rpt  <-  summary_VL_rejPvt %>% 
  mutate(
    `Oct-Dec`  =  round((ifelse(is.na(`Oct-Dec`),0, `Oct-Dec`)/OctDec_Total)*100,1), 
    `Jan-Mar`  =  round((ifelse(is.na(`Jan-Mar`),0, `Jan-Mar`)/JanMar_Total)*100, 1)
  ) 

#### the table
tbl_summary_VL_rejt_Rpt  <-  flextable(summary_VL_rejt_Rpt)
tbl_summary_VL_rejt_Rpt

#### Graph - common VL sample rejection reasons ####
# Prepare data and reorder Reject_reason by max(prop)
VL_rejtGraph <- summary_VL_rejt_Rpt %>%
  select(Reject_reason, `Oct-Dec`, `Jan-Mar`) %>%
  pivot_longer(
    cols = c("Oct-Dec", "Jan-Mar"),
    names_to = "Qtr",
    values_to = "prop"
  ) %>%
  filter(prop >= 5.0) %>%
  group_by(Reject_reason) %>%
  mutate(max_prop = max(prop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Reject_reason = reorder(Reject_reason, -max_prop))  # reorder descending by max prop

#### ensuring period flow, earliest to latest
VL_rejtGraph$Qtr <- factor(
  VL_rejtGraph$Qtr,
    labels = c("Oct-Dec", "Jan-Mar")
)

# Plot
ggplot(data = VL_rejtGraph, aes(x = Reject_reason, y = prop, fill = Qtr)) +
  geom_col(position = "dodge") +
  labs(
    x = "Rejection Reason",
    y = "% of total no. of samples rejected",
    title = "Most common reasons for VL Sample Rejection by Qtr"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 7),
    legend.position = "top"
  )

#### Report - EID reasons for rejection ####
EID_rej_reason <- Edf %>%
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(Qtr,Reject_reason) %>% 
  summarise(
    N = n(),
    .groups = "drop"
  ) %>% 
  filter(!is.na(Reject_reason))

#### renaming the reasons
summary_EID_rej_reason  <- EID_rej_reason %>% 
  mutate(Reject_reason    =   recode(Reject_reason,
            "DBS card had scratched Dried Blood Spots(contaminated card)" = "Poor sample quality, integrity/Wrong sample",
            "DBS card was sent to CPHL without any blood spot on it" = "Poor sample quality, integrity/Wrong sample",
             "DBS card was sent with overlapping blood spots(compromising the test integrity)" = "Poor sample quality, integrity/Wrong sample",
            "Wrong sample type" = "Poor sample quality, integrity/Wrong sample",
            "EXP Number is shared by different infants from the same facility" = "Inconclusive patient specimen identifier",
            "Infant details were written twice on the dispatch form.Results for one detail will be issued out" = "Inconclusive patient specimen identifier",
            "Mismatching ID/name between the sample and the form" = "Missing,mismatching sample identifier",
            "DBS sample was not recieved at CPHL" = "Sample not recieved at CPHL Lab",
            "Person was above 18 months of age(right age is between 1.5 to 18 months old)" = "Others",
            "Sample was sent for viral load testing i.e used wrong requisition form" = "Others",
            "Sample had insufficient blood spots less than 1/3 of a circle"  = "Sample with insufficient Blood Spots"
  ))

# Summary of reasons            
summary_EID_rej_reason  <- summary_EID_rej_reason %>% 
  group_by(Qtr,Reject_reason) %>% 
  summarise(
    No_samples = sum(N),
    .groups = "drop"
  )

#### re-arrange the data
summary_EID_rejPvt   <-  summary_EID_rej_reason %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_samples
  )


#### obtain the totals
JanMar_ETotal  <- sum(summary_EID_rejPvt$`Jan-Mar`,na.rm = TRUE)
OctDec_ETotal  <- sum (summary_EID_rejPvt$`Oct-Dec`, na.rm = TRUE)

#### Create a table for the number of rejections
No_Erejects <- data.frame(JanMar_ETotal, OctDec_ETotal) %>%
  mutate(`No. of rejected samples` = JanMar_ETotal + OctDec_ETotal) %>%
  select(`No. of rejected samples`, everything())  # Moves the new column to the first position

#### the table
tble_No_Erejects  <-  flextable(No_Erejects)

tble_No_Erejects   <-  tble_No_Erejects %>% 
  add_header_row(values = "Number of EID rejected samples",
                 colwidths = ncol(No_Erejects))

tble_No_Erejects

# add a colum  of the proportions (Denominator)
summary_EID_rejt_Rpt  <-  summary_EID_rejPvt %>% 
  mutate(
    `Oct-Dec`  =  round((ifelse(is.na(`Oct-Dec`),0, `Oct-Dec`)/OctDec_ETotal)*100,1), 
    `Jan-Mar`  =  round((ifelse(is.na(`Jan-Mar`),0, `Jan-Mar`)/JanMar_ETotal)*100, 1)
  ) 

#### the table
tbl_summary_EID_rejt_Rpt  <-  flextable(summary_EID_rejt_Rpt)
tbl_summary_EID_rejt_Rpt

#### THE GRAPH
# Prepare data and reorder Reject_reason by max(prop)
EID_rejtGraph <- summary_EID_rejt_Rpt %>%
  select(Reject_reason, `Oct-Dec`, `Jan-Mar`) %>%
  pivot_longer(
    cols = c("Oct-Dec", "Jan-Mar"),
    names_to = "Qtr",
    values_to = "prop"
  ) %>%
  filter(prop >= 4.0) %>%
  group_by(Reject_reason) %>%
  mutate(max_prop = max(prop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Reject_reason = reorder(Reject_reason, -max_prop))  # reorder descending by max prop

#### ensuring period flow, earliest to latest
EID_rejtGraph$Qtr <- factor(
  EID_rejtGraph$Qtr,
  labels = c("Oct-Dec", "Jan-Mar")
)

# Plot
ggplot(data = EID_rejtGraph, aes(x = Reject_reason, y = prop, fill = Qtr)) +
  geom_col(position = "dodge") +
  labs(
    x = "Rejection Reason",
    y = "% of total no. of samples rejected",
    title = "Most common reasons for EID Sample Rejection by Qtr"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 7),
    legend.position = "top"
  )

#############################################################################
##############################################################################
#### Annex  Incomplete ####
#### Report - Line list HF with high VL rejection rate ####
#### VL Rejection rate
HF_VL_Rate  <- df %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(!is.na(Status)) %>% 
  group_by(RRH, HFacility, Qtr, Status) %>% 
  summarise(
    N = n(),
    .groups = "drop"
  )
#### correct data type
HF_VL_Rate$Status  <-  as.character(HF_VL_Rate$Status)

# Raname 0 and 1
HF_VL_Rate <- HF_VL_Rate %>% 
  mutate(Status   =   recode(Status,
             "1"  = "Accepted",
            "0"  =  "Rejected"
  ))

#### re-arrange the data set
HF_VL_RatePvt   <-  HF_VL_Rate %>% 
  pivot_wider(
    names_from = Status,
    values_from = N
  )

#### calculate the rejection rate
HF_VL_RateRpt <- HF_VL_RatePvt %>%
  rowwise() %>%
  mutate(
    Total = sum(c(Accepted, Rejected), na.rm = TRUE),
    EID = ifelse(Total == 0, NA, round((Rejected / Total) * 100, 1))
  ) %>%
  ungroup() %>%
  select(RRH, HFacility, Qtr, EID) %>% 
    filter(!is.na(EID))

#### the facilities with highest Rejection rate
HFVL_list  <-  HF_VL_RateRpt %>% 
  filter(EID > 1.0)

#### re-arrange the data set
HFVL_listPvt  <-   HFVL_list %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = EID
  )

#### constantly high rejection rates
HFVL_listhigh  <-  HFVL_listPvt %>% 
  filter(!is.na(`Oct-Dec`) & !is.na(`Jan-Mar`)
  )

#### the table
tbl_HFVL_listhigh   <-  flextable(HFVL_listhigh)

tbl_HFVL_listhigh  <-  tbl_HFVL_listhigh %>% 
  add_header_row(values = "Line list of sites with >1% VL rejection rate in 2 consecutive quarter",
                 colwidths = ncol(HFVL_listhigh))

tbl_HFVL_listhigh

#### Report - Line list of HF with high EID rejection rate ####
HF_EID_Rate  <-  Edf %>% 
filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(!is.na(Status)) %>% 
  group_by(RRH, HFacility, Qtr, Status) %>% 
  summarise(
    N = n(),
    .groups = "drop"
  )

#### re-arrange the data set
HF_EID_RatePvt   <-  HF_EID_Rate  %>% 
  pivot_wider(
    names_from = Status,
    values_from = N
  )

#### calculate the rejection rate
HF_EID_RatePvtRpt <- HF_EID_RatePvt %>%
  rowwise() %>%
  mutate(
    Total = sum(c(Accepted, Rejected), na.rm = TRUE),
    EID = ifelse(Total == 0, NA, round((Rejected / Total) * 100, 1))
  ) %>%
  ungroup() %>%
  select(RRH, HFacility, Qtr, EID) %>% 
  filter(!is.na(EID))

#### the facilities with highest Rejection rate
HFEID_list  <-  HF_EID_RatePvtRpt %>% 
  filter(EID > 1.0)

#### re-arrange the data set
HFEID_listPvt  <-   HFEID_list %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = EID
  )

#### constantly high rejection rates
HFEID_listhigh  <-  HFEID_listPvt  %>% 
  filter(!is.na(`Oct-Dec`) & !is.na(`Jan-Mar`)
  )

#### the table
tbl_HFEID_listhigh   <-  flextable(HFEID_listhigh)

tbl_HFEID_listhigh  <-  tbl_HFEID_listhigh %>% 
  add_header_row(values = "Line list of sites with >1% EID rejection rate in 2 consecutive quarter",
                 colwidths = ncol(HFEID_listhigh))

tbl_HFEID_listhigh

#### Report - VL rejection rate by Hub ####
Hub_VL_Rate <- df %>% 
  filter(Qtr %in% c("Oct-Dec", "Jan-Mar")) %>% 
  filter(!is.na(Status)) %>%
  group_by(RRH,Qtr,HName) %>% 
  summarise(
    N = n(),
    N_reject = sum(Status == "0"),
    .groups = "drop")

Hub_VL_Rate <- Hub_VL_Rate %>% 
  rowwise() %>% 
  mutate(R_Rate = round((N_reject/N)*100,1)
  ) %>% 
  select(RRH, Qtr, HName,R_Rate)

# capaitalize each work
Hub_VL_Rate$HName  <-  toupper(Hub_VL_Rate$HName)

#### re-arrange the data
Hub_VL_RatePvt   <-  Hub_VL_Rate %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = R_Rate
  )

# Table 
Tbk_Hub_VLRtnPvt <- flextable(Hub_VL_RatePvt)

Tbk_Hub_VLRtnPvt  <-  Tbk_Hub_VLRtnPvt %>% 
  add_header_row(values = "VL rejection rate by hub", colwidths = 
                   ncol(Hub_VL_RatePvt))
Tbk_Hub_VLRtnPvt


#### The report #####

# Create a Word document
LabCOP24Q1 <- read_docx()


# Tests_done_summary
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(tbl_TestDone)

# Overall TAT
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(tble_OvllTAT)


# LabsTAT
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(tble_LabTAT)

# sample tranport TAT
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(tble_trpTAT)

# Donwload TAT
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(tble_dldTAT)


# national 1st pocr
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(`1stPCR2m`)

# nat.1st_12m
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(`1stPCR12m`)

# positivity rate
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(posty)

# NTable_RRH_pos
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(Tab_Pos_RRHRpt)

# positivity by RRH
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(tbl_PCR_PosPvt)


# VL rejection rate
LabCOP24Q1 <- LabCOP24Q1 %>%
  body_add_flextable(tbl_RRH_Rejects)

# Save the document
print(LabCOP24Q1, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Report/Analysis downloads/labQ2.docx")



#### Annex section of report ####
# Create a Word document
AnnexLab <- read_docx()

# with >1% VL rejection rate in 2 consecutive quarter
AnnexLab <- AnnexLab %>%
  body_add_flextable(tbl_HFVL_listhigh)


# Line list of sites with >1% EID rejection rate in 2 consecutive quarter
AnnexLab <- AnnexLab %>%
  body_add_flextable(tbl_HFEID_listhigh)

# VL rejection rate by hub
AnnexLab <- AnnexLab %>%
  body_add_flextable(Tbk_Hub_VLRtnPvt)


# Save the document
print(AnnexLab, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Report/Analysis downloads/annex.docx")


