#### EQA Analysis ####
#### Packages ####
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
  data.table,
  hrbrthemes,
  viridis,
  forcats,
  fmsb)

#### Import data sets ####
#### Databases####
Regions   <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/Regions.xlsx")
Hubs_RRH  <- import("D:/R works/CPHL_Lab/CPHL_Data/Databases/NSRTN/RRH_Hubs.xlsx")
testHF    <-  import("D:/R works/CPHL_Lab/CPHL_Data/Databases/HF_test.xlsx")
master    <-  import("D:/R works/CPHL_Lab/CPHL_Data/Databases/Health Facility Master File.xlsx")

#### EID data set ####
EID_enrolled4  <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/scheduled/EID_Sche.xlsx")  # Oct-Dec 24
EID_enrolled1  <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/EID scheduled.xlsx") # Jan - Mar 25
EID_enrolled2  <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/EID PT.xlsx")
EID_enrolled3  <-   import ("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/EID_response.xlsx")

#### combined data set 
EID_enrolled  <-  rbind(EID_enrolled4,EID_enrolled1, EID_enrolled2, EID_enrolled3)

#### Pass and Response
EID_EQA4       <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/EQA/EIDEQA.xlsx")
EID_EQA1       <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/EIDEQA.xlsx")
EID_EQA2       <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/EID PT.xlsx")
EID_EQA3       <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/EID_HIV.xlsx")

#### combine data sheets
#### remove the column on country
EID_EQA2   <-   EID_EQA2 %>% 
  select(-Country)

EID_EQA3   <-   EID_EQA3 %>% 
  select(-Country)
  
#### merge
EID_EQA   <-  rbind(EID_EQA4,EID_EQA1, EID_EQA2, EID_EQA3)


#### VL Data sets ####
VL_enrolled4        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/scheduled/HIV_Sche.xlsx")
VL_enrolled1        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/HIV Scheduled.xlsx")
VL_enrolled2        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/HIV PT.xlsx")
VL_enrolled3        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/HIV_VL_response.xlsx")

#### select columns
VL_enrolled3   <-   VL_enrolled3 %>% 
  select("EQA Code", "Facilty Name", "District", "Region", "Status")

#### add quarter
VL_enrolled3   <-   VL_enrolled3 %>% 
  mutate(
    Qtr  =  "Q3")
#### combine the data frames
VL_enrolled  <-  rbind(VL_enrolled4, VL_enrolled1, VL_enrolled2, VL_enrolled3)

### Pass 
VL_EQA4        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/EQA/HIVVLEQA.xlsx")
VL_EQA1        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/HIVEQA.xlsx")
VL_EQA2        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/HIV PT.xlsx")
VL_EQA3        <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/VL_HIV.xlsx")


#### select columns of interest
#Q2
VL_EQA2  <-  VL_EQA2 %>% 
  select("Facility", "District", "Overrall Score", "Period")
# Q3
VL_EQA3  <-  VL_EQA3 %>% 
  select("Facility", "District", "Overrall Score", "Period")

#### combine
VL_EQA  <-  rbind(VL_EQA4, VL_EQA1, VL_EQA2, VL_EQA3)

#### CrAg Data set ####
CrAg_enrolled4   <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/scheduled/CrAg_Sche.xlsx")
CrAg_enrolled1   <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/Crag Scheduled.xlsx")
CrAg_enrolled2   <-   import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/Crag PT.xlsx")
CrAg_enrolled3  <-    import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/CrAg_response.xlsx")

#### combine data sheets
#### remove unwanted columns
# Q4
CrAg_enrolled4  <-   CrAg_enrolled4 %>% 
  select(-c(`...1`, `Implementing Partner`,`Date Panels Released`, `Contact Person`,
            `Contact Email`, `Contact Phone`,Action ))
#### Add column of Qtr
CrAg_enrolled4   <-   CrAg_enrolled4 %>%
  mutate(
    Qtr =  "Q4"
  )

# Q3
CrAg_enrolled3  <-   CrAg_enrolled3 %>% 
  select(-c(`...1`, Country, `Implementing Partner`,`Date Panels Released`, `Contact Person`,
            `Contact Email`, `Contact Phone`,Action ))
#### Add column of Qtr
CrAg_enrolled3   <-   CrAg_enrolled3 %>%
  mutate(
    Qtr =  "Q3"
  )

#### merge the sheets
CrAg_enrolled   <-  rbind(CrAg_enrolled4,CrAg_enrolled1, CrAg_enrolled2, CrAg_enrolled3)

#### Pass and Response
CrAgEQA4  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/EQA/CrAgEQA.xlsx")
CrAgEQA1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/CrAgEQA.xlsx")
CrAgEQA2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/Crag PT.xlsx")
CrAgEQA3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/Serum_CrAg.xlsx")


#### combine sheets
# Q2
CrAgEQA2  <-  CrAgEQA2 %>% 
  select(-Country)

# Q3
CrAgEQA3  <-  CrAgEQA3 %>% 
  select(-Country)

#### Combine the sheets
CrAg_EQA  <-  rbind(CrAgEQA4,CrAgEQA1, CrAgEQA2, CrAgEQA3)

#### HBV Data set ####
HBV_Enrolled4  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/scheduled/HBV_Sche.xlsx")
HBV_Enrolled1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/HBV Scheduled.xlsx") 
HBV_Enrolled2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/HBV PT.xlsx")
HBV_Enrolled3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/HBV_response.xlsx") 


#### combine sheets
# Q2
HBV_Enrolled2  <-  HBV_Enrolled2 %>% 
  select(-c(`...1`,  Country, `Implementing Partner` , `Date Panels Released`,
            `Contact Person`, `Contact Email`, `Contact Phone`, Action)) %>% 
  
        mutate(Qtr =  "Q2")

# Q3
HBV_Enrolled3  <-  HBV_Enrolled3 %>% 
  select(-c(`...1`,  Country, `Implementing Partner` , `Date Panels Released`,
            `Contact Person`, `Contact Email`, `Contact Phone`, Action)) %>% 
  
  mutate(Qtr =  "Q3")

#### merge
HBV_Enrolled   <-  rbind(HBV_Enrolled4,HBV_Enrolled1, HBV_Enrolled2, HBV_Enrolled3)

### Pass and Response
HBV_EQA4  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/EQA/HBVEQA.xlsx")
HBV_EQA1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/HBVEQA.xlsx")
HBV_EQA2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/HBV PT.xlsx")
HBV_EQA3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/HBV.xlsx")

### Combine sheets
#Q2
HBV_EQA2   <-   HBV_EQA2 %>% 
  select(-Country)
#Q3
HBV_EQA3   <-   HBV_EQA3 %>% 
  select(-Country)
  
#### Merge
HBVEQA  <-  rbind(HBV_EQA4,HBV_EQA1, HBV_EQA2, HBV_EQA3)


#### HPV Data set  ####
HPV_enrolled4   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/scheduled/HPV_Sche.xlsx")
HPV_enrolled1   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/HPV Scheduled.xlsx")
HPV_enrolled2   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/HPV PT.xlsx")
HPV_enrolled3   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/HPV_response.xlsx")

#### select columns of interest
HPV_enrolled3  <-   HPV_enrolled3 %>% 
  select("EQA Code", "Facilty Name", "District", "Region", "Status")
#### add column for quarter
HPV_enrolled3  <-   HPV_enrolled3 %>% 
  mutate(Qtr  =  "Q3")

#### combine sheets
HPV_enrolled   <-  rbind(HPV_enrolled4,HPV_enrolled1, HPV_enrolled2, HPV_enrolled3)


## Pass and Response
HPV_EQA4  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/EQA/HPVEQA.xlsx")
HPV_EQA1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/HPVEQA.xlsx")
HPV_EQA2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/HPV PT.xlsx")
HPV_EQA3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/HPV.xlsx")

### combine sheets
# Q2
HPV_EQA2  <-   HPV_EQA2 %>% 
  select(-Country)

# Q3
HPV_EQA3  <-   HPV_EQA3 %>% 
  select(-Country)

#### merge
HPV_EQA  <-  rbind(HPV_EQA4,HPV_EQA1, HPV_EQA2, HPV_EQA3)

#### Malaria Data set ####
Mal_enrolled4   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/scheduled/Malaria_Sche.xlsx")
Mal_enrolled1   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/Malaria Scheduled.xlsx")
Mal_enrolled2   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/Malaria PT.xlsx")
Mal_enrolled3   <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/Malaria_response.xlsx")  

#### select columns of interest
Mal_enrolled3   <-   Mal_enrolled3 %>% 
  select("EQA Code", "Facilty Name", "District", "Region", "Status")

#### add the quarters
Mal_enrolled3   <-   Mal_enrolled3 %>% 
  mutate(Qtr  =  "Q3")

## combine sheets
Mal_enrolled  <-  rbind(Mal_enrolled4,Mal_enrolled1, Mal_enrolled2, Mal_enrolled3)


##Pass and Response
Mal_EQA4  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/EQA/MalariaEQA.xlsx")
Mal_EQA1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/MalariaEQA.xlsx")
Mal_EQA2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/Malaria PT.xlsx")
Mal_EQA3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/Malaria.xlsx")

#### combine sheets
# Q2
Mal_EQA2   <-  Mal_EQA2 %>% 
  select(-Country)
# Q3
Mal_EQA3   <-  Mal_EQA3 %>% 
  select(-Country)

#### merge
Mal_EQA   <-  rbind(Mal_EQA4,Mal_EQA1, Mal_EQA2, Mal_EQA3)


#### Gram Data set ####
Gram_Enrolled4  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/scheduled/Gram_Sche.xlsx")
Gram_Enrolled1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/Gram Scheduled.xlsx") 
Gram_Enrolled2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/Gram PT.xlsx")
Gram_Enrolled3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/Gram_response.xlsx")

## combine sheets
#Q2
Gram_Enrolled2   <-    Gram_Enrolled2 %>% 
  select(-Country)
#Q3
Gram_Enrolled3   <-    Gram_Enrolled3 %>% 
  select(-Country)

####adding date that was missed in database
# Q2
Gram_Enrolled2   <-    Gram_Enrolled2 %>% 
  mutate(`Date Panels Released` = ifelse(`Date Panels Released` == "-", "May 27, 2025", `Date Panels Released`))

# Q3
Gram_Enrolled3   <-    Gram_Enrolled3 %>% 
  mutate(`Date Panels Released` = ifelse(`Date Panels Released` == "-", "August 27, 2025", `Date Panels Released`))

#### merge
Gram_Enrolled  <-  rbind(Gram_Enrolled4,Gram_Enrolled1, Gram_Enrolled2, Gram_Enrolled3)

#### Pass and Response
Gram_EQA4  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/QA/EQA/GramEQA.xlsx")
Gram_EQA1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/GramEQA.xlsx")
Gram_EQA2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/Gram PT.xlsx")
Gram_EQA3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/Gram.xlsx")

#### combine sheets
# Q2
Gram_EQA2   <-   Gram_EQA2 %>% 
  select(-Country)
# Q3
Gram_EQA3   <-   Gram_EQA3 %>% 
  select(-Country)

#### merge
Gram_EQA   <-  rbind(Gram_EQA4,Gram_EQA1, Gram_EQA2, Gram_EQA3)


#### CD4 data sets ####
#### enrolled
CD4_enrolled1  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/scheduled/CD4 Scheduled.xlsx")
CD4_enrolled2  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Scheduled/CD4 PT.xlsx")
CD4_enrolled3  <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Response/CD4_response.xlsx")

#### select columns of interest
CD4_enrolled3   <-  CD4_enrolled3 %>% 
  select("EQA Code", "Facilty Name", "District", "Region", "Status")

#### add period
CD4_enrolled3   <-  CD4_enrolled3 %>% 
  mutate(Qtr  = "Q3")

#### merge
CD4_enrolled   <-  rbind(CD4_enrolled1, CD4_enrolled2, CD4_enrolled3)

#### pass
CD4_EQA1       <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/QA/pass rate/CD4EQA.xlsx")
CD4_EQA2       <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/Pass/CD4 PT.xlsx")
CD4_EQA3       <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/Pass/CD4.xlsx")

#### merge
#Q2
CD4_EQA2   <-   CD4_EQA2 %>% 
  select(-Country)
#Q3
CD4_EQA3   <-   CD4_EQA3 %>% 
  select(-Country)

#### merge
CD4_EQA   <-  rbind(CD4_EQA1, CD4_EQA2, CD4_EQA3)

########################################################################
########################################################################
#### Cleaning EQA data sets ####
#### EID EQA ####
#### Correct district names
EIDEQA   <-   EID_EQA %>% 
  mutate(District   =   recode(District,
                  "Luweero"   =   "LUWERO"))

# change names of facility    
EIDEQA    <-  EIDEQA %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

# correcting the names
EIDEQA   <-   EIDEQA %>% 
  mutate(HFacility     =   recode(HFacility,
            # New               #  Old                         
            "Maracha HCI V"    =   "MARACHA HC IV",
            "Yinga HC IV"   =  "YINGA HC III",
            "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
            "Mt.St Mary`s Hospital"  =  "MT. ST. MARY'S HOSPITAL-DOK",
            "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
            "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                  "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                  "Gulu TASO Gulu Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                  "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                  "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                  "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                  "Ober HC IV"   =   "OBER HC III",
                                  "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                  "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                  "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                  "Bukuya HC III"    =   "BUKUYA HC IV",
                                  "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                  "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III",
                                  "Bukwo Hospital"    =   "BUKWO GENERAL HOSPITAL",
                                   "St. Anthony's Hospital"   =  "ST. ANTHONY'S TORORO HOSPITAL",
                                   "TASO Gulu  Special Clinic"   =   "TASO GULU  SPECIAL CLINIC",
                                   "UNHLS / Central Public Health Laboratories (EID LAB II)"   = "CENTRAL PUBLIC HEALTH LABORATORIES",
                                   "Mildmay Hospital Uganda"  =  "MILDMAY UGANDA HOSPITAL",
            "UNHLS / Central Public Health Laboratories (EID LAB I)"   = "CENTRAL PUBLIC HEALTH LABORATORIES",
            "TASO GULU  SPECIAL CLINIC"    =   "TASO GULU SPECIAL CLINIC"
                                  
                              ))

#### Capitalize the names
EIDEQA$District  <- toupper(EIDEQA$District)
EIDEQA$HFacility  <- toupper(EIDEQA$HFacility)


# Add RRH to the file
EIDEQA_Data    <-  EIDEQA %>%
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )
    
#### print names missing
EIDCheck <-  EIDEQA_Data %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>%  
  print()


#### print names missing RRH
EIDCheckRRH <-  EIDEQA_Data %>% 
  filter(is.na(RRH)) %>% 
  print()

#### EID Scheduled ####
# Correct district names
EID_sche   <-   EID_enrolled %>% 
  mutate(District   =   recode(District,
                "Luweero"   =   "LUWERO"))

# correcting the names
EID_sche   <-   EID_sche %>% 
  mutate(`Facilty Name`     =   recode(`Facilty Name`,
            # Old              #  New                        
            "Maracha HCI V"    =   "MARACHA HC IV",
            "Yinga HC IV"   =  "YINGA HC III",
            "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
            "Mt.St Mary`s Hospital"  =  "MT. ST. MARY'S HOSPITAL-DOK",
            "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
            "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
            "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
            "TASO Gulu  Special Clinic"  =  "TASO GULU  SPECIAL CLINIC",
            "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
            "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
            "Lwampanga HC IV"    =  "LWAMPANGA HC III",
            "Ober HC IV"   =   "OBER HC III",
            "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
            "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
            "Kotido HC IV"   =  "KOTIDO HOSPITAL",
            "Bukuya HC III"    =   "BUKUYA HC IV",
            "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
            "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III",
            "Bukwo Hospital"    =   "BUKWO GENERAL HOSPITAL",
            "St. Anthony's Hospital"   =  "ST. ANTHONY'S TORORO HOSPITAL",
            "TASO Gulu  Special Clinic"   =   "TASO GULU  SPECIAL CLINIC",
            "UNHLS / Central Public Health Laboratories (EID LAB II)"   = "CENTRAL PUBLIC HEALTH LABORATORIES",
            "UNHLS / Central Public Health Laboratories (EID LAB I)"   = "CENTRAL PUBLIC HEALTH LABORATORIES",
            "Otuke HCIV"    =  "OTUKE HC IV",
            "Mildmay Hospital Uganda"  =  "MILDMAY UGANDA HOSPITAL",
            "Uganda Blood Transfusion Services (UBTS)"   =  "UGANDA BLOOD TRANSFUSION SERVICE (UBTS)",
            "TASO GULU  SPECIAL CLINIC"    =   "TASO GULU SPECIAL CLINIC"
             ))

# rename columns
EID_sche <-   EID_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

# capitalize district
EID_sche$District    <-  toupper(EID_sche$District) # capitalize district
EID_sche$HFacility  <-  toupper(EID_sche$HFacility)

#### add RRH details
EID_sche <-   EID_sche %>% 
  left_join(Regions, by = "District")

# select columns of interest for analysis
# selecting common sites from EID EQA data frame
EID_scheData <-   EID_sche %>% 
  select(RRH,District,HFacility,Status,Qtr)



#### print names missing
EIDscCheck <-  EID_scheData %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>%  
  print()

#### print names missing RRH
EIDChecsckRRH <-  EIDEQA_Data %>% 
  filter(is.na(RRH)) %>% 
  print()


#### HIV EQA ####
#### Pass , response
VLEQA  <-   VL_EQA %>% 
  mutate(District   =   recode(District,
                "Luweero"   =   "LUWERO",
                "luweero"   =   "LUWERO",
                "Ssembabule"    = "SEMBABULE",
                "CPHL"    =   "KAMPALA",
                "Ruhoko" =   "IBANDA",
                "cphl"    =   "KAMPALA"))

# change names of facility    
VLEQA    <-  VLEQA %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

#### CORRECT HC NAMING
VLEQA$HFacility  <-  gsub("HCIV", "HC IV", VLEQA$HFacility)
VLEQA$HFacility  <-  gsub("H/C", "HC", VLEQA$HFacility)
VLEQA$HFacility  <-  gsub("HCIII", "HC III", VLEQA$HFacility)
VLEQA$HFacility  <-  gsub("RR HOSPITAL", "REGIONAL REFERRAL HOSPITAL", VLEQA$HFacility)


# correcting the names
VLEQA    <-  VLEQA %>%
  mutate(HFacility     =   recode(HFacility,
              # New               #  Old                         
              "Maracha HCI V"    =   "MARACHA HC IV",
              "Yinga HC IV"   =  "YINGA HC III",
              "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
              "Mt.St Mary`s Hospital"  =  "MT. ST. MARY'S HOSPITAL-DOK",
              "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
              "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
              "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
              "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
              "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
              "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
              "Lwampanga HC IV"    =  "LWAMPANGA HC III",
              "Ober HC IV"   =   "OBER HC III",
              "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
              "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
              "Kotido HC IV"   =  "KOTIDO HOSPITAL",
              "Bukuya HC III"    =   "BUKUYA HC IV",
              "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
              "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III",
              "Bukwo Hospital"    =   "BUKWO GENERAL HOSPITAL",
              "St. Anthony's Hospital"   =  "ST. ANTHONY'S TORORO HOSPITAL",
              "TASO Gulu  Special Clinic"   =   "TASO GULU SPECIAL CLINIC",
              "UNHLS / Central Public Health Laboratories (EID LAB II)"   = "CENTRAL PUBLIC HEALTH LABORATORIES",
              "Otuke HCIV"    =  "OTUKE HC IV",
              "Butemba HC IV"    =  "BUTEMBA HC III",
              "Katabi Military Hospital"   =  "KATABI MILITARY HC III",
              "ST. JOSEPH HOSPITAL KITGUM"  =  "ST. JOSEPH'S KITGUM HOSPITAL",
              "Kajjansi HC III"      =   "KAJJANSI HC IV",
              "St. Luke Angal Hospital"   =   "ANGAL HOSPITAL",
              "Mbarara Municipal Hospital"   =  "MBARARA MUNICIPAL COUNCIL HC IV",
              "Lukaya Health Care Services"   =  "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
              "Luwunga HC III(1st Div Hospital)"   =  "LUWUNGA BARRACKS HC III",
              "Fortportal regional Referral Hospital"  = "FORT PORTAL REGIONAL REFERRAL HOSPITAL",
              "HOLY FAMILY VIRIKA HOSPITAL"   =  "VIRIKA HOSPITAL",
              "Masaka Police Clinic"    =   "MASAKA POLICE HC III",
              "MildMay Hospital"    =   "MILDMAY UGANDA HOSPITAL",
              "Bidi Bidi HC III"     =   "BIDIBIDI HC III",
              "Kihiihi HC IV"               =  "KIHIHI HC IV",
              "Gulu Prisons HC III"    =   "GULU PRISON HC III",
              "Kasese Minicipal Council"   =  "KASESE MUNICIPAL COUNCIL HC III",
              "Mwera HC III"      =   "MWERA HC IV",
              "Madi Opei HC IV"   =  "MADI-OPEI HC IV",
              "Magale HC IV"   =    "MAGALE (UCMB) HC IV",
              "Malaba HC IV"     =  "MALABA HC III",
              "Mbale Prison HC III"    =   "MBALE MAIN PRISONS HC III",
              "Mt.St Mary's Hospital"   =  "MT. ST. MARY'S HOSPITAL-DOK",
              "Rushooka HC IV (Mother Francisca Lechner)"  =  "MOTHER FRANCISCA LECHNER HC IV",
              "Holy Family Hospital Nyape"   =  "NYAPEA HOSPITAL",
              "St. Josephs Maracha Hospital"   =  "ST. JOSEPHS MARACHA HOSPITAL",
              "Updf 2nd Div. Hc Iv/Makenke"   =  "UPDF 2ND DIV. HC IV",
              "St. Joseph Kitovu Hospital"   =   "KITOVU HOSPITAL",
              "Kitgum General Hospital"   =  "KITGUM HOSPITAL",
              "Yumbe RR Hospital"   =  "YUMBE REGIONAL REFERRAL HOSPITAL",
              "UNHLS /Central Public Health Laboratories (COBAS 8800)"  =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (ALINITY)"   =   "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (HOLOGIC)"   =   "CENTRAL PUBLIC HEALTH LABORATORIES",
              "MildMay Hospital"   =   "MILDMAY UGANDA HOSPITAL",
              "Bidi Bidi HC III"   =  "BIDIBIDI HC III",
              "Gulu Prisons HC III"   =  "GULU PRISON HC III",
              "Katoojo Prisons HC III"   =  "KATOJO PRISONS HC III",
              "Kawaala HC III"    =   "KAWAALA HC IV",
              "kiswa HC IV"   =   "KISWA HC III",
              "PIDC MULAGO"     =   "MULAGO NRH - PIDC COE BAYLOR CLINIC",
              "Agwata HC III"   =   "AGWATTA HC III",
              "UNHLS/Central Public Health Laboratories (VL LAB-Alinity)"  =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS/Central Public Health Laboratories (VL LAB-Hologic)"    =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS/Central Public Health Laboratories (VL LAB-COBAS 8800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (C6800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "LMK Medical laboratories and Consultancies LTD (Main)"   =   "LMK MEDICAL LABORATORY AND CONSULTANCIES LTD",
              "Holy Family Hospital Nyapea"  =  "NYAPEA HOSPITAL",
              "Nsambya Hospital"    =  "ST. FRANCIS NSAMBYA HOSPITAL",
              "Nakawuka Hciii"   =   "NAKAWUKA HC III",
              "Kasensero HCII"    =  "KASENSERO HC II",
              "Gulu Prisons HC II"   =   "GULU PRISON HC III",
              "Taso Mulago"    =   "TASO MULAGO SPECIAL CLINIC",
              "st kizito Hospital Matany"   =  "MATANY HOSPITAL",
              "Ssekanyonyi HVIV"    =  "SSEKANYONYI HC IV",
              "Mbarara Prisons HC III"   =  "MBARARA MAIN PRISONS HC III",
              "Mulago NRH - PIDC Baylor"   =  "MULAGO NRH - PIDC COE BAYLOR CLINIC",
              "Garilaya HC III"    =  "GALIRAYA HC III",
              "Gulu Prisons HC III"    =  "GULU PRISON HC III",
              "St. Joseph Maracha Hospital"   =  "ST. JOSEPHS MARACHA HOSPITAL",
              "UNHLS /Central Public Health Laboratories (COBAS 6800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (Cobas 8800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (Hologic)"      =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (Alinity)"     =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (Cobas 8800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories(c6800"     =   "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS/Central Public Health Laboratories (VL LAB-COBAS 6800)"  =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (Cobas 8800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (C8800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (COBAS 6800)"  =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (c8800)"    =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "UNHLS /Central Public Health Laboratories (Cobas 6800)"   =  "CENTRAL PUBLIC HEALTH LABORATORIES",
              "Mildmay Hospital Uganda"      =   "MILDMAY UGANDA HOSPITAL",
              "International Hospital Kampala Hospital (IHK)"  =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
              "Mildmay Hospital Uganda."   =   "MILDMAY UGANDA HOSPITAL",
              "MILDMAY HOSPITAL UGANDA."   =   "MILDMAY UGANDA HOSPITAL",
              "Mbarara Municipal Council HC IV"   = "MBARARA MUNICIPAL COUNCIL HC IV",
              "Rushooka HC IV"      =   "MOTHER FRANCISCA LECHNER HC IV",
              "Mulago Baylor"       =   "MULAGO NRH - PIDC COE BAYLOR CLINIC",
              "Mulago NRH - MUJHU"  =  "MULAGO NRH - MUJHU CLINIC",
              "Garilaya HC III"    =  "GALIRAYA HC III",
              "Fortportal regional Referreal Hospital"  =  "FORT PORTAL REGIONAL REFERRAL HOSPITAL",
              "Kyankwanzi HC IV"   =  "KYANKWANZI HC III",
              "St. Joseph Kitovu Hospital"   =  "Kitovu Hospital",
              "St. Luke Angal Hospital"   =  "ANGAL HOSPITAL",
              "Yumbe Hospital"    =  "YUMBE REGIONAL REFERRAL HOSPITAL",
              "Rushooka HC IV (Mother Francisca Lechner)"   =  "MOTHER FRANCISCA LECHNER HC IV",
              "HOLY FAMILY VIRIKA HOSPITAL"   =  "VIRIKA HOSPITAL",
              "Agwata HC III" =   "AGWATTA HC III",
              "Masaka Police Clinic"   =  "MASAKA POLICE HC III",
              "MJAP-MMC  HC IV"    =   "MULAGO NRH - MJAP ISS CLINIC",
              "Kajjansi HC III"     =   "KAJJANSI HC IV",
              "Luwunga HC III(1st Div Hospital)"  = "LUWUNGA BARRACKS HC III",
              "MildMay Hospital"    =  "MILDMAY UGANDA HOSPITAL",
              "Gulu Prisons HC III"   =  "GULU PRISON HC III",
              "Bombo military Hospital"   =   "BOMBO GENERAL MILITARY HOSPITAL",
              "Butemba HC IV"   =  "BUTEMBA HC III",
              "Kasaala HC III"    =   "ST. MARY'S KASAALA HC III",
              "Kasese Minicipal Council"   =  "KASESE MUNICIPAL COUNCIL HC III",
              "Katabi Military Hospital"   =  "KATABI MILITARY HC III",
              "Katoojo Prisons"    =    "KATOJO PRISONS HC III",
              "Kauga Prisons"   =  "KAUGA PRISONS HC II",
              "Kawaala HC III"    =  "KAWAALA HC IV",
              "Lukaya Health Care Services"  =  "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
              "Madi Opei HC IV"    =  "MADI-OPEI HC IV",
              "Magale HC IV"   =         "MAGALE (UCMB) HC IV",
              "Malaba HC IV"      =  "MALABA HC III",
              "Mbale Prison HC III"          =  "MBALE MAIN PRISONS HC III",
              "Mbarara Prisons HC III" =  "MBARARA MAIN PRISONS HC III",
              "Mt.St Mary's Hospital"   =   "MT. ST. MARY'S HOSPITAL-DOK",
              "Mwera HC III"       =   "MWERA HC IV",
              "Nakawuka Hciii"     =   "NAKAWUKA HC III",
              "Nsambya Hospital"         =  "ST. FRANCIS NSAMBYA HOSPITAL",
              "St. Joseph Maracha Hospital"  = "ST. JOSEPHS MARACHA HOSPITAL",
              "Taso Mulago"         =  "TASO MULAGO SPECIAL CLINIC",
              "Updf 2nd Div. Hc Iv/Makenke"   =   "UPDF 2ND DIV. HC IV",
              "Acholi-Pii Military HC IV"   =  "ACHOLPII HC III",
              "Mildmay Hospital Uganda"    =  "MILDMAY UGANDA HOSPITAL",
              "Mildmay Hospital Uganda."    =  "MILDMAY UGANDA HOSPITAL",
              "Mbarara Municipal Hospital"   =   "MBARARA MUNICIPAL COUNCIL HC IV",
              "St. Josephs Kitgum Hospital"    =   "ST. JOSEPH'S KITGUM HOSPITAL",
              "Bidibidi Health Centre HC III"    =    "BIDIBIDI HC III",
              "St Luke Angal Hospital"     =   "ANGAL HOSPITAL",
              "Bidi Bidi HC III"   =         "BIDIBIDI HC III",
              "Fortportal regional Referral Hospital" =  "FORT PORTAL REGIONAL REFERRAL HOSPITAL"
               ))

#### capaitalize names
VLEQA$District  <- toupper(VLEQA$District)
VLEQA$HFacility  <- toupper(VLEQA$HFacility)


# Add RRH to the file
VLEQA    <-  VLEQA %>% 
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )


#### rename 
VLEQA_data  <-  VLEQA_data %>% 
  mutate(
    `Overrall Score`   =  recode(`Overrall Score`,
      # Old   =   # New
      "Satisfactory" =  "SATISFACTORY",
      "Un satisfactory"  = "UNSATISFACTORY"  
  ))

#### CHECKING NAMES
VLEQA_dataCHECK <- VLEQA_data %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE)

print(unique(VLEQA_dataCHECK$HFacility))

#### print names missing RRH
VLEQA_dataRRHCHEC <-  VLEQA_data %>% 
  filter(is.na(RRH)) %>% 
  print()


#### HIV scheduled ####
# Correct district names
VL_sche   <-   VL_enrolled %>% 
  mutate(District   =   recode(District,
            # Old       # New
            "Luweero"   =   "LUWERO",
            "Ssembabule"  =  "SEMBABULE",
            "luweero"     =   "LUWERO" 
                        ))

# rename columns
VL_sche   <-   VL_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

### correct the spellings of the health centres
VL_sche$HFacility <- gsub("HCIV", "HC IV", VL_sche$HFacility)
VL_sche$HFacility <- gsub("HCIII", "HC III", VL_sche$HFacility)
VL_sche$HFacility <- gsub("H/C IV", "HC IV", VL_sche$HFacility)
VL_sche$HFacility <- gsub("H/C III", "HC III", VL_sche$HFacility)
VL_sche$HFacility <- gsub("HCII", "HC II", VL_sche$HFacility)

# correcting the names
VL_sche   <-   VL_sche %>% 
  mutate(HFacility     =   recode(HFacility,
          # Old              #  New                        
            "Maracha HCI V"    =   "MARACHA HC IV",
            "Yinga HC IV"   =  "YINGA HC III",
            "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
            "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
            "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
            "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
            "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
            "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
            "Lwampanga HC IV"    =  "LWAMPANGA HC III",
            "Ober HC IV"   =   "OBER HC III",
            "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
            "Kotido HC IV"   =  "KOTIDO HOSPITAL",
            "Bukuya HC III"    =   "BUKUYA HC IV",
            "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
            "Acholi-Pii Military HCIV"  =  "ACHOLPII HC III",
            "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III",
            "Bukwo Hospital"        =   "BUKWO HC IV",
            "Mt.St Mary`s Hospital"   =  "MT. ST. MARY'S HOSPITAL-DOK",
            "St. Anthony's Hospital"  =  "ST. ANTHONY'S TORORO HOSPITAL",
            "TASO Gulu  Special Clinic"   =  "TASO GULU SPECIAL CLINIC",
            "Rushooka HCIV"               =   "MOTHER FRANCISCA LECHNER HC IV",
            "Otuke HCIV"                 =  "OTUKE HC IV",
          "MILDMAY HOSPITAL UGANDA."   =   "MILDMAY UGANDA HOSPITAL",
          "Mbarara Municipal Council HC IV"   = "MBARARA MUNICIPAL COUNCIL HC IV",
          "Rushooka HC IV"      =   "MOTHER FRANCISCA LECHNER HC IV",
          "Mulago Baylor"       =   "MULAGO NRH - PIDC COE BAYLOR CLINIC",
          "Mulago NRH - MUJHU"  =  "MULAGO NRH - MUJHU CLINIC",
          "Garilaya HC III"    =  "GALIRAYA HC III",
          "Fortportal regional Referreal Hospital"  =  "FORT PORTAL REGIONAL REFERRAL HOSPITAL",
          "Kyankwanzi HC IV"   =  "KYANKWANZI HC III",
          "St. Joseph Kitovu Hospital"   =  "Kitovu Hospital",
          "St. Luke Angal Hospital"   =  "ANGAL HOSPITAL",
          "Yumbe Hospital"    =  "YUMBE REGIONAL REFERRAL HOSPITAL",
          "Rushooka HC IV (Mother Francisca Lechner)"   =  "MOTHER FRANCISCA LECHNER HC IV",
          "HOLY FAMILY VIRIKA HOSPITAL"   =  "VIRIKA HOSPITAL",
          "Agwata HC III" =   "AGWATTA HC III",
          "Masaka Police Clinic"   =  "MASAKA POLICE HC III",
          "MJAP-MMC  HC IV"    =   "MULAGO NRH - MJAP ISS CLINIC",
          "Kajjansi HC III"     =   "KAJJANSI HC IV",
          "Luwunga HC III(1st Div Hospital)"  = "LUWUNGA BARRACKS HC III",
          "MildMay Hospital"    =  "MILDMAY UGANDA HOSPITAL",
          "Gulu Prisons HC III"   =  "GULU PRISON HC III",
          "Bombo military Hospital"   =   "BOMBO GENERAL MILITARY HOSPITAL",
          "Butemba HC IV"   =  "BUTEMBA HC III",
          "Kasaala HC III"    =   "ST. MARY'S KASAALA HC III",
          "Kasese Minicipal Council"   =  "KASESE MUNICIPAL COUNCIL HC III",
          "Katabi Military Hospital"   =  "KATABI MILITARY HC III",
          "Katoojo Prisons"    =    "KATOJO PRISONS HC III",
          "Kauga Prisons"   =  "KAUGA PRISONS HC II",
          "Kawaala HC III"    =  "KAWAALA HC IV",
          "Lukaya Health Care Services"  =  "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
          "Madi Opei HC IV"    =  "MADI-OPEI HC IV",
          "Magale HC IV"   =         "MAGALE (UCMB) HC IV",
          "Malaba HC IV"      =  "MALABA HC III",
          "Mbale Prison HC III"          =  "MBALE MAIN PRISONS HC III",
          "Mbarara Prisons HC III" =  "MBARARA MAIN PRISONS HC III",
          "Mt.St Mary's Hospital"   =   "MT. ST. MARY'S HOSPITAL-DOK",
          "Mwera HC III"       =   "MWERA HC IV",
          "Nakawuka Hciii"     =   "NAKAWUKA HC III",
           "Nsambya Hospital"         =  "ST. FRANCIS NSAMBYA HOSPITAL",
           "St. Joseph Maracha Hospital"  = "ST. JOSEPHS MARACHA HOSPITAL",
           "Taso Mulago"         =  "TASO MULAGO SPECIAL CLINIC",
          "Updf 2nd Div. Hc Iv/Makenke"   =   "UPDF 2ND DIV. HC IV",
          "Acholi-Pii Military HC IV"   =  "ACHOLPII HC III",
          "Mildmay Hospital Uganda"    =  "MILDMAY UGANDA HOSPITAL",
          "Mildmay Hospital Uganda."    =  "MILDMAY UGANDA HOSPITAL",
          "Mbarara Municipal Hospital"   =   "MBARARA MUNICIPAL COUNCIL HC IV",
          "St. Josephs Kitgum Hospital"    =   "ST. JOSEPH'S KITGUM HOSPITAL",
          "Bidibidi Health Centre HC III"    =    "BIDIBIDI HC III",
          "St Luke Angal Hospital"     =   "ANGAL HOSPITAL",
           "Bidi Bidi HC III"   =         "BIDIBIDI HC III",
          "Fortportal regional Referral Hospital" =  "FORT PORTAL REGIONAL REFERRAL HOSPITAL"
          ))

# capitalize district
VL_sche$District    <-  toupper(VL_sche$District) # capitalize district
VL_sche$HFacility  <-  toupper(VL_sche$HFacility)

#### add RRH details
VL_sche <-   VL_sche %>% 
  left_join(Regions, by = "District")

#### selecting columsn of interest
VL_scheData <-   VL_sche %>% 
  select(RRH,District,HFacility,Status,Qtr)


#### CHECK THE NAMES
VLEQADF_dataCHECK <- VL_scheData %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE)

print(VLEQADF_dataCHECK$HFacility)

#### print names missing RRH
VLEQA_dataRRHDFCHEC <-  VL_scheData %>% 
  filter(is.na(RRH)) %>% 
  print()



#### Crag EQA ####
CrAgEQA    <-   CrAg_EQA %>% 
mutate(District   =   recode(District,
        "Luweero"   =   "LUWERO"))

# change names of facility    
CrAgEQA    <-   CrAgEQA %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

# correcting the names
CrAgEQA    <-   CrAgEQA %>% 
  mutate(HFacility     =   recode(HFacility,
                                  # New               #  Old                         
                                  "Maracha HCI V"    =   "MARACHA HC IV",
                                  "Yinga HC IV"   =  "YINGA HC III",
                                  "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                  "MT. ST. MARY'S HOSPITAL-DOK"   =   "Mt.St Mary`s Hospital",
                                  "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                  "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                  "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                  "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                  "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                  "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                  "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                  "Ober HC IV"   =   "OBER HC III",
                                  "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                  "Rushooka HCIV"   =  "RUSHOKA HC IV",
                                  "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                  "Bukuya HC III"    =   "BUKUYA HC IV",
                                  "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                  "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                  "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                  "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                  "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                  "St. Anthony's Hospital"   =   "ST. ANTHONY'S TORORO HOSPITAL",
                                  "St. Francis Health Care Services HC III"   =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                  "Mt.St Mary`s Hospital"        =   "MT. ST. MARY'S HOSPITAL-DOK",
                                  "AIDS Information Centre"     =    "AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                  "Lancet Laboratories"     =   "LANCET LABORATORIES UGANDA"
                   ))

#### capitalize the names
CrAgEQA$District  <- toupper(CrAgEQA$District)
CrAgEQA$HFacility  <- toupper(CrAgEQA$HFacility)

# Add RRH to the file
CrAgEQA_Data    <-   CrAgEQA %>% 
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )


#### checking facility names
CrAgEQAcheck   <-  CrAgEQA_Data %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()
  
#### print names missing RRH
CrAgEQACHEC <-  CrAgEQA %>% 
  filter(is.na(RRH)) %>% 
  print()

#### CrAg Scheduled ####
# Correct district names
CrAg_sche   <-   CrAg_enrolled %>% 
  mutate(District   =   recode(District,
              "Luweero"   =   "LUWERO"))

# correcting the names
CrAg_sche   <-   CrAg_sche %>% 
  mutate(`Facilty Name`     =   recode(`Facilty Name`,
                                       # Old              #  New                        
                                       "Maracha HCI V"    =   "MARACHA HC IV",
                                       "Yinga HC IV"   =  "YINGA HC III",
                                       "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                       "Mt.St Mary`s Hospital"  =    "MT. ST. MARY'S HOSPITAL-DOK",   
                                       "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                       "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                       "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                       "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                       "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                       "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                       "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                       "Ober HC IV"   =   "OBER HC III",
                                       "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                       "Rushooka HCIV"   =  "RUSHOKA HC IV",
                                       "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                       "Bukuya HC III"    =   "BUKUYA HC IV",
                                       "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                       "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                       "Mulago National Referral Hospital"  =   "MULAGO HOSPITAL NATIONAL REFFERAL HOSPITAL-WARD 4A",
                                       "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                       "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                       "AIDS Information Centre"   =  "AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                       "St. Anthony's Hospital"  = "ST. ANTHONY'S TORORO HOSPITAL",
                                       "St. Francis Health Care Services HC III"  =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                       "Lancet Laboratories"     =   "LANCET LABORATORIES UGANDA",
                                       "St. Marys Clinical Laboratory (Moyo Hospital)"  =  "MOYO HOSPITAL"
                                       
                             ))

# rename columns
CrAg_sche <-   CrAg_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

# capitalize district
CrAg_sche$District    <-  toupper(CrAg_sche$District) # capitalize district
CrAg_sche$HFacility  <-  toupper(CrAg_sche$HFacility)


#### add RRH details
CrAg_sche <-   CrAg_sche %>% 
  left_join(Regions, by = "District")

# selecting common sites from EQA data frame
CrAg_scheData <-   CrAg_sche %>% 
  select(RRH,District,HFacility,Status,Qtr)


#### checking facility names
CrAg_schecheck   <-  CrAg_sche%>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()

#### print names missing RRH
CrAg_scheCHEC <-  CrAg_sche %>% 
  filter(is.na(RRH)) %>% 
  print()


#### HBV EQA ####
HBV_EQA   <-  HBVEQA %>% 
  mutate(District   =   recode(District,
                    "Luweero"   =   "LUWERO"))

# change names of facility    
HBV_EQA    <-   HBV_EQA %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

# correcting the names
HBV_EQA    <-   HBV_EQA %>% 
  mutate(HFacility     =   recode(HFacility,
                                  # New               #  Old                         
                                  "Maracha HCI V"    =   "MARACHA HC IV",
                                  "Yinga HC IV"   =  "YINGA HC III",
                                  "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                  "MT. ST. MARY'S HOSPITAL-DOK"   =   "Mt.St Mary`s Hospital",
                                  "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                  "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                  "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                  "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                  "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                  "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                  "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                  "Ober HC IV"   =   "OBER HC III",
                                  "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                  "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                  "Bukuya HC III"    =   "BUKUYA HC IV",
                                  "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                  "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                  "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                  "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                  "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                  "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                  "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                  "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                  "Bukwo Hospital"     =  "BUKWO HC IV",
                                  "St. Anthony's Hospital"   =   "ST. ANTHONY'S TORORO HOSPITAL",
                                  "KYALI HC III"    =   "NSAMU/KYALI HC III",
                                  "Watoto Medical Laboratory Suubi"   =  "WATOTO HC II",
                                  "St Apollo H/C III-Namasuba"  =  "APOLLO MEMORIAL MEDICAL CENTER",
                                  "Kyali HC III"    =  "NSAMU/KYALI HC III",
                                  "Touchlife medical care foundation LTD"    =   "TOUCHLIFE MEDICAL CARE FOUNDATION",
                                  "Marine Military HC II"   =   "MARINE MILITARY HC III",
                                  "KATWE (KIBOGA) HC III"     =  "KATWE (DWANIRO) HC III",
                                  "Rushooka HCIV"     =  "MOTHER FRANCISCA LECHNER HC IV",
                                  "Acholi-Pii Military HCIV"    =   "ACHOLPII HC III",
                                  "LMK Medical laboratories and Consultancies LTD (Main)"  =  "LMK MEDICAL LABORATORY AND CONSULTANCIES LTD",
                                  "Mengo Hospital Counselling and Homecare"   =  "MENGO HOSPITAL",
                                  "FAD MILITARY HCIV"    =   "FAD MILITARY HOSPITAL",
                                  "Uganda Cares-Soroti"   = "UGANDA CARES HC II",
                                  "Mildmay Hospital Uganda"   =  "MILDMAY UGANDA HOSPITAL",
                                  "Mt.St Mary`s Hospital"     =  "MT. ST. MARY'S HOSPITAL-DOK",
                                  "St Magdalene H/c II"    =   "ST. MAGDALENE HC II",
                                  "Hoima Police HC II"     =   "HOIMA POLICE HC III",
                                  "St.Joseph`s Buyege H/C III"  =  "BUYEGE HC III"
                                            ))

#### capitalize the names
HBV_EQA $District  <- toupper(HBV_EQA $District)
HBV_EQA $HFacility  <- toupper(HBV_EQA $HFacility)

# Add RRH to the file
HBVEQA_Data    <-   HBV_EQA %>% 
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )

#### checking facility names
HBV_EQAcheck   <-  HBV_EQA %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()

#### print names missing RRH
HBV_EQACHEC <-  HBV_EQA %>% 
  filter(is.na(RRH)) %>% 
  print()


#### HBV Scheduled ####
HBV_sche  <-  HBV_Enrolled %>% 
mutate(District   =   recode(District,
              "Luweero"   =   "LUWERO"))

# correcting the names
HBV_sche   <-   HBV_sche %>% 
  mutate(`Facilty Name`     =   recode(`Facilty Name`,
            # Old              #  New                        
           "Maracha HCI V"    =   "MARACHA HC IV",
          "Yinga HC IV"   =  "YINGA HC III",
          "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
          "Mt.St Mary`s Hospital"  =    "MT. ST. MARY'S HOSPITAL-DOK",   
         "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
          "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
          "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
          "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
          "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
         "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
          "Lwampanga HC IV"    =  "LWAMPANGA HC III",
          "Ober HC IV"   =   "OBER HC III",
          "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
          "Kotido HC IV"   =  "KOTIDO HOSPITAL",
          "Bukuya HC III"    =   "BUKUYA HC IV",
          "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
          "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
          "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
          "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
          "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
          "AIDS Information Centre"   =  "AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
          "St. Anthony's Hospital"  = "ST. ANTHONY'S TORORO HOSPITAL",
          "St. Francis Health Care Services HC III"  =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
          "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
          "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
          "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
          "Acholi-Pii Military HCIV"    =   "ACHOLPII HC III",
         "Bethany women`s Hospital"   =  "BETHANY WOMENS AND FAMILY HC III",
         "Bukwo Hospital"  =  "BUKWO GENERAL HOSPITAL",
         "Hoima Police HC II"    =  "HOIMA POLICE HC III",
         "Joint Clinical Research Center (JCRC)"  =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
         "Kyali HC III"     =  "NSAMU/KYALI HC III",
         "Marine Military HC II"   =  "MARINE MILITARY HC III",
         "St Apollo H/C III-Namasuba"  = "APOLLO MEMORIAL MEDICAL CENTER",
         "St Magdalene H/c II"   =  "ST. MAGDALENE HC II",
         "St.Joseph`s Buyege H/C III"   =  "ST. JOSEPH CLINIC HC III",
         "Watoto Medical Laboratory Suubi"    =  "WATOTO HC II",
         "Mengo Hospital Counselling and Homecare"  =  "MENGO HOSPITAL",
         "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
         "Joint Clinical Research Centre (JCRC_UG)"    =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
          "TMCG Clinical Laboratory"  =  "THE MEDICAL CONCIERGE GROUP (TMCG)",
         "Touchlife medical care foundation LTD"    =   "TOUCHLIFE MEDICAL CARE FOUNDATION",
         "KATWE (KIBOGA) HC III"     =  "KATWE (DWANIRO) HC III",
        "LMK Medical laboratories and Consultancies LTD (Main)"  =  "LMK MEDICAL LABORATORY AND CONSULTANCIES LTD",
          "FAD MILITARY HCIV"    =   "FAD MILITARY HOSPITAL",
        "ST.Mary`s Clinical Laboratory"   =  "MOYO HOSPITAL",
        "Mildmay Hospital Uganda"     =  "MILDMAY UGANDA HOSPITAL",
        "Uganda Cares-Soroti"      =  "UGANDA CARES HC II"
                   ))

# rename columns
HBV_sche   <-   HBV_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

# capitalize district
HBV_sche$District    <-  toupper(HBV_sche$District) # capitalize district
HBV_sche$HFacility  <-  toupper(HBV_sche$HFacility)

#### add RRH details
HBV_sche <-   HBV_sche %>% 
  left_join(Regions, by = "District")

# selecting common sites from EID EQA data frame
HBV_scheData <-   HBV_sche %>% 
  select(RRH,District,HFacility,Status,Qtr)

#### checking facility names
HBV_schecheck   <-  HBV_sche%>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()
#### print names missing RRH
HBV_EQAschCHEC <-  HBV_sche %>% 
  filter(is.na(RRH)) %>% 
  print()

#### HPV EQA ####
HPVEQA   <-  HPV_EQA %>% 
  mutate(District   =   recode(District,
                      "Luweero"   =   "LUWERO"))

# change names of facility    
HPVEQA     <-   HPVEQA  %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

# correcting the names
HPVEQA     <-   HPVEQA  %>% 
  mutate(HFacility     =   recode(HFacility,
                                  # New               #  Old                         
                                  "Maracha HCI V"    =   "MARACHA HC IV",
                                  "Yinga HC IV"   =  "YINGA HC III",
                                  "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                  "MT. ST. MARY'S HOSPITAL-DOK"   =   "Mt.St Mary`s Hospital",
                                  "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                  "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                  "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                  "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                  "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                  "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                  "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                  "Ober HC IV"   =   "OBER HC III",
                                  "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                  "Rushooka HCIV"   =  "RUSHOKA HC IV",
                                  "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                  "Bukuya HC III"    =   "BUKUYA HC IV",
                                  "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                  "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                  "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                  "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                  "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                  "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                  "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                  "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                  "Joint Clinical Research Center (JCRC)"  =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                  "UNHLS / Central Public Health Laboratories"  =  "CENTRAL PUBLIC HEALTH LABORATORIES"
                      ))


#### capitalize the names
HPVEQA$District  <- toupper(HPVEQA$District)
HPVEQA$HFacility  <- toupper(HPVEQA$HFacility)



# Add RRH to the file
HPVEQA_Data    <-   HPVEQA %>% 
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )


#### checking facility names
HPVEQAcheck   <-  HPVEQA %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()

#### print names missing RRH
HPVEQAschCHEC <-  HPVEQA %>% 
  filter(is.na(RRH)) %>% 
  print()

#### HPV Schedule ####
HPV_sche  <-  HPV_enrolled %>% 
  mutate(District   =   recode(District,
                               "Luweero"   =   "LUWERO"))

# correcting the names
HPV_sche   <-   HPV_sche %>% 
  mutate(`Facilty Name`     =   recode(`Facilty Name`,
                                       # Old              #  New                        
                                       "Maracha HCI V"    =   "MARACHA HC IV",
                                       "Yinga HC IV"   =  "YINGA HC III",
                                       "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                       "Mt.St Mary`s Hospital"  =    "MT. ST. MARY'S HOSPITAL-DOK",   
                                       "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                       "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                       "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                       "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                       "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                       "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                       "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                       "Ober HC IV"   =   "OBER HC III",
                                       "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                       "Rushooka HCIV"   =  "RUSHOKA HC IV",
                                       "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                       "Bukuya HC III"    =   "BUKUYA HC IV",
                                       "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                       "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                       "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                       "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                       "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                       "AIDS Information Centre"   =  "AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                       "St. Anthony's Hospital"  = "ST. ANTHONY'S TORORO HOSPITAL",
                                       "St. Francis Health Care Services HC III"  =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                       "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                       "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                       "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                       "Acholi-Pii Military HCIV"    =   "ACHOLPII HC III",
                                       "Bethany women`s Hospital"   =  "BETHANY WOMENS AND FAMILY HC III",
                                       "Bukwo Hospital"  =  "BUKWO GENERAL HOSPITAL",
                                       "Hoima Police HC II"    =  "HOIMA POLICE HC III",
                                       "Joint Clinical Research Center (JCRC)"  =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                       "Kyali HC III"     =  "NSAMU/KYALI HC III",
                                       "Marine Military HC II"   =  "MARINE MILITARY HC III",
                                       "St Apollo H/C III-Namasuba"  = "ST. APOLO HC III",
                                       "St Magdalene H/c II"   =  "ST. MAGDALENE HC II",
                                       "St.Joseph`s Buyege H/C III"   =  "ST. JOSEPH CLINIC HC III",
                                       "Watoto Medical Laboratory Suubi"    =  "WATOTO HC II",
                                       "Mengo Hospital Counselling and Homecare"  =  "MENGO HOSPITAL",
                                       "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                       "UNHLS / Central Public Health Laboratories"  =  "CENTRAL PUBLIC HEALTH LABORATORIES"
                             ))

# rename columns
HPV_sche   <-   HPV_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

# capitalize district
HPV_sche$District    <-  toupper(HPV_sche$District) # capitalize district
HPV_sche$HFacility  <-  toupper(HPV_sche$HFacility)

#### add RRH details
HPV_sche <-   HPV_sche %>% 
  left_join(Regions, by = "District")


# selecting common sites from EID EQA data frame
HPV_scheData <-   HPV_sche %>% 
  select(RRH,District,HFacility,Status,Qtr)


#### checking facility names
HPV_schecheck   <-  HPV_scheData %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()

#### print names missing RRH
HPVEQAschdfCHEC <-  HPV_sche %>% 
  filter(is.na(RRH)) %>% 
  print()

#### Malaria EQA ####
MalEQA   <-  Mal_EQA %>% 
  mutate(District   =   recode(District,
                    "Luweero"   =   "LUWERO"))

# change names of facility    
MalEQA   <-   MalEQA %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

# correcting the names
MalEQA   <-   MalEQA %>% 
  mutate(HFacility     =   recode(HFacility,
                                  # New               #  Old                         
                                  "Maracha HCI V"    =   "MARACHA HC IV",
                                  "Yinga HC IV"   =  "YINGA HC III",
                                  "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                  "MT. ST. MARY'S HOSPITAL-DOK"   =   "Mt.St Mary`s Hospital",
                                  "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                  "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                  "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                  "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                  "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                  "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                  "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                  "Ober HC IV"   =   "OBER HC III",
                                  "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                  "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                  "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                  "Bukuya HC III"    =   "BUKUYA HC IV",
                                  "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                  "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                  "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                  "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                  "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                  "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                  "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                  "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                  "watoto medical laboratory(LALOGI)"   =  "WATOTO HC II (LALOGI)",
                                  "FAD MILITARY HCIV"  =   "FAD MILITARY HOSPITAL",
                                  "Bukwo Hospital"   =   "BUKWO GENERAL HOSPITAL",
                                  "KATWE (KIBOGA) HC III"   =  "KATWE (DWANIRO) HC III",
                                  "Watoto Medical Laboratory Suubi"  =  "WATOTO HC II",
                                  "St Apollo H/C III-Namasuba"  =   "ST. APOLO HC III",
                                  "St. Anthony's Hospital"    =   "ST. ANTHONY'S TORORO HOSPITAL",
                                  "Acholi-Pii Military HCIV"    =   "ACHOLPII HC III",
                                  "Kyali HC III"   =   "NSAMU/KYALI HC III",
                                  "ST.Mary`s Clinical Laboratory"   =    "MOYO HOSPITAL",
                                  "St Magdalene H/c II"  =   "ST. MAGDALENE HC II",
                                  "Marine Military HC II"   =  "MARINE MILITARY HC III",
                                  "Touchlife medical care foundation LTD"   =  "TOUCHLIFE MEDICAL CARE FOUNDATION",
                                  "LMK Medical laboratories and Consultancies LTD (Main)"  =  "LMK MEDICAL LABORATORY AND CONSULTANCIES LTD",
                                  "St.Joseph`s Buyege H/C III"    =  "BUYEGE HC III",
                                  "Ruhoko HC II"    =  "RUHOKO HC III",
                                  "Joint Clinical Research Center (JCRC)"  =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                  "Hoima Police HC II"    =  "HOIMA POLICE HC III",
                                  "Uganda Cares-Soroti"    =  "UGANDA CARES HC II",
                                  "Mildmay Hospital Uganda"   =   "MILDMAY UGANDA HOSPITAL",
                                  "Mt.St Mary`s Hospital"    =  "MT. ST. MARY'S HOSPITAL-DOK"
                                    ))
#### capitalize the names
MalEQA$District  <- toupper(MalEQA$District)
MalEQA$HFacility  <- toupper(MalEQA$HFacility)


# Add RRH to the file
MalEQA_Data    <-   MalEQA %>% 
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )


#### checking facility names
MalEQAcheck   <-  MalEQA %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()


#### print names missing RRH
MalEQAdfCHEC <-  MalEQA %>% 
  filter(is.na(RRH)) %>% 
  print()


#### Malaria Schedule ####
Mal_sche  <-  Mal_enrolled %>% 
  mutate(District   =   recode(District,
                          "Luweero"   =   "LUWERO"))

# correcting the names
Mal_sche   <-   Mal_sche %>% 
  mutate(`Facilty Name`     =   recode(`Facilty Name`,
                                       # Old              #  New                        
                                       "Maracha HCI V"    =   "MARACHA HC IV",
                                       "Yinga HC IV"   =  "YINGA HC III",
                                       "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                       "Mt.St Mary`s Hospital"  =    "MT. ST. MARY'S HOSPITAL-DOK",   
                                       "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                       "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                       "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                       "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                       "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                       "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                       "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                       "Ober HC IV"   =   "OBER HC III",
                                       "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                       "Rushooka HCIV"   =  "RUSHOKA HC IV",
                                       "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                       "Bukuya HC III"    =   "BUKUYA HC IV",
                                       "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                       "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                       "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                       "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                       "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                       "AIDS Information Centre"   =  "AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                       "St. Anthony's Hospital"  = "ST. ANTHONY'S TORORO HOSPITAL",
                                       "St. Francis Health Care Services HC III"  =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                       "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                       "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                       "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                       "Acholi-Pii Military HCIV"    =   "ACHOLPII HC III",
                                       "Bethany women`s Hospital"   =  "BETHANY WOMENS AND FAMILY HC III",
                                       "Bukwo Hospital"  =  "BUKWO GENERAL HOSPITAL",
                                       "Hoima Police HC II"    =  "HOIMA POLICE HC III",
                                       "Joint Clinical Research Center (JCRC)"  =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                       "Kyali HC III"     =  "NSAMU/KYALI HC III",
                                       "Marine Military HC II"   =  "MARINE MILITARY HC III",
                                       "St Apollo H/C III-Namasuba"  = "APOLLO MEMORIAL MEDICAL CENTER",
                                       "St Magdalene H/c II"   =  "ST. MAGDALENE HC II",
                                       "St.Joseph`s Buyege H/C III"   =  "ST. JOSEPH CLINIC HC III",
                                       "Watoto Medical Laboratory Suubi"    =  "WATOTO HC II",
                                       "Mengo Hospital Counselling and Homecare"  =  "MENGO HOSPITAL",
                                       "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                       "FAD MILITARY HCIV"    =   "FAD MILITARY HOSPITAL",
                                       "KATWE (KIBOGA) HC III"   =  "KATWE (DWANIRO) HC III",
                                       "watoto medical laboratory(LALOGI)"  =  "WATOTO HC II (LALOGI)",
                                       "watoto medical laboratory(WAKISO)"  =   "WATOTO HC II (WAKISO)",
                                       "Joint Clinical Research Centre (JCRC_UG)"   =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                       "ST.Mary`s Clinical Laboratory"   =    "MOYO HOSPITAL",
                                       "Touchlife medical care foundation LTD"   =  "TOUCHLIFE MEDICAL CARE FOUNDATION",
                                       "Ruhoko HC II"    =   "RUHOKO HC III",
                                       "Rushooka HCIV"    =  "MOTHER FRANCISCA LECHNER HC IV",
                                       "Mildmay Hospital Uganda"   =   "MILDMAY UGANDA HOSPITAL",
                                       "Uganda Cares-Soroti"    =   "UGANDA CARES HC II"
                                        ))

# rename columns
Mal_sche   <-   Mal_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

# capitalize district
Mal_sche$District    <-  toupper(Mal_sche$District) # capitalize district
Mal_sche$HFacility  <-  toupper(Mal_sche$HFacility)

#### add RRH details
Mal_sche <-   Mal_sche %>% 
  left_join(Regions, by = "District")


# selecting common sites from EID EQA data frame
Mal_scheData <-   Mal_sche %>% 
  select(RRH,District,HFacility,Status,Qtr)



#### checking facility names
Mal_schecheck   <-  Mal_sche%>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()

#### print names missing RRH
Mal_schesddfCHEC <-  Mal_sche %>% 
  filter(is.na(RRH)) %>% 
  print()

#### 

#### Gram EQA ####
GramEQA   <-  Gram_EQA %>% 
  mutate(District   =   recode(District,
                        "Luweero"   =   "LUWERO"))

# change names of facility    
GramEQA    <-   GramEQA %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

# correcting the names
GramEQA    <-   GramEQA %>% 
  mutate(HFacility     =   recode(HFacility,
                                  # New               #  Old                         
                                  "Maracha HCI V"    =   "MARACHA HC IV",
                                  "Yinga HC IV"   =  "YINGA HC III",
                                  "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                  "MT. ST. MARY'S HOSPITAL-DOK"   =   "Mt.St Mary`s Hospital",
                                  "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                  "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                  "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                  "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                  "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                  "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                  "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                  "Ober HC IV"   =   "OBER HC III",
                                  "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                  "Rushooka HCIV"   =  "RUSHOKA HC IV",
                                  "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                  "Bukuya HC III"    =   "BUKUYA HC IV",
                                  "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                  "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                  "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                  "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                  "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                  "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                  "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                  "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                  "Bundibugyo Hospital."     =   "BUNDIBUGYO HOSPITAL",
                                  "Mulago Specialised Women and Neonatal Hospital"   =  "MULAGO SWN HOSPITAL"
                                         ))


#### capitalize the names
GramEQA$District  <- toupper(GramEQA$District)
GramEQA$HFacility  <- toupper(GramEQA$HFacility)

# Add RRH to the file
GramEQA_Data    <-   GramEQA %>% 
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )


#### checking facility names
GramEQAcheck   <-  GramEQA %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test)) %>% 
  print()

#### print names missing RRH
GramEQAschesddfCHEC <-  GramEQA %>% 
  filter(is.na(RRH)) %>% 
  print()


#### Gram Schedule ####
Gram_sche  <-  Gram_Enrolled %>% 
  mutate(District   =   recode(District,
                      "Luweero"   =   "LUWERO"))

# correcting the names
Gram_sche   <-   Gram_sche %>% 
  mutate(`Facilty Name`     =   recode(`Facilty Name`,
                                       # Old              #  New                        
                                       "Maracha HCI V"    =   "MARACHA HC IV",
                                       "Yinga HC IV"   =  "YINGA HC III",
                                       "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                       "Mt.St Mary`s Hospital"  =    "MT. ST. MARY'S HOSPITAL-DOK",   
                                       "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                       "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                       "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                       "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                       "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                       "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                       "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                       "Ober HC IV"   =   "OBER HC III",
                                       "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                       "Rushooka HCIV"   =  "RUSHOKA HC IV",
                                       "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                       "Bukuya HC III"    =   "BUKUYA HC IV",
                                       "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                       "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                       "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                       "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                       "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                       "AIDS Information Centre"   =  "AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                       "St. Anthony's Hospital"  = "ST. ANTHONY'S TORORO HOSPITAL",
                                       "St. Francis Health Care Services HC III"  =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                       "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                       "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                       "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                       "Acholi-Pii Military HCIV"    =   "ACHOLPII HC III",
                                       "Bethany women`s Hospital"   =  "BETHANY WOMENS AND FAMILY HC III",
                                       "Bukwo Hospital"  =  "BUKWO GENERAL HOSPITAL",
                                       "Hoima Police HC II"    =  "HOIMA POLICE HC III",
                                       "Joint Clinical Research Center (JCRC)"  =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                       "Kyali HC III"     =  "NSAMU/KYALI HC III",
                                       "Marine Military HC II"   =  "MARINE MILITARY HC III",
                                       "St Apollo H/C III-Namasuba"  = "APOLLO MEMORIAL MEDICAL CENTER",
                                       "St Magdalene H/c II"   =  "ST. MAGDALENE HC II",
                                       "St.Joseph`s Buyege H/C III"   =  "ST. JOSEPH CLINIC HC III",
                                       "Watoto Medical Laboratory Suubi"    =  "WATOTO HC II",
                                       "Mengo Hospital Counselling and Homecare"  =  "MENGO HOSPITAL",
                                       "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                       "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                       "Bundibugyo Hospital."     =   "BUNDIBUGYO HOSPITAL",
                                       "Mulago Specialised Women and Neonatal Hospital"   =  "MULAGO SWN HOSPITAL"
                                  ))

# rename columns
Gram_sche   <-   Gram_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

# capitalize district
Gram_sche$District    <-  toupper(Gram_sche$District) # capitalize district
Gram_sche$HFacility  <-  toupper(Gram_sche$HFacility)

#### Adding columns for month and quarters
#### Add month and year
Gram_sche$`Date Panels Released`  <-  as.Date(Gram_sche$`Date Panels Released`, format = "%b %d, %Y")

Gram_sche <-   Gram_sche %>% 
  mutate(
    Month =  month(`Date Panels Released`),
    Yr    =  year(`Date Panels Released`)
  ) 

#### creating quarter column
Gram_sche <-   Gram_sche %>% 
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Q4",
      Month   %in% c(1,2,3)     ~ "Q1",
      Month  %in%   c(4,5,6)     ~ "Q2",
      Month  %in%  c(7,8,9)      ~ "Q3"
    ))

# select columns of interest for analysis
# selecting common sites from EID EQA data frame
Gram_scheData <-   Gram_sche %>% 
  select(RRH,District,HFacility,Status,Yr,Qtr)

#### checking facility names
Gram_schecheck   <-  Gram_scheData%>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()
#### add RRH details
Gram_sche <-   Gram_sche %>% 
  left_join(Regions, by = "District")


#### CD4 EQA ####
CD4EQA   <-  CD4_EQA %>% 
  mutate(District   =   recode(District,
                    "Luweero"   =   "LUWERO"))

# change names of facility    
CD4EQA    <-   CD4EQA %>% 
  rename(
    # New     # Old
    "HFacility"     =   "Facility")

# correcting the names
CD4EQA    <-   CD4EQA %>%
  mutate(HFacility     =   recode(HFacility,
                                  # New               #  Old                         
                                  "Maracha HCI V"    =   "MARACHA HC IV",
                                  "Yinga HC IV"   =  "YINGA HC III",
                                  "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                  "MT. ST. MARY'S HOSPITAL-DOK"   =   "Mt.St Mary`s Hospital",
                                  "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                  "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                  "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                  "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                  "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                  "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                  "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                  "Ober HC IV"   =   "OBER HC III",
                                  "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                  "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                  "Bukuya HC III"    =   "BUKUYA HC IV",
                                  "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                  "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                  "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                  "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                  "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                  "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                  "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                  "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                  "FAD MILITARY HCIV"   =  "FAD MILITARY HOSPITAL",
                                  "KYETUME CBHC HC III"  =  "KYETUME CBHC HC IV",
                                  "NAGURU POLICE HC II"   =  "NAGURU POLICE HC IV",
                                  "ST. FRANCIS HEALTH CARE SERVICES HC III"   =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                  "ST. ANTHONY'S HOSPITAL"    =  "ST. ANTHONY'S TORORO HOSPITAL",
                                  "Kyetume Cbhc HC III"      =  "KYETUME CBHC HC IV",
                                  "Naguru Police HC II"    =  "NAGURU POLICE HC IV",
                                  "St. Francis Health Care Services HC III"  =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                  "St. Anthony's Hospital"    =  "ST. ANTHONY'S TORORO HOSPITAL",
                                  "Mildmay Hospital Uganda"     =  "MILDMAY UGANDA HOSPITAL",
                                  "Oli HCIV"     =  "RIVER OLI HC IV",
                                  "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                  "Uganda Cares-Soroti"  =  "UGANDA CARES HC II"
                                  ))

#### capitalize the names
CD4EQA$District  <- toupper(CD4EQA$District)
CD4EQA$HFacility  <- toupper(CD4EQA$HFacility)


# Add RRH to the file
CD4EQA    <-   CD4EQA %>% 
  left_join(
    master %>%
      select(HFacility, HRegion, hflevel, ownership, RRH),
    by = "HFacility"
  )



#### checking facility names
CD4_EQAcheck   <-  CD4EQA %>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test)) %>% 
  print()


#### CD4 Schedule ####
CD4_sche   <-  CD4_enrolled %>% 
  mutate(District   =   recode(District,
                    "Luweero"   =   "LUWERO"))

# correcting the names
CD4_sche   <-   CD4_sche %>% 
  mutate(`Facilty Name`     =   recode(`Facilty Name`,
                                       # Old              #  New                        
                                       "Maracha HCI V"    =   "MARACHA HC IV",
                                       "Yinga HC IV"   =  "YINGA HC III",
                                       "Gombe (Butambala) Hospital"   =  "GOMBE HOSPITAL",
                                       "Mt.St Mary`s Hospital"  =    "MT. ST. MARY'S HOSPITAL-DOK",   
                                       "Rwamwanja HC IV"    =  "RWAMWANJA HC III",
                                       "St Joseph`s Hospital Kitgum"   =  "ST. JOSEPH'S KITGUM HOSPITAL",
                                       "St. Marys Lacor Hospital"   =  "ST. MARY'S HOSPITAL LACOR",
                                       "TASO Gulu  Special Clinic"  =  "TASO GULU SPECIAL CLINIC",
                                       "Murchision Bay Hospital"  =  "MURCHISION BAY MAIN HOSPITAL",
                                       "Kawaala Health Centre HC IV"  =  "KAWAALA HC IV",
                                       "Lwampanga HC IV"    =  "LWAMPANGA HC III",
                                       "Ober HC IV"   =   "OBER HC III",
                                       "Tororo Hospital"    =  "TORORO GENERAL HOSPITAL",
                                       "Kotido HC IV"   =  "KOTIDO HOSPITAL",
                                       "Bukuya HC III"    =   "BUKUYA HC IV",
                                       "Kasambya (Mubende) HC III"   =   "MUBENDE KASAMBYA HC III GOVT",
                                       "Bidibidi Health Centre HC III"   =  "BIDIBIDI HC III", 
                                       "Mulago National Referral Hospital"  =   "MULAGO NRH - MJAP ISS CLINIC",
                                       "International Hospital Kampala Hospital (IHK)" =  "INTERNATIONAL HOSPITAL KAMPALA HOSPITAL",
                                       "St Joseph`s Hospital"   =   "ST JOSEPHS MEDICAL CENTRE",
                                       "AIDS Information Centre"   =  "AIDS INFORMATION CENTRE (KAMPALA) SPECIAL CLINIC",
                                       "St. Anthony's Hospital"  = "ST. ANTHONY'S TORORO HOSPITAL",
                                       "St. Francis Health Care Services HC III"  =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                       "Katunguru HC III"   =  "KATUNGURU HC III (RUBIRIZI)",
                                       "Kabaale HC III"    =  "KABAALE HC III (HOIMA)",
                                       "Mulago Specialized women and neonatal Hospital"  =  "MULAGO SWN HOSPITAL",
                                       "Acholi-Pii Military HCIV"    =   "ACHOLPII HC III",
                                       "Bethany women`s Hospital"   =  "BETHANY WOMENS AND FAMILY HC III",
                                       "Bukwo Hospital"  =  "BUKWO GENERAL HOSPITAL",
                                       "Hoima Police HC II"    =  "HOIMA POLICE HC III",
                                       "Joint Clinical Research Center (JCRC)"  =  "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                       "Kyali HC III"     =  "NSAMU/KYALI HC III",
                                       "Marine Military HC II"   =  "MARINE MILITARY HC III",
                                       "St Apollo H/C III-Namasuba"  = "APOLLO MEMORIAL MEDICAL CENTER",
                                       "St Magdalene H/c II"   =  "ST. MAGDALENE HC II",
                                       "St.Joseph`s Buyege H/C III"   =  "ST. JOSEPH CLINIC HC III",
                                       "Watoto Medical Laboratory Suubi"    =  "WATOTO HC II",
                                       "Mengo Hospital Counselling and Homecare"  =  "MENGO HOSPITAL",
                                       "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                       "Karita HC III"   =  "KARITA HC IV",
                                       "Naguru Police HC II"   =  "NAGURU POLICE HC IV",
                                       "RUSHOKA HC IV"     =  "MOTHER FRANCISCA LECHNER HC IV",
                                       "FAD MILITARY HCIV"   =  "FAD MILITARY HOSPITAL",
                                       "Kyetume Cbhc HC III"  =  "KYETUME CBHC HC IV",
                                       "Naguru Police HC II"   =  "NAGURU POLICE HC IV",
                                       "St. Francis Health Care Services HC III"   =  "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                       "St. Anthony's Hospital"    =  "ST. ANTHONY'S TORORO HOSPITAL",
                                       "Kyetume Cbhc HC III"      =  "KYETUME CBHC HC IV",
                                       "Naguru Police HC II"    =  "NAGURU POLICE HC IV",
                                       "Mildmay Hospital Uganda"     =  "MILDMAY UGANDA HOSPITAL",
                                       "Oli HCIV"     =  "RIVER OLI HC IV",
                                       "Rushooka HCIV"   =  "MOTHER FRANCISCA LECHNER HC IV",
                                       "Uganda Cares-Soroti"  =  "UGANDA CARES HC II"
                                   ))

# rename columns
CD4_sche   <-   CD4_sche %>% 
  rename(
    "HFacility"   =  "Facilty Name")

# capitalize district
CD4_sche$District    <-  toupper(CD4_sche$District) # capitalize district
CD4_sche$HFacility  <-  toupper(CD4_sche$HFacility)

#### add RRH details
CD4_sche <-   CD4_sche %>% 
  left_join(Regions, by = "District")

# selecting common sites from EID EQA data frame
CD4_scheData <-   CD4_sche %>% 
  select(RRH,District,HFacility,Status,Qtr)


#### checking facility names
CD4_schecheck   <-  CD4_sche%>% 
  left_join(master, by = "HFacility") %>% 
  filter(is.na(test))%>% 
  distinct(HFacility, .keep_all = TRUE) %>% 
  print()


#### print names missing RRH
CD4_schehesddfCHEC <-  CD4_sche %>% 
  filter(is.na(RRH)) %>% 
  print()
#### 

##################################################################
##################################################################
#### Response Rate ####
#### EID EQA ####
## Enrolled by Quarter
eidsdf1  <-  EID_scheData %>% 
  group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    EID_Enroll = n_distinct(HFacility),          # Count distinct facilities
    EID_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 

#### the calculation
eidsdf   <-    eidsdf1 %>%  
  mutate(
    EID  =  round((EID_Response/EID_Enroll)*100,0)
  ) 

#### select columns of interest for combining all EQA schemes
eidSche  <-  eidsdf %>% 
  select(Qtr,EID) %>% 
    pivot_wider(
      names_from = Qtr,
      values_from = EID
    ) %>% 
  mutate(
    `EQA Scheme`  =  "EID"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)

#### HIV EQA ####
## Enrolled by Quarter
vlsdf1  <-  VL_scheData %>% 
  group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    VL_Enroll = n_distinct(HFacility),          # Count distinct facilities
    VL_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 

#### the proportions
vlsdf   <-  vlsdf1 %>% 
  mutate(
    VL  =  round((VL_Response/VL_Enroll)*100,0)
  ) 


#### select columns of interest for combining all EQA schemes 
VLSche  <-  vlsdf %>% 
  select(Qtr,VL)%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = VL
  ) %>% 
  mutate(
    `EQA Scheme`  =  "VL"
  ) %>% 
  select(`EQA Scheme`,Q4, Q1, Q2, Q3)

#### CrAg EQA ####
## Enrolled by Quarter
cragsdf1  <-  CrAg_scheData %>% 
  group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    CrAg_Enroll = n_distinct(HFacility),          # Count distinct facilities
    CrAg_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 
#### proportions
cragsdf   <-  cragsdf1 %>%  
  mutate(
    CrAg  =  round((CrAg_Response/CrAg_Enroll)*100,0)
  ) 


#### select columns of interest for combining all EQA schemes
CrAgSche  <-  cragsdf %>% 
  select(Qtr,CrAg)%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = CrAg
  ) %>% 
  mutate(
    `EQA Scheme`  =  "CrAg"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)

#### HBV EQA ####
## Enrolled by Quarter
HBVsdf1  <-  HBV_scheData %>% 
  group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    HBV_Enroll = n_distinct(HFacility),          # Count distinct facilities
    HBV_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 

#### the proportion
HBVsdf    <-  HBVsdf1 %>%  
  mutate(
    HBV  =  round((HBV_Response/HBV_Enroll)*100,0)
  ) 

#### select columns of interest for combining all EQA schemes 
HBVSche  <-  HBVsdf %>% 
  select(Qtr,HBV)%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = HBV
  ) %>% 
  mutate(
    `EQA Scheme`  =  "HBV"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)


#### HPV EQA ####
## Enrolled by Quarter
HPVsdf1  <-  HPV_scheData %>% 
  group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    HPV_Enroll = n_distinct(HFacility),          # Count distinct facilities
    HPV_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 

#### proportion
HPVsdf    <-  HPVsdf1 %>%  
  mutate(
    HPV  =  round((HPV_Response/HPV_Enroll)*100,0)
  ) 


#### select columns of interest for combining all EQA schemes
HPVSche  <-  HPVsdf %>% 
  select(Qtr,HPV)%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = HPV
  ) %>% 
  mutate(
    `EQA Scheme`  =  "HPV"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)



#### Malaria EQA ####
## Enrolled by Quarter
Malsdf1  <-  Mal_scheData %>% 
  group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    Mal_Enroll = n_distinct(HFacility),          # Count distinct facilities
    Mal_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 

#### calculation
Malsdf   <-  Malsdf1 %>%  
  mutate(
    Malaria  =  round((Mal_Response/Mal_Enroll)*100,0)
  ) 


#### select columns of interest for combining all EQA schemes
MalSche  <-  Malsdf %>% 
  select(Qtr,Malaria)%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Malaria
  ) %>% 
  mutate(
    `EQA Scheme`  =  "Malaria"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)

#### Gram EQA ####
## Enrolled by Quarter
Gramsdf1  <-  Gram_scheData %>% 
  group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    Gram_Enroll = n_distinct(HFacility),          # Count distinct facilities
    Gram_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 

#### the calculation
Gramsdf   <-  Gramsdf1 %>% 
  mutate(
    Gram  =  round((Gram_Response/Gram_Enroll)*100,0)
  ) 


#### select columns of interest for combining all EQA schemes
GramSche  <-  Gramsdf %>% 
  select(Qtr,Gram)%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Gram
  ) %>% 
  mutate(
    `EQA Scheme`  =  "Gram"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)


#### CD4 EQA ####
## Enrolled by Quarter
CD4sdf1  <-  CD4_scheData %>% 
    group_by(Qtr, HFacility) %>%       # Group by both Quarter and Facility
  summarise(Status = first(Status),  # Pick the first status for each facility
            .groups = "drop") %>%    # Ungroup for summarization
  group_by(Qtr) %>%
  summarise(
    CD4_Enroll = n_distinct(HFacility),          # Count distinct facilities
    CD4_Response = sum(Status == "Approved", na.rm = TRUE) # Count "Approved" statuses
  ) 
#### the calculation
CD4sdf  <-   CD4sdf1 %>% 
  mutate(
    CD4  =  round((CD4_Response/CD4_Enroll)*100,0)
  ) 


#### select columns of interest for combining all EQA schemes 
CD4Sche  <-  CD4sdf %>% 
  select(Qtr,CD4)%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = CD4
  ) %>% 
  mutate(
    `EQA Scheme`  =  "CD4"
  ) %>% 
  select(`EQA Scheme`,Q1, Q2, Q3)


#### Report - EQA Response for all EQA Schemes ####
all_response  <-  bind_rows(eidSche, CrAgSche,HBVSche,VLSche,
                            HPVSche,MalSche,GramSche,CD4Sche)

#### the graph
all_response_grh   <-  all_response %>% 
  pivot_longer(
    cols = c(Q4,Q1, Q2, Q3),
    names_to = "Cycle",
    values_to = "Response_Rate"
  ) %>% 
  mutate(
    Cycle  =  factor(Cycle, levels = c("Q4","Q1", "Q2", "Q3"))
  ) %>% 
  mutate(
    Cycle    =    recode(Cycle,
                  Q4  =  "Oct-Dec",
                  Q1  =   "Jan-Mar",
                  Q2  =   "Apri-Jun",
                  Q3  =   "Jul-Sept")
  )


#### The graph
ggplot(all_response_grh, aes(x = `EQA Scheme`, y = Response_Rate, fill = Cycle)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = Response_Rate),
    vjust = -0.3,
    size = 2.5,
    position = position_dodge(width = 1.0)
  ) +
  geom_hline(yintercept = 80, color = "#950606", linetype = "dashed") +
  annotate("text", x = 2, y = 80, label = "Target (80%)", vjust = -0.5, color = "#950606") +
  # Add vertical separator lines between each EQA Scheme
  geom_vline(
    xintercept = seq(1.5, length(unique(all_response_grh$`EQA Scheme`)) - 0.5, by = 1),
    color = "black",
    linetype = "dotted",
    linewidth = 0.5
  ) +
  theme_minimal() +
  labs(
    title = "EQA Response Rate by Quarter",
    x = "EQA Scheme",
    y = "Response Rate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    # Remove background grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#### the table

#### function for coloring
EQARate   <-  function(column_data){
  ifelse(column_data >= 80, "green",
         ifelse(column_data > 50 & column_data <= 79, "yellow","red"))
}

#### The table
tbl_EQARate   <-  flextable(all_response)

tbl_EQARate
### format the table
tbl_EQARate  <-  tbl_EQARate %>% 
  add_header_row(values = c(
    `EQA Scheme`  =  "EQA Scheme",
     "EQA Response Rate by Quarter", "","", ""
  ))

## set the labels
tbl_EQARate  <-  tbl_EQARate %>%
  set_header_labels(
    `EQA Scheme`  =  "EQA Scheme",
    Q4  =   "Oct-Dec",
    Q1    =    "Jan-Mar",
    Q2    =   "Apri-Jun",
    Q3    =    "Jul-Sept"
  )


### List Columns to Apply Coloring
columns <- c("Q4", "Q1", "Q2", "Q3")

### Apply Background Color ###
for (col in columns) {
  tbl_EQARate  <-  tbl_EQARate %>%
    bg(j = col, bg = EQARate(all_response[[col]]), part = "body")
}

tbl_EQARate  <-  tbl_EQARate %>%
  # Merging columns 
  merge_at(i = 1, j = 2:5, part = "header")

# Merging vertically for the first five columns in the header
tbl_EQARate  <-  tbl_EQARate %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_EQARate  <-  tbl_EQARate %>%
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_EQARate  <-  tbl_EQARate %>%
  add_header_row(values="EQA Schemes Response Rate", 
                 colwidths = ncol(all_response))

tbl_EQARate

#### Report - Overall EQA Response rate  ####
overallReponseEQA   <-  eidsdf1 %>% 
  full_join(vlsdf1, by = "Qtr") %>% 
  full_join(cragsdf1, by = "Qtr") %>% 
  full_join(HBVsdf1, by = "Qtr") %>%
  full_join(HPVsdf1, by = "Qtr") %>%
  full_join(Malsdf1, by = "Qtr") %>%
  full_join(Gramsdf1, by = "Qtr") %>%
  full_join(CD4sdf1, by = "Qtr")

#### Add the pass and response values, get proportions
overallReponseEQAdf <- overallReponseEQA %>% 
  mutate(
    No.enroll = rowSums(across(contains("Enroll")), na.rm = TRUE),
    No.Respond       = rowSums(across(contains("Response")), na.rm = TRUE)
  ) %>% 
  mutate(
    `%age` = round((No.Respond / No.enroll) * 100, 0)
  ) %>% 
  select(Qtr, No.enroll,No.Respond, `%age`) 

#### the table
tbl_overallReponseEQAdf  <-  flextable(overallReponseEQAdf)

tbl_overallReponseEQAdf <-  tbl_overallReponseEQAdf %>% 
  add_header_row(values = "Overall EQA Pass Rate",
                 colwidths = ncol(overallReponseEQAdf))

tbl_overallReponseEQAdf


##################################################################
#### Report - EQA Response by RRH for all EQA Schemes with color ####
#### EID #####
RRHEID_Response  <-  EID_scheData %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    EID = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, EID) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = EID
  ) %>% 
  select(RRH, Q4, Q1, Q2, Q3)


#### VL ####
RRHVL_Response  <-  VL_scheData %>%
group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    VL = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, VL) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = VL
  )%>% 
  select(RRH, Q4, Q1, Q2, Q3)


#### CrAg ####
RRHCrAg_Response  <-  CrAg_scheData %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    CrAg = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, CrAg) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = CrAg
  )%>% 
  select(RRH, Q4, Q1, Q2, Q3)


#### HBV ####
RRHHBV_Response  <-  HBV_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    HBV = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, HBV) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = HBV
  )%>% 
  select(RRH, Q4, Q1, Q2, Q3)

#### HPV ####
RRHHPV_Response  <-  HPV_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    HPV = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, HPV) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = HPV
  )%>% 
  select(RRH, Q4, Q1, Q2, Q3)


#### Malaria ####
RRHMal_Response  <-  Mal_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    Malaria = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, Malaria) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Malaria
  )%>% 
  select(RRH, Q4, Q1, Q2, Q3)


#### Gram ####
RRHGram_Response  <-  Gram_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    Gram = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, Gram) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = Gram
  ) %>% 
  select(RRH, Q4, Q1, Q2, Q3)


#### CD4 ####
RRHCD4_Response  <-  CD4_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_enrolled = n(),
    No_response       =  sum(Status == "Approved"),
    .groups = "drop"
  ) %>% 
  mutate(
    CD4 = round((No_response / No_enrolled) * 100, 0)) %>% 
  select(RRH, Qtr, CD4) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = CD4
  ) %>% 
  select(RRH, Q1, Q2, Q3)

#### Report - All RRH EQA Response Rate ####
RRHall_EQAResponse  <- RRHEID_Response %>% 
  left_join(RRHVL_Response, by = "RRH") %>% 
  left_join(RRHCrAg_Response, by = "RRH") %>%
  left_join(RRHHBV_Response, by = "RRH") %>%
  left_join(RRHHPV_Response, by = "RRH") %>%
  left_join(RRHMal_Response, by = "RRH") %>%
  left_join(RRHGram_Response, by = "RRH") %>%
  left_join(RRHCD4_Response, by = "RRH")


# function to color the columns
EQA_R   <-  function(column_data){
  ifelse(column_data >= 80, "green",
         ifelse(column_data >= 50 & column_data < 80, "yellow","red"))
}

#### the table
tbl_RRHall_EQAResponse  <-  flextable(RRHall_EQAResponse)


### format the table
tbl_RRHall_EQAResponse   <-   tbl_RRHall_EQAResponse %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "EID", "","","",
    "VL","", "","",
    "CrAg", "", "","",
    "HBV", "", "","",
    "HPV", "", "","",
    "Mal", "", "","",
    "Gram", "", "","",
    "CD4", "",""
  ))


## set the labels
tbl_RRHall_EQAResponse   <-   tbl_RRHall_EQAResponse %>% 
  set_header_labels(
    
    RRH  =  "RRH",
    Q4.x    =   "Oct-Dec",
    Q1.x    =    "Jan-Mar",
    Q2.x    =   "Apri-Jun",
    Q3.x    =   "Jul-Sept",
    
    Q4.y    =  "Oct-Dec",
    Q1.y    =    "Jan-Mar",
    Q2.y    =   "Apri-Jun",
    Q3.y    =   "Jul-Sept",
    
    Q4.x.x    =   "Oct-Dec",
    Q1.x.x    =    "Jan-Mar",
    Q2.x.x    =    "Apri-Jun",
    Q3.x.x    =   "Jul-Sept",
    
    Q4.y.y    =   "Oct-Dec",
    Q1.y.y    =    "Jan-Mar",
    Q2.y.y    =    "Apri-Jun",
    Q3.y.y    =   "Jul-Sept",
    
    Q4.x.x.x    =   "Oct-Dec",
    Q1.x.x.x    =    "Jan-Mar",
    Q2.x.x.x    =    "Apri-Jun",
    Q3.x.x.x   =   "Jul-Sept",
    
    Q4.y.y.y    =   "Oct-Dec",
    Q1.y.y.y    =    "Jan-Mar",
    Q2.y.y.y    =    "Apri-Jun",
    Q3.y.y.y    =   "Jul-Sept",
    
    Q4  =    "Oct-Dec",
    Q1.x.x.x.x           =   "Jan-Mar",
    Q2.x.x.x.x  =    "Apri-Jun",
    Q3.x.x.x.x    =   "Jul-Sept",
    
    Q1.y.y.y.y      =    "Jan-Mar",
    Q2.y.y.y.y     =    "Apri-Jun",
    Q3.y.y.y.y   =   "Jul-Sept"
  )


### List Columns to Apply Coloring ###
# Get column names between the two markers
columns <- names(RRHall_EQAResponse)
start_col <- which(columns == "Q4.x")
end_col <- which(columns == "Q3.y.y.y.y")

### target columns
target1_columns <- columns[start_col:end_col]

### Apply Background Color ###
for (col in target1_columns) {
  tbl_RRHall_EQAResponse   <-   tbl_RRHall_EQAResponse %>% 
    bg(j = col, bg = EQA_R(RRHall_EQAResponse[[col]]), part = "body")
}

# Merging columns 
tbl_RRHall_EQAResponse   <-   tbl_RRHall_EQAResponse %>% 
 
  merge_at(i = 1, j = 2:5, part = "header") %>%  # EID
  merge_at(i = 1, j = 6:9, part = "header") %>%  # VL
  merge_at(i = 1, j = 10:13, part = "header") %>% # CrAg
  merge_at(i = 1, j = 14:17, part = "header") %>% # HBV
  merge_at(i = 1, j = 18:21, part = "header") %>% # HPV
  merge_at(i = 1, j = 22:25, part = "header") %>% # Mal
  merge_at(i = 1, j = 26:29, part = "header") %>% # Gram
  merge_at(i = 1, j = 30:32, part = "header")     # CD4


# Merging vertically for the first five columns in the header
tbl_RRHall_EQAResponse   <-   tbl_RRHall_EQAResponse %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_RRHall_EQAResponse   <-   tbl_RRHall_EQAResponse %>% 
  vline(j = c(1, 5, 9, 13, 17, 21, 25, 29, 32), part = "all")


tbl_RRHall_EQAResponse   <-   tbl_RRHall_EQAResponse %>% 
  add_header_row(values="%age of sites responsing to the EQA Schemes by RRH and Quarter", 
                 colwidths = ncol(RRHall_EQAResponse))

# Define a thick dark border
dark_border <- fp_border(color = "black", width = 2)

# Add styling
tbl_RRHall_EQAResponse <- tbl_RRHall_EQAResponse %>%
  # Make all text bold and dark purple
  bold(part = "all") %>%
  color(color = "#4B0082", part = "all") %>%  # Dark purple
  
  # Apply dark thick borders
  border(part = "all", border = dark_border) %>%
  border_inner(border = dark_border, part = "all") %>%
  border_outer(border = dark_border, part = "all")

tbl_RRHall_EQAResponse

################################################################
################################################################
#### Annex - list sites not responding to EQA ####
#### EID EQA ####
eidNoRes   <-   EID_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)

#### establish those facilities missing in more one quarter
eidNoRes_summary_filtered <- eidNoRes %>%
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
    arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_eidNoRescommon  <-  flextable(eidNoRes_summary_filtered)

#### format the table
tbl_eidNoRescommon   <-   tbl_eidNoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_eidNoRescommon   <-   tbl_eidNoRescommon %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_eidNoRescommon   <-   tbl_eidNoRescommon %>%
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_eidNoRescommon   <-   tbl_eidNoRescommon %>%
  add_header_row(values="List of facilities no responding to EID EQA in one or more EQA cycles", 
                 colwidths = ncol(eidNoRes_summary_filtered))

tbl_eidNoRescommon



#### VL EQA ####
vlNoRes   <-   VL_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)

#### establish those facilities missing in both Q1 and Q4
VLNoRes_summary_filtered <- vlNoRes %>%
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_VLNoRescommon  <-  flextable(VLNoRes_summary_filtered)

#### format the table
tbl_VLNoRescommon   <-   tbl_VLNoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_VLNoRescommon   <-   tbl_VLNoRescommon %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_VLNoRescommon   <-   tbl_VLNoRescommon %>%
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_VLNoRescommon   <-   tbl_VLNoRescommon %>%
  add_header_row(values="List of facilities no responding to VL EQA in one or more EQA cycles", 
                 colwidths = ncol(VLNoRes_summary_filtered))

tbl_VLNoRescommon


#### CrAg EQA ####
CrAgNoRes   <-   CrAg_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)

#### establish those facilities missing in both Q1 and Q4
CrAgNoRes_summary_filtered <- CrAgNoRes %>%
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_CrAgNoRescommon  <-  flextable(CrAgNoRes_summary_filtered)

#### format the table
tbl_CrAgNoRescommon   <-   tbl_CrAgNoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_CrAgNoRescommon   <-   tbl_CrAgNoRescommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_CrAgNoRescommon   <-   tbl_CrAgNoRescommon %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_CrAgNoRescommon   <-   tbl_CrAgNoRescommon %>% 
  add_header_row(values="List of facilities no responding to CrAg EQA in one or more EQA cycles", 
                 colwidths = ncol(CrAgNoRes_summary_filtered))

tbl_CrAgNoRescommon

#### HBV EQA ####
HBVNoRes   <-   HBV_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)

#### establish those facilities missing in both Q1 and Q4
HBVNoRescommon <- HBVNoRes %>%
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_HBVNoRescommon  <-  flextable(HBVNoRescommon)

#### format the table
tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  add_header_row(values="List of facilities no responding to HBV EQA in one or more EQA cycles", 
                 colwidths = ncol(HBVNoRescommon))

tbl_HBVNoRescommon


#### HPV EQA ####
HPVNoRes   <-   HPV_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)

#### establish those facilities missing in both Q1 and Q4
HPVNoRescommon <- HPVNoRes %>%
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_HBVNoRescommon  <-  flextable(HBVNoRescommon)

#### format the table
tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_HBVNoRescommon   <-   tbl_HBVNoRescommon %>% 
  add_header_row(values="List of facilities no responding to HBV EQA in one or more EQA cycles", 
                 colwidths = ncol(HBVNoRescommon))

tbl_HBVNoRescommon



#### Malaria EQA ####
MalNoRes   <-   Mal_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)

#### establish those facilities missing in both Q1 and Q4
MalNoRescommon <- MalNoRes %>%
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_MalNoRescommon  <-  flextable(MalNoRescommon)

#### format the table
tbl_MalNoRescommon   <-   tbl_MalNoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_MalNoRescommon   <-   tbl_MalNoRescommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_MalNoRescommon   <-   tbl_MalNoRescommon %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_MalNoRescommon   <-   tbl_MalNoRescommon %>% 
  add_header_row(values="List of facilities no responding to Malaria EQA in one or more EQA cycles", 
                 colwidths = ncol(MalNoRescommon))

tbl_MalNoRescommon


#### Gram EQA ####
GramNoRes   <-   Gram_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)

#### establish those facilities missing in both Q1 and Q4
GramNoRescommon <- GramNoRes %>%
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_GramNoRescommon  <-  flextable(GramNoRescommon)

#### format the table
tbl_GramNoRescommon   <-   tbl_GramNoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_GramNoRescommon   <-   tbl_GramNoRescommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_GramNoRescommon   <-   tbl_GramNoRescommon %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_GramNoRescommon   <-   tbl_GramNoRescommon %>% 
  add_header_row(values="List of facilities no responding to Gram EQA in one or more EQA cycles", 
                 colwidths = ncol(GramNoRescommon))

tbl_GramNoRescommon


#### CD4 EQA ####
CD4NoRes   <-   CD4_scheData %>% 
  filter(Status !=  "Approved") %>% 
  group_by(RRH, Qtr)
#### establish those facilities missing in both Q1 and Q4
CD4NoRescommon  <-   CD4NoRes %>% 
  group_by(RRH,District,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Qtr))),
    n_quarters = n_distinct(Qtr),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, n_quarters, Quarters)


#### the table
tbl_CD4NoRescommon  <-  flextable(CD4NoRescommon)

#### format the table
tbl_CD4NoRescommon   <-   tbl_CD4NoRescommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    n_quarters  =  "No. of quarters where site did not respond to EQA",
    Quarters   =  "Quarters where site did not respond to EQA"
  ))


# color coding the columns
tbl_CD4NoRescommon   <-   tbl_CD4NoRescommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_CD4NoRescommon   <-   tbl_CD4NoRescommon %>% 
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_CD4NoRescommon   <-   tbl_CD4NoRescommon %>% 
  add_header_row(values="List of facilities no responding to CD4 EQA in one or more EQA cycles", 
                 colwidths = ncol(CD4NoRescommon))

tbl_CD4NoRescommon



##########################################################################
##########################################################################
#### Pass Rate ####
#### EID #####
EID_PassRdf  <-  EIDEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory")
  ) 

#### calculate the proportions
EID_PassR   <-  EID_PassRdf %>% 
    mutate(
    EID  =  round((No_Pass/No_Responding)*100,0)
  ) %>% 
  select(Period, EID)
#### Creating the data frame to compile list
EID_PassRpt   <-  EID_PassR %>% 
  pivot_wider(
    names_from = Period,
    values_from = EID
  ) %>% 
  mutate(
    `EQA Scheme`  =  "EID"
  ) %>% 
  select(`EQA Scheme`,`2024-Q4`,`2025-Q1`, `2025-Q2`, `2025-Q3`)

#### VL ####
VL_PassRdf  <-  VLEQA_data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY")
  ) 

### calculate the percentage
VL_PassR   <-  VL_PassRdf %>% 
  mutate(
    VL  =  round((No_Pass/No_Responding)*100,0)
  ) %>% 
  select(Period, VL)
#### Creating the data frame to compile list
VL_PassRpt   <-  VL_PassR %>% 
  pivot_wider(
    names_from = Period,
    values_from = VL
  ) %>% 
  mutate(
    `EQA Scheme`  =  "VL"
  ) %>% 
  select(`EQA Scheme`,`2024-Q4` , `2025-Q1`, `2025-Q2`, `2025-Q3` )


#### CrAg ####
CrAg_PassRdf  <-  CrAgEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory")
  ) 

#### calculation
CrAg_PassR   <-   CrAg_PassRdf %>%
  mutate(
    CrAg  =  round((No_Pass/No_Responding)*100,0)
  )%>% 
  select(Period, CrAg)

#### Creating the data frame to compile list
CrAg_PassRpt   <-  CrAg_PassR %>% 
  pivot_wider(
    names_from = Period,
    values_from = CrAg
  ) %>% 
  mutate(
    `EQA Scheme`  =  "CrAg"
  ) %>% 
  select(`EQA Scheme`,`2024-Q4`,`2025-Q1`, `2025-Q2`, `2025-Q3`)

#### HBV ####
HBV_PassRdf  <-  HBVEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory")
  ) 


#### calculate
HBV_PassR   <-  HBV_PassRdf %>% 
  mutate(
    HBV  =  round((No_Pass/No_Responding)*100,0)
  )%>% 
  select(Period, HBV)

#### Creating the data frame to compile list
HBV_PassRpt   <-  HBV_PassR %>% 
  pivot_wider(
    names_from = Period,
    values_from = HBV
  ) %>% 
  mutate(
    `EQA Scheme`  =  "HBV"
  ) %>% 
  select(`EQA Scheme`,`2024-Q4`,`2025-Q1`, `2025-Q2`, `2025-Q3`)

#### HPV ####
HPV_PassRdf  <-  HPVEQA_Data %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory")
  ) 

#### calculate the proportios
HPV_PassR    <-  HPV_PassRdf  %>% 
  mutate(
    HPV  =  round((No_Pass/No_Responding)*100,0)
  )%>% 
  select(Period, HPV)

#### Creating the data frame to compile list
HPV_PassRpt   <-  HPV_PassR %>% 
  pivot_wider(
    names_from = Period,
    values_from = HPV
  ) %>% 
  mutate(
    `EQA Scheme`  =  "HPV"
  ) %>% 
  select(`EQA Scheme`,`2024-Q4`,`2025-Q1`, `2025-Q2`, `2025-Q3`)


#### Malaria ####
Mal_PassRdf  <-  MalEQA_Data %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory")
  ) 

#### calculate 
Mal_Pass    <-   Mal_PassRdf %>% 
  mutate(
    Malaria  =  round((No_Pass/No_Responding)*100,0)
  )%>% 
  select(Period, Malaria)

#### Creating the data frame to compile list
Mal_PassRpt   <-  Mal_Pass %>% 
  pivot_wider(
    names_from = Period,
    values_from = Malaria
  ) %>% 
  mutate(
    `EQA Scheme`  =  "Malaria"
  ) %>% 
  select(`EQA Scheme`,`2024-Q4`,`2025-Q1`, `2025-Q2`, `2025-Q3`)

#### Gram ####
Gram_PassRdf  <-  GramEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory")
  ) 

#### calculate the proportions
Gram_PassR   <-   Gram_PassRdf %>%  
  mutate(
    Gram  =  round((No_Pass/No_Responding)*100,0)
  )%>% 
  select(Period, Gram)


#### Creating the data frame to compile list
Gram_PassRpt   <-  Gram_PassR %>% 
  pivot_wider(
    names_from = Period,
    values_from = Gram
  ) %>% 
  mutate(
    `EQA Scheme`  =  "Gram"
  ) %>% 
  select(`EQA Scheme`,`2024-Q4`,`2025-Q1`, `2025-Q2`, `2025-Q3`)

#### CD4 ####
CD4_PassRdf  <-  CD4_EQA %>% 
  filter(
    !(`Overrall Score` %in% c("UNGRADED", "-")) & 
      !is.na(`Overrall Score`)
  ) %>%
  group_by(Period) %>%
  summarise(
    No_Responding = n(),
    No_Pass = sum(`Overrall Score` == "SATISFACTORY")
  )

####
CD4_PassR  <-   CD4_PassRdf %>% 
  mutate(
    CD4  =  round((No_Pass/No_Responding)*100,0)
  )%>% 
  select(Period, CD4)

#### Creating the data frame to compile list
CD4_PassRpt   <-  CD4_PassR %>% 
  pivot_wider(
    names_from = Period,
    values_from = CD4
  ) %>% 
  mutate(
    `EQA Scheme`  =  "CD4"
  ) %>% 
  select(`EQA Scheme`,`2025-Q1`, `2025-Q2`, `2025-Q3`)


#### Report - All EQA Pass Rate ####
all_EQAPass  <-  bind_rows(EID_PassRpt, CrAg_PassRpt,HBV_PassRpt,VL_PassRpt,
                           HPV_PassRpt,Mal_PassRpt,Gram_PassRpt,CD4_PassRpt
                           )


#### the graph
all_EQAPass_grh   <-  all_EQAPass %>% 
  pivot_longer(
    cols = c(`2024-Q4`,`2025-Q1`, `2025-Q2`, `2025-Q3`),
    names_to = "Cycle",
    values_to = "Pass_Rate"
  ) %>% 
  mutate(
    Cycle  =  factor(Cycle, levels = c("2024-Q4","2025-Q1", "2025-Q2", "2025-Q3"))
  ) %>% 
  mutate(
    Cycle    =    recode(Cycle,
                         `2024-Q4`  =  "Oct-Dec",
                         `2025-Q1`  =   "Jan-Mar",
                         `2025-Q2`  =   "Apri-Jun",
                         `2025-Q3`  =   "Jul-Sept"
                         )
    )

#### The graph
ggplot(all_EQAPass_grh, aes(x = `EQA Scheme`, y = Pass_Rate, fill = Cycle)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = Pass_Rate),
    vjust = -0.3,
    size = 2.0,
    position = position_dodge(width = 1.0)
  ) +
  geom_hline(yintercept = 80, color = "#950606", linetype = "dashed") +
  annotate("text", x = 2, y = 80, label = "Target (80%)", vjust = -0.5, color = "#950606") +
  # Add vertical separator lines between each EQA Scheme
  geom_vline(
    xintercept = seq(1.5, length(unique(all_EQAPass_grh$`EQA Scheme`)) - 0.5, by = 1),
    color = "black",
    linetype = "dotted",
    linewidth = 0.5
  ) +
  theme_minimal() +
  labs(
    title = "EQA Pass Rate by Quarter",
    x = "EQA Scheme",
    y = "Pass Rate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    # Remove background grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


#### The table 
#### function for coloring
EQARate   <-  function(column_data){
  ifelse(column_data >= 80, "green",
         ifelse(column_data > 50 & column_data <= 79, "yellow","red"))
}

#### The table
tbl_all_EQAPass   <-  flextable(all_EQAPass)


### format the table
tbl_all_EQAPass  <-  tbl_all_EQAPass %>% 
  add_header_row(values = c(
    `EQA Scheme`  =  "EQA Scheme",
    "EQA Pass Rate by Quarter", "", "",""
  ))

## set the labels
tbl_all_EQAPass  <-  tbl_all_EQAPass %>%
  set_header_labels(
    `EQA Scheme`  =  "EQA Scheme",
    `2024-Q4`  =   "Oct-Dec",
    `2025-Q1`    =    "Jan-Mar",
    `2025-Q2`    =   "Apri-June",
    `2025-Q3`    =   "Jul-Sept"
  )


### List Columns to Apply Coloring
columns <- c("2024-Q4", "2025-Q1", "2025-Q2", "2025-Q3")

### Apply Background Color ###
for (col in columns) {
  tbl_all_EQAPass  <-  tbl_all_EQAPass %>%
    bg(j = col, bg = EQARate(all_EQAPass[[col]]), part = "body")
}

tbl_all_EQAPass  <-  tbl_all_EQAPass %>%
  # Merging columns 
  merge_at(i = 1, j = 2:5, part = "header")

# Merging vertically for the first five columns in the header
tbl_all_EQAPass  <-  tbl_all_EQAPass %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_all_EQAPass  <-  tbl_all_EQAPass %>%
  vline(j = c(1, 2, 3, 4,5), part = "all")


tbl_all_EQAPass  <-  tbl_all_EQAPass %>%
  add_header_row(values="EQA Schemes Pass Rate", 
                 colwidths = ncol(all_EQAPass))

tbl_all_EQAPass

#### Report - overall EQA Pass Rate ####
overallEQA   <-  EID_PassRdf %>% 
  full_join(VL_PassRdf, by = "Period") %>% 
  full_join(CrAg_PassRdf, by = "Period") %>% 
  full_join(HBV_PassRdf, by = "Period") %>%
  full_join(HPV_PassRdf, by = "Period") %>%
  full_join(Mal_PassRdf, by = "Period") %>%
  full_join(Gram_PassRdf, by = "Period") %>%
  full_join(CD4_PassRdf, by = "Period")
  
#### Add the pass and response values, get proportions
overallEQAdf <- overallEQA %>% 
  mutate(
    No.responding = rowSums(across(starts_with("No_Responding")), na.rm = TRUE),
    No.Pass       = rowSums(across(starts_with("No_Pass")), na.rm = TRUE)
  ) %>% 
  mutate(
    `%age` = round((No.Pass / No.responding) * 100, 0)
  ) %>% 
  select(Period, No.responding,No.Pass, `%age`) 

#### the table
tbl_overallEQAdf  <-  flextable(overallEQAdf)

tbl_overallEQAdf <-  tbl_overallEQAdf %>% 
  add_header_row(values = "Overall EQA Pass Rate",
                 colwidths = ncol(overallEQAdf))
  
  tbl_overallEQAdf
  
###########################################################################
#### Report - EQA Pass Rate by RRH ####
#### EID #####
RRHEID_PassR  <-  EIDEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH,Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) %>% 
 mutate(
   EID = ifelse(No_Responding < 10,
                paste0(No_Pass, "/", No_Responding),
                paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
   )) %>% 
  select(RRH, Period, EID) %>% 
    pivot_wider(
      names_from = Period,
      values_from = EID
    )


#### VL ####
RRHVL_PassR  <-  VLEQA_data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) %>% 
  mutate(
    VL = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(RRH, Period, VL) %>% 
  pivot_wider(
    names_from = Period,
    values_from = VL
  ) %>% 
  select(RRH, `2024-Q4`, `2025-Q1`, `2025-Q2`, `2025-Q3`)

#### CrAg ####
RRHCrAg_PassR  <-  CrAgEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    CrAg = ifelse(No_Responding < 10,
                paste0(No_Pass, "/", No_Responding),
                paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(RRH, Period, CrAg) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CrAg
  )

#### HBV ####
RRHHBV_PassR  <-  HBVEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    HBV = ifelse(No_Responding < 10,
                  paste0(No_Pass, "/", No_Responding),
                  paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(RRH, Period, HBV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HBV
  ) %>% 
  select(RRH, `2024-Q4`, `2025-Q1`, `2025-Q2`, `2025-Q3`)

#### HPV ####
RRHHPV_PassR  <-  HPVEQA_Data %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    HPV = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(RRH, Period, HPV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HPV
  )

#### Malaria ####
RRHMal_PassR  <-  MalEQA_Data %>% 
  filter(!is.na(RRH)) %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    Malaria = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(RRH, Period, Malaria) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Malaria
  )

#### Gram ####
RRHGram_PassR  <-  GramEQA_Data %>% 
  filter(!is.na(RRH)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  )%>% 
  mutate(
    Gram = ifelse(No_Responding < 10,
                     paste0(No_Pass, "/", No_Responding),
                     paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(RRH, Period, Gram) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Gram
  )

#### CD4 ####
RRHCD4_PassR  <-  CD4EQA %>% 
  filter(!is.na(RRH)) %>% 
    filter(
      !(`Overrall Score` %in% c("UNGRADED", "-")) & 
        !is.na(`Overrall Score`)
    ) %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) %>% 
  mutate(
    CD4 = ifelse(No_Responding < 10,
                  paste0(No_Pass, "/", No_Responding),
                  paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))  %>% 
  select(RRH, Period, CD4) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CD4
  )


#### Report - All RRH EQA Pass Rate ####
RRHall_EQAPass  <- RRHEID_PassR %>% 
  left_join(RRHVL_PassR, by = "RRH") %>% 
  left_join(RRHCrAg_PassR, by = "RRH") %>%
  left_join(RRHHBV_PassR, by = "RRH") %>%
  left_join(RRHHPV_PassR, by = "RRH") %>%
  left_join(RRHMal_PassR, by = "RRH") %>%
  left_join(RRHGram_PassR, by = "RRH") %>%
  left_join(RRHCD4_PassR, by = "RRH")

#### the table
tbl_RRHall_EQAPass  <-  flextable(RRHall_EQAPass)

### format the table
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    
    "EID", "","","",
    "VL","","","",
    "CrAg", "","","",
    "HBV", "","","",
    "HPV", "","","",
    "Mal", "","","",
    "Gram", "","","",
    "CD4", "",""
  ))

## set the labels
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  set_header_labels(
    RRH  =  "RRH",
    Q4.x    =   "Oct-Dec",
    Q1.x    =    "Jan-Mar",
    Q2.x    =   "Apri-Jun",
    Q3.x    =   "Jul-Sept",
    
    Q4.y    =  "Oct-Dec",
    Q1.y    =    "Jan-Mar",
    Q2.y    =   "Apri-Jun",
    Q3.y    =   "Jul-Sept",
    
    Q4.x.x    =   "Oct-Dec",
    Q1.x.x    =    "Jan-Mar",
    Q2.x.x    =    "Apri-Jun",
    Q3.x.x    =   "Jul-Sept",
    
    Q4.y.y    =   "Oct-Dec",
    Q1.y.y    =    "Jan-Mar",
    Q2.y.y    =    "Apri-Jun",
    Q3.y.y    =   "Jul-Sept",
    
    Q4.x.x.x    =   "Oct-Dec",
    Q1.x.x.x    =    "Jan-Mar",
    Q2.x.x.x    =    "Apri-Jun",
    Q3.x.x.x   =   "Jul-Sept",
    
    Q4.y.y.y    =   "Oct-Dec",
    Q1.y.y.y    =    "Jan-Mar",
    Q2.y.y.y    =    "Apri-Jun",
    Q3.y.y.y    =   "Jul-Sept",
    
    Q4  =    "Oct-Dec",
    Q1.x.x.x.x           =   "Jan-Mar",
    Q2.x.x.x.x  =    "Apri-Jun",
    Q3.x.x.x.x    =   "Jul-Sept",
    
    Q1.y.y.y.y      =    "Jan-Mar",
    Q2.y.y.y.y     =    "Apri-Jun",
    Q3.y.y.y.y   =   "Jul-Sept"
    
    )


tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:5, part = "header") %>%  # EID
  merge_at(i = 1, j = 6:9, part = "header") %>%  # VL
  merge_at(i = 1, j = 10:13, part = "header") %>% # CrAg
  merge_at(i = 1, j = 14:17, part = "header") %>% # HBV
  merge_at(i = 1, j = 18:21, part = "header") %>% # HPV
  merge_at(i = 1, j = 22:25, part = "header") %>% # Mal
  merge_at(i = 1, j = 26:29, part = "header") %>% # Gram
  merge_at(i = 1, j = 30:32, part = "header")     # CD4

# Merging vertically for the first five columns in the header
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  merge_v(j = 1, part = "header") 


# Adding vertical lines to improve readability
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  vline(j = c(1, 5, 9, 13, 17, 21, 25, 29, 32), part = "all")


tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  add_header_row(values="%age of sites passing the EQA Schemes by RRH and Quarter", 
                 colwidths = ncol(RRHall_EQAPass))

tbl_RRHall_EQAPass


##############################################################
################################################################
#### Report - EQA Pass Rate by RRH with colors ####
#### EID #####
RRHEID_PassR1  <-  EIDEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH,Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    EID = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, EID) %>% 
  pivot_wider(
    names_from = Period,
    values_from = EID
  )


#### VL ####
RRHVL_PassR1  <-  VLEQA_data %>% 
  filter(!is.na(RRH)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) %>% 
  mutate(
    VL = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, VL) %>% 
  pivot_wider(
    names_from = Period,
    values_from = VL
  ) %>% 
  select(RRH, `2024-Q4`, `2025-Q1`, `2025-Q2`)

#### CrAg ####
RRHCrAg_PassR1  <-  CrAgEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    CrAg = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, CrAg) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CrAg
  )

#### HBV ####
RRHHBV_PassR1  <-  HBVEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    HBV = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, HBV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HBV
  ) %>% 
  select(RRH, `2024-Q4`, `2025-Q1`, `2025-Q2`)

#### HPV ####
RRHHPV_PassR1  <-  HPVEQA_Data %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    HPV = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, HPV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HPV
  )

#### Malaria ####
RRHMal_PassR1  <-  MalEQA_Data %>% 
  filter(!is.na(RRH)) %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) %>% 
  mutate(
    Malaria = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, Malaria) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Malaria
  )

#### Gram ####
RRHGram_PassR1  <-  GramEQA_Data %>% 
  filter(!is.na(RRH)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  )%>% 
  mutate(
    Gram = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, Gram) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Gram
  )

#### CD4 ####
RRHCD4_PassR1  <-  CD4EQA %>% 
  filter(!is.na(RRH)) %>% 
  filter(`Overrall Score` != "UNGRADED") %>%   # remove the ungraded from the denominator
  group_by(RRH, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) %>% 
  mutate(
    CD4 = round((No_Pass / No_Responding) * 100, 0)) %>% 
  select(RRH, Period, CD4) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CD4
  )


#### Report - All RRH EQA Pass Rate ####
RRHall_EQAPass1  <- RRHEID_PassR1 %>% 
  left_join(RRHVL_PassR1, by = "RRH") %>% 
  left_join(RRHCrAg_PassR1, by = "RRH") %>%
  left_join(RRHHBV_PassR1, by = "RRH") %>%
  left_join(RRHHPV_PassR1, by = "RRH") %>%
  left_join(RRHMal_PassR1, by = "RRH") %>%
  left_join(RRHGram_PassR1, by = "RRH") %>%
  left_join(RRHCD4_PassR1, by = "RRH")


# function to color the columns
EQA_R   <-  function(column_data){
  ifelse(column_data >= 80, "green",
         ifelse(column_data >= 50 & column_data < 80, "yellow","red"))
}

#### the table
tbl_RRHall_EQAPass1  <-  flextable(RRHall_EQAPass1)


### format the table
tbl_RRHall_EQAPass1   <-   tbl_RRHall_EQAPass1 %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "EID", "","",
    "VL", "","",
    "CrAg", "","",
    "HBV", "","",
    "HPV", "","",
    "Mal", "","",
    "Gram", "","",
    "CD4", ""
  ))

## set the labels
tbl_RRHall_EQAPass1   <-   tbl_RRHall_EQAPass1 %>% 
  set_header_labels(
    RRH  =  "RRH",
    RRH  =  "RRH",
    `2024-Q4.x`    =   "Oct-Dec",
    `2025-Q1.x`    =    "Jan-Mar",
    `2025-Q2.x`    =    "Apri-June",
    
    `2024-Q4.y`   =   "Oct-Dec",
    `2025-Q1.y`     =    "Jan-Mar",
    `2025-Q2.y`    =    "Apri-June",
    
    `2024-Q4.x.x`   =   "Oct-Dec",
    `2025-Q1.x.x`    =    "Jan-Mar",
    `2025-Q2.x.x`    =    "Apri-June",
    
    
    `2024-Q4.y.y`    =   "Oct-Dec",
    `2025-Q1.y.y`    =    "Jan-Mar",
    `2025-Q2.y.y`    =    "Apri-June",
    
    
    `2024-Q4.x.x.x`    =   "Oct-Dec",
    `2025-Q1.x.x.x`    =    "Jan-Mar",
    `2025-Q2.x.x.x`    =    "Apri-June",
    
    `2024-Q4.y.y.y`    =   "Oct-Dec",
    `2025-Q1.y.y.y`    =    "Jan-Mar",
    `2025-Q2.y.y.y`    =    "Apri-June",
    
    `2024-Q4`    =   "Oct-Dec",
    `2025-Q1.x.x.x.x`    =    "Jan-Mar",
    `2025-Q2.x.x.x.x`      =    "Apri-June",
    
    
    `2025-Q1.y.y.y.y`   =    "Jan-Mar",
    `2025-Q2.y.y.y.y`   =    "Apri-June"
  )



### List Columns to Apply Coloring ###
# Get column names between the two markers
columns <- names(RRHall_EQAPass1)
start_col <- which(columns == "2024-Q4.x")
end_col <- which(columns == "2025-Q2.y.y.y.y")

### target columns
target_columns <- columns[start_col:end_col]

### Apply Background Color ###
for (col in target_columns) {
  tbl_RRHall_EQAPass1   <-   tbl_RRHall_EQAPass1 %>% 
    bg(j = col, bg = EQA_R(RRHall_EQAPass1[[col]]), part = "body")
}


tbl_RRHall_EQAPass1   <-   tbl_RRHall_EQAPass1 %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:4, part = "header") %>% 
  merge_at(i = 1, j = 5:7, part = "header") %>% 
  merge_at(i = 1, j = 8:10, part = "header") %>% 
  merge_at(i = 1, j = 11:13, part = "header") %>%
  merge_at(i = 1, j = 14:16, part = "header") %>%
  merge_at(i = 1, j = 17:19, part = "header") %>%
  merge_at(i = 1, j = 20:22, part = "header") %>%
  merge_at(i = 1, j = 23:24, part = "header")

# Merging vertically for the first five columns in the header
tbl_RRHall_EQAPass1   <-   tbl_RRHall_EQAPass1 %>% 
  merge_v(j = 1, part = "header")

# Adding vertical lines to improve readability
tbl_RRHall_EQAPass1   <-   tbl_RRHall_EQAPass1 %>% 
  vline(j = c(1,4, 7, 10, 13, 16, 19, 22, 24), part = "all")


tbl_RRHall_EQAPass1   <-   tbl_RRHall_EQAPass1 %>% 
  add_header_row(values="%age of sites passing the EQA Schemes by RRH and Quarter", 
                 colwidths = ncol(RRHall_EQAPass1))


# Define a thick dark border
dark_border <- fp_border(color = "black", width = 2)

# Add styling
tbl_RRHall_EQAPass1 <- tbl_RRHall_EQAPass1 %>%
  # Make all text bold and dark purple
  bold(part = "all") %>%
  color(color = "#4B0082", part = "all") %>%  # Dark purple
  
  # Apply dark thick borders
  border(part = "all", border = dark_border) %>%
  border_inner(border = dark_border, part = "all") %>%
  border_outer(border = dark_border, part = "all")

tbl_RRHall_EQAPass1

###########################################################################
#### Report - Pass Rate by District ####
#### EID #####
dstEIDPassR  <-  EIDEQA_Data %>% 
  filter(!is.na(District)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(District,Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) 

#### proportions by district
dstEID_PassR   <-  dstEIDPassR %>% 
  mutate(
    EID = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    )) %>% 
  select(District, Period, EID) %>% 
  pivot_wider(
    names_from = Period,
    values_from = EID
  )


#### VL ####
dstVLPassR  <-  VLEQA_data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(District, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) 

#### Pass rate by district
dstVL_PassR  <-  dstVLPassR %>% 
  mutate(
    VL = ifelse(No_Responding < 10,
                paste0(No_Pass, "/", No_Responding),
                paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    )) %>% 
  select(District, Period, VL) %>% 
  pivot_wider(
    names_from = Period,
    values_from = VL
  ) %>% 
  select(District, `2024-Q4`, `2025-Q1`, `2025-Q2`)

#### CrAg ####
dstCrAgPassR  <-  CrAgEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(District, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) 

#### proportions by district
dstCrAg_PassR   <-    dstCrAgPassR %>% 
  mutate(
    CrAg = ifelse(No_Responding < 10,
                  paste0(No_Pass, "/", No_Responding),
                  paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(District, Period, CrAg) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CrAg
  )

#### HBV ####
dstHBVPassR  <-  HBVEQA_Data %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(District, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) 

#### pass rates
RRHHBV_PassR  <-  dstHBVPassR %>%  
  mutate(
    HBV = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(District, Period, HBV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HBV
  ) %>% 
  select(District, `2024-Q4`, `2025-Q1`, `2025-Q2`)

#### HPV ####
dstHPVPassR  <-  HPVEQA_Data %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(District, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) 


### pass rates
dstHPV_PassR  <-  dstHPVPassR %>% 
  mutate(
    HPV = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(District, Period, HPV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HPV
  )

#### Malaria ####
dstMalPassR  <-  MalEQA_Data %>% 
  filter(!is.na(District)) %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(District, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) 


#### pass rates 
dstMal_PassR  <-  dstMalPassR %>% 
  mutate(
    Malaria = ifelse(No_Responding < 10,
                     paste0(No_Pass, "/", No_Responding),
                     paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(District, Period, Malaria) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Malaria
  )

#### Gram ####
dstGramPassR  <-  GramEQA_Data %>% 
  filter(!is.na(District)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(District, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  )


### pass rates
dstGram_PassR    <-   dstGramPassR %>% 
  mutate(
    Gram = ifelse(No_Responding < 10,
                  paste0(No_Pass, "/", No_Responding),
                  paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(District, Period, Gram) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Gram
  )

#### CD4 ####
dstCD4PassR  <-  CD4EQA %>% 
  filter(!is.na(District)) %>% 
  filter(`Overrall Score` != "UNGRADED") %>%   # remove the ungraded from the denominator
  group_by(District, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) 

#### pass rates
dstCD4_PassR   <-  dstCD4PassR %>% 
  mutate(
    CD4 = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))  %>% 
  select(District, Period, CD4) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CD4
  )

#### combinining all EQA schemes
dstEQA  <-  dstEIDPassR %>% 
  full_join(dstVLPassR, by = c("District", "Period")) %>% 
  full_join(dstCrAgPassR, by = c("District", "Period")) %>% 
  full_join(dstHBVPassR, by = c("District", "Period")) %>%
  full_join(dstHPVPassR, by = c("District", "Period")) %>%
  full_join(dstMalPassR, by = c("District", "Period")) %>%
  full_join(dstGramPassR, by = c("District", "Period")) %>%
  full_join(dstCD4PassR, by = c("District", "Period"))
  

#### sum responding and passing
dstEQAdf  <-  dstEQA %>% 
  mutate(
    response  =  rowSums(across(starts_with("No_Responding")), na.rm = TRUE),
    pass      =  rowSums(across(starts_with("No_Pass")), na.rm = TRUE)
  ) %>% 
  select(District,Period, response, pass)

#### Report - %age passing 80% of test menu
dstEQARpt   <-   dstEQAdf %>% 
  group_by(Period, District) %>% 
    summarise(
       PassRate   =  round((pass/response)*100,0),
      .groups = "drop"
      ) 

#### Pvt 
dstEQARptPvt  <-   dstEQARpt %>% 
  pivot_wider(
    names_from = Period,
    values_from = PassRate
  )
    
#### print the excel
### write.xlsx(ETAT_Wk_overall_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/EID/EIDOverall.xls")

#### Report - Pass Rate by sub county ####
#### kampala
EIDEQA_Datakla  <-   EIDEQA_Data %>%
  filter(District == "KAMPALA") %>% 
    left_join(master, by = "HFacility")


#### EID #####
EIDPassRcty  <-  EIDEQA_Datakla %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n_distinct(HFacility),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) 

#### proportions by suncounty
EIDPassRcty1   <-  EIDPassRcty %>% 
  filter(!is.na(SubCounty)) %>% 
  mutate(
    EID = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    )) %>% 
  select(SubCounty, Period, EID) %>% 
  pivot_wider(
    names_from = Period,
    values_from = EID
  )


#### VL ####
VLEQA_datakla  <-   VLEQA_data %>%
 filter(District == "KAMPALA") %>% 
  left_join(master, by = "HFacility")


dstVLPassRcty  <-  VLEQA_datakla %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) 

#### Pass rate by district
dstVLPassRcty1  <-  dstVLPassRcty %>% 
  filter(!is.na(SubCounty)) %>% 
  mutate(
    VL = ifelse(No_Responding < 10,
                paste0(No_Pass, "/", No_Responding),
                paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    )) %>% 
  select(SubCounty, Period, VL) %>% 
  pivot_wider(
    names_from = Period,
    values_from = VL
  ) %>% 
  select(SubCounty, `2024-Q4`, `2025-Q1`, `2025-Q2`)

#### CrAg ####
CrAgEQA_Datakla  <-   CrAgEQA_Data %>%
  filter(District == "KAMPALA") %>% 
  left_join(master, by = "HFacility")



dstCrAgPassRcty  <-  CrAgEQA_Datakla %>% 
  filter(!is.na(SubCounty)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) 

#### proportions by district
dstCrAgPassRcty1   <-    dstCrAgPassRcty %>% 
  mutate(
    CrAg = ifelse(No_Responding < 10,
                  paste0(No_Pass, "/", No_Responding),
                  paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(SubCounty, Period, CrAg) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CrAg
  )

#### HBV ####
HBVEQA_Datakla  <-   HBVEQA_Data %>%
  filter(District == "KAMPALA") %>% 
  left_join(master, by = "HFacility")



dstHBVPassRcty  <-  HBVEQA_Datakla %>%
  filter(!is.na(SubCounty)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  ) 

#### pass rates
dstHBVPassRcty1  <-  dstHBVPassRcty %>%  
  mutate(
    HBV = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(SubCounty, Period, HBV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HBV
  ) %>% 
  select(SubCounty, `2024-Q4`, `2025-Q1`, `2025-Q2`)

#### HPV ####
HPVEQA_Datakla  <-   HPVEQA_Data %>%
  filter(District == "KAMPALA") %>% 
  left_join(master, by = "HFacility")



dstHPVPassRcty  <-  HPVEQA_Datakla %>% 
  filter(!is.na(SubCounty)) %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) 


### pass rates
dstHPVPassRcty1  <-  dstHPVPassRcty %>% 
  mutate(
    HPV = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(SubCounty, Period, HPV) %>% 
  pivot_wider(
    names_from = Period,
    values_from = HPV
  )

#### Malaria ####
MalEQA_Datakla  <-   MalEQA_Data %>%
  filter(District == "KAMPALA") %>% 
  left_join(master, by = "HFacility")




dstMalPassRcty  <-  MalEQA_Datakla %>% 
  filter(!is.na(SubCounty)) %>% 
  filter(Grading != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(Grading == "Satisfactory"),
    .groups = "drop"
  ) 


#### pass rates 
dstMalPassRcty1  <-  dstMalPassRcty %>% 
  mutate(
    Malaria = ifelse(No_Responding < 10,
                     paste0(No_Pass, "/", No_Responding),
                     paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(SubCounty, Period, Malaria) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Malaria
  )

#### Gram ####
GramEQA_Datakla  <-   GramEQA_Data %>%
  filter(District == "KAMPALA") %>% 
  left_join(master, by = "HFacility")


dstGramPassRcty  <-  GramEQA_Datakla %>% 
  filter(!is.na(SubCounty)) %>% 
  filter(`Overrall Score` != "Ungraded") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "Satisfactory"),
    .groups = "drop"
  )


### pass rates
dstGramPassRcty1    <-   dstGramPassRcty %>% 
  mutate(
    Gram = ifelse(No_Responding < 10,
                  paste0(No_Pass, "/", No_Responding),
                  paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))%>% 
  select(SubCounty, Period, Gram) %>% 
  pivot_wider(
    names_from = Period,
    values_from = Gram
  )

#### CD4 ####
CD4EQAkla  <-   CD4EQA %>%
  filter(District == "KAMPALA") %>% 
  left_join(master, by = "HFacility")




dstCD4PassRcty  <-  CD4EQAkla %>% 
  filter(!is.na(SubCounty)) %>% 
  filter(`Overrall Score` != "UNGRADED") %>%   # remove the ungraded from the denominator
  group_by(SubCounty, Period) %>% 
  summarise(
    No_Responding = n(),
    No_Pass       =  sum(`Overrall Score` == "SATISFACTORY"),
    .groups = "drop"
  ) 

#### pass rates
dstCD4PassRcty1   <-  dstCD4PassRcty %>% 
  mutate(
    CD4 = ifelse(No_Responding < 10,
                 paste0(No_Pass, "/", No_Responding),
                 paste0(No_Pass, "/", No_Responding, " (", round((No_Pass / No_Responding) * 100, 1), "%)")
    ))  %>% 
  select(SubCounty, Period, CD4) %>% 
  pivot_wider(
    names_from = Period,
    values_from = CD4
  )

#### combinining all EQA schemes
dstEQActy  <-  EIDPassRcty %>% 
  full_join(dstVLPassRcty, by = c("SubCounty", "Period")) %>% 
  full_join(dstCrAgPassRcty, by = c("SubCounty", "Period")) %>% 
  full_join(dstHBVPassRcty, by = c("SubCounty", "Period")) %>%
  full_join(dstHPVPassRcty, by = c("SubCounty", "Period")) %>%
  full_join(dstMalPassRcty, by = c("SubCounty", "Period")) %>%
  full_join(dstGramPassRcty, by = c("SubCounty", "Period")) %>%
  full_join(dstCD4PassRcty, by = c("SubCounty", "Period"))


#### sum responding and passing
dstEQAdf1  <-  dstEQActy %>% 
  mutate(
    response  =  rowSums(across(starts_with("No_Responding")), na.rm = TRUE),
    pass      =  rowSums(across(starts_with("No_Pass")), na.rm = TRUE)
  ) %>% 
  select(SubCounty,Period, response, pass)

#### Report - %age passing 80% of test menu
dstEQARpt1   <-   dstEQAdf1 %>% 
  group_by(Period, SubCounty) %>% 
  summarise(
    PassRate   =  round((pass/response)*100,0),
    .groups = "drop"
  ) 

#### Pvt 
dstEQARptPvt1  <-   dstEQARpt1 %>% 
  pivot_wider(
    names_from = Period,
    values_from = PassRate
  )

#### print the excel
### write.xlsx(ETAT_Wk_overall_Pvt11, file = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q1/Data/EID/EIDOverall.xls")




#############################################################
#### Report - District EQA Pass Rate ####
RRHall_EQAPass  <- RRHEID_PassR %>% 
  left_join(RRHVL_PassR, by = "RRH") %>% 
  left_join(RRHCrAg_PassR, by = "RRH") %>%
  left_join(RRHHBV_PassR, by = "RRH") %>%
  left_join(RRHHPV_PassR, by = "RRH") %>%
  left_join(RRHMal_PassR, by = "RRH") %>%
  left_join(RRHGram_PassR, by = "RRH") %>%
  left_join(RRHCD4_PassR, by = "RRH")

#### the table
tbl_RRHall_EQAPass  <-  flextable(RRHall_EQAPass)

### format the table
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "EID", "","",
    "VL","","",
    "CrAg", "","",
    "HBV", "","",
    "HPV", "","",
    "Mal", "","",
    "Gram", "","",
    "CD4", ""
  ))

## set the labels
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  set_header_labels(
    RRH  =  "RRH",
    `2024-Q4.x`    =   "Oct-Dec",
    `2025-Q1.x`    =    "Jan-Mar",
    `2025-Q2.x`    =   "Apri-Jun", 
    
    `2024-Q4.y`    =   "Oct-Dec",
    `2025-Q1.y`    =    "Jan-Mar",
    `2025-Q2.y`    =   "Apri-Jun", 
    
    
    `2024-Q4.x.x`    =   "Oct-Dec",
    `2025-Q1.x.x`    =    "Jan-Mar",
    `2025-Q2.x.x`    =   "Apri-Jun", 
    
    `2024-Q4.y.y`    =   "Oct-Dec",
    `2025-Q1.y.y`    =    "Jan-Mar",
    `2025-Q2.y.y`    =   "Apri-Jun",
    
    `2024-Q4.x.x.x`    =   "Oct-Dec",
    `2025-Q1.x.x.x`    =    "Jan-Mar",
    `2025-Q2.x.x.x`    =   "Apri-Jun",
    
    `2024-Q4.y.y.y`    =   "Oct-Dec",
    `2025-Q1.y.y.y`    =    "Jan-Mar",
    `2025-Q2.y.y.y`    =   "Apri-Jun",
    
    
    `2024-Q4`    =   "Oct-Dec",
    `2025-Q1.x.x.x.x`    =    "Jan-Mar",
    `2025-Q2.x.x.x.x`    =   "Apri-Jun",
    
    `2025-Q1.y.y.y.y`    =    "Jan-Mar",
    `2025-Q2.y.y.y.y`    =   "Apri-Jun"
    
  )


tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:4, part = "header") %>% 
  merge_at(i = 1, j = 5:7, part = "header") %>% 
  merge_at(i = 1, j = 8:10, part = "header") %>% 
  merge_at(i = 1, j = 11:13, part = "header") %>%
  merge_at(i = 1, j = 14:16, part = "header") %>%
  merge_at(i = 1, j = 17:19, part = "header") %>%
  merge_at(i = 1, j = 20:22, part = "header") %>%
  merge_at(i = 1, j = 23:24, part = "header")

# Merging vertically for the first five columns in the header
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  merge_v(j = 1, part = "header") 


# Adding vertical lines to improve readability
tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  vline(j = c(4, 7, 10, 13, 16, 19, 22, 24), part = "all")


tbl_RRHall_EQAPass   <-   tbl_RRHall_EQAPass %>% 
  add_header_row(values="%age of sites passing the EQA Schemes by RRH and Quarter", 
                 colwidths = ncol(RRHall_EQAPass))

tbl_RRHall_EQAPass

##############################################################






#### Annex Reports - Sites not passing EQA ####
#### EID EQA ####
eidNoPas   <-   EIDEQA_Data %>%
  ungroup() %>% 
  filter(`Overrall Score` ==  "Unsatisfactory") %>% 
  group_by(RRH, Period, hflevel)

#### establish those facilities missing in > 2 cycles
eidNoPascommon <- eidNoPas %>%
  group_by(RRH,District, hflevel, HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel, n_quarters, Quarters
         )
#### the table
tbl_eidNoPascommon  <-  flextable(eidNoPascommon)
tbl_eidNoPascommon
#### format the table
tbl_eidNoPascommon   <-   tbl_eidNoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_eidNoPascommon   <-   tbl_eidNoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_eidNoPascommon   <-   tbl_eidNoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_eidNoPascommon   <-   tbl_eidNoPascommon %>% 
  add_header_row(values="List of facilities not passing the EID EQA in one or more EQA cycles", 
                 colwidths = ncol(eidNoPascommon))

tbl_eidNoPascommon


#### No. failing by health level 
EIDEQA_lvl   <-   EIDEQA_Data %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )


#### Report section EID facilities missing in > 2 cycles ####
eidNoPascommohln_summary <- eidNoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
eidsites   <-  eidNoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_eid`   =  n_distinct(HFacility)
  )

# combine the data frames
eidsitesrpt  <-  EIDEQA_lvl %>% 
  left_join(eidsites, by = "hflevel") %>% 
  left_join(eidNoPascommohln_summary, by = "hflevel") %>% 
    
    filter(!is.na(`2ormore_eid`))

### include the fractions
eidsitesrpt   <-   eidsitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_eid`), "", paste0(hf2, "/", `2ormore_eid`)),
    `3/4` = ifelse(is.na(hf3) | is.na(`2ormore_eid`), "", paste0(hf3, "/", `2ormore_eid`))
                       )
  
# the table
tbl_eidsitesrpt   <-  flextable(eidsitesrpt)
tbl_eidsitesrpt

#### VL EQA ####
VLNoPas   <-   VLEQA_data %>% 
  filter(`Overrall Score` ==  "UNSATISFACTORY") %>% 
  group_by(RRH, Period)

#### establish those facilities missing in both Q1 and Q4
VLNoPascommon <- VLNoPas %>%
  group_by(RRH,District, hflevel, HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel, n_quarters, Quarters
         )


#### the table
tbl_VLNoPascommon  <-  flextable(VLNoPascommon)

#### format the table
tbl_VLNoPascommon   <-   tbl_VLNoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_VLNoPascommon   <-   tbl_VLNoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_VLNoPascommon   <-   tbl_VLNoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_VLNoPascommon   <-   tbl_VLNoPascommon %>%  
  add_header_row(values="List of facilities not passing the VL EQA in one or more EQA cycles", 
                 colwidths = ncol(VLNoPascommon))

tbl_VLNoPascommon

#### Report - Section VL missing > 2 cycles ####
#### no. failing by health level
VLEQA_lvl   <-   VLEQA_data %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )

#### no sites failing by health level

vlNoPascommohln_summary <- VLNoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
vlsites   <-  VLNoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_vl`   =  n_distinct(HFacility)
  )

# combine the data frames
vlsitesrpt  <-  VLEQA_lvl %>% 
  left_join(vlsites, by = "hflevel") %>% 
  left_join(vlNoPascommohln_summary, by = "hflevel") %>% 
  
  filter(!is.na(`2ormore_vl`))

### include the fractions
vlsitesrpt   <-   vlsitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_vl`), "", paste0(hf2, "/", `2ormore_vl`)),
    `3/4` = ifelse(is.na(hf3) | is.na(`2ormore_vl`), "", paste0(hf3, "/", `2ormore_vl`))
  )

#####
#### CrAg EQA ####
CrAgNoPas   <-   CrAgEQA_Data %>% 
  filter(`Overrall Score` ==  "Unsatisfactory") %>% 
  group_by(RRH, Period)

#### establish those facilities missing in both Q1 and Q4
CrAgNoPascommon <- CrAgNoPas %>%
  group_by(RRH,District, hflevel, HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel,n_quarters, Quarters
         )


#### the table
tbl_CrAgNoPascommon  <-  flextable(CrAgNoPascommon)

#### format the table
tbl_CrAgNoPascommon   <-   tbl_CrAgNoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_CrAgNoPascommon   <-   tbl_CrAgNoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_CrAgNoPascommon   <-   tbl_CrAgNoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_CrAgNoPascommon   <-   tbl_CrAgNoPascommon %>%  
  add_header_row(values="List of facilities not passing the CrAg EQA in one or more EQA cycles", 
                 colwidths = ncol(CrAgNoPascommon))

tbl_CrAgNoPascommon

#### Report - Section CrAg missing > 2 cycles ####
#### no. failing by health level
CrAgEQA_lvl   <-   CrAgEQA_Data %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )

#### no sites failing by health level

CrAgNoPascommohln_summary <- CrAgNoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
CrAgsites   <-  CrAgNoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_CrAg`   =  n_distinct(HFacility)
  )

# combine the data frames
CrAgsitesrpt  <-  CrAgEQA_lvl %>% 
  left_join(CrAgsites, by = "hflevel") %>% 
  left_join(CrAgNoPascommohln_summary, by = "hflevel") %>% 
  
  filter(!is.na(`2ormore_CrAg`))

### include the fractions
CrAgsitesrpt   <-   CrAgsitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_CrAg`), "", paste0(hf2, "/", `2ormore_CrAg`)),
    `3/4` = ifelse(is.na(hf3) | is.na(`2ormore_CrAg`), "", paste0(hf3, "/", `2ormore_CrAg`))
  )

#### HBV EQA ####
HBVNoPas   <-   HBVEQA_Data %>% 
  filter(`Overrall Score` ==  "Unsatisfactory") %>% 
  group_by(RRH, Period)

#### establish those facilities missing in both Q1 and Q4
HBVNoPascommon <- HBVNoPas %>%
  group_by(RRH,District, hflevel, HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel, n_quarters, 
         Quarters)


#### the table
tbl_HBVNoPascommon  <-  flextable(HBVNoPascommon)

#### format the table
tbl_HBVNoPascommon   <-   tbl_HBVNoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_HBVNoPascommon   <-   tbl_HBVNoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_HBVNoPascommon   <-   tbl_HBVNoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_HBVNoPascommon   <-   tbl_HBVNoPascommon %>% 
  add_header_row(values="List of facilities not passing the HBV EQA in one or more EQA cycles", 
                 colwidths = ncol(HBVNoPascommon))

tbl_HBVNoPascommon

#### Report - Section HBV missing > 2 cycles ####
#### no. failing by health level
HBVEQA_lvl   <-   HBVEQA_Data %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )

#### no sites failing by health level

HBVNoPascommohln_summary <- HBVNoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
HBVsites   <-  HBVNoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_hbv`   =  n_distinct(HFacility)
  )

# combine the data frames
hbvsitesrpt  <-  HBVEQA_lvl %>% 
  left_join(HBVsites, by = "hflevel") %>% 
  left_join(HBVNoPascommohln_summary, by = "hflevel") %>% 
  
  filter(!is.na(`2ormore_hbv`))

### include the fractions
hbvsitesrpt   <-   hbvsitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_hbv`), "", paste0(hf2, "/", `2ormore_hbv`)
  ))

#####
#### HPV EQA ####
HPVNoPas   <-   HPVEQA_Data %>% 
  filter(Grading ==  "Unsatisfactory") %>% 
  group_by(RRH, Period)

#### establish those facilities missing in both Q1 and Q4
HPVNoPascommon <- HPVNoPas %>%
  group_by(RRH,District,hflevel, HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel, n_quarters, Quarters
         )

#### the table
tbl_HPVNoPascommon  <-  flextable(HPVNoPascommon)

#### format the table
tbl_HPVNoPascommon   <-   tbl_HPVNoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_HPVNoPascommon   <-   tbl_HPVNoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_HPVNoPascommon   <-   tbl_HPVNoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_HPVNoPascommon   <-   tbl_HPVNoPascommon %>% 
  add_header_row(values="List of facilities not passing the HPV EQA in one or more EQA cycles", 
                 colwidths = ncol(HPVNoPascommon))

tbl_HPVNoPascommon


#### Report - Section HPV missing > 2 cycles ####
#### no. failing by health level
HPVEQA_lvl   <-   HPVEQA_Data %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )

#### no sites failing by health level

HPVNoPascommohln_summary <- HPVNoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
HPVsites   <-  HPVNoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_hpv`   =  n_distinct(HFacility)
  )

# combine the data frames
hpvsitesrpt  <-  HPVEQA_lvl %>% 
  left_join(HPVsites, by = "hflevel") %>% 
  left_join(HPVNoPascommohln_summary, by = "hflevel") %>% 
  
  filter(!is.na(`2ormore_hpv`))

### include the fractions
hpvsitesrpt   <-   hpvsitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_hpv`), "", paste0(hf2, "/", `2ormore_hpv`),
    `3/4` = ifelse(is.na(hf3) | is.na(`2ormore_hpv`), "", paste0(hf3, "/", `2ormore_hpv`)
    ))
  )

#### Malaria EQA ####
MalNoPas   <-   MalEQA_Data %>% 
  filter(Grading ==  "Unsatisfactory") %>% 
  group_by(RRH, Period)

#### establish those facilities missing in both Q1 and Q4
MalNoPascommon <- MalNoPas %>%
  group_by(RRH,District, hflevel, HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel, n_quarters, Quarters
         )


#### the table
tbl_MalNoPascommon <-  flextable(MalNoPascommon)

#### format the table
tbl_MalNoPascommon   <-   tbl_MalNoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_MalNoPascommon   <-   tbl_MalNoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_MalNoPascommon   <-   tbl_MalNoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_MalNoPascommon   <-   tbl_MalNoPascommon %>% 
  add_header_row(values="List of facilities not passing the Malaria EQA in one or more EQA cycles", 
                 colwidths = ncol(MalNoPascommon))

tbl_MalNoPascommon

#### Report - Section Malaria missing > 2 cycles ####
#### no. failing by health level
MalEQA_lvl   <-   MalEQA_Data %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )

#### no sites failing by health level

MalNoPascommohln_summary <- MalNoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
Malsites   <-  MalNoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_mal`   =  n_distinct(HFacility)
  )

# combine the data frames
malsitesrpt  <-  MalEQA_lvl %>% 
  left_join(Malsites, by = "hflevel") %>% 
  left_join(MalNoPascommohln_summary, by = "hflevel") %>% 
  
  filter(!is.na(`2ormore_mal`))

### include the fractions
malsitesrpt   <-   malsitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_mal`), "", paste0(hf2, "/", `2ormore_mal`)
                   ))


#### Gram EQA ####
GramNoPas   <-   GramEQA_Data %>% 
  filter(`Overrall Score` ==  "Unsatisfactory") %>% 
  group_by(RRH, Period)

#### establish those facilities missing in both Q1 and Q4
GramNoPascommon <- GramNoPas %>%
  group_by(RRH,District, hflevel, HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel, n_quarters, Quarters
         )


#### the table
tbl_GramNoPascommon  <-  flextable(GramNoPascommon)

#### format the table
tbl_GramNoPascommon   <-   tbl_GramNoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_GramNoPascommon   <-   tbl_GramNoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_GramNoPascommon   <-   tbl_GramNoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_GramNoPascommon   <-   tbl_GramNoPascommon %>% 
  add_header_row(values="List of facilities not passing the Gram EQA in one or more EQA cycles", 
                 colwidths = ncol(GramNoPascommon))

tbl_GramNoPascommon

#### Report - Section Gram missing > 2 cycles ####
#### no. failing by health level
GramEQA_lvl   <-   GramEQA_Data %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )

#### no sites failing by health level

GramNoPascommohln_summary <- GramNoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
Gramsites   <-  GramNoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_Gram`   =  n_distinct(HFacility)
  )

# combine the data frames
Gramsitesrpt  <-  GramEQA_lvl %>% 
  left_join(Gramsites, by = "hflevel") %>% 
  left_join(GramNoPascommohln_summary, by = "hflevel") %>% 
  
  filter(!is.na(`2ormore_Gram`))

### include the fractions
Gramsitesrpt   <-   Gramsitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_Gram`), "", paste0(hf2, "/", `2ormore_Gram`)),
    `3/4` = ifelse(is.na(hf3) | is.na(`2ormore_Gram`), "", paste0(hf3, "/", `2ormore_Gram`)),
    `4/4` = ifelse(is.na(hf4) | is.na(`2ormore_Gram`), "", paste0(hf4, "/", `2ormore_Gram`)                 
    ))
    
#### CD4 EQA ####
CD4NoPas   <-   CD4EQA %>% 
  filter(
    !(`Overrall Score` %in% c("UNGRADED", "-")) & 
      !is.na(`Overrall Score`)
  ) %>%
  group_by(RRH, Period)

#### establish those facilities missing in both Q1 and Q4
CD4NoPascommon <- CD4NoPas %>%
  group_by(RRH,District,hflevel,  HFacility) %>%
  summarise(
    Quarters = toString(sort(unique(Period))),
    n_quarters = n_distinct(Period),
    More_Once = ifelse(n_quarters > 1, "Yes", "No"),
    .groups = "drop"
  ) %>%
  filter(More_Once == "Yes") %>% 
  arrange(RRH) %>% 
  select(RRH, District, HFacility, hflevel, n_quarters, Quarters
         )


#### the table
tbl_CD4NoPascommon  <-  flextable(CD4NoPascommon)

#### format the table
tbl_CD4NoPascommon   <-   tbl_CD4NoPascommon %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    District   =  "District",
    HFacility  =  "Health Facility",
    hflevel    =   "Health Level",
    n_quarters  =  "No. of quarters where site did not Pass the EQA",
    Quarters   =  "Quarters where site did not Pass the EQA"
  ))


# color coding the columns
tbl_CD4NoPascommon   <-   tbl_CD4NoPascommon %>% 
  bg(
    j = "n_quarters",
    i = ~ n_quarters == 2,
    bg = "yellow"
  ) %>%
  bg(
    j = "n_quarters",
    i = ~ n_quarters != 2,
    bg = "red"
  )

# Adding vertical lines to improve readability
tbl_CD4NoPascommon   <-   tbl_CD4NoPascommon %>% 
  vline(j = c(1:6), part = "all")


tbl_CD4NoPascommon   <-   tbl_CD4NoPascommon %>% 
  add_header_row(values="List of facilities not passing the CD4 EQA in one or more EQA cycles", 
                 colwidths = ncol(CD4NoPascommon))

tbl_CD4NoPascommon

#### Report - Section CD4 missing > 2 cycles ####
#### no. failing by health level
CD4EQA_lvl   <-   CD4EQA %>% 
  group_by(hflevel) %>% 
  summarise(
    No.sites   =   n_distinct(HFacility)
  )

#### no sites failing by health level

CD4NoPascommohln_summary <- CD4NoPascommon %>%
  group_by(hflevel, n_quarters) %>%
  summarise(
    n_facilities = n_distinct(HFacility),
    .groups = "drop"
  ) %>%
  filter(n_quarters %in% c(2, 3, 4)) %>%
  tidyr::pivot_wider(
    names_from = n_quarters,
    values_from = n_facilities,
    names_prefix = "hf"
  )

# number of facilities
CD4sites   <-  CD4NoPascommon %>% 
  group_by(hflevel) %>% 
  summarise(
    `2ormore_CD4`   =  n_distinct(HFacility)
  )

# combine the data frames
CD4sitesrpt  <-  CD4EQA_lvl %>% 
  left_join(CD4sites, by = "hflevel") %>% 
  left_join(CD4NoPascommohln_summary, by = "hflevel") %>% 
  
  filter(!is.na(`2ormore_CD4`))

### include the fractions
CD4sitesrpt   <-   CD4sitesrpt %>% 
  mutate(
    `2/4` = ifelse(is.na(hf2) | is.na(`2ormore_CD4`), "", paste0(hf2, "/", `2ormore_CD4`)),
    `3/4` = ifelse(is.na(hf3) | is.na(`2ormore_CD4`), "", paste0(hf3, "/", `2ormore_CD4`))
    )



#### Report - Proportion sites missing >=2 cycles (all) ####
allEQA_rpt   <-  eidsitesrpt %>% 
  full_join(vlsitesrpt, by = "hflevel") %>% 
  full_join(hbvsitesrpt, by = "hflevel") %>% 
  full_join(malsitesrpt, by = "hflevel") %>%
  full_join(Gramsitesrpt, by = "hflevel") %>%
  full_join(CD4sitesrpt, by = "hflevel")

#### Report - Proportion of sites failing >2 cycles ####
allEQA_rpt1   <-  allEQA_rpt %>% 
  select(hflevel, 
         starts_with("2/4"), starts_with("3/4"), starts_with("4/4")
          )

# the table
tbl_allEQA_rpt1   <-  flextable(allEQA_rpt1)

tbl_allEQA_rpt1   <-  tbl_allEQA_rpt1 %>% 
  add_header_row(
    values = c(
      hflevel   =  "Health Level",
      "Failed 2/4 EQA cycles", "", "", "", "","",
      "Failed 3/4 EQA cycles","","","",
      "Failed 4/4 EQA cycles"
    )
  )


tbl_allEQA_rpt1   <-  tbl_allEQA_rpt1 %>% 
  set_header_labels(
    hflevel   =  "Health Level",
    
    `2/4.x`  =  "EID",
    `2/4.y`   =  "VL",
    `2/4.x.x`   =  "HBV",
    `2/4.y.y`  =  "Malaria", 
    `2/4.x.x.x`  =  "Gram",
    `2/4.y.y.y` =   "CD4",
    
    `3/4.x`   =  "EID",
    `3/4.y`  =  "VL", 
    `3/4.x.x`  =  "Gram",
    `3/4.y.y` =   "CD4",
    
    `4/4` =   "Gram"
      )

#### merge column
tbl_allEQA_rpt1   <-  tbl_allEQA_rpt1 %>% 
  merge_at(i = 1, j=2:7, part = "header") %>% 
  merge_at(i = 1, j=8:11, part = "header")  

#### merge vertical columns
tbl_allEQA_rpt1   <-  tbl_allEQA_rpt1 %>% 
  merge_v(j=1, part = "header")


# improve borders for clarity
tbl_allEQA_rpt1 <- tbl_allEQA_rpt1 %>% 
  border_remove() %>%  # remove default borders
  hline(part = "all", border = fp_border(color = "gray70", width = 0.5)) # horizontal lines
  
# add vertical lines
tbl_allEQA_rpt1 <- tbl_allEQA_rpt1 %>%
  vline(j = c(1:12), part = "all", border = fp_border(color = "gray70", width = 0.5)) # vertical lines

  

#### the title
tbl_allEQA_rpt1   <-  tbl_allEQA_rpt1 %>% 
  add_header_row(values = "Proportion of sites failing >= 2 cycles",
                 colwidths = ncol(allEQA_rpt1))


# format table for better spacing
tbl_allEQA_rpt1 <- tbl_allEQA_rpt1 %>% 
  set_table_properties(width = .95, layout = "autofit") %>% 
  align(align = "center", part = "all") %>% 
  bold(part = "header") %>% 
  fontsize(size = 10, part = "all")

tbl_allEQA_rpt1

#### Report - No. of sites failing 2 or more cycles by level ####
allEQA_sites    <-   allEQA_rpt %>% 
  select(hflevel, starts_with("No."), starts_with("2ormore_")
         ) %>% 
  # calculate proportions of total sites
  mutate(
    EID = ifelse(is.na(No.sites.x) | is.na(`2ormore_eid`), "", paste0(`2ormore_eid`, "/", No.sites.x)),
    VL = ifelse(is.na(No.sites.y) | is.na(`2ormore_vl`), "", paste0(`2ormore_vl`, "/",No.sites.y)),
    HBV = ifelse(is.na(No.sites.x.x) | is.na(`2ormore_hbv`), "", paste0(`2ormore_hbv`, "/", No.sites.x.x)),
    Malaria = ifelse(is.na(No.sites.y.y) | is.na(`2ormore_mal`), "", paste0(`2ormore_mal`, "/", No.sites.y.y)),
    Gram = ifelse(is.na(No.sites.x.x.x) | is.na(`2ormore_Gram`), "", paste0(`2ormore_Gram`, "/", No.sites.x.x.x)),
    CD4 = ifelse(is.na(No.sites.y.y.y) | is.na(`2ormore_CD4`), "", paste0(`2ormore_CD4`, "/", No.sites.y.y.y))            
    )

# select columns of interest
allEQA_sitesrpt  <-  allEQA_sites %>% 
  select(-starts_with("No."), -starts_with("2ormore"))


# the table
tbl_allEQA_sites   <-  flextable(allEQA_sitesrpt)

tbl_allEQA_sites  <-  tbl_allEQA_sites %>% 
add_header_row(
  values = c(
    hflevel   =  "Health Level",
    "Proportion of sites failing 2 or more EQA cycles", "", "", "", "",""
    ))

#### merge column
tbl_allEQA_sites  <-  tbl_allEQA_sites %>% 
  merge_at(i = 1, j=2:7, part = "header")  

#### merge vertical columns
tbl_allEQA_sites  <-  tbl_allEQA_sites %>% 
  merge_v(j=1, part = "header")


# improve borders for clarity
tbl_allEQA_sites  <-  tbl_allEQA_sites %>% 
  border_remove() %>%  # remove default borders
  hline(part = "all", border = fp_border(color = "gray70", width = 0.5)) # horizontal lines

# add vertical lines
tbl_allEQA_sites  <-  tbl_allEQA_sites %>% 
  vline(j = c(1:7), part = "all", border = fp_border(color = "gray70", width = 0.5)) # vertical lines

#### the title
tbl_allEQA_sites  <-  tbl_allEQA_sites %>% 
  add_header_row(values = "Proportion of sites failing 2 or more EQA cycles",
                 colwidths = ncol(allEQA_sitesrpt))


# format table for better spacing
tbl_allEQA_sites  <-  tbl_allEQA_sites %>% 
  set_table_properties(width = .95, layout = "autofit") %>% 
  align(align = "center", part = "all") %>% 
  bold(part = "header") %>% 
  fontsize(size = 10, part = "all")

tbl_allEQA_sites

#### Report - % sites failing twice or more ####

#### EID ####
#### No. of unique sites failing across the 4 quarters
EIDuniqueF <- eidNoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

#### no failing more than once
EID_failing  <-  eidNoPascommon %>% 
  group_by(n_quarters) %>% 
  
  summarise(
    No_sites  =  n(),
    .groups = "drop"
  ) 
#### re-arrange the data frame
### change data type
EID_failing$n_quarters   <-  as.character(EID_failing$n_quarters)


####4
# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
EID_failing <- eidNoPascommon %>% 
  # make sure n_quarters is character to match all_quarters
  mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
  # ensure all quarters are included, fill missing with 0
  complete(n_quarters = all_quarters, fill = list(No_sites = 0))

### add a reference column
EID_failing  <-  EID_failing %>% 
  mutate(EQA_Scheme  =  "EID")

#### the pivot
EID_failingPvt   <-   EID_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
EIDFail  <-  cbind(EID_failingPvt, EIDuniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")
  
#### VL ####
#### No. of unique sites failing across the 4 quarters
VLuniqueF <- VLNoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

#### no failing more than once
# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
VL_failing  <-  VLNoPascommon %>% 
# make sure n_quarters is character to match all_quarters
mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
# ensure all quarters are included, fill missing with 0
complete(n_quarters = all_quarters, fill = list(No_sites = 0))


### add a reference column
VL_failing  <-  VL_failing %>% 
  mutate(EQA_Scheme  =  "VL")

#### the pivot
VL_failingPvt   <-   VL_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
VLFail  <-  cbind(VL_failingPvt, VLuniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")

#### CrAg ####
CrAguniqueF <- CrAgNoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
Cr_failing <- CrAgNoPascommon %>% 
  # make sure n_quarters is character to match all_quarters
  mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
  # ensure all quarters are included, fill missing with 0
  complete(n_quarters = all_quarters, fill = list(No_sites = 0))

### add a reference column
Cr_failing  <-  Cr_failing %>% 
  mutate(EQA_Scheme  =  "CrAg")

#### the pivot
Cr_failingPvt   <-   Cr_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
CragFail  <-  cbind(Cr_failingPvt, CrAguniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")


#### HBV ####
HBVuniqueF <- HBVNoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
HBV_failing <- HBVNoPascommon %>% 
  # make sure n_quarters is character to match all_quarters
  mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
  # ensure all quarters are included, fill missing with 0
  complete(n_quarters = all_quarters, fill = list(No_sites = 0))

### add a reference column
HBV_failing  <-  HBV_failing %>% 
  mutate(EQA_Scheme  =  "HBV")

#### the pivot
HBV_failingPvt   <-   HBV_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
HBVFail  <-  cbind(HBV_failingPvt, HBVuniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")

#### HPV ####
HPVuniqueF <- HPVNoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
HPV_failing <- HPVNoPascommon %>% 
  # make sure n_quarters is character to match all_quarters
  mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
  # ensure all quarters are included, fill missing with 0
  complete(n_quarters = all_quarters, fill = list(No_sites = 0))

### add a reference column
HPV_failing  <-  HPV_failing %>% 
  mutate(EQA_Scheme  =  "HPV")

#### the pivot
HPV_failingPvt   <-   HPV_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
HPVFail  <-  cbind(HPV_failingPvt, HPVuniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")

#### MALARIA ####
MaluniqueF <- MalNoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
Mal_failing <- MalNoPascommon %>% 
  # make sure n_quarters is character to match all_quarters
  mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
  # ensure all quarters are included, fill missing with 0
  complete(n_quarters = all_quarters, fill = list(No_sites = 0))

### add a reference column
Mal_failing  <-  Mal_failing %>% 
  mutate(EQA_Scheme  =  "Malaria")

#### the pivot
Mal_failingPvt   <-   Mal_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
MalFail  <-  cbind(Mal_failingPvt, MaluniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")

#### Gram ####
GramuniqueF <- GramNoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
Gram_failing <- GramNoPascommon %>% 
  # make sure n_quarters is character to match all_quarters
  mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
  # ensure all quarters are included, fill missing with 0
  complete(n_quarters = all_quarters, fill = list(No_sites = 0))

### add a reference column
Gram_failing  <-  Gram_failing %>% 
  mutate(EQA_Scheme  =  "Gram")

#### the pivot
Gram_failingPvt   <-   Gram_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
GramFail  <-  cbind(Gram_failingPvt, GramuniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")

#### CD4 ####
CD4uniqueF <- CD4NoPas %>%
  ungroup() %>%
  summarise(Unique_Sites = n_distinct(HFacility))

# Define all possible quarters as character
all_quarters <- as.character(2:4)  # adjust if needed

# Calculate number of cycles and ensure all quarters are included
CD4_failing <- CD4NoPascommon %>% 
  # make sure n_quarters is character to match all_quarters
  mutate(n_quarters = as.character(n_quarters)) %>%
  group_by(n_quarters) %>% 
  summarise(
    No_sites = n(),
    .groups = "drop"
  ) %>%
  # ensure all quarters are included, fill missing with 0
  complete(n_quarters = all_quarters, fill = list(No_sites = 0))

### add a reference column
CD4_failing  <-  CD4_failing %>% 
  mutate(EQA_Scheme  =  "CD4")

#### the pivot
CD4_failingPvt   <-   CD4_failing %>% 
  pivot_wider(
    names_from = n_quarters,
    values_from = No_sites,
  )
### add the unique sites
CD4Fail  <-  cbind(CD4_failingPvt, CD4uniqueF) %>% 
  select("EQA_Scheme", "Unique_Sites", "2", "3", "4")

#### Report - all schemes proportions failing by quarter ####
allFail   <- rbind(EIDFail, VLFail, CragFail, HBVFail, HPVFail, MalFail, GramFail,
                   CD4Fail) %>% 
  
                arrange(desc(Unique_Sites))           

#### number of unique health facilities that respnded to EQA
#### EID
eidall  <-  EID_scheData %>% 
  filter(Status == "Approved") %>% 
      summarise(
        Responded  =  n_distinct(HFacility)
      )
#### add the scheme
eidall  <-   eidall %>% 
  mutate(EQA_Scheme = "EID") %>% 
    select(EQA_Scheme, Responded)
  

#### VL
vlall  <-  VL_scheData %>% 
  filter(Status == "Approved") %>% 
  summarise(
    Responded  =  n_distinct(HFacility)
  )
#### add the scheme
vlall  <-   vlall %>% 
  mutate(EQA_Scheme = "VL") %>% 
  select(EQA_Scheme, Responded)


#### CrAg 
CrAgall  <-  CrAg_scheData %>% 
  filter(Status == "Approved") %>% 
  summarise(
    Responded  =  n_distinct(HFacility)
  )
#### add the scheme
CrAgall  <-   CrAgall %>% 
  mutate(EQA_Scheme = "CrAg") %>% 
  select(EQA_Scheme, Responded)

#### HBV
HBVall  <-  HBV_scheData %>% 
  filter(Status == "Approved") %>% 
  summarise(
    Responded  =  n_distinct(HFacility)
  )
#### add the scheme
HBVall  <-   HBVall %>% 
  mutate(EQA_Scheme = "HBV") %>% 
  select(EQA_Scheme, Responded)

#### HPV
HPVall  <-  HPV_scheData %>% 
  filter(Status == "Approved") %>% 
  summarise(
    Responded  =  n_distinct(HFacility)
  )
#### add the scheme
HPVall  <-   HPVall %>% 
  mutate(EQA_Scheme = "HPV") %>% 
  select(EQA_Scheme, Responded)

#### Malaria  
Malall  <-  Mal_scheData %>% 
  filter(Status == "Approved") %>% 
  summarise(
    Responded  =  n_distinct(HFacility)
  )
#### add the scheme
Malall  <-   Malall %>% 
  mutate(EQA_Scheme = "Malaria") %>% 
  select(EQA_Scheme, Responded)


#### Gram
Gramall  <-  Gram_scheData %>% 
  filter(Status == "Approved") %>% 
  summarise(
    Responded  =  n_distinct(HFacility)
  )
#### add the scheme
Gramall  <-   Gramall %>% 
  mutate(EQA_Scheme = "Gram") %>% 
  select(EQA_Scheme, Responded)

####  CD4
CD4all  <-  CD4_scheData %>% 
  filter(Status == "Approved") %>% 
  summarise(
    Responded  =  n_distinct(HFacility)
  )
#### add the scheme
CD4all  <-   CD4all %>% 
  mutate(EQA_Scheme = "CD4") %>% 
  select(EQA_Scheme, Responded)

#### combine rows
uniqueSites   <-  rbind(eidall, vlall, CrAgall, HBVall, HPVall, Malall,
                        Gramall, CD4all)


#### combine with table on sites with frequent fails
allFailN   <-  allFail %>% 
  left_join(uniqueSites, by = "EQA_Scheme") %>% 
    select("EQA_Scheme", "Responded", "Unique_Sites", "2", "3", "4"           
           ) %>% 
  
    mutate(
      `%everfailed`   =   round((Unique_Sites/Responded)*100,1) ,
      `%failed2/4cycles`  =  round((`2`/Unique_Sites)*100,1),
      `%failed3/4cycles`  =  round((`3`/Unique_Sites)*100,1),
      `%failed4/4cycles`  =  round((`4`/Unique_Sites)*100,1)
    )

#### the table
tbl_allFailN   <-  flextable(allFailN)
tbl_allFailN
#################################################################
##################################################################
#### Enrolled onto EQA Schemes ####
#### EID ####
EIDEnrolled  <-  EID_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    EID    =   n_distinct(HFacility)
  ) %>% 
#### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = EID
  ) %>% 
  mutate(
    `EQA Scheme`  =  "EID"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)



#### VL ####
VLEnrolled  <-  VL_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    VL    =   n_distinct(HFacility)
  )  %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = VL
  ) %>% 
  mutate(
    `EQA Scheme`  =  "VL"
  ) %>% 
  select(`EQA Scheme`,Q4, Q1, Q2, Q3)
#### CrAg  ####
CrAg_scheData   <-  CrAg_scheData %>% 
  select(RRH, District, HFacility, Status, Qtr)
### the analysis
CrAgEnrolled  <-  CrAg_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    CrAg    =   n_distinct(HFacility)
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = CrAg
  ) %>% 
  mutate(
    `EQA Scheme`  =  "CrAg"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)

#### HBV ####
HBVEnrolled  <-  HBV_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    HBV   =   n_distinct(HFacility)
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = HBV
  ) %>% 
  mutate(
    `EQA Scheme`  =  "HBV"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)

#### HPV ####
HPVEnrolled  <-  HPV_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    HPV    =   n_distinct(HFacility)
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = HPV
  ) %>% 
  mutate(
    `EQA Scheme`  =  "HPV"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)

#### Malaria ####
MalEnrolled  <-  Mal_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    Malaria    =   n_distinct(HFacility)
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = Malaria
  ) %>% 
  mutate(
    `EQA Scheme`  =  "Malaria"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)
#### Gram ####
Gram_scheData  <-   Gram_scheData %>% 
  select(RRH, District, HFacility, Status, Qtr)

#### the analysis
GramEnrolled  <-  Gram_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    Gram    =   n_distinct(HFacility)
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = Gram
  ) %>% 
  mutate(
    `EQA Scheme`  =  "Gram"
  ) %>% 
  select(`EQA Scheme`,Q4,Q1, Q2, Q3)

#### CD4 ####
CD4Enrolled  <-  CD4_scheData %>%
  group_by(Qtr) %>% 
  summarise(
    CD4    =   n_distinct(HFacility)
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = CD4
  ) %>% 
  mutate(
    `EQA Scheme`  =  "CD4"
  ) %>% 
  select(`EQA Scheme`,Q1, Q2, Q3)

#### Report - All EQA Enrollment ####
all_Enrolled  <-  bind_rows(EIDEnrolled, CrAgEnrolled,HBVEnrolled,VLEnrolled,
                           HPVEnrolled,MalEnrolled,GramEnrolled,CD4Enrolled)


#### The table
tbl_all_Enrolled   <-  flextable(all_Enrolled)

### format the table
tbl_all_Enrolled  <-  tbl_all_Enrolled %>% 
  add_header_row(values = c(
    `EQA Scheme`  =  "EQA Scheme",
    "No. enrolled into the respective EQA schemes", "", "",""
  ))

## set the labels
tbl_all_Enrolled  <-  tbl_all_Enrolled %>%
  set_header_labels(
    `EQA Scheme`  =  "EQA Scheme",
    Q4  =   "Oct-Dec",
    Q1    =    "Jan-Mar",
    Q2    =   "Apri-Jun",
    Q3    =   "Jul-Sept"
  )



tbl_all_Enrolled  <-  tbl_all_Enrolled %>%
  # Merging columns 
  merge_at(i = 1, j = 2:5, part = "header")

# Merging vertically for the first five columns in the header
tbl_all_Enrolled  <-  tbl_all_Enrolled %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_all_Enrolled  <-  tbl_all_Enrolled %>%
  vline(j = c(1, 2, 3, 4, 5), part = "all")


tbl_all_Enrolled  <-  tbl_all_Enrolled %>%
  add_header_row(values="No. enrolled into different EQA Schemes", 
                 colwidths = ncol(all_Enrolled))

tbl_all_Enrolled

#### Report - unique no. of health facilities enrolled in EQA schmes ####
#### combine the data sets
unihf  <-  rbind(EID_scheData, VL_scheData, CrAg_scheData, HBV_scheData, 
                 HPV_scheData, Mal_scheData, Gram_scheData, CD4_scheData
                 )
#### list of unique sites
unihfunique <- unihf %>% 
  group_by(RRH) %>% 
  summarise(
    No.facilities  =  n_distinct(HFacility)
  )

#### analyze the data to find unique health facilities
unihfdf  <-  unihf %>% 
  group_by(Qtr) %>% 
  summarise(
    No.facilities  =  n_distinct(HFacility)
  )

#### the table
tbl_unihfdf   <-  flextable(unihfdf)

tbl_unihfdf   <-  tbl_unihfdf %>% 
  add_header_row(values = "No. of unique health facilities enrolled on EQA schemes",
                 colwidths = ncol(unihfdf))
tbl_unihfdf
##################################################################
#### Report - Enrolled by RRH ####
#### EID ####
EIDRRH_enroll   <- EID_scheData %>%
  group_by(RRH, Qtr) %>% 
  summarise(
    EID    =   n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = EID
  )%>% 
  select(RRH, Q4, Q1, Q2)
#### VL ####
VLRRH_Enroll  <-  VL_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    VL    =   n_distinct(HFacility),
    .groups = "drop"
  )  %>% 
    pivot_wider(
    names_from = Qtr,
    values_from = VL
  ) %>% 
  select(RRH, Q4, Q1, Q2)


#### CrAg  ####
RRHCrAg_Enroll  <-  CrAg_scheData %>%
  group_by(RRH, Qtr) %>% 
  summarise(
    CrAg    =   n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = CrAg
  ) %>% 
  select(RRH, Q4, Q1, Q2)

#### HBV ####
RRHHBV_Enroll  <-  HBV_scheData %>%
  group_by(RRH, Qtr) %>% 
  summarise(
    HBV   =   n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = HBV
  )%>% 
  select(RRH, Q4, Q1, Q2)

#### HPV ####
RRHHPV_Enroll  <-  HPV_scheData %>%
  group_by(RRH, Qtr) %>% 
  summarise(
    HPV    =   n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = HPV
  ) %>% 
  select(RRH, Q4, Q1, Q2)

#### Malaria ####
RRHMal_Enroll  <-  Mal_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH, Qtr) %>% 
  summarise(
    Malaria    =   n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = Malaria
  ) %>% 
  select(RRH, Q4, Q1, Q2)

#### Gram ####
RRHGram_Enroll  <-  Gram_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    Gram    =   n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = Gram
  ) %>% 
  select(RRH, Q4, Q1, Q2)

#### CD4 ####
CD4Enrolled  <-  CD4_scheData %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH, Qtr) %>% 
  summarise(
    CD4    =   n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  #### Creating the data frame to compile list
  pivot_wider(
    names_from = Qtr,
    values_from = CD4
  )

#### Report - Enrolled by RRH and EQA Scheme ####
RRHall_enroll  <- EIDRRH_enroll %>% 
  left_join(VLRRH_Enroll, by = "RRH") %>% 
  left_join(RRHCrAg_Enroll, by = "RRH") %>%
  left_join(RRHHBV_Enroll, by = "RRH") %>%
  left_join(RRHHPV_Enroll, by = "RRH") %>%
  left_join(RRHMal_Enroll, by = "RRH") %>%
  left_join(RRHGram_Enroll, by = "RRH") %>%
  left_join(CD4Enrolled, by = "RRH") 

#### the table
tbl_RRHall_enroll  <-  flextable(RRHall_enroll)

### format the table
tbl_RRHall_enroll   <-   tbl_RRHall_enroll %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "EID", "","",
    "VL", "","",
    "CrAg", "","",
    "HBV", "","",
    "HPV", "","",
    "Mal", "","",
    "Gram", "","",
    "CD4",""
      ))

## set the labels
tbl_RRHall_enroll   <-   tbl_RRHall_enroll %>% 
  set_header_labels(
    RRH  =  "RRH",
    Q4.x    =   "Oct-Dec",
    Q1.x    =    "Jan-Mar",
    Q2.x    =    "Apri-Jun",
    
    Q4.y    =    "Oct-Dec",
    Q1.y    =   "Jan-Mar",
    Q2.y    =   "Apri-Jun",
    
    Q4.x.x    =    "Oct-Dec",
    Q1.x.x    =   "Jan-Mar",
    Q2.x.x    =   "Apri-Jun",
    
    Q4.y.y    =    "Oct-Dec",
    Q1.y.y    =   "Jan-Mar",
    Q2.y.y     =   "Apri-Jun",
    
    Q4.x.x.x    =    "Oct-Dec",
    Q1.x.x.x    =   "Jan-Mar",
    Q2.x.x.x    =   "Apri-Jun",
    
    Q4.y.y.y    =    "Oct-Dec",
    Q1.y.y.y    =   "Jan-Mar",
    Q2.y.y.y   =   "Apri-Jun",
    
    Q4    =    "Oct-Dec",
    Q1.x.x.x.x    =    "Jan-Mar",
    Q2.x.x.x.x    =    "Apri-Jun",
    
    Q1.y.y.y.y  =   "Jan-Mar",
    Q2.y.y.y.y   =    "Apri-Jun"
    
  )


tbl_RRHall_enroll   <-   tbl_RRHall_enroll %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:4, part = "header") %>% 
  merge_at(i = 1, j = 5:7, part = "header") %>% 
  merge_at(i = 1, j = 8:10, part = "header") %>% 
  merge_at(i = 1, j = 11:13, part = "header") %>%
  merge_at(i = 1, j = 14:16, part = "header") %>%
  merge_at(i = 1, j = 17:19, part = "header") %>%
  merge_at(i = 1, j = 20:22, part = "header") %>%
  merge_at(i = 1, j = 23:24, part = "header")
  
# Merging vertically for the first five columns in the header
tbl_RRHall_enroll   <-   tbl_RRHall_enroll %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_RRHall_enroll   <-   tbl_RRHall_enroll %>% 
  vline(j = c(1, 4, 7, 10, 13, 16, 19, 22, 24), part = "all")


tbl_RRHall_enroll   <-   tbl_RRHall_enroll %>% 
  add_header_row(values="No. sites enrolled on the respective EQA Schemes, by Quarter", 
                 colwidths = ncol(RRHall_enroll))

tbl_RRHall_enroll

##################################################
##################################################
#### The report ####
#### create a document
EQARpt <- read_docx()

### 
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_overallReponseEQAdf)

#### 
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_EQARate)

#### 
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_RRHall_enroll)

####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_RRHall_EQAResponse)

####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_all_EQAPass)


####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_overallReponseEQAdf)

####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_overallEQAdf)

####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_RRHall_EQAPass)


EQARpt <- EQARpt %>%
  body_add_flextable(tbl_RRHall_EQAPass1)


EQARpt <- EQARpt %>%
  body_add_flextable(tbl_all_Enrolled)


####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_RRHall_enroll)


####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_unihfdf)

####
EQARpt <- EQARpt %>%
  body_add_flextable(tbl_allFailN)



# Save the document
print(EQARpt, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q4/Data/QA/EQARptQ4.docx")

tbl_RRHall_enroll

#### Annex ####
#### create a document
EQAAnnex <- read_docx()

#### Proportion of sites failing >= 2 cycles
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_allEQA_rpt1)

#### No. of sites failing >= 2 cycles
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_allEQA_sites)


#### %age ever failed
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_allFailN)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_eidNoRescommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_VLNoRescommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_vlNoRescommon)

####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_CrAgNoRescommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_RRHall_EQAResponse)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_HBVNoRescommon)



####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_HPVNoRescommon)

####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_MalNoRescommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_GramNoRescommon)


####

EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_eidNoPascommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_VLNoPascommon)

####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_CrAgNoPascommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_HBVNoPascommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_HPVNoPascommon)

####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_MalNoPascommon)


####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_CD4NoPascommon)





####
EQAAnnex <- EQAAnnex %>%
  body_add_flextable(tbl_GramNoPascommon)

# Save the document
print(EQAAnnex, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/EQARptQ3Annex2.docx")

##### Annex 2 ####
EQAAnnex1 <- read_docx()

####
EQAAnnex1 <- EQAAnnex1 %>%
  body_add_flextable(tbl_GramNoPascommon)

####
EQAAnnex1 <- EQAAnnex1 %>%
  body_add_flextable(tbl_HPVNoRescommon)

####
EQAAnnex1 <- EQAAnnex1 %>%
  body_add_flextable(tbl_HPVNoRescommon)

####
EQAAnnex1 <- EQAAnnex1 %>%
  body_add_flextable(tbl_MalNoRescommon)

####
EQAAnnex1 <- EQAAnnex1 %>%
  body_add_flextable(tbl_HBVNoRescommon)

####
EQAAnnex1 <- EQAAnnex1 %>%
  body_add_flextable(tbl_CD4NoRescommon)


# Save the document
print(EQAAnnex1, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q3/Data/QA/EQARptQ3annexb.docx")

#### add ons/corrections ####
add1 <- read_docx()

####
add1 <- add1 %>%
  body_add_flextable(tbl_all_EQAPass)

####
add1 <- add1 %>%
  body_add_flextable(tbl_EQARate)


####
add1 <- add1 %>%
  body_add_flextable(tbl_allFailN)

####
add1 <- add1 %>%
  body_add_flextable(tbl_eidsitesrpt)


# Save the document
print(add1, target = "D:CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Support Supervision/EQAadded1.docx")

##################################################

