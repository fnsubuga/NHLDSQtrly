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
#### Creating POC sites data set ####
# Creating the HFacility vector
HFacility <- c(
  "Aber Hospital","Abim Hospital","Aboke HC IV","Achol Pii Millitary HC IV","Adjumani Hospital","Alebtong HC IV",
  "Amai Community Hospital","Amuria Hospital","Anaka Hospital","Angal Hospital","Anyeke HC IV","Arua Regional Referral Hospital",
  "Atiak HC IV","Bidi Bidi HC III","Biiso HC IV","Bombo General Military Hospital","Bubukwanga HC III","Bubulo HC IV",
  "Budadiri HC IV","Budaka HC IV","Bududa Hospital","Bugamba HC IV","Bugiri Hospital","Buhanika HC III","Bukedea HC IV",
  "Bukomero HC IV","Bukulula HC IV","Bukwo HC IV","Buluba Hospital","Bundibugyo Hospital","Busesa HC IV","Bushenyi HC IV",
  "Busia HC IV","Busiu HC IV","Busolwe Hospital","Butebo HC IV","Butemba HC IV","Butenga HC IV","Buvuma HC IV","Buyinja HC IV",
  "Bwera Hospital","Bwindi Community Hospital","China Uganda Friendship (Naguru) Regional Referral Hospital","CPHL",
  "Entebbe Regional Referral Hospital","Franciscan HC KAKOOGE","Garilaya HC III","Gombe Hospital","Hamurwa HC IV",
  "Hoima Regional Referral Hospital","Ibuje HC III","Iganga Hospital","Ishaka Adventist Hospital","Ishongororo HC IV",
  "Itojo Hospital","Jinja Regional Referral Hospital","Kabale Regional Referral Hospital","Kaberamaido Hospital","Kabuyanda HC IV",
  "Kagadi Hospital","Kagando Hospital","Kajjansi HC IV","Kakindo HC IV","Kakumiro HC IV","Kakuuto HC IV","Kalangala HC IV",
  "Kambuga Hospital","Kamuganguzi HC III","Kangulumira HC IV","Kapchorwa Hospital","Kapelebyong HC IV","Karugutu HC IV",
  "Kasambya HC III","Kasawo HC III","Kasensero HC II","Kasese Minicipal Council","Kassanda HC IV","Katakwi Hospital",
  "Kataraka HC IV","Katooke HC III","Kawaala HC III","Kawolo Hospital","Kayunga Regional Referral Hospital","Kebisoni HC IV",
  "Kibaale HC IV","Kibiito HC IV","Kiboga Hospital","Kibuku HC IV","Kichinjaji HC III","Kidera HC IV","Kiganda HC IV",
  "Kigandalo HC IV","Kigorobya HC IV","Kihiihi HC IV","Kilembe Mines Hospital","Kimwanyi HC III","Kinoni HC IV","Kiruhura HC IV",
  "Kisenyi HC IV","Kisiizi Hospital","Kisoro Hospital","Kiswa HC III","Kitagata Hospital","Kitgum Hospital","Kitwe HC IV",
  "Kityerera HC IV","Kiwoko Hospital","Koboko Hospital","Kotido Hospital","Kuluva Hospital","Kumi HC IV","Kyangwali HC IV",
  "Kyarusozi HC IV","Kyegegwa Hospital","Kyenjojo Hospital","Lalogi HC IV","Lira Regional Referral Hospital","Lolwe HC III",
  "Lubaga Hospital","Lwengo HC IV","Maddu HC IV","Madi Opei HC IV","Madudu HC III","Malaba HC IV","Maracha HC IV","Masafu Hospital",
  "Masaka Police HC III","Masaka Regional Referral Hospital","Mayuge HC IV","Mbale Regional Referral Hospital","Mbarara Municipal Council HC IV",
  "Mbarara Regional Referral Hospital","Mengo Hospital","Midigo HC IV","Mitooma HC IV","Mityana Hospital","Moyo Hospital",
  "Mparo HC IV","Mpigi HC IV","Mubende Regional Referral Hospital","Mukono Hospital","Mukuju HC IV","Mulago Nrh - Pidc Coe Baylor Clinic",
  "Mulanda HC IV","Mungula HC IV","Muyembe HC IV","Nabiganda HC IV","Nagongera HC IV","Nakaseke Hospital","Nakasongola HC IV",
  "Nakawuka HC III","Namatala HC IV","Namayumba HC IV","Namokora HC IV","Namutumba HC III","Nankoma HC IV","Nebbi Hospital",
  "Ngoma HC IV","Nkozi Hospital","Nsiika HC IV","Ntara HC IV","Ntungamo HC IV","Ntuusi HC IV","Ntwetwe HC IV","Nyahuka HC IV",
  "Nyantabooma HC III","Nyapea Hospital","Obongi HC IV","Omugo HC IV","Padibe HC IV","Pajule HC IV","Pakwach HC IV","Pallisa Hospital",
  "Panyadooli HC IV","Patongo HC III","Rakai Hospital","Rhino Camp HC IV","Rubaare HC IV","Rubongi Military Hospital","Rugazi HC IV",
  "Ruhoko HC IV","Rukunyu Hospital","Rushere Community Hospital","Rushoka HC IV","Rwekubo HC IV","Serere HC IV",
  "Soroti Regional Referral Hospital","Ssekanyonyi HC IV","Ssembabule HC IV","St Benedict Health Center","St. Anthony Hospital",
  "St. Francis Naggalama Hospital","St. Francis Nyenga Hospital","St. Joseph Maracha Hospital","St. Joseph's Hospital Kitgum",
  "St. Kalori Lwanga Hospital Nyakibale","St. Kizito Matany Hospital","St. Mary's Lacor Hospital","Tegot HCII","Tororo General Hospital",
  "Villa Maria Hospital","Virika Hospital","Wakiso HC IV","Walukuba HC IV","Warr HC IV","Yinga HC III","Yumbe Regional Referral Hospital",
  "Bigasa HC III","KIU Teaching Hospital","Awach HC IV","TASO Gulu","Bugono HC IV","CDC-Kiruddu","Kawempe National Referral Hospital",
  "Kiruddu National Refferal Hospital","Kisugu HC III","Mulago National Hospital-MUJHU Clinic","Kamuli District Govt Hospital",
  "Rwamwanja HC III","Kikuube HC IV","Aduku HC IV","Kalisizo Hospital","Ober HC III","Kyazanga HC IV","Kitovu Hospital",
  "Bwizibwera HC IV","Kojja HC IV","Hukeseho HC III","St Elizabeth HC IV-Magale","Ngora HC IV","Otwal HC III","Kisubi Hospital",
  "Luwunga HC III(1st Div Hospital)","NAKASONGOLA MILITARY HOSPITAL","DR.CHARLES FURTHING CLINIC","CPHL"
)

# Creating the District vector
District <- c(
  "Oyam","Abim","Kole","Pader","Adjumani","Alebtong","Amolatar","Amuria","Nwoya","Nebbi","Oyam","Arua","Amuru","Yumbe","Buliisa",
  "Luwero","Bundibugyo","Manafwa","Sironko","Budaka","Bududa","Mbarara","Bugiri","Hoima","Bukedea","Kiboga","Kalungu","Bukwo",
  "Mayuge","Bundibugyo","Iganga","Bushenyi","Busia","Mbale","Butaleja","Butebo","Kyankwanzi","Bukomansimbi","Buvuma","Namayingo",
  "Kasese","Kanungu","Kampala","Kampala","Wakiso","Nakasongola","Kayunga","Butambala","Rubanda","Hoima","Apac","Iganga",
  "Bushenyi","Ibanda","Ntungamo","Jinja","Kabale","Kaberamaido","Isingiro","Kagadi","Kasese","Wakiso","Kakumiro","Kakumiro",
  "Kyotera","Kalangala","Kanungu","Kabale","Kayunga","Kapchorwa","Amuria","Ntoroko","Mubende","Mukono","Kyotera","Kasese",
  "Kassanda","Katakwi","Kabarole","Kyenjojo","Kampala","Buikwe","Kayunga","Rukungiri","Kibaale","Bunyangabu","Kiboga","Kibuku",
  "Soroti","Buyende","Kassanda","Mayuge","Hoima","Kanungu","Kasese","Lwengo","Mbarara","Kiruhura","Kampala","Rukungiri","Kisoro",
  "Kampala","Sheema","Kitgum","Ntungamo","Mayuge","Nakaseke","Koboko","Kotido","Arua","Kumi","Kikuube","Kyenjojo","Kyegegwa",
  "Kyenjojo","Omoro","Lira","Namayingo","Kampala","Lwengo","Gomba","Lamwo","Mubende","Tororo","Maracha","Busia","Masaka",
  "Masaka","Mayuge","Mbale","Mbarara","Mbarara","Kampala","Yumbe","Mitooma","Mityana","Moyo","Rukiga","Mpigi","Mubende",
  "Mukono","Tororo","Kampala","Tororo","Adjumani","Bulambuli","Butaleja","Tororo","Nakaseke","Nakasongola","Wakiso","Mbale",
  "Wakiso","Kitgum","Namutumba","Bugiri","Nebbi","Nakaseke","Mpigi","Buhweju","Kamwenge","Ntungamo","Sembabule","Kyankwanzi",
  "Bundibugyo","Kabarole","Zombo","Obongi","Arua","Lamwo","Pader","Pakwach","Pallisa","Kiryandongo","Agago","Rakai","Arua",
  "Ntungamo","Tororo","Rubirizi","Ibanda","Kamwenge","Kiruhura","Ntungamo","Isingiro","Serere","Soroti","Mityana","Sembabule",
  "Jinja","Tororo","Mukono","Buikwe","Maracha","Kitgum","Rukungiri","Napak","Gulu","Mbarara","Tororo","Kalungu","Kabarole",
  "Wakiso","Jinja","Zombo","Arua","Yumbe","Bukomansimbi","Bushenyi","Gulu","Gulu","Iganga","Kampala","Kampala","Kampala",
  "Kampala","Kampala","Kamuli","Kamwenge","Kikuube","Kwania","Kyotera","Lira","Lwengo","Masaka","Mbarara","Mukono","Namayingo",
  "Namisindwa","Ngora","Oyam","Wakiso","Wakiso","NAKASONGOLA","KAMPALA","KAMPALA"
)
### creating the data frame
POCSites  <- data.frame(
  HFacility,
  District
)
## capitalizing
POCSites$HFacility   <-  toupper(POCSites$HFacility)
POCSites$District    <-  toupper(POCSites$District)

#######################################################################
### Importing data sets ####
EIDALIS       <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/EID/EIDALIS_Oct-Mar.xlsx")
VLALIS        <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/VL/VLALISPOCQ1_Q2.xlsx")
EIDGxP        <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/EID/GxPEIDQ1_Q2.xlsx")
VLGxP         <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/VL/GxPVLQ1_Q2.xlsx")
TBGxP         <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/multi testing/TBQ1Q2.xlsx")
HPVGxP        <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/multi testing/HPVQ1Q2.xlsx")
EIDPima       <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/EID/mPimaEIDQ1_Q2.xlsx")
VLPima        <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/VL/mPimaVLQ1_Q2.xlsx")
EIDCon        <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/EID/EIDOct24-Mar25.xlsx")
GxPError      <- import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/multi testing/Error rates/allErrorsOct-Mar.xlsx")

#### consumption #### 
consumption    <-  import("D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/CQI and research/POC/Data sets/commodities/POC consumption data.xlsx")
consumption$Yr   <-  as.character(consumption$Yr)

########################################################################
########################################################################
### Cleaning EID conventional data set####
EIDCon$District <- trimws(EIDCon$District)    # Trim the district names

EIDCon <- EIDCon %>%                          # Working data set
  filter(!is.na(Result)) %>% 
  select(No,"Facility Name",Level,District,Result,`Age in Months`,`PCR 1st/2nd`,`Date Collected`,`Date Received`,`Date Tested`)

EIDCon <- EIDCon %>%                           # rename to join
  rename(
    "HFacility" =   "Facility Name",
    "Age"       =    "Age in Months",
    "PCR"    =     "PCR 1st/2nd",
    "CDate"      =   "Date Collected",
    "RDate"     =  "Date Received",
    "TDate"     =    "Date Tested")
EIDCon <- EIDCon %>%  
  mutate(District    = recode(District,
                              "Luweero"           = "Luwero",
                              "Maracha Terego"    = "Maracha",
                              "Ssembabule"        = "Sembabule"))
# CAPITALIZE
EIDCon$District  <- toupper(EIDCon$District) 
EIDCon$HFacility <- toupper(EIDCon$HFacility)

# Adding RRH Column
EIDCon <- EIDCon %>% 
  left_join(Regions, by = "District")
# Add the IP
EIDCon <- EIDCon %>% 
  left_join(IP, by = "District")

# clean out the extra RRH and District columns
EIDCon <- EIDCon %>% 
  select(HFacility,Level,District,Result,Age,PCR,CDate,RDate,TDate,HRegion,RRH.x,    
         IM) %>% 
  rename("RRH"  =  "RRH.x")

#### Add year and month
EIDCon <- EIDCon %>%
  mutate(
    Yr  =  year(CDate),
    Month =  month(CDate)
  ) 
# Add quarter, "Qtr"
EIDCon <- EIDCon %>%
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))

####  Number of tests done by region 
RRH_EIDCon <- EIDCon %>% 
  filter(!is.na(TDate)) %>% 
  group_by(RRH,Qtr,IM) %>% 
  summarise(Con_Tests = n()) %>% 
    ungroup()

### Totals done
TEIDCon <- RRH_EIDCon %>% 
  summarise(
    RRH    =  "RRH",
    Qtr    =  "Qtr",
    IM     =  "IM",
    Con_Tests  =  sum(Con_Tests)
  )

## Add rows
RRH_EIDCon <- rbind(RRH_EIDCon,TEIDCon)

#############################################################################
#############################################################################
#### Cleaning ALIS POC EID and VL Data sets ####
#### EID ALIS (EID_Data) #####
##EID
## Renaming columns
EIDALIS <- EIDALIS %>%
  rename(
    # New                         #old
    "HFacility"         =       "facility",
    "SFacility"         =       "facility_name",
    "C_No"              =       "sample_id",       
    "CollectionDate"    =       "date_of_sample_collection",
    "TestDate"          =       "date_dbs_tested",
    "pcr"               =       "eid_pcr",
    "Result"            =       "eid_result",
    "Sample_DispatchDate"    =   "date_dispatched_from_facility",
    "DownloadDate"           =   "date_downloaded",  # Date printed,
    "Age"        =    "age_in_months"
        )

# Capitalize HFacility and district
EIDALIS$SFacility <- toupper(EIDALIS$SFacility)
EIDALIS$HFacility<- toupper(EIDALIS$HFacility)

##### cleaning the EID ALIS facility names ####
# triming 
EIDALIS$HFacility <- trimws(EIDALIS$HFacility)

# cleaning facility column
EIDALIS$HFacility <- gsub("H/C","HC", EIDALIS$HFacility)
EIDALIS$HFacility  <-  gsub("R R HOSPITAL","REGIONAL REFERRAL HOSPITAL",EIDALIS$HFacility)

# cleaning sending facility column
EIDALIS$SFacility <- gsub("H/C","HC", EIDALIS$SFacility)
EIDALIS$SFacility <- gsub("R R HOSPITAL","REGIONAL REFERRAL HOSPITAL", EIDALIS$SFacility)

# cleaning the names
EIDALIS <- EIDALIS %>% 
  mutate(HFacility   = recode(HFacility,
                              #   Old                         # New
                              "KALAGALA HC IV"  = "KALAGALA HC IV",
                              "KAMULI DISTRICT GOVT HOSPITAL"  = "KAMULI HOSPITAL",
                              "KASESE MUNICIPAL COUNCIL HC III (KMC)"  = "KASESE MUNICIPAL COUNCIL HC III",
                              "KAWAALA HC III"  = "KAWAALA HEALTH CENTRE HC IV",
                              "KAYUNGA HOSPITAL"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                              "KICHINJAJI  HC  III"  = "KICHINJAJI HC III",
                              "KIHIIHI HC IV"  = "KIHIHI HC IV",
                              "KITWE HC IV (NTUNGAMO)"  = "KITWE HC IV",
                              "KOBOKO GENERAL HOSPITAL"  = "KOBOKO HOSPITAL",
                              "KOOME HC III"  = "KOOME HC III",
                              "KOTIDO HC IV"  = "KOTIDO HOSPITAL",
                              "LACOR HOSPITAL"  = "ST. MARY'S HOSPITAL LACOR",
                              "LALOGI  HC IV"  = "LALOGI HC IV",
                              "LUWEERO GENERAL HOSPITAL"  = "LUWERO HOSPITAL",
                              "MADI OPEI HC IV"  = "MADI-OPEI HC IV",
                              "MALABA HC III (TORORO)"  = "MALABA HC III",
                              "MAYUGE HC III (MAYUGE)"  = "MAYUGE HC IV",
                              "MT ST.MARYS HOSPITAL"  = "MT. ST. MARY'S HOSPITAL-DOK",
                              "MUYEMBE  HC IV"  = "MUYEMBE HC IV",
                              "NAGGALAMA HOSPITAL"  = "ST. FRANCIS NAGGALAMA HOSPITAL",
                              "NAKASEKE  HOSPITAL"  = "NAKASEKE HOSPITAL",
                              "NAMUTUMBA HC  III (NAMUTUMBA)"  = "NAMUTUMBA HC III",
                              "NYAKWAE HC III"  = "NYAKWAE HC III",
                              "PAJULE HC  IV"  = "PAJULE HC IV",
                              "PALLISA GENERAL HOSPITAL"  = "PALLISA HOSPITAL",
                              "PANYADOLI HC III"  = "PANYADOLI HC IV",
                              "PNC (BAYLOR-MULAGO)"  = "MULAGO NRH - PIDC COE BAYLOR CLINIC",
                              "RUKUNYU GENERAL HOSPITAL"  = "RUKUNYU HOSPITAL",
                              "ST ELIZABETH HC IV-MAGALE"  = "MAGALE (UCMB) HC IV",
                              "ST JOSEPH HOSPITAL  KITGUM"  = "ST. JOSEPH'S KITGUM HOSPITAL",
                              "ST JOSEPH HOSPITAL (MARACHA)"  = "ST. JOSEPHS MARACHA HOSPITAL",
                              "STD/MARPI CLINIC-MULAGO"  = "MULAGO NRH - MARPI STI PROJECT CLINIC",
                              "TASO GULU"  = "TASO GULU  SPECIAL CLINIC",
                              "YUMBE HOSPITAL"  = "YUMBE REGIONAL REFERRAL HOSPITAL",
                              "FORTPORTAL REGIONAL REFERRAL HOSPITAL"   =   "FORT PORTAL REGIONAL REFERRAL HOSPITAL",
                              "KIRUDDU NATIONAL REFFERAL HOSPITAL"      =   "KIRUDDU NATIONAL REFERRAL HOSPITAL",
                              "2ND DIV MILITARY HOSPITAL(MAKENKE)"      =   "UPDF 2ND DIV. HC IV",
                              "ANGAL HOSPITAL (ST LUKE)"                =    "ANGAL HOSPITAL",
                              "BOMBO MILITARY HOSPITAL"                 =    "BOMBO GENERAL MILITARY HOSPITAL",
                              "DR AMBROSOLIC MEMORIAL HOSPITAL"         =     "KALONGO AMBROSOLI MEMORIAL HOSPITAL",
                              "ENTEBBE HOSPITAL MCH GENERAL"            =      "ENTEBBE REGIONAL REFERRAL HOSPITAL",
                              "GALIRAAYA HC III"                        =      "GALIRAYA HC III",
                              "HOLY FAMILY HOSPITAL-NYAPEA"             =      "NYAPEA HOSPITAL",
                              "ISS CLINIC (MBARARA REGIONAL REFERRAL HOSPITAL)"   =   "MBARARA REGIONAL REFERRAL HOSPITAL",
                              "KAGADI  HOSPITAL"                             =   "KAGADI HOSPITAL",
                              "KAJJANSI HC III"                             =   "KAJJANSI HC IV",
                              "LUWUNGA HC III(1ST DIV HOSPITAL)"                            =   "LUWUNGA BARRACKS HC III",
                              "RUBAARE HC IV"                 =  "RUBARE HC IV",
                              "RUSHOOKA HC II"                =   "RUSHOKA HC IV",
                              "BULUBA HOSPITAL ST FRANCIS"    =   "BULUBA HOSPITAL",
                              "GULU MILITARY HOSPITAL (4TH DIVISION)"   =   "GULU MILITARY HOSPITAL",
                              "NYANTABOOMA HC III"                      =   "NYANTABOMA HC III",
                              "KASAMBYA HC IV (MUBENDE)"   =  "MUBENDE KASAMBYA HC III GOVT",
                              "KYEGEGWA HC IV"     =   "KYEGEGWA HOSPITAL",
                              "KINONI HC IV (MBARARA)"   =   "KINONI HC IV",
                              "WARR HC III"     =   "WARR HC IV",
                              "NYENGA HOSPITAL"   =   "ST. FRANCIS NYENGA HOSPITAL",
                              "BUTEMBA HC III(KYANKWANZI)" =   "BUTEMBA HC III",
                              "BUKUYA HC III"    =   "BUKUYA HC IV",
                              "NYAKIBALE HOSPITAL"   =  "KAROLI LWANGA (NYAKIBALE) HOSPITAL",
                              "FRANCISCAN HC KAKOOGE"   =  "ST. FRANCISCAN HC IV",
                              "KISENYI HC IV (KAMPALA)"   =  "KISENYI HC IV",
                              "KYATEREKERA HC III (KAGADI)"   =   "KYATEREKERA HC III",
                              "AZUR CHRISTIAN HC IV"    =    "AZUR HC IV",
                              "KABERAMAIDO GENERAL HOSPITAL"  =  "KABERAMAIDO HOSPITAL",
                              "AMURIA GENERAL HOSPITAL"     =   "AMURIA HOSPITAL",
                              "MASAKA POLICE CLINIC"    =   "MASAKA POLICE HC III",
                              "BIISO HC III"      =   "BIISO HC IV"
                                              ))

#### sending sites
# capitalize the names
EIDALIS$SFacility    <-  toupper(EIDALIS$SFacility)

# clean the names
EIDALIS <- EIDALIS %>% 
  mutate(SFacility   = recode(SFacility,
                              
                              "AMURIA GENERAL HOSPITAL"    =     "AMURIA HOSPITAL",
                              "ANGAL HOSPITAL (ST LUKE)"   =      "ANGAL HOSPITAL",
                              "BIDIBIDI HC III"            =      "BIDI BIDI HC III",
                              "BIISO HC III"               =      "BIISO HC IV",
                              "BOMBO MILITARY HOSPITAL"    =      "BOMBO GENERAL MILITARY HOSPITAL",
                              "BULUBA HOSPITAL ST FRANCIS" =      "BULUBA HOSPITAL",
                              "BUTEMBA HC III(KYANKWANZI)" =      "BUTEMBA HC IV",
                              "CHINA UGANDA FRIENDSHIP HOSPITAL- NAGURU"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                              "ENTEBBE HOSPITAL MCH GENERAL" =     "ENTEBBE REGIONAL REFERRAL HOSPITAL",
                              "GALIRAAYA HC III"             =     "GARILAYA HC III",
                              "HOLY FAMILY HOSPITAL-NYAPEA"  =     "NYAPEA HOSPITAL",
                              "ISS CLINIC (MBARARA REGIONAL REFERRAL HOSPITAL)" = "MBARARA REGIONAL REFERRAL HOSPITAL",
                              "KABERAMAIDO GENERAL HOSPITAL" =    "KABERAMAIDO HOSPITAL",
                              "KAJJANSI HC III"              =   "KAJJANSI HC IV",
                              "KASAMBYA HC IV (MUBENDE)"     =    "KASAMBYA HC III",
                              "KASESE MUNICIPAL COUNCIL HC III (KMC)" = "KASESE MINICIPAL COUNCIL",
                              "KATAKWI HC IV"                =     "KATAKWI HOSPITAL",
                              "KAYUNGA HOSPITAL"             =     "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                              "KICHINJAJI  HC  III"          =     "KICHINJAJI HC III",
                              "KINONI HC IV (MBARARA)"       =      "KINONI HC IV",
                              "KISENYI HC IV (KAMPALA)"      =      "KISENYI HC IV",
                              "KISIIZI HOSPITAL C.O.U (RUKUNGIRI)"  = "KISIIZI HOSPITAL",
                              "KISWA HC IV"                  =      "KISWA HC III",
                              "KITGUM GENERAL HOSPITAL"      =      "KITGUM HOSPITAL",
                              "KITWE HC IV (NTUNGAMO)"       =      "KITWE HC IV",
                              "KOBOKO GENERAL HOSPITAL"      =      "KOBOKO HOSPITAL",
                              "KOTIDO HC IV"                 =      "KOTIDO HOSPITAL",
                              "KYEGEGWA HC IV"               =       "KYEGEGWA HOSPITAL",
                              "LACOR HOSPITAL"               =      "ST. MARY'S LACOR HOSPITAL",
                              "MADI OPEI HC IV"              =     "MADI-OPEI HC IV",
                              "MALABA HC III (TORORO)"       =     "MALABA HC III",
                              "MASAKA POLICE CLINIC"         =      "MASAKA POLICE HC III",
                              "MATANY HOSPITAL"              =      "MATANY HOSPITAL",
                              "MAYUGE HC III (MAYUGE)"       =       "MAYUGE HC IV",
                              "MBARARA HC IV (MUNICIPAL)"    =       "MBARARA MUNICIPAL COUNCIL HC IV",
                              "MENGO HOSPITAL COUNSELLING & HOMECARE"  = "MENGO HOSPITAL",
                              "MUKONO GENERAL HOSPITAL"     =        "MUKONO GENERAL HOSPITAL",
                              "NABIGANDA HC III"            =         "NABIGANDA HC IV",
                              "NAGGALAMA HOSPITAL"          =         "ST. FRANCIS NAGGALAMA HOSPITAL",
                              "NAMUTUMBA HC  III (NAMUTUMBA)"  =      "NAMUTUMBA HC III",
                              "NGOMA HC IV (NAKASEKE)"      =          "NGOMA HC IV",
                              "NYAKIBALE HOSPITAL"          =          "KAROLI LWANGA (NYAKIBALE) HOSPITAL",
                              "NYENGA HOSPITAL"             =          "ST. FRANCIS NYENGA HOSPITAL",
                              "PALLISA GENERAL HOSPITAL"    =          "PALLISA HOSPITAL",
                              "PANYADOLI HC III"            =          "PANYADOOLI HC IV",
                              "PNC (BAYLOR-MULAGO)"         =           "MULAGO NRH - PIDC COE BAYLOR CLINIC",
                              "RUKUNYU GENERAL HOSPITAL"    =          "RUKUNYU HOSPITAL",
                              "RUSHOOKA HC II"              =          "RUSHOKA HC IV",
                              "ST ANTHONY HOSPITAL"         =           "ST. ANTHONY'S TORORO HOSPITAL",
                              "ST JOSEPH HOSPITAL  KITGUM"  =          "ST. JOSEPH'S KITGUM HOSPITAL",
                              "ST JOSEPH HOSPITAL (MARACHA)" =          "ST. JOSEPHS MARACHA HOSPITAL",
                              "TORORO DISTRICT HOSPITAL"     =           "TORORO GENERAL HOSPITAL",
                              "WARR HC III"                  =          "WARR HC IV",
                              "YUMBE HOSPITAL"               =           "YUMBE REGIONAL REFERRAL HOSPITAL",
                              "MUYEMBE  HC IV"              =             "MUYEMBE HC IV",
                              "NAKASEKE  HOSPITAL"          =              "NAKASEKE HOSPITAL",
                              "PANYADOLI HC IV"             =              "PANYADOOLI HC IV",
                              "TASO GULU"                  =               "TASO GULU SPECIAL CLINIC",
                              "CDC-KIRUDDU"                 =               "KIRUDDU NRH - MJAP (CDC) MULAGO",
                              "KIRUDDU NATIONAL REFFERAL HOSPITAL" =      "KIRUDDU NATIONAL REFERRAL HOSPITAL",
                              "MULAGO NATIONAL HOSPITAL-MUJHU CLINIC"    =  "MULAGO NRH - MUJHU CLINIC",
                              "KAMULI DISTRICT GOVT HOSPITAL"            =   "KAMULI HOSPITAL",
                              "ST ELIZABETH HC IV-MAGALE"                =   "MAGALE (UCMB) HC IV",
                              "LUWUNGA HC III(1ST DIV HOSPITAL)"         =   "LUWUNGA BARRACKS HC III",
                              "KALAGALA HC IV"  = "KALAGALA HC IV",
                              "KAWAALA HC III"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                              "KIHIIHI HC IV"  = "KIHIHI HC IV",
                              "LALOGI  HC IV"  = "LALOGI HC IV",
                              "LUWEERO GENERAL HOSPITAL"  = "LUWERO HOSPITAL",
                              "MT ST.MARYS HOSPITAL"  = "MT. ST. MARY'S HOSPITAL-DOK",
                              "PAJULE HC  IV"  = "PAJULE HC IV",
                              "STD/MARPI CLINIC-MULAGO"  = "MULAGO NRH - MARPI STI PROJECT CLINIC",
                              "FORTPORTAL REGIONAL REFERRAL HOSPITAL"   =   "FORT PORTAL REGIONAL REFERRAL HOSPITAL",
                              "2ND DIV MILITARY HOSPITAL(MAKENKE)"      =   "UPDF 2ND DIV. HC IV",
                              "DR AMBROSOLIC MEMORIAL HOSPITAL"         =     "KALONGO AMBROSOLI MEMORIAL HOSPITAL",
                              "KAGADI  HOSPITAL"                             =   "KAGADI HOSPITAL",
                              "RUBAARE HC IV"                 =  "RUBARE HC IV",
                              "GULU MILITARY HOSPITAL (4TH DIVISION)"   =   "GULU MILITARY HOSPITAL",
                              "NYANTABOOMA HC III"                      =   "NYANTABOMA HC III"
                              
  ))

##############################################################
#### cleaning EID ALIS continued ####
EIDALIS$SFacility <- trimws(EIDALIS$SFacility)

# Convert to POSIXct and format to dd/mm/yyyy
EIDALIS$CollectionDate <- as.Date(EIDALIS$CollectionDate)
EIDALIS$TestDate <- as.Date(EIDALIS$TestDate)
EIDALIS$DownloadDate   <-  as.Date(EIDALIS$DownloadDate)


#### cleaning other fields
EIDALIS$Age    <-  as.numeric(EIDALIS$Age)

# Obtain RRH and District columns 
EID_Data <- EIDALIS %>%
  left_join(dhis2names, by = "HFacility")


EID_Data %>%
  filter(is.na(District)) %>%
  distinct(HFacility) %>%
  print()

# selecting columns of interest
EID_Data <- EID_Data %>%
  select(SFacility, HFacility, RRH, District, Age, CollectionDate,Sample_DispatchDate,
         TestDate, DownloadDate, pcr, Result
         )

#### Add year and month
EID_Data <- EID_Data %>%
  mutate(
    Yr  =  year(TestDate),
    Month =  month(TestDate)
  ) 
# Add quarter, "Qtr"
EID_Data <- EID_Data %>%
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))

#### VL ALIS data set#####
VLALIS$testing_facility_name <- gsub("H/C","HC", VLALIS$testing_facility_name)
VLALIS$testing_facility_name <- gsub("R R Hospital","Regional Referral Hospital", VLALIS$testing_facility_name)

# Capitalize HFacility and district
VLALIS$testing_facility_name <- toupper(VLALIS$testing_facility_name)
VLALIS$district  <- toupper(VLALIS$district)
VLALIS$requesting_facility <- toupper(VLALIS$requesting_facility)

# Trim spaces
VLALIS$testing_facility_name <- trimws(VLALIS$testing_facility_name)

# Correct health facility names
VLALIS <- VLALIS %>% 
  mutate(testing_facility_name   = recode(testing_facility_name,
                                          "KALAGALA HC IV"  = "KALAGALA HC IV",
                                          "KAMULI DISTRICT GOVT HOSPITAL"  = "KAMULI HOSPITAL",
                                          "KASESE MUNICIPAL COUNCIL HC III (KMC)"  = "KASESE MUNICIPAL COUNCIL HC III",
                                          "KAWAALA HC III"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                                          "KAYUNGA HOSPITAL"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                                          "KICHINJAJI  HC  III"  = "KICHINJAJI HC III",
                                          "KIHIIHI HC IV"  = "KIHIHI HC IV",
                                          "KITWE HC IV (NTUNGAMO)"  = "KITWE HC IV",
                                          "KOBOKO GENERAL HOSPITAL"  = "KOBOKO HOSPITAL",
                                          "KOOME HC III"  = "KOOME HC III",
                                          "KOTIDO HC IV"  = "KOTIDO HOSPITAL",
                                          "LACOR HOSPITAL"  = "ST. MARY'S HOSPITAL LACOR",
                                          "LALOGI  HC IV"  = "LALOGI HC IV",
                                          "LUWEERO GENERAL HOSPITAL"  = "LUWERO HOSPITAL",
                                          "MADI OPEI HC IV"  = "MADI-OPEI HC IV",
                                          "MALABA HC III (TORORO)"  = "MALABA HC III",
                                          "MAYUGE HC III (MAYUGE)"  = "MAYUGE HC IV",
                                          "MT ST.MARYS HOSPITAL"  = "MT. ST. MARY'S HOSPITAL-DOK",
                                          "MUYEMBE  HC IV"  = "MUYEMBE HC IV",
                                          "NAGGALAMA HOSPITAL"  = "ST. FRANCIS NAGGALAMA HOSPITAL",
                                          "NAKASEKE  HOSPITAL"  = "NAKASEKE HOSPITAL",
                                          "NAMUTUMBA HC  III (NAMUTUMBA)"  = "NAMUTUMBA HC III",
                                          "NYAKWAE HC III"  = "NYAKWAE HC III",
                                          "PAJULE HC  IV"  = "PAJULE HC IV",
                                          "PALLISA GENERAL HOSPITAL"  = "PALLISA HOSPITAL",
                                          "PANYADOLI HC III"  = "PANYADOLI HC IV",
                                          "PNC (BAYLOR-MULAGO)"  = "MULAGO NRH - PIDC COE BAYLOR CLINIC",
                                          "RUKUNYU GENERAL HOSPITAL"  = "RUKUNYU HOSPITAL",
                                          "ST ELIZABETH HC IV-MAGALE"  = "MAGALE (UCMB) HC IV",
                                          "ST JOSEPH HOSPITAL  KITGUM"  = "ST. JOSEPH'S KITGUM HOSPITAL",
                                          "ST JOSEPH HOSPITAL (MARACHA)"  = "ST. JOSEPHS MARACHA HOSPITAL",
                                          "STD/MARPI CLINIC-MULAGO"  = "MULAGO NRH - MARPI STI PROJECT CLINIC",
                                          "TASO GULU"  = "TASO GULU  SPECIAL CLINIC",
                                          "YUMBE HOSPITAL"  = "YUMBE REGIONAL REFERRAL HOSPITAL",
                                          "FORTPORTAL REGIONAL REFERRAL HOSPITAL"   =   "FORT PORTAL REGIONAL REFERRAL HOSPITAL",
                                          "KIRUDDU NATIONAL REFFERAL HOSPITAL"      =   "KIRUDDU NATIONAL REFERRAL HOSPITAL",
                                          "2ND DIV MILITARY HOSPITAL(MAKENKE)"      =   "UPDF 2ND DIV. HC IV",
                                          "ANGAL HOSPITAL (ST LUKE)"                =    "ANGAL HOSPITAL",
                                          "BOMBO MILITARY HOSPITAL"                 =    "BOMBO GENERAL MILITARY HOSPITAL",
                                          "DR AMBROSOLIC MEMORIAL HOSPITAL"         =     "KALONGO AMBROSOLI MEMORIAL HOSPITAL",
                                          "ENTEBBE HOSPITAL MCH GENERAL"            =      "ENTEBBE REGIONAL REFERRAL HOSPITAL",
                                          "GALIRAAYA HC III"                        =      "GALIRAYA HC III",
                                          "HOLY FAMILY HOSPITAL-NYAPEA"             =      "NYAPEA HOSPITAL",
                                          "ISS CLINIC (MBARARA REGIONAL REFERRAL HOSPITAL)"   =   "MBARARA REGIONAL REFERRAL HOSPITAL",
                                          "KAGADI  HOSPITAL"                             =   "KAGADI HOSPITAL",
                                          "KAJJANSI HC III"                             =   "KAJJANSI HC IV",
                                          "LUWUNGA HC III(1ST DIV HOSPITAL)"                            =   "LUWUNGA BARRACKS HC III",
                                          "RUBAARE HC IV"                 =  "RUBARE HC IV",
                                          "RUSHOOKA HC II"                =   "RUSHOKA HC IV",
                                          "ACHOL PII MILLITARY HC IV"     =    "ACHOLPII HC III",
                                          "BIISO HC III"                  =    "BIISO HC IV",
                                          "BULUBA HOSPITAL ST FRANCIS"    =    "BULUBA HOSPITAL",
                                          "BUTEMBA HC III(KYANKWANZI)"    =    "BUTEMBA HC III",
                                          "CHINA UGANDA FRIENDSHIP HOSPITAL- NAGURU"    =   "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                                          "FRANCISCAN HC KAKOOGE"       =   "ST. FRANCISCAN HC IV",
                                          "GOLI HC III (NEBBI)"         =  "GOLI HC IV",
                                          "KASAMBYA HC IV (MUBENDE)"    =  "MUBENDE KASAMBYA HC III GOVT",
                                          "KATAKWI HC IV"               =  "KATAKWI HOSPITAL",
                                          "KINONI HC IV (MBARARA)"      =  "KINONI HC IV",
                                          "KISENYI HC IV (KAMPALA)"              =   "KISENYI HC IV",
                                          "KISWA HC IV"                          =  "KISWA HC III",
                                          "KYEGEGWA HC IV"                    =  "KYEGEGWA HOSPITAL",
                                          "MBARARA HC IV (MUNICIPAL)"         =   "MBARARA MUNICIPAL COUNCIL HC IV",
                                          "MULAGO NATIONAL HOSPITAL-MUJHU CLINIC"       =  "MULAGO NRH - MUJHU CLINIC",
                                          "NABIGANDA HC III"       =  "NABIGANDA HC IV",
                                          "NGOMA HC IV (NAKASEKE)"  =   "NGOMA HC IV",
                                          "NYAKIBALE HOSPITAL"      =    "KAROLI LWANGA (NYAKIBALE) HOSPITAL",
                                          "NYANTABOOMA HC III"      =    "NYANTABOMA HC III",
                                          "NYENGA HOSPITAL"         =      "ST. FRANCIS NYENGA HOSPITAL",
                                          "ST ANTHONY HOSPITAL"     =  "ST. ANTHONY'S TORORO HOSPITAL",
                                          "TORORO DISTRICT HOSPITAL"  =  "TORORO GENERAL HOSPITAL",
                                          "GULU MILITARY HOSPITAL (4TH DIVISION)"   =   "GULU MILITARY HOSPITAL",
                                          "BUKUYA HC III"    =  "BUKUYA HC IV"
                          ))


## Recode / Change names to match the names in the regions database
VLALIS <- VLALIS %>% 
  mutate(district  =        recode(district,
                                   # Old               # New
                                   "Luweero"           =      "Luwero",
                                   "Maracha-Terego"    =       "Maracha",
                                   "Ssembabule"         =       "Sembabule"))

## Renaming columns
VLALIS <- VLALIS %>%
  rename(
    # New                         #old
    "FNo."         =             "form_number",
    "District"           =        "district",
    "RFacility"          =       "requesting_facility",
    "HFacility"          =       "testing_facility_name",
    "Device"             =       "poc_device",  
    "CDate"               =       "sample_collection_date",       
    "TDate"              =       "test_date")




#### Obtain RRH and District columns 
VL_Data <- VLALIS %>%
  left_join(dhis2names, by = "HFacility")

#### check for missing district names
VL_Data %>%
  filter(is.na(District.y)) %>%
  distinct(HFacility) %>%
  print()

#### selecting columns of interest
VL_Data <- VL_Data %>%
  select(id,RFacility,HFacility,District.y,RRH,Device,CDate,TDate,created_at,updated_at,is_pregnant,is_breastfeeding,result
  ) %>% 
  rename(
    "District" =  "District.y" 
  )


#### Add year and month
VL_Data <- VL_Data %>%
  mutate(
    Yr  =  year(CDate),
    Month =  month(CDate)
  ) 
# Add quarter, "Qtr"
VL_Data <- VL_Data %>%
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))

###########################################################################
#### Cleaning M-Pima Data sets ####
#### Cleaning EID m-PIMA ####
#EID mPima Database
EIDPima$Site <- gsub("RRH", "Regional Referral Hospital", EIDPima$Site) # substitute RRH for Regional Referral Hospital
# Capitalize HFacility name
EIDPima$Site <- toupper(EIDPima$Site)

EIDPima$Site <- trimws(EIDPima$Site)

EIDPima <- EIDPima %>%
  mutate(Site = recode(Site,
                       # old                           # new
                       "GULU RRH ART CLINIC"  = "GULU REGIONAL REFERRAL HOSPITAL",
                       "GULU RRH LAB"  = "GULU REGIONAL REFERRAL HOSPITAL",
                       "KABALE RRH"  = "KABALE REGIONAL REFERRAL HOSPITAL",
                       "KASAMBYA HC III"  = "KASAMBYA (KAKUMIRO) HC III",
                       "KATABI  GENERAL HOSPITAL"  = "KATABI MILITARY HC III",
                       "KAYUNGA HOSPITAL"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                       "KISIIZI HOSPITAL"  = "COU KISIIZI HOSPITAL",
                       "KYANAMUKAKA HC IV"  = "KYANAMUKAAKA HC IV",
                       "KYEGEGWA HC IV"  = "KYEGEGWA HOSPITAL",
                       "MBARARA MUNICIPAL HC IV"  = "MBARARA MUNICIPAL COUNCIL HC IV",
                       "MOROTO RRH"  = "MOROTO REGIONAL REFERRAL HOSPITAL",
                       "MUKONO HOSPITAL"  = "MUKONO GENERAL HOSPITAL",
                       "MULAGO HOSPITAL BAYLOR"  = "MULAGO NRH - PIDC COE BAYLOR CLINIC",
                       "NAGURU RRH"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                       "OBER HC IV"  = "OBER HC III",
                       "PATONGO HC IV"  = "PATONGO HC III",
                       "RUBAARE HC IV"  = "RUBARE HC IV",
                       "RWAMWANJA HC IV"  = "RWAMWANJA HC III",
                       "SEMBABULE  HC IV"  = "SSEMBABULE HC IV",
                       "ST MARY'S HOSPITAL KASESE"  = "MT. ST. MARY'S HOSPITAL-DOK",
                       "ST. JOSEPH KITOVU HOSPITAL"  = "KITOVU HOSPITAL",
                       "ST.JOSEPH KITGUM HOSPITAL"  = "ST. JOSEPH'S KITGUM HOSPITAL",
                       "TOKORA  HC IV"  = "TOKORA HC IV",
                       "BOMBO MILITARY HOSPITAL"  = "BOMBO GENERAL MILITARY HOSPITAL",
                       "GALIRAYA HC III"  = "GALIRAYA HC III",
                       "LUWUNGA HC III"  = "LUWUNGA BARRACKS HC III",
                       "NABIGANDA HC III"  = "NABIGANDA HC IV",
                       "MULAGO NRH - MUJHU"  = "MULAGO NRH - MUJHU CLINIC",
                       "NAGURU REGIONAL REFERRAL HOSPITAL"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                       "NAKASONGOLA HC IV"  = "NAKASONGOLA HC IV",
                       "SEMBABULE HC IV"  = "SSEMBABULE HC IV",
                       "ST. KAROLI LWANGA NYAKIBALE HOSPITAL"  = "KAROLI LWANGA (NYAKIBALE) HOSPITAL",
                       "TASO MULAGO HOSPITAL"  = "TASO MULAGO SPECIAL CLINIC",
                       "TORORO HOSPITAL"  = "TORORO GENERAL HOSPITAL",
                       "UPDF 2ND DIVISION HC IV"  = "UPDF 2ND DIV. HC IV",
                       "KATABI MILITARY HOSPITAL"  = "KATABI MILITARY HC III",
                       "ST. JOSEPH HOSPITAL KITGUM"  = "ST. JOSEPH'S KITGUM HOSPITAL"
  ))

# Convert dates, specifying format
EIDPima$`Result Date` <- as.Date(EIDPima$`Result Date`)
EIDPima$`Raw Result Date` <- as.Date(EIDPima$`Raw Result Date`)
## Rename columes
EIDPima <- EIDPima %>% 
  rename(
    # New                      # Old
    "RDate"      = "Result Date",
    "Raw_RDate"  = "Raw Result Date",
    "Status"     = "HIV 1 M/N",
    "HFacility"  = "Site",
    "Error"      = "Error Code")

## Add RRH and District columns
EIDPima <- EIDPima %>% 
  left_join(dhis2names, by = "HFacility")
# final m-Pima dataset
Pima_data <- EIDPima %>% 
  select(HFacility,District,RRH,RDate,Status,Error)


#### Add year and month
Pima_data <- Pima_data %>%
  mutate(
    Yr  =  year(RDate),
    Month =  month(RDate)
  ) 
# Add quarter, "Qtr"
Pima_data <- Pima_data %>%
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))

# Add week number 
Pima_data  <-  Pima_data %>% 
  mutate (Wnum = lubridate::isoweek(RDate)) %>% 
  mutate(Wnum = as.character(Wnum))  

#### Pima expected upload in week
dfPima_data  <-   Pima_data %>% 
  group_by(HFacility,Wnum) %>% 
  summarise(
    Pimatests  =  n())

#### cleaning VL mPima Database####
VLPima$Site <- gsub("RRH", "Regional Referral Hospital", VLPima$Site) # substitute RRH for Regional Referral Hospital
# Capitalize the Health facility names
VLPima$Site <- toupper(VLPima$Site)
VLPima$Site <- trimws(VLPima$Site)

VLPima <- VLPima %>% 
  mutate(Site      =   recode(Site,
                              # old                           # new
                              "GULU RRH ART CLINIC"  = "GULU REGIONAL REFERRAL HOSPITAL",
                              "GULU RRH LAB"  = "GULU REGIONAL REFERRAL HOSPITAL",
                              "KABALE RRH"  = "KABALE REGIONAL REFERRAL HOSPITAL",
                              "KASAMBYA HC III"  = "KASAMBYA (KAKUMIRO) HC III",
                              "KATABI  GENERAL HOSPITAL"  = "KATABI MILITARY HC III",
                              "KAYUNGA HOSPITAL"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                              "KISIIZI HOSPITAL"  = "COU KISIIZI HOSPITAL",
                              "KYANAMUKAKA HC IV"  = "KYANAMUKAAKA HC IV",
                              "KYEGEGWA HC IV"  = "KYEGEGWA HOSPITAL",
                              "MBARARA MUNICIPAL HC IV"  = "MBARARA MUNICIPAL COUNCIL HC IV",
                              "MOROTO RRH"  = "MOROTO REGIONAL REFERRAL HOSPITAL",
                              "MUKONO HOSPITAL"  = "MUKONO GENERAL HOSPITAL",
                              "MULAGO HOSPITAL BAYLOR"  = "MULAGO NRH - PIDC COE BAYLOR CLINIC",
                              "NAGURU RRH"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                              "OBER HC IV"  = "OBER HC III",
                              "PATONGO HC IV"  = "PATONGO HC III",
                              "RUBAARE HC IV"  = "RUBARE HC IV",
                              "RWAMWANJA HC IV"  = "RWAMWANJA HC III",
                              "SEMBABULE  HC IV"  = "SSEMBABULE HC IV",
                              "ST MARY'S HOSPITAL KASESE"  = "MT. ST. MARY'S HOSPITAL-DOK",
                              "ST. JOSEPH KITOVU HOSPITAL"  = "KITOVU HOSPITAL",
                              "ST.JOSEPH KITGUM HOSPITAL"  = "ST. JOSEPH'S KITGUM HOSPITAL",
                              "TOKORA  HC IV"  = "TOKORA HC IV",
                              "BOMBO MILITARY HOSPITAL"  = "BOMBO GENERAL MILITARY HOSPITAL",
                              "GALIRAYA HC III"  = "GALIRAYA HC III",
                              "LUWUNGA HC III"  = "LUWUNGA BARRACKS HC III",
                              "NABIGANDA HC III"  = "NABIGANDA HC IV",
                              "MULAGO NRH - MUJHU"  = "MULAGO NRH - MUJHU CLINIC",
                              "NAGURU REGIONAL REFERRAL HOSPITAL"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                              "NAKASONGOLA HC IV"  = "NAKASONGOLA HC IV",
                              "SEMBABULE HC IV"  = "SSEMBABULE HC IV",
                              "ST. KAROLI LWANGA NYAKIBALE HOSPITAL"  = "KAROLI LWANGA (NYAKIBALE) HOSPITAL",
                              "TASO MULAGO HOSPITAL"  = "TASO MULAGO SPECIAL CLINIC",
                              "TORORO HOSPITAL"  = "TORORO GENERAL HOSPITAL",
                              "UPDF 2ND DIVISION HC IV"  = "UPDF 2ND DIV. HC IV",
                              "KATABI MILITARY HOSPITAL"  = "KATABI MILITARY HC III",
                              "ST. JOSEPH HOSPITAL KITGUM"  = "ST. JOSEPH'S KITGUM HOSPITAL"
  ))

# Convert dates, specifying format
VLPima$`Result Date` <- as.Date(VLPima$`Result Date`)
VLPima$`Raw Result Date` <- as.Date(VLPima$`Raw Result Date`)
## Rename columes
VLPima <- VLPima %>% 
  rename(
    # New                      # Old
    "RDate"      = "Result Date",
    "Raw_RDate"  = "Raw Result Date",
    "Status"     = "HIV 1 M/N",
    "HFacility"  = "Site",
    "Error"      = "Error Code")

## Add RRH and District columns
VLPima_data <- VLPima %>% 
  left_join(dhis2names, by = "HFacility")

# final VL m-Pima dataset
VLPima_data <- VLPima_data %>% 
  select(HFacility,District,RRH,Error,Raw_RDate,RDate,Status)

#### Add year and month
VLPima_data <- VLPima_data %>%
  mutate(
    Yr  =  year(RDate),
    Month =  month(RDate)
  ) 
# Add quarter, "Qtr"
VLPima_data <- VLPima_data %>%
  mutate(
    Qtr  =  case_when(
      Month  %in% c(10, 11, 12) ~ "Oct-Dec",
      Month   %in% c(1,2,3)     ~ "Jan-Mar",
      Month  %in%   c(4,5,6)     ~ "Apri-Jun",
      Month  %in%  c(7,8,9)      ~ "Jul-Sept"
    ))


#######################################################################
#### Cleaning GxP data / LabXpert database####
##### EID Tests####
EIDGxP$Facility <- toupper(EIDGxP$Facility)   # capitalize each word

# clean the name of buluba
EIDGxP <- EIDGxP %>%
  group_by(Facility = ifelse(Facility %in% c("ST FRANCIS BULUBA HOSPITAL"), "ST FRANCIS HOSPITAL BULUBA", Facility)
           ) 


EIDGxP <- EIDGxP %>% 
  mutate(Facility      = recode(Facility,
                                # Old                            # New
                                "YUMBE HOSPITAL"  = "YUMBE REGIONAL REFERRAL HOSPITAL",
                                "TORORO HOSPITAL"  = "TORORO GENERAL HOSPITAL",
                                "KAYUNGA REGIONAL REFERRAL HOSPITAL 1"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                                "KAWAALA HEALTH CENTRE HC III"  = "KAWAALA HEALTH CENTRE HC IV",
                                "MT ST MARY'S HOSPITAL-DOK"  = "MT. ST. MARY'S HOSPITAL-DOK",
                                "MOROTO PRISONS"  = "MOROTO PRISONS HC II",
                                "LUKAYA HEALTH CARE CENTER UGANDA CARES HC II"  = "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
                                "BIDI BIDI HC III"  = "BIDIBIDI HC III",
                                "BUTABIKA HOSPITAL"  = "BUTABIKA NATIONAL REFERRAL HOSPITAL",
                                "NAGURU R.R HOSPITAL"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                                "KOMAMBOGA HCIII"  = "KOMAMBOGA HC III",
                                "LIRA PRISONS HCIII"  = "LIRA PRISONS HC II",
                                "NAKASONGOLA MAIN PRISON HC III"  = "NAKASONGOLA PRISONS HC III",
                                "LACOR HOSPITAL"  = "ST. MARY'S HOSPITAL LACOR",
                                "RUSHOOKA HCII"  = "RUSHOKA HC IV",
                                "MASAKA MAIN PRISON HC III"  = "MASAKA PRISONS HC III",
                                "NSAMBYA POLICE HC III"  = "NSAMBYA POLICE HC IV",
                                "KITALYA PRISONS HCIII"  = "KITALYA PRISONS HC II",
                                "TASO GULU"  = "TASO GULU  SPECIAL CLINIC",
                                "ARUA MAIN PRISON"  = "ARUA MAIN PRISONS HC III",
                                "IDI MULAGO"  = "MULAGO NRH - INFECTIOUS DISEASE INSTITUTE",
                                "JCRC LUBOWA"  = "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                "PADREPIO HC IV"  = "PADRE PIO HC III",
                                "ST FRANCIS BULUBA HOSPITAL"  = "BULUBA HOSPITAL",
                                "IGANGA MUNICIPAL HC III"  = "IGANGA TOWN COUNCIL HC III",
                                "ST. FRANCIS HEALTH CARE SERVICES HC III"  = "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                "ST. BENEDICT HOSPITAL"  = "BENEDICT HC IV",
                                "MAKENKE HC IV"  = "UPDF 2ND DIV. HC IV",
                                "NYAMIRAMA HCIII"  = "NYAMIRAMA HC III",
                                "HOLY INNOCENT"  = "HOLY INNOCENT HC III",
                                "ACHOL PII MILITARY HCIV GXIV 816619"  = "ACHOLPII HC III",
                                "ST FRANCIS HOSPITAL BULUBA"      =   "BULUBA HOSPITAL",
                                "KOTIDO HC IV"                    =    "KOTIDO HOSPITAL",
                                "IRIIRI HC III"                   =     "IRIRI HC III"
  ))

## rename lab to HFacility
EIDGxP <- EIDGxP %>% 
  rename(
       "HFacility" =  "Facility",
       "GxPEID"    =  "Total")

EIDGxP <- EIDGxP %>% 
  left_join(modules, by = "HFacility")

####VL GxP #### 
VLGxP$Facility <- toupper(VLGxP$Facility)

# clean the name of buluba
VLGxP <- VLGxP %>%
  group_by(Facility = ifelse(Facility %in% c("ST FRANCIS BULUBA HOSPITAL"), "ST FRANCIS HOSPITAL BULUBA", Facility)
           ) 

# clean the names
VLGxP <- VLGxP %>% 
  mutate(Facility     =    recode(Facility,
                                  # Old                            # New
                                  "YUMBE HOSPITAL"  = "YUMBE REGIONAL REFERRAL HOSPITAL",
                                  "TORORO HOSPITAL"  = "TORORO GENERAL HOSPITAL",
                                  "KAYUNGA REGIONAL REFERRAL HOSPITAL 1"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                                  "KAWAALA HEALTH CENTRE HC III"  = "KAWAALA HEALTH CENTRE HC IV",
                                  "MT ST MARY'S HOSPITAL-DOK"  = "MT. ST. MARY'S HOSPITAL-DOK",
                                  "MOROTO PRISONS"  = "MOROTO PRISONS HC II",
                                  "LUKAYA HEALTH CARE CENTER UGANDA CARES HC II"  = "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
                                  "BIDI BIDI HC III"  = "BIDIBIDI HC III",
                                  "BUTABIKA HOSPITAL"  = "BUTABIKA NATIONAL REFERRAL HOSPITAL",
                                  "NAGURU R.R HOSPITAL"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                                  "KOMAMBOGA HCIII"  = "KOMAMBOGA HC III",
                                  "LIRA PRISONS HCIII"  = "LIRA PRISONS HC II",
                                  "NAKASONGOLA MAIN PRISON HC III"  = "NAKASONGOLA PRISONS HC III",
                                  "LACOR HOSPITAL"  = "ST. MARY'S HOSPITAL LACOR",
                                  "RUSHOOKA HCII"  = "RUSHOKA HC IV",
                                  "MASAKA MAIN PRISON HC III"  = "MASAKA PRISONS HC III",
                                  "NSAMBYA POLICE HC III"  = "NSAMBYA POLICE HC IV",
                                  "KITALYA PRISONS HCIII"  = "KITALYA PRISONS HC II",
                                  "TASO GULU"  = "TASO GULU  SPECIAL CLINIC",
                                  "ARUA MAIN PRISON"  = "ARUA MAIN PRISONS HC III",
                                  "IDI MULAGO"  = "MULAGO NRH - INFECTIOUS DISEASE INSTITUTE",
                                  "JCRC LUBOWA"  = "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                  "PADREPIO HC IV"  = "PADRE PIO HC III",
                                  "ST FRANCIS BULUBA HOSPITAL"  = "BULUBA HOSPITAL",
                                  "IGANGA MUNICIPAL HC III"  = "IGANGA TOWN COUNCIL HC III",
                                  "ST. FRANCIS HEALTH CARE SERVICES HC III"  = "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                  "ST. BENEDICT HOSPITAL"  = "BENEDICT HC IV",
                                  "MAKENKE HC IV"  = "UPDF 2ND DIV. HC IV",
                                  "NYAMIRAMA HCIII"  = "NYAMIRAMA HC III",
                                  "HOLY INNOCENT"  = "HOLY INNOCENT HC III",
                                  "ACHOL PII MILITARY HCIV GXIV 816619"  = "ACHOLPII HC III",
                                  "ST FRANCIS HOSPITAL BULUBA"      =   "BULUBA HOSPITAL",
                                  "KOTIDO HC IV"                    =    "KOTIDO HOSPITAL",
                                  "IRIIRI HC III"                   =     "IRIRI HC III"
  ))
## rename lab to HFacility
VLGxP <- VLGxP %>% 
  rename("HFacility" = "Facility",
         "GxPVL"    =  "Total")
VLGxP$HFacility <- toupper(VLGxP$HFacility)
#### Add number of modules
VLGxP <- VLGxP %>% 
  left_join(modules, by = "HFacility")


####HPV GxP ####
HPVGxP$Facility <- toupper(HPVGxP$Facility)

# clean the name of buluba
HPVGxP <- HPVGxP %>%
  group_by(Facility = ifelse(Facility %in% c("ST FRANCIS BULUBA HOSPITAL"), "ST FRANCIS HOSPITAL BULUBA", Facility)
           ) 

# clean the faclity names
HPVGxP <- HPVGxP %>% 
  mutate(Facility                  =  recode(Facility,
                                             # Old                            # New
                                             "YUMBE HOSPITAL"  = "YUMBE REGIONAL REFERRAL HOSPITAL",
                                             "TORORO HOSPITAL"  = "TORORO GENERAL HOSPITAL",
                                             "KAYUNGA REGIONAL REFERRAL HOSPITAL 1"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
                                             "KAWAALA HEALTH CENTRE HC III"  = "KAWAALA HEALTH CENTRE HC IV",
                                             "MT ST MARY'S HOSPITAL-DOK"  = "MT. ST. MARY'S HOSPITAL-DOK",
                                             "MOROTO PRISONS"  = "MOROTO PRISONS HC II",
                                             "LUKAYA HEALTH CARE CENTER UGANDA CARES HC II"  = "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
                                             "BIDI BIDI HC III"  = "BIDIBIDI HC III",
                                             "BUTABIKA HOSPITAL"  = "BUTABIKA NATIONAL REFERRAL HOSPITAL",
                                             "NAGURU R.R HOSPITAL"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
                                             "KOMAMBOGA HCIII"  = "KOMAMBOGA HC III",
                                             "LIRA PRISONS HCIII"  = "LIRA PRISONS HC II",
                                             "NAKASONGOLA MAIN PRISON HC III"  = "NAKASONGOLA PRISONS HC III",
                                             "LACOR HOSPITAL"  = "ST. MARY'S HOSPITAL LACOR",
                                             "RUSHOOKA HCII"  = "RUSHOKA HC IV",
                                             "MASAKA MAIN PRISON HC III"  = "MASAKA PRISONS HC III",
                                             "NSAMBYA POLICE HC III"  = "NSAMBYA POLICE HC IV",
                                             "KITALYA PRISONS HCIII"  = "KITALYA PRISONS HC II",
                                             "TASO GULU"  = "TASO GULU  SPECIAL CLINIC",
                                             "ARUA MAIN PRISON"  = "ARUA MAIN PRISONS HC III",
                                             "IDI MULAGO"  = "MULAGO NRH - INFECTIOUS DISEASE INSTITUTE",
                                             "JCRC LUBOWA"  = "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                             "PADREPIO HC IV"  = "PADRE PIO HC III",
                                             "ST FRANCIS BULUBA HOSPITAL"  = "BULUBA HOSPITAL",
                                             "IGANGA MUNICIPAL HC III"  = "IGANGA TOWN COUNCIL HC III",
                                             "ST. FRANCIS HEALTH CARE SERVICES HC III"  = "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                             "ST. BENEDICT HOSPITAL"  = "BENEDICT HC IV",
                                             "MAKENKE HC IV"  = "UPDF 2ND DIV. HC IV",
                                             "NYAMIRAMA HCIII"  = "NYAMIRAMA HC III",
                                             "HOLY INNOCENT"  = "HOLY INNOCENT HC III",
                                             "ACHOL PII MILITARY HCIV GXIV 816619"  = "ACHOLPII HC III",
                                             "ST FRANCIS HOSPITAL BULUBA"      =   "BULUBA HOSPITAL",
                                             "KOTIDO HC IV"                    =    "KOTIDO HOSPITAL",
                                             "IRIIRI HC III"                   =     "IRIRI HC III"  
  ))
## rename lab to HFacility
HPVGxP <- HPVGxP %>% 
  rename("HFacility" = "Facility",
         "GxPHPV"    =  "Total")

HPVGxP$HFacility <- toupper(HPVGxP$HFacility)

HPVGxP <- HPVGxP %>% 
  left_join(modules, by = "HFacility")
# renameing column
HPVGxP <- HPVGxP %>% 
  rename(
  "Errors"   =   "ERRORS"
)

#### TB Tests####
# capitalize 
TBGxP$Site  <- toupper(TBGxP$Site)

# clean the name of buluba
TBGxP <- TBGxP %>%
  group_by(Site = ifelse(Site %in% c("ST FRANCIS BULUBA HOSPITAL"), "ST FRANCIS HOSPITAL BULUBA", Site)
           ) 

# cleaning the names
TBGxP <- TBGxP %>% 
  mutate(Site                  =  recode(Site,
                                         # Old                            # New
              "YUMBE HOSPITAL"  = "YUMBE REGIONAL REFERRAL HOSPITAL",
              "TORORO HOSPITAL"  = "TORORO GENERAL HOSPITAL",
              "KAYUNGA REGIONAL REFERRAL HOSPITAL 1"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
              "KAWAALA HEALTH CENTRE HC III"  = "KAWAALA HEALTH CENTRE HC IV",
              "MT ST MARY'S HOSPITAL-DOK"  = "MT. ST. MARY'S HOSPITAL-DOK",
              "MOROTO PRISONS"  = "MOROTO PRISONS HC II",
              "LUKAYA HEALTH CARE CENTER UGANDA CARES HC II"  = "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
              "BIDI BIDI HC III"  = "BIDIBIDI HC III",
              "BUTABIKA HOSPITAL"  = "BUTABIKA NATIONAL REFERRAL HOSPITAL",
              "NAGURU R.R HOSPITAL"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
              "KOMAMBOGA HCIII"  = "KOMAMBOGA HC III",
              "LIRA PRISONS HCIII"  = "LIRA PRISONS HC II",
              "NAKASONGOLA MAIN PRISON HC III"  = "NAKASONGOLA PRISONS HC III",
              "LACOR HOSPITAL"  = "ST. MARY'S HOSPITAL LACOR",
              "RUSHOOKA HCII"  = "RUSHOKA HC IV",
              "MASAKA MAIN PRISON HC III"  = "MASAKA PRISONS HC III",
              "NSAMBYA POLICE HC III"  = "NSAMBYA POLICE HC IV",
              "KITALYA PRISONS HCIII"  = "KITALYA PRISONS HC II",
              "TASO GULU"  = "TASO GULU  SPECIAL CLINIC",
              "ARUA MAIN PRISON"  = "ARUA MAIN PRISONS HC III",
             "IDI MULAGO"  = "MULAGO NRH - INFECTIOUS DISEASE INSTITUTE",
             "JCRC LUBOWA"  = "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
              "PADREPIO HC IV"  = "PADRE PIO HC III",
              "IGANGA MUNICIPAL HC III"  = "IGANGA TOWN COUNCIL HC III",
              "ST. FRANCIS HEALTH CARE SERVICES HC III"  = "ST. FRANCIS HEALTH CARE SERVICES HC IV",
              "ST. BENEDICT HOSPITAL"  = "BENEDICT HC IV",
              "MAKENKE HC IV"  = "UPDF 2ND DIV. HC IV",
              "NYAMIRAMA HCIII"  = "NYAMIRAMA HC III",
              "HOLY INNOCENT"  = "HOLY INNOCENT HC III",
              "ACHOL PII MILITARY HCIV GXIV 816619"  = "ACHOLPII HC III",
              "BULISA GENERAL HOSPITAL"         =    "BULIISA HOSPITAL",
              "KOTIDO HC IV"                    =    "KOTIDO HOSPITAL",
              "IRIIRI HC III"                   =     "IRIRI HC III",
              "ST FRANCIS HOSPITAL BULUBA"      =      "BULUBA HOSPITAL" 
  ))

## rename lab to HFacility
TBGxP <- TBGxP %>% 
  rename("HFacility" = "Site",
         "GxPTB"    =  "Total samples")
# capitalize 
TBGxP$HFacility  <- toupper(TBGxP$HFacility)

# Assign RRH
TBGxP <- TBGxP %>% 
  left_join(modules, by = "HFacility")


TBGxP <- TBGxP %>% 
  rename(
    "Errors"  =  "ERRORS"
  )

#### Cleaning GxP Error Rate data set ####
GxPError   <-  GxPError %>% 
    mutate(Site                  =  recode(Site,
        # Old                            # New
          "YUMBE HOSPITAL"  = "YUMBE REGIONAL REFERRAL HOSPITAL",
          "TORORO HOSPITAL"  = "TORORO GENERAL HOSPITAL",
          "KAYUNGA REGIONAL REFERRAL HOSPITAL 1"  = "KAYUNGA REGIONAL REFERRAL HOSPITAL",
          "KAWAALA HEALTH CENTRE HC III"  = "KAWAALA HEALTH CENTRE HC IV",
          "MT ST MARY'S HOSPITAL-DOK"  = "MT. ST. MARY'S HOSPITAL-DOK",
          "MOROTO PRISONS"  = "MOROTO PRISONS HC II",
          "LUKAYA HEALTH CARE CENTER UGANDA CARES HC II"  = "LUKAYA HEALTH CARE CENTER/UGANDA CARES HC II",
          "BIDI BIDI HC III"  = "BIDIBIDI HC III",
          "BUTABIKA HOSPITAL"  = "BUTABIKA NATIONAL REFERRAL HOSPITAL",
          "NAGURU R.R HOSPITAL"  = "CHINA UGANDA FRIENDSHIP (NAGURU) REGIONAL REFERRAL HOSPITAL",
          "KOMAMBOGA HCIII"  = "KOMAMBOGA HC III",
          "LIRA PRISONS HCIII"  = "LIRA PRISONS HC II",
          "NAKASONGOLA MAIN PRISON HC III"  = "NAKASONGOLA PRISONS HC III",
          "LACOR HOSPITAL"  = "ST. MARY'S HOSPITAL LACOR",
          "RUSHOOKA HCII"  = "RUSHOKA HC IV",
          "MASAKA MAIN PRISON HC III"  = "MASAKA PRISONS HC III",
          "NSAMBYA POLICE HC III"  = "NSAMBYA POLICE HC IV",
          "KITALYA PRISONS HCIII"  = "KITALYA PRISONS HC II",
          "TASO GULU"  = "TASO GULU  SPECIAL CLINIC",
          "ARUA MAIN PRISON"  = "ARUA MAIN PRISONS HC III",
                                         "IDI MULAGO"  = "MULAGO NRH - INFECTIOUS DISEASE INSTITUTE",
                                         "JCRC LUBOWA"  = "JOINT CLINICAL RESEARCH CENTER (JCRC) HC IV",
                                         "PADREPIO HC IV"  = "PADRE PIO HC III",
                                         "IGANGA MUNICIPAL HC III"  = "IGANGA TOWN COUNCIL HC III",
                                         "ST. FRANCIS HEALTH CARE SERVICES HC III"  = "ST. FRANCIS HEALTH CARE SERVICES HC IV",
                                         "ST. BENEDICT HOSPITAL"  = "BENEDICT HC IV",
                                         "MAKENKE HC IV"  = "UPDF 2ND DIV. HC IV",
                                         "NYAMIRAMA HCIII"  = "NYAMIRAMA HC III",
                                         "HOLY INNOCENT"  = "HOLY INNOCENT HC III",
                                         "ACHOL PII MILITARY HCIV GXIV 816619"  = "ACHOLPII HC III",
                                         "BULISA GENERAL HOSPITAL"         =    "BULIISA HOSPITAL",
                                         "KOTIDO HC IV"                    =    "KOTIDO HOSPITAL",
                                         "IRIIRI HC III"                   =     "IRIRI HC III",
                                         "ST FRANCIS HOSPITAL BULUBA"      =      "BULUBA HOSPITAL",
                                         "ST FRANCIS BULUBA HOSPITAL"      =      "BULUBA HOSPITAL"
              ))
#### changing data types
GxPError$Yr   <-  as.character(GxPError$Yr)
GxPError$Error  <-  as.character(GxPError$Error)
GxPError$Site   <-  toupper(GxPError$Site)


###########################################################################
###########################################################################
#### POC Commodity distribution ####
#### Report - Line list of HF EID test vols and Commodity dispatch ####
#### re-organizing consumption data 
EID_Comptn   <-  consumption %>% 
  group_by(HFacility,Platform,Yr,Qtr) %>% 
  summarise(
    EID   =  sum(EID, na.rm = TRUE)
  ) 
# pivot for period
EID_ComptnPvt   <-   EID_Comptn%>% 
  pivot_wider(
    id_cols = c(HFacility,Platform),
    names_from = c(Yr,Qtr),
    values_from = EID
  )

#### join the consumption to test data
EID_DatasetC  <-  EID_Dataset %>% 
  full_join(EID_ComptnPvt, by = "HFacility")

#### select columns of interest, and re-arrange the columns
EID_DatasetC   <-   EID_DatasetC %>% 
  select(HFacility,REGION,HUB,`IP/REGION`, EID_Platform,`2024_Jul-Sept`,               
         `2024_Oct-Dec`,`2025_Jan-Mar`,`ALIS-2024(Oct-Dec)`,`mPima-2024(Oct-Dec)`,         
         `GxP-2024(Oct-Dec)`,`ALIS-2025(Jan-Mar)`,          
         `mPima-2025(Jan-Mar)`,`GxP-2025(Jan-Mar)`,           
         `Highest Report-2024(Oct-Dec)`, `Highest Report-2025(Jan-Mar)`)


#### Calculate totals and percentages
EID_DatasetC   <-   EID_DatasetC %>% 
  mutate(
    `Total Commodities dispatched in period of interest` = rowSums(across(c(`2024_Jul-Sept`,`2024_Oct-Dec`,                
                                                                            `2025_Jan-Mar` )), na.rm = TRUE),
    `Total_tests(fy24/25)`                =  rowSums(across(c(`Highest Report-2024(Oct-Dec)`,
                                                              `Highest Report-2025(Jan-Mar)`)), na.rm = TRUE),
    `%age tests against cdties dispatched` = round((`Total_tests(fy24/25)`/ `Total Commodities dispatched in period of interest`) * 100, 0)
  ) 

#### the table
tble_EID_DatasetC   <-  flextable(EID_DatasetC)

#### Format the table
# Add the header row
tble_EID_DatasetC <- tble_EID_DatasetC %>% 
  add_header_row(
    values = c(
      "HFacility",
      "REGION",
      "HUB",
      "IP/REGION",
      "EID testing Platform",
      "No. of commodities dispatched to the respective facilities in the indicated quarters",
      "",
      "",
      "Volume of samples tested, as derived from the indicated databases; for the respective Quarters",
      "",
      "",
      "",
      "",
      "",
      "Highest volume of samples reported by any of the 3 databases (Used in report as vols done)",
      "",
      "Total no. of commodities dispatched to facility in running COP Year",
      "Total test volumes reported in the COP Year",
      "%age tests against commodities dispatched in the running COP Year"
    ))
# Set the header labels
tble_EID_DatasetC <- tble_EID_DatasetC %>%
  set_header_labels(
    HFacility = "HFacility",
    REGION = "REGION",
    HUB = "HUB",
    `IM/IP`  =  "IP/REGION",
    EID_Platform = "EID testing Platform",
    `2024_Jul-Sept` = "2024_Jul-Sept",
    `2024_Oct-Dec` = "2024_Oct-Dec",
    `2025_Jan-Mar` = "2025_Jan-Mar",
    `ALIS-2024(Oct-Dec)` = "ALIS-2024(Oct-Dec)",
    `mPima-2024(Oct-Dec)` = "mPima-2024(Oct-Dec)",
    `GxP-2024(Oct-Dec)` = "GxP-2024(Oct-Dec)",
    `ALIS-2025(Jan-Mar)` = "ALIS-2025(Jan-Mar)",
    `mPima-2025(Jan-Mar)` = "mPima-2025(Jan-Mar)",
    `GxP-2025(Jan-Mar)` = "GxP-2025(Jan-Mar)",
    `Highest Report-2024(Oct-Dec)` = "Highest Report-2024(Oct-Dec)",
    `Highest Report-2025(Jan-Mar)` = "Highest Report-2025(Jan-Mar)",
    `Total Commodities dispatched in period of interest` = "Total no. of commodities dispatched to facility in running COP Year",
    `Total_tests(fy24/25)` = "Total test volumes reported in the COP Year",
    `%age tests against cdties dispatched` = "%age tests against commodities dispatched in the running COP Year"
  )

# Merging columns 
tble_EID_DatasetC <- tble_EID_DatasetC %>%
  merge_at(i = 1, j = 6:8, part = "header") %>%
  merge_at(i = 1, j = 9:11, part = "header") %>% 
  merge_at(i = 1, j = 12:14, part = "header") %>% 
  merge_at(i = 1, j = 15:16, part = "header")

# Merging vertically for the first five columns in the header
tble_EID_DatasetC <- tble_EID_DatasetC %>%
  merge_v(j = 1:5, part = "header") %>% 
  merge_v(j = 17:19, part = "header")


# Adding vertical lines to improve readability
tble_EID_DatasetC <- tble_EID_DatasetC %>%
  vline(j = c(1, 5, 8, 14,16), part = "all")

#Font and sice
tble_EID_DatasetC <- tble_EID_DatasetC %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

# the table
tble_EID_DatasetC

#### Report - RRH IP/IM summary of test and commodity details ####
EID_cdty <- EID_DatasetC %>%
  group_by(REGION, `IP/REGION`) %>%
  summarise(
    `Total Commodities dispatched in period of interest` = sum(`2024_Jul-Sept`, `2024_Oct-Dec`, `2025_Jan-Mar`, na.rm = TRUE),
    `Total_tests(fy24/25)` = sum(`Highest Report-2024(Oct-Dec)`, `Highest Report-2025(Jan-Mar)`, na.rm = TRUE)
  ) %>%
  mutate(
    `%age tests against cdties dispatched` = round((`Total_tests(fy24/25)` / `Total Commodities dispatched in period of interest`) * 100, 0)
  ) %>% 
  ungroup()

### the table
tbl_EID_cdty   <-  flextable(EID_cdty)
tbl_EID_cdty

#### Overall
All_EID_cdty    <-  EID_cdty %>% 
  select(REGION, `Total Commodities dispatched in period of interest`,`Total_tests(fy24/25)`) %>% 
  summarise(
    `Total Commodities dispatched in period of interest` = sum(`Total Commodities dispatched in period of interest`, na.rm = TRUE),
    `Total_tests(fy24/25)` = sum(`Total_tests(fy24/25)`, na.rm = TRUE)
  ) %>%
  mutate(
    `%age tests against cdties dispatched` = round((`Total_tests(fy24/25)` / `Total Commodities dispatched in period of interest`) * 100, 0)
  )

# The table
tbl_All_EID_cdty   <-  flextable(All_EID_cdty)
tbl_All_EID_cdty

#### Report - Line list of HF VL test vols and Commodity dispatch  ####
#### re-organizing consumption data 
VL_Comptn   <-  consumption %>% 
  group_by(HFacility,Platform,Yr,Qtr) %>% 
  summarise(
    VL   =  sum(EID, na.rm = TRUE)
  ) 
# pivot for period
VL_ComptnPvt   <-   VL_Comptn%>% 
  pivot_wider(
    id_cols = c(HFacility,Platform),
    names_from = c(Yr,Qtr),
    values_from = VL
  )

#### join the consumption to test data
VL_DatasetC  <-  VL_Dataset %>% 
  full_join(VL_ComptnPvt, by = "HFacility")

#### select columns of interest, and re-arrange the columns
VL_DatasetC   <-   VL_DatasetC %>% 
  select(HFacility,REGION,HUB,`IP/REGION`,Platform,`2024_Jul-Sept`,               
         `2024_Oct-Dec`,`2025_Jan-Mar`,`ALIS-2024(Oct-Dec)`,`mPima-2024(Oct-Dec)`,         
         `GxP-2024(Oct-Dec)`,`ALIS-2025(Jan-Mar)`,          
         `mPima-2025(Jan-Mar)`,`GxP-2025(Jan-Mar)`,           
         `Highest Report-2024(Oct-Dec)`, `Highest Report-2025(Jan-Mar)`)


#### Calculate totals and percentages
VL_DatasetC   <-   VL_DatasetC %>% 
  mutate(
    `Total Commodities dispatched in period of interest` = rowSums(across(c(`2024_Jul-Sept`,`2024_Oct-Dec`,                
                                                                            `2025_Jan-Mar` )), na.rm = TRUE),
    `Total_tests(fy24/25)`                =  rowSums(across(c(`Highest Report-2024(Oct-Dec)`,
                                                              `Highest Report-2025(Jan-Mar)`)), na.rm = TRUE),
    `%age tests against cdties dispatched` = round((`Total_tests(fy24/25)`/ `Total Commodities dispatched in period of interest`) * 100, 0)
  ) 

#### the table
tble_VL_DatasetC   <-  flextable(VL_DatasetC)


#### Format the table (Working)
# Add the header row
tble_VL_DatasetC <- tble_VL_DatasetC %>% 
  add_header_row(
    values = c(
      "HFacility",
      "REGION",
      "HUB",
      "IP/REGION",
      "Platform",
      "No. of commodities dispatched to the respective facilities in the indicated quarters",
      "",
      "",
      "Volume of samples tested, as derived from the indicated databases; Q1",
      "",
      "",
      "Volume of samples tested, as derived from the indicated databases; Q2",
      "",
      "",
      "Highest volume of samples reported by any of the 3 databases (Used in report as vols done)",
      "",
      "Total no. of commodities dispatched to facility in running COP Year",
      "Total test volumes reported in the COP Year",
      "%age tests against commodities dispatched in the running COP Year"
    ))
# Set the header labels
tble_VL_DatasetC <- tble_VL_DatasetC %>% 
  set_header_labels(
    HFacility = "HFacility",
    REGION = "REGION",
    HUB = "HUB",
    `IM/IP`  =  "IP/REGION",
    `VL testing Platform` = "Platform",
    `2024_Jul-Sept` = "2024_Jul-Sept",
    `2024_Oct-Dec` = "2024_Oct-Dec",
    `2025_Jan-Mar` = "2025_Jan-Mar",
    `ALIS-2024(Oct-Dec)` = "ALIS-2024(Oct-Dec)",
    `mPima-2024(Oct-Dec)` = "mPima-2024(Oct-Dec)",
    `GxP-2024(Oct-Dec)` = "GxP-2024(Oct-Dec)",
    `ALIS-2025(Jan-Mar)` = "ALIS-2025(Jan-Mar)",
    `mPima-2025(Jan-Mar)` = "mPima-2025(Jan-Mar)",
    `GxP-2025(Jan-Mar)` = "GxP-2025(Jan-Mar)",
    `Highest Report-2024(Oct-Dec)` = "Highest Report-2024(Oct-Dec)",
    `Highest Report-2025(Jan-Mar)` = "Highest Report-2025(Jan-Mar)",
    `Total Commodities dispatched in period of interest` = "Total no. of commodities dispatched to facility in running COP Year",
    `Total_tests(fy24/25)` = "Total test volumes reported in the COP Year",
    `%age tests against cdties dispatched` = "%age tests against commodities dispatched in the running COP Year"
  )

# Merging columns 
tble_VL_DatasetC <- tble_VL_DatasetC %>% 
  merge_at(i = 1, j = 6:8, part = "header") %>%
  merge_at(i = 1, j = 9:11, part = "header") %>% 
  merge_at(i = 1, j = 12:14, part = "header") %>% 
  merge_at(i = 1, j = 15:16, part = "header")

# Merging vertically for the first five columns in the header
tble_VL_DatasetC <- tble_VL_DatasetC %>% 
  merge_v(j = 1:5, part = "header") %>% 
  merge_v(j = 17:19, part = "header")


# Adding vertical lines to improve readability
tble_VL_DatasetC <- tble_VL_DatasetC %>% 
  vline(j = c(1, 5, 8,11, 14,16), part = "all")

#Font and sice
tble_VL_DatasetC <- tble_VL_DatasetC %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

# the table
tble_VL_DatasetC

#### Report - RRH IP/IM summary of test and commodity details ####
VL_cdty <- VL_DatasetC %>%
  group_by(REGION, `IP/REGION`) %>%
  summarise(
    `Total Commodities dispatched in period of interest` = sum(`2024_Jul-Sept`, `2024_Oct-Dec`, `2025_Jan-Mar`, na.rm = TRUE),
    `Total_tests(fy24/25)` = sum(`Highest Report-2024(Oct-Dec)`, `Highest Report-2025(Jan-Mar)`, na.rm = TRUE)
  ) %>%
  mutate(
    `%age tests against cdties dispatched` = round((`Total_tests(fy24/25)` / `Total Commodities dispatched in period of interest`) * 100, 0)
  ) %>% 
  ungroup()

### the table
tbl_VL_cdty   <-  flextable(VL_cdty)
tbl_VL_cdty

#### National ####
All_VL_cdty    <-  VL_cdty %>% 
  select(REGION, `Total Commodities dispatched in period of interest`,`Total_tests(fy24/25)`) %>% 
  summarise(
    `Total Commodities dispatched in period of interest` = sum(`Total Commodities dispatched in period of interest`, na.rm = TRUE),
    `Total_tests(fy24/25)` = sum(`Total_tests(fy24/25)`, na.rm = TRUE)
  ) %>%
  mutate(
    `%age tests against cdties dispatched` = round((`Total_tests(fy24/25)` / `Total Commodities dispatched in period of interest`) * 100, 0)
  )

# The table
tbl_All_VL_cdty   <-  flextable(All_VL_cdty)
tbl_All_VL_cdty

###########################################################################
############################################################################
#### POC Data sets (considering all data bases ####
#### EID Data set####
#### Number tests done in ALIS by Testing site
EID_ALIS <- EID_Data %>% 
  group_by(HFacility,Yr,Qtr) %>% 
    summarise(ALIS_No = n(),
              .groups = "drop")
# organise table with Yr and Qtr as columns
EID_ALISPvt   <-  EID_ALIS %>% 
    pivot_wider(
      id_cols = HFacility,
      names_from = c(Yr,Qtr),
      values_from = ALIS_No
    ) 
# select period of interest
EID_ALISRpt   <-  EID_ALISPvt %>% 
  select(HFacility,`2024_Oct-Dec`,`2025_Jan-Mar`)

#### Number tests done by m-Pima 
EID_PimaDone <- Pima_data %>% 
  group_by(HFacility,Yr,Qtr) %>% 
  summarise(EIDonPima = n(),
            .groups = "drop")
# organise table with Yr and Qtr as columns
EID_PimaDonePvt   <-  EID_PimaDone %>% 
  filter(Qtr != "Apri-Jun") %>% 
  pivot_wider(
    id_cols = HFacility,
    names_from = c(Yr,Qtr),
    values_from = EIDonPima
  ) 
# select period of interest
EID_PimaDoneRpt   <-  EID_PimaDonePvt %>% 
  select(HFacility,`2024_Oct-Dec`,`2025_Jan-Mar`)


#### Number of EID GxP done 
EIDGxP <- EIDGxP %>%
  group_by(HFacility,Yr,Qtr) %>% 
  summarise(
    GxPEID  =  sum(GxPEID),
    .groups = "drop")
# organise table with Yr and Qtr as columns
EIDGxPPvt   <-  EIDGxP %>% 
  pivot_wider(
    id_cols = HFacility,
    names_from = c(Yr,Qtr),
    values_from = GxPEID
  ) 
# select period of interest
EIDGxPRpt   <-  EIDGxPPvt %>% 
  select(HFacility,`2024_Oct-Dec`,`2025_Jan-Mar`)


#### combine the datasets
EID_Dataset <- POC_Sites %>% 
  full_join(EID_ALISRpt, by = "HFacility") %>% 
  full_join(EID_PimaDoneRpt, by = "HFacility") %>% 
  full_join(EIDGxPRpt, by = "HFacility") 

#### re-arrange the data frame and re name some columnes
EID_Dataset   <-   EID_Dataset %>% 
  rename(
    "ALIS-2024(Oct-Dec)"    =   "2024_Oct-Dec.x",
    "ALIS-2025(Jan-Mar)"   =   "2025_Jan-Mar.x",
    "mPima-2024(Oct-Dec)" =    "2024_Oct-Dec.y",
    "mPima-2025(Jan-Mar)"  =   "2025_Jan-Mar.y",
    "GxP-2024(Oct-Dec)"    =    "2024_Oct-Dec",
    "GxP-2025(Jan-Mar)"    =   "2025_Jan-Mar"
  ) %>% 
    select(HFacility,REGION,`IP/REGION`,HUB,EID_Platform,`ALIS-2024(Oct-Dec)`,`mPima-2024(Oct-Dec)`,
           `GxP-2024(Oct-Dec)`,`ALIS-2025(Jan-Mar)`,`mPima-2025(Jan-Mar)`,`GxP-2025(Jan-Mar)`
           ) %>% 
        arrange(REGION)
  
#### calculate total tested and add the new columns 
EID_Dataset <- EID_Dataset %>%
  mutate(`Highest Report-2024(Oct-Dec)` = pmax(`ALIS-2024(Oct-Dec)`,`mPima-2024(Oct-Dec)`,`GxP-2024(Oct-Dec)`,na.rm = TRUE),
         `Highest Report-2025(Jan-Mar)` = pmax(`ALIS-2025(Jan-Mar)`,`mPima-2025(Jan-Mar)`,`GxP-2025(Jan-Mar)`,na.rm = TRUE)
                                                ) %>% 
  ungroup()
## clean names 
EID_Dataset <- EID_Dataset %>%
  mutate(REGION = recode(REGION,
                         "FORT PORTAL" =  "FORTPORTAL"))

#### Ebsuring regions are identified
EID_Dataset %>% 
  filter(is.na(REGION)) %>% 
  distinct(HFacility) %>% 
  print()

# the table
tble_EID_Dataset   <-  flextable(EID_Dataset)

tble_EID_Dataset   <-  tble_EID_Dataset %>% 
  add_header_row(values = "Reported no. of EID tests", colwidths = ncol(EID_Dataset)
  )
tble_EID_Dataset

#### VL Data set ####
#### Number tests done in ALIS by Testing site
VL_ALIS <- VL_Data %>% 
  group_by(HFacility,Yr,Qtr) %>% 
  summarise(ALIS_No = n(),
            .groups = "drop")
# organise table with Yr and Qtr as columns
VL_ALISPvt   <-  VL_ALIS %>% 
  pivot_wider(
    id_cols = HFacility,
    names_from = c(Yr,Qtr),
    values_from = ALIS_No
  ) 
# select period of interest
VL_ALISRpt   <-  VL_ALISPvt %>% 
  select(HFacility,`2024_Oct-Dec`,`2025_Jan-Mar`)

#### Number tests done by m-Pima 
VLPima_done <- VLPima_data %>% 
  group_by(HFacility,Yr,Qtr) %>% 
  summarise(VLonPima = n(),
            .groups = "drop")
# organise table with Yr and Qtr as columns
VLPima_donePvt   <-  VLPima_done %>% 
  pivot_wider(
    id_cols = HFacility,
    names_from = c(Yr,Qtr),
    values_from = VLonPima
  ) 
# select period of interest
VLPima_donePvtRpt   <-  VLPima_donePvt %>% 
  select(HFacility,`2024_Oct-Dec`,`2025_Jan-Mar`)


#### number done on Gxp
VLGxP_Done <- VLGxP %>% 
  group_by(HFacility,Yr,Qtr) %>% 
  summarise(
    GxPVL  =  sum(GxPVL),
    .groups = "drop")
# organise table with Yr and Qtr as columns
VLGxP_DonePvt   <-  VLGxP_Done %>% 
  pivot_wider(
    id_cols = HFacility,
    names_from = c(Yr,Qtr),
    values_from = GxPVL
  ) 
# select period of interest
VLGxPRpt   <-  VLGxP_DonePvt %>% 
  select(HFacility,`2024_Oct-Dec`,`2025_Jan-Mar`)

#### combine the datasets
VL_Dataset <- POC_Sites %>% 
  full_join(VL_ALISRpt, by = "HFacility") %>% 
  full_join(VLPima_donePvtRpt, by = "HFacility") %>% 
  full_join(VLGxPRpt, by = "HFacility") 

#### re-arrange the data frame and re name some columnes
VL_Dataset   <-   VL_Dataset %>% 
  rename(
    "ALIS-2024(Oct-Dec)"    =   "2024_Oct-Dec.x",
    "ALIS-2025(Jan-Mar)"   =   "2025_Jan-Mar.x",
    "mPima-2024(Oct-Dec)" =    "2024_Oct-Dec.y",
    "mPima-2025(Jan-Mar)"  =   "2025_Jan-Mar.y",
    "GxP-2024(Oct-Dec)"    =    "2024_Oct-Dec",
    "GxP-2025(Jan-Mar)"    =   "2025_Jan-Mar"
  ) %>% 
  select(HFacility,REGION,`IP/REGION`,HUB,EID_Platform,`ALIS-2024(Oct-Dec)`,`mPima-2024(Oct-Dec)`,
         `GxP-2024(Oct-Dec)`,`ALIS-2025(Jan-Mar)`,`mPima-2025(Jan-Mar)`,`GxP-2025(Jan-Mar)`
  )

#### Obtain highest number tested across platforms 
VL_Dataset   <-   VL_Dataset %>% 
  mutate(`Highest Report-2024(Oct-Dec)` = pmax(`ALIS-2024(Oct-Dec)`,`mPima-2024(Oct-Dec)`,`GxP-2024(Oct-Dec)`,na.rm = TRUE),
         `Highest Report-2025(Jan-Mar)` = pmax(`ALIS-2025(Jan-Mar)`,`mPima-2025(Jan-Mar)`,`GxP-2025(Jan-Mar)`,na.rm = TRUE)
  ) %>% 
  arrange(REGION)

#### Ebsuring regions are identified
VL_Dataset %>% 
  filter(is.na(REGION)) %>% 
  distinct(HFacility) %>% 
  print()

#### the table
tble_VL_Dataset   <-  flextable(VL_Dataset)

tble_VL_Dataset   <-  tble_VL_Dataset %>% 
  add_header_row(values = "Reported no. of VL tests", colwidths = ncol(VL_Dataset)
  )
tble_VL_Dataset

########################################################################
########################################################################
#### Report - POC EID and VL test columns ####
#### Overall  ####
#### EID tests
EID_Done  <-  EID_Dataset %>% 
  summarise(
    `Test Category`    =  "EID POC Tests",
    Q1   =  sum(`Highest Report-2024(Oct-Dec)`,na.rm = TRUE),
    Q2   =  sum(`Highest Report-2025(Jan-Mar)`,na.rm = TRUE)
  ) 
#### VL tests
VL_Done   <-  VL_Dataset %>% 
  summarise(
    `Test Category`    =  "VL POC Tests",
    Q1   =  sum(`Highest Report-2024(Oct-Dec)`,na.rm = TRUE),
    Q2  =  sum(`Highest Report-2025(Jan-Mar)`,na.rm = TRUE)
  )

EID_VL_Done   <-  rbind(EID_Done,VL_Done)

# the table
tble_EID_VL_Done   <-  flextable(EID_VL_Done)

tble_EID_VL_Done    <-   tble_EID_VL_Done %>% 
  add_header_row(values = "# of POC EID and VL tests done",
                     colwidths = ncol(EID_VL_Done))

tble_EID_VL_Done
#### RRH POC EID by RRH, Qtr  ####  
EIDPOC_RRH <- EID_Dataset %>% 
  group_by(REGION) %>% 
  summarise(
    Q1 = sum(`Highest Report-2024(Oct-Dec)`, na.rm = TRUE),
    Q2 =sum(`Highest Report-2025(Jan-Mar)`, na.rm = TRUE)) 

## Total EID POC done
EID_Nat. <-  EIDPOC_RRH %>% 
  summarise(
    REGION    =  "NATIONAL",
    Q1        =  sum(Q1),
    Q2        =  sum(Q2)
  )

EIDPOC_RRH  <-  rbind(EIDPOC_RRH,EID_Nat.)

# obtain difference between the 2
EIDPOC_RRH <- EIDPOC_RRH %>%
  mutate(
    DiffQ2_Q1 = Q2-Q1
  )

# Create table
EIDPOC <- flextable(EIDPOC_RRH)
EIDPOC

# Add conditional formatting for positive and negative differences
EIDPOC <- theme_vanilla(EIDPOC) %>%
  color(j = "DiffQ2_Q1", color = ifelse(EIDPOC_RRH$DiffQ2_Q1 >= 0, "green", "red"), part = "body"
  )

# add vertical lines to separate Recovered and Died sections
EIDPOC <- EIDPOC %>% 
  vline(j = 1, part = "all") %>% 
  vline(j = 2, part = "all") %>% 
  vline(j = 3, part = "all")

# Align in the centre
EIDPOC <- EIDPOC %>%  
  align(align = "center", j = c(2:4), part = "all") 

EIDPOC <-   EIDPOC %>% 
  add_header_row(values = "No of EID POC tests done by RRH",
                 colwidths = ncol(EIDPOC_RRH))

EIDPOC
#### RRH POC VL by RRH, Qtr ####
POCVL_tests <- VL_Dataset %>% 
  group_by(REGION) %>% 
  summarise(
    Q1 = sum(`Highest Report-2024(Oct-Dec)`, na.rm = TRUE),
    Q2 =sum(`Highest Report-2025(Jan-Mar)`, na.rm = TRUE)) 

## Total EID POC done
VL_Nat. <-  POCVL_tests %>% 
  summarise(
    REGION    =  "NATIONAL",
    Q1        =  sum(Q1),
    Q2        =  sum(Q2)
  )

POCVL_tests  <-  rbind(POCVL_tests,VL_Nat.)

# obtain difference between the 2
POCVL_tests <- POCVL_tests %>%
  mutate(
    DiffQ2_Q1 = Q2-Q1
  )
# Create table
tble_POCVL_tests <- flextable(POCVL_tests)
tble_POCVL_tests

# Add conditional formatting for positive and negative differences
tble_POCVL_tests <- theme_vanilla(tble_POCVL_tests) %>%
  color(j = "DiffQ2_Q1", color = ifelse(POCVL_tests$DiffQ2_Q1 >= 0, "green", "red"), part = "body"
  )

# add vertical lines to separate Recovered and Died sections
tble_POCVL_tests <- tble_POCVL_tests %>% 
  vline(j = 1, part = "all") %>% 
  vline(j = 2, part = "all") %>% 
  vline(j = 3, part = "all")

# Align in the centre
tble_POCVL_tests <- tble_POCVL_tests %>%
  align(align = "center", j = c(2:4), part = "all") 

tble_POCVL_tests    <-   tble_POCVL_tests %>% 
  add_header_row(values = "No of VL POC tests done by RRH",
                 colwidths = ncol(POCVL_tests))

tble_POCVL_tests
####################################################################
####################################################################
#### Report - Outcome of POC testing ####
#### %age of EID tests performed at POC sites, National and RRH #### 
#### Number of conventional tests done by RRH
RRH_con   <-  EIDCon %>%
  filter(!is.na(TDate)) %>% 
  group_by(RRH,Yr,Qtr) %>% 
  summarise(
    Con_Tests   =  n(),
    .groups = "drop"
  ) %>% 
  ungroup()
# Organising the data to have test volumes by quarter
RRH_conPvt   <-   RRH_con %>% 
  pivot_wider(
    id_cols = RRH,
    names_from = c(Yr,Qtr),
    values_from = Con_Tests
  ) %>% 
  select(RRH,`2024_Oct-Dec`,`2025_Jan-Mar`)

#### Creating data frame for EID performed on POC
Prop_EPOC <- EID_Dataset %>% 
  group_by(REGION) %>% 
  summarise(
    `PoC Oct-Dec'24` = sum(`Highest Report-2024(Oct-Dec)`, na.rm = TRUE),
    `PoC Jan-Mar'25` = sum(`Highest Report-2025(Jan-Mar)`, na.rm = TRUE),
    .groups = "drop"
  )

#### Combining the two data frames, POC and Conventional
EID_Prop    <-   Prop_EPOC %>% 
  left_join(RRH_conPvt, by = c("REGION" = "RRH"))

# Calculating the proportions
EID_Prop <- EID_Prop %>%
  mutate(
    `TOct-Dec` = `PoC Oct-Dec'24` + `2024_Oct-Dec`, 
    `TJan-Mar` = `PoC Jan-Mar'25` + `2025_Jan-Mar`
  ) 
# Obtain national values
nat.EID_Prop  <-   EID_Prop %>% 
  summarise(
    REGION   =   "NATIONAL",
    `PoC Oct-Dec'24`  =  sum(`PoC Oct-Dec'24`, na.rm = TRUE),
    `PoC Jan-Mar'25`  =  sum(`PoC Jan-Mar'25`, na.rm = TRUE),
    `2024_Oct-Dec`    =  sum(`2024_Oct-Dec`, na.rm = TRUE),
    `2025_Jan-Mar`    =  sum(`2025_Jan-Mar`, na.rm = TRUE),
    `TOct-Dec`        =  sum(`TOct-Dec`, na.rm = TRUE),
    `TJan-Mar`        =  sum(`TJan-Mar`, na.rm = TRUE)
  )
# Add the national values to the regional data frame
EID_Prop  <-  rbind(EID_Prop,nat.EID_Prop)

#### calculating the proportions
EID_Prop   <-   EID_Prop %>% 
  mutate(
    `PctOct-Dec` = round((`PoC Oct-Dec'24` / `TOct-Dec`) * 100, 0),
    `PctJan-Mar` = round((`PoC Jan-Mar'25` / `TJan-Mar`) * 100, 0)
  )

# selecting columns of interest
EID_PropR <- EID_Prop %>%
  filter(!is.na(REGION)) %>%
  select(REGION,`PctOct-Dec`,`PctJan-Mar`)


# Creating table
tble_EID_PropR <- flextable(EID_PropR)
tble_EID_PropR

# Format the table
tble_EID_PropR <- tble_EID_PropR %>% 
  add_header_row(values = "% of EID tests performed on at POC sites",
                 colwidths = ncol(EID_PropR))


# Adding color shading based on Pct_EPOC values
tble_EID_PropR <- tble_EID_PropR %>%
  bg(j = "PctOct-Dec", bg = ifelse(EID_PropR$`PctOct-Dec` >= 45, "green",
                                   ifelse(EID_PropR$`PctOct-Dec` >= 25 & EID_PropR$`PctOct-Dec` <= 44, "yellow", "red"))) %>%
  bg(j = "PctJan-Mar", bg = ifelse(EID_PropR$`PctJan-Mar` >= 45, "green",
                                   ifelse(EID_PropR$`PctJan-Mar` >= 25 & EID_PropR$`PctJan-Mar` <= 44, "yellow", "red"))
  )

tble_EID_PropR

#### %age of tests performed on PBFW by RRH ####
pbfw_rrh <- VL_Data %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    Tests  = sum(!str_detect(result, regex("Error|Invalid", ignore_case = TRUE))), # Count rows containing 'Error' or 'Invalid'
    No_preg    = sum(replace_na(is_pregnant == "Y",FALSE)),
    No_bf     =  sum(replace_na(is_breastfeeding == "Y",FALSE)),
    .groups = "drop"
  )

#### re arrange the data 
pbfw_regionPvt  <-  pbfw_rrh %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = c(Tests,No_preg,No_bf)
  )

#### Obtain national values
nat.pbfw_region  <-   pbfw_regionPvt %>% 
  summarise(
    RRH   =   "NATIONAL",
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)
    ))


# Add the national values to the regional data frame
pbfw_regionPvt  <-  rbind(pbfw_regionPvt,nat.pbfw_region)


#### calculating the proportion
pbfw_regionRpt <- pbfw_regionPvt %>% 
  rowwise() %>% 
  mutate(
    pbfwOct_Dec = `No_preg_Oct-Dec` + `No_bf_Oct-Dec`,
    pbfwJan_Mar  = `No_preg_Jan-Mar` + `No_bf_Jan-Mar`
  ) %>% 
  ungroup()


# CALCUALTE THE PROPROTION
pbfw_regionRpt <- pbfw_regionRpt %>% 
  rowwise() %>% 
  mutate(
    Pct_pbfwOct_Dec  = round((pbfwOct_Dec/`Tests_Oct-Dec`)*100,0),
    Pct_pbfwJan_Mar  = round((pbfwJan_Mar/`Tests_Jan-Mar`)*100,0)
  )

#### select columns of interest
pbfw_regionRpt <- pbfw_regionRpt %>% 
  select("RRH",starts_with("Pct_pbfw"))


# The report table
pbfw_table <- flextable(pbfw_regionRpt)
# Color the columns based on the targets
pbfw_table   <-    pbfw_table %>% 
  bg(j = "Pct_pbfwOct_Dec", bg = ifelse(pbfw_regionRpt$Pct_pbfwOct_Dec >= 85, "green",
                                ifelse(pbfw_regionRpt$Pct_pbfwOct_Dec >= 50 
                                       & pbfw_regionRpt$Pct_pbfwOct_Dec <= 84, "yellow", "red"))) %>% 
  bg(j = "Pct_pbfwJan_Mar", bg = ifelse(pbfw_regionRpt$Pct_pbfwJan_Mar >= 85, "green",
                                ifelse(pbfw_regionRpt$Pct_pbfwJan_Mar >= 50 
                                       & pbfw_regionRpt$Pct_pbfwJan_Mar <= 84, "yellow", "red"))
  ) 

pbfw_table   <-    pbfw_table %>% 
  add_header_row(values = "%age of PBFW tested through POC",
                 colwidths = ncol(pbfw_region_pivot))

pbfw_table

#### % 1st of infants accessing 1st PCR within 2 months, National, RRH ####
FirstPcr <- EID_Data %>% 
  filter(pcr == "1st PCR")

# total number of HEIs
FirstPcr_all <- FirstPcr %>% 
  filter(Result %in% c("Positive","Negative")) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_HEI = n(),.groups = "drop") 

# Re-organize the data set
FirstPcr_Pvt   <-  FirstPcr_all %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_HEI
  ) 
# national value
nat.FirstPcr_Pvt   <-  FirstPcr_Pvt %>% 
  summarise(
    RRH   =  "NATIONAL",
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  )

#### add the RRH and National data frames
FirstPcr_Pvt  <-  rbind(FirstPcr_Pvt,nat.FirstPcr_Pvt)

#### add the columns on number tested timely
FirstPcr2M <- FirstPcr %>%
  filter(Result %in% c("Positive","Negative")) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    No_timely = sum(Age >= 0 & Age < 2.1, na.rm = TRUE),
    .groups = "drop"
  )

## pivot the data frame to re arrange table
FirstPcr2M_Pvt   <-  FirstPcr2M %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_timely
  )
## national
nat.pcr   <-   FirstPcr2M_Pvt %>% 
  summarise(
    RRH = "NATIONAL",
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  )
### combine the tables
FirstPcr2M_Pvt  <-  rbind(FirstPcr2M_Pvt,nat.pcr)


#### create the data frame to calculate %age of infants at 2month
Pcr_Report <- FirstPcr_Pvt %>% 
  left_join(FirstPcr2M_Pvt, by = "RRH") %>% 
  filter(!is.na(RRH))

# Pct 1st PCR at 2 months table
Pcr_Report <- Pcr_Report %>%  
  mutate(
    across(ends_with(".y"),
           ~ round(.x/get(sub(".y$",".x",cur_column()))*100,0),
           .names = "Pct_{.col}"
    )
  )
#### select columns of interest
Pcr_Report <- Pcr_Report %>%  
  select(RRH,`Pct_Oct-Dec.y`,`Pct_Jan-Mar.y`)

# function to color the columns
timely_colors   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data > 50 & column_data < 85, "yellow","red"))
}

# The table
Table_1st_PCR <- flextable(Pcr_Report)
# format the table
Table_1st_PCR   <-  Table_1st_PCR %>% 
  add_header_row(
    values = c(
      RRH  =  "RRH",
      "%age of HEI accesing timely 1st PCR", ""
    ))

Table_1st_PCR   <-  Table_1st_PCR %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Pct_Oct-Dec.y`   =  "Oct-Dec",
    `Pct_Jan-Mar.y`   =  "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Pct_Oct-Dec.y", "Pct_Jan-Mar.y")

### Apply Background Color ###
for (col in columns) {
  Table_1st_PCR   <-  Table_1st_PCR %>% 
    bg(j = col, bg = timely_colors(Pcr_Report[[col]]), part = "body")
}

Table_1st_PCR   <-  Table_1st_PCR %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
Table_1st_PCR   <-  Table_1st_PCR %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
Table_1st_PCR   <-  Table_1st_PCR %>% 
  vline(j = c(1,2,3), part = "all")


Table_1st_PCR   <-  Table_1st_PCR %>% 
  add_header_row(values = "%age of HEIs accessing timely EID",
                 colwidths = ncol(Pcr_Report))

Table_1st_PCR

#### %age infants at 2 - 12 months, National, RRH ####
FirstPcr2_12m <- FirstPcr %>% 
  filter(pcr == "1st PCR")

# total number of HEIs
Pcr2_12m <- FirstPcr2_12m %>% 
  filter(Result %in% c("Positive","Negative")) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_HEI = n(),.groups = "drop") 

# Re-organize the data set
Pcr2_12m_Pvt   <-  Pcr2_12m %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_HEI
  ) 
# national value
nat.Pcr2_12m_Pvt   <-  Pcr2_12m_Pvt %>% 
  summarise(
    RRH   =  "NATIONAL",
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  )

#### add the RRH and National data frames
Pcr2_12m_Pvt  <-  rbind(Pcr2_12m_Pvt,nat.Pcr2_12m_Pvt)

#### add the columns on number tested timely
Pcr2M_12 <- FirstPcr %>%
  filter(Result %in% c("Positive","Negative")) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    No_timely = sum(Age >= 2 & Age <=12, na.rm = TRUE),
    .groups = "drop"
  )

## pivot the data frame to re arrange table
Pcr2M_12_Pvt   <-  Pcr2M_12 %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_timely
  )
## national
nat.pcr12m   <-   Pcr2M_12_Pvt %>% 
  summarise(
    RRH = "NATIONAL",
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  )
### combine the tables
Pcr2M_12_Pvt  <-  rbind(Pcr2M_12_Pvt,nat.pcr12m)


#### create the data frame to calculate %age of infants at 2month
Pcr12m_Report <- Pcr2_12m_Pvt %>% 
  left_join(Pcr2M_12_Pvt, by = "RRH") %>% 
  filter(!is.na(RRH))

# Pct 1st PCR at 2 months table
Pcr12m_Report <- Pcr12m_Report %>%  
  mutate(
    across(ends_with(".y"),
           ~ round(.x/get(sub(".y$",".x",cur_column()))*100,0),
           .names = "Pct_{.col}"
    )
  )
#### select columns of interest
Pcr12m_Report <- Pcr12m_Report %>%  
  select(RRH,`Pct_Oct-Dec.y`,`Pct_Jan-Mar.y`)

# The table
Table_Pcr12m_Report <- flextable(Pcr12m_Report)
# format the table
Table_Pcr12m_Report   <-  Table_Pcr12m_Report %>% 
  add_header_row(
    values = c(
      RRH  =  "RRH",
      "%age of HEI accesing 1st PCR at 2-12m", ""
    ))

Table_Pcr12m_Report   <-  Table_Pcr12m_Report %>%
  set_header_labels(
    RRH  =  "RRH",
    `Pct_Oct-Dec.y`   =  "Oct-Dec",
    `Pct_Jan-Mar.y`   =  "Jan-Mar"
  )

Table_Pcr12m_Report   <-  Table_Pcr12m_Report %>%
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
Table_Pcr12m_Report   <-  Table_Pcr12m_Report %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
Table_Pcr12m_Report   <-  Table_Pcr12m_Report %>%
  vline(j = c(1,2,3), part = "all")


Table_Pcr12m_Report   <-  Table_Pcr12m_Report %>%
  add_header_row(values = "%age of HEIs accessing 1st PCR at 2-12m",
                 colwidths = ncol(Pcr12m_Report))
Table_Pcr12m_Report
#### %age 1st PCR at 12 months, nataional, RRH ####
FirstPcr_12m <- FirstPcr %>% 
  group_by(RRH) %>% 
  summarise(
    No_HEIs = n(),
    No_12m = sum(Age >= 0 & Age < 12.1))

# National
Nat.12mths <- FirstPcr_12m %>% 
  summarise(
    RRH = "National",
    No_HEIs  = sum(No_HEIs),
    No_12m   = sum(No_12m)
  )

FirstPcr_12m <- bind_rows(FirstPcr_12m,Nat.12mths)

# Pct 1st PCR at 2 - 12 months table
FirstPcr_12m <- FirstPcr_12m %>% 
  mutate(
    Pct12M = round((No_12m/No_HEIs)*100,0)
  )
# The table
Table_1PCR_12 <- flextable(FirstPcr_12m)
Table_1PCR_12

#### the age at Positivity at 1st PCR  ####
posi_1stPCR  <- FirstPcr %>% 
  filter(Result %in% c("Positive","Negative")) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(pcr  ==  "1st PCR") %>% 
  group_by(Qtr) %>% 
  summarise(
    `2m`           = sum(Age >= 0 & Age < 2.1),
    `2m-12m`       = sum(Age >  2.1 & Age < 12.1),
    `>12m`         = sum(Age > 12)
  )
# change to longer
posi_1stPCR <- posi_1stPCR %>% 
  pivot_longer(
    cols      = c("2m","2m-12m",">12m"),
    names_to  = "Age Bracket",
    values_to = "No. of HEI"
  )

### identified positives
pos_identified <- EID_Data %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  filter(Result == "Positive") %>% 
  group_by(Qtr) %>% 
  summarise(
    `2m`           = sum(Age < 2.1),
    `2m-12m`       = sum(Age >  2.1 & Age < 12.1),
    `>12m`         = sum(Age > 12)
  )
#  re-arrange the data frame - change to longer
pos_identified  <- pos_identified %>% 
  pivot_longer(
    cols = c("2m","2m-12m",">12m"),
    names_to = "Age Bracket",
    values_to = "Number Positive"
  )

# Add tables
posi_report  <-  posi_1stPCR %>% 
  left_join(pos_identified, by = c("Qtr","Age Bracket"))

#### calculate positives against numbers tested
posi_report  <-  posi_report %>% 
  mutate(
    `%Positive`  = round((`Number Positive`/`No. of HEI`)*100,1)
  )
#### re-organize data frame
posi_reportdf  <-  posi_report %>%
  select(Qtr,`Age Bracket`,`%Positive`)

### re-arrange periods
posi_reportdf   <-   posi_reportdf %>% 
  mutate(
    `Age Bracket` = factor(`Age Bracket`, levels = c("2m", "2m-12m", ">12m")),
    Qtr           = factor(Qtr, levels =  c("Oct-Dec","Jan-Mar"))
  )

# The graph
ggplot(posi_reportdf, aes(x = `Age Bracket`, y = `%Positive`, fill = Qtr)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = `%Positive`), vjust = -0.3, size = 3.5, position = position_dodge(width = 0.9)) +  # Ensure text aligns with bars
  theme_minimal() +
  labs(
    title = "Percentage identified Positives by Age Bracket",
    x = "Age Bracket",
    y = "Percentage of Positives"
  )

################################################################
################################################################
#### POC TAT ####
#### Weekly EID TAT  ####
WklyTAT    <-  EID_Data %>% 
  mutate(
    CDate = as.Date(CollectionDate),  # Ensure Date format
    TDate = as.Date(TestDate),  
    Wk   =  week(CollectionDate)
  ) %>% 
  mutate(                 # Calculate the TAT
    LabTAT    =   as.numeric(TestDate - CollectionDate)  
  ) %>% 
  filter(LabTAT >= 0 & LabTAT < 30)     #  Filter outliers
### change Wk to character
WklyTAT$Wk  <-  as.character(WklyTAT$Wk)

#### weekly report
WklyTATRpt  <-  WklyTAT  %>% 
    group_by(Yr, Wk)  %>% 
  summarise(
    TATL  =  median(LabTAT, na.rm = TRUE), .groups = "drop"
  ) %>% 
   arrange(Yr, Wk)  # Ensure correct ordering

### Ensuring correct order of the Weeks
WklyTAT   <-   WklyTAT %>% 
  mutate(
    Wk   =  factor(Wk, levels = unique(Wk))
  )
#### the graph
ggplot(WklyTAT, aes(x=Wk, y=LabTAT, fill = Wk)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")+
  labs(
    title = "Weekly EID POC Lab TAT"
  )

#### Quarterly EID POC LabTAT ####
QtrPoCTAT <- WklyTAT %>% 
  group_by(Yr, Qtr) %>% 
  summarise(
    QLabTAT = median(LabTAT, na.rm = TRUE),  # Ensure NA handling
    .groups = "drop"
  ) %>%
  arrange(Yr, Qtr)  # Ensure correct order


#### the table
tbl_QtrPoCTAT  <-  flextable(QtrPoCTAT)

tbl_QtrPoCTAT   <-  tbl_QtrPoCTAT %>% 
  add_header_row(values = "EID POC Lab TAT",
                 colwidths = ncol(QtrPoCTAT))
  tbl_QtrPoCTAT

#### Report - %age of Sample results released in 1 day TAT ####
PctTimely   <-   WklyTAT %>% 
  filter(Qtr   !=  "Jul-Sept") %>% 
  group_by(Yr,Qtr) %>% 
    summarise(
      No_samples  = n(),
      No_1Day     =  sum(LabTAT == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      Pct_1DayTAT  =  round((No_1Day/No_samples)*100,0)
    )

###  the graph
grp_PctTimely   <-   PctTimely %>% 
  mutate(
    Category   =  case_when(
      Pct_1DayTAT   >= 75   ~  ">75%",
      Pct_1DayTAT    > 50   &  Pct_1DayTAT < 75  ~   "50%-74%",
      Pct_1DayTAT    < 50   ~   "<50%"
  ))
### Ensuring order
grp_PctTimely   <-   grp_PctTimely %>% 
  mutate(
    Qtr   =  factor(Qtr, levels = c("Jul-Sept", "Oct-Dec", "Jan-Mar"))
  )

# the graph
ggplot(grp_PctTimely, aes(x=Qtr, y=Pct_1DayTAT,fill = Category))+
  geom_col()+
  geom_text(aes(label = Pct_1DayTAT))+
  scale_fill_manual(values  =  c(">75%"="green","50%-74%"="yellow","<50%"="red"))+
    labs(
      title = "%age of EID POC lab results released on same day",
      x = "Quarter",
      y = "%age of samples"
    )

#### Report - Quarterly EID POC LabTAT by RRH ####
RQtrPoCTAT <- WklyTAT %>% 
  filter(Qtr   !=  "Jul-Sept") %>%
  group_by(RRH, Yr, Qtr) %>% 
  summarise(
    No_samples  =  n(),
    QLabTAT = median(LabTAT, na.rm = TRUE),  # Ensure NA handling
    .groups = "drop"
  ) %>%
  arrange(Yr, Qtr)  # Ensure correct order


#### Pivot for period
pvt_RQtrPoCTAT   <-   RQtrPoCTAT %>% 
  pivot_wider(
    names_from = c(Yr,Qtr),
    values_from = c(No_samples,QLabTAT)
  )

#### set table coloring
row_color   <-  function(column_data){
  ifelse(column_data == 0, "green",
    ifelse(column_data > 0 & column_data <= 3, "yellow","red"))
      }

### The table
tbl_pvt_RQtr  <-  flextable(pvt_RQtrPoCTAT)
tbl_pvt_RQtr

### Format the Table Headers ###
tbl_pvt_RQtr <- tbl_pvt_RQtr %>% 
  add_header_row(
    values = c(RRH      =  "RRH", 
               "No. of reported samples tested by Qtr","",
               "LabTAT for respective quarters","")
        ) 


tbl_pvt_RQtr <- tbl_pvt_RQtr %>% 
  set_header_labels(
    RRH = "RRH",
    `No_samples_2024_Oct-Dec` = "Oct-Dec",
    `No_samples_2025_Jan-Mar` = "Jan-Mar",
    `QLabTAT_2024_Oct-Dec` = "Oct-Dec TAT",
    `QLabTAT_2025_Jan-Mar` = "Jan-Mar TAT"
  )

### List Columns to Apply Coloring ###
columns <- c("QLabTAT_2024_Oct-Dec", "QLabTAT_2025_Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tbl_pvt_RQtr <- tbl_pvt_RQtr %>%
    bg(j = col, bg = row_color(pvt_RQtrPoCTAT[[col]]), part = "body")
}

tbl_pvt_RQtr   <-   tbl_pvt_RQtr %>% 
# Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertically for the first five columns in the header
tbl_pvt_RQtr   <-   tbl_pvt_RQtr %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_pvt_RQtr   <-   tbl_pvt_RQtr %>% 
  vline(j = c(1, 3, 5), part = "all")


tbl_pvt_RQtr   <-   tbl_pvt_RQtr %>% 
  add_header_row(values = "EID POC Lab TAT by RRH",
                 colwidths = ncol(pvt_RQtrPoCTAT))


tbl_pvt_RQtr

#### Report - RRH %age of Sample results released in 1 day TAT ####
RPctTimely   <-   WklyTAT %>% 
  filter(Qtr   !=  "Jul-Sept") %>% 
  group_by(RRH,Yr,Qtr) %>% 
  summarise(
    No_samples  = n(),
    No_1Day     =  sum(LabTAT == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    Pct_1DayTAT  =  round((No_1Day/No_samples)*100,0)
  )

#### Pivot the table
Pvt_RPct   <-   RPctTimely %>% 
  pivot_wider(
    id_cols = RRH,
    names_from = c(Yr,Qtr),
    values_from = c(No_samples,Pct_1DayTAT)
  )

#### plan for color in table
applyrow_color   <-  function(column_data){
  ifelse(column_data >= 75, "green",
         ifelse(column_data > 50 & column_data < 75, "yellow","red"))
}

#### The table
tble_RPctTimely   <-  flextable(Pvt_RPct)

# format the table
tble_RPctTimely   <-   tble_RPctTimely %>% 
  add_header_row(
    values = c(RRH      =  "RRH", 
               "No. of reported samples tested by Qtr","",
               "%age of samples released within 0 Day TAT for respective quarters","")
  ) 

# Set the header
tble_RPctTimely   <-   tble_RPctTimely %>% 
  set_header_labels(
    RRH = "RRH",
    `No_samples_2024_Oct-Dec` = "Oct-Dec",
    `No_samples_2025_Jan-Mar` = "Jan-Mar",
    `Pct_1DayTAT_2024_Oct-Dec` = "Oct-Dec TAT",
    `Pct_1DayTAT_2025_Jan-Mar` = "Jan-Mar TAT"
  )

### List Columns to Apply Coloring ###
columns <- c("Pct_1DayTAT_2024_Oct-Dec", "Pct_1DayTAT_2025_Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tble_RPctTimely   <-   tble_RPctTimely %>% 
    bg(j = col, bg = applyrow_color(Pvt_RPct[[col]]), part = "body")
}

tble_RPctTimely   <-   tble_RPctTimely %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertically for the first five columns in the header
tble_RPctTimely   <-   tble_RPctTimely %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_pvt_RQtr   <-   tbl_pvt_RQtr %>% 
  vline(j = c(1, 3, 5), part = "all")


tble_RPctTimely   <-   tble_RPctTimely %>% 
  add_header_row(values = "%age of EID POC lab results released on same day",
                 colwidths = ncol(pvt_RQtrPoCTAT))


tble_RPctTimely

###################################################################
###################################################################
#### Report - Connectivity ####
#### Report - Number of POC sites reporting in different databases ####
sites_reporting <- EID_Dataset %>%
  reframe(
    Database = c("ALIS","Sympheous","LabXpert"),
    `No.Expected Sites`  =  c(
      sum(EID_Platform %in% c("GeneXpert", "16 Module GeneXpert", "m-PIMA"), na.rm = TRUE), # ALIS
      sum(EID_Platform == "m-PIMA", na.rm = TRUE),                                          # Sympheous
      sum(EID_Platform %in% c("GeneXpert", "16 Module GeneXpert"), na.rm = TRUE)            # LabXpert
    ),
    Q1 = c(
      n_distinct(HFacility[`ALIS-2024(Oct-Dec)` > 0]),
      n_distinct(HFacility[`mPima-2024(Oct-Dec)` > 0]),
      n_distinct(HFacility[`GxP-2024(Oct-Dec)` > 0])
    ),
    Q2  =  c(
      n_distinct(HFacility[`ALIS-2025(Jan-Mar)` > 0]),
      n_distinct(HFacility[`mPima-2025(Jan-Mar)` > 0]),
      n_distinct(HFacility[`GxP-2025(Jan-Mar)` > 0])
    ))

#### calculating the proportions
sites_reporting  <-   sites_reporting %>% 
  rowwise() %>% 
  mutate(
    PctQ1   =  round((Q1/`No.Expected Sites`)*100,0),
    PctQ2   =  round((Q2/`No.Expected Sites`)*100,0)
  )



connect_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data > 50 & column_data <= 84, "yellow","red"))
}


#### the table
sites_connectivty <- flextable(sites_reporting)

sites_connectivty  <-   sites_connectivty %>% 
  add_header_row(
    values = c(
      Database   =  "Database",
      `No.Expected Sites`  =  "No.Expected Sites",
      "No. of sites reporting in the indicated database in the respective quarters", "",
      "%age of sites reporting in the indicated database in the respective quarters", ""
    )
  )

#### set header
sites_connectivty  <-   sites_connectivty %>% 
  set_header_labels(
    Database   =  "Database",
    `No.Expected Sites`  =  "No.Expected Sites",
    Q1   =  "Oct-Dec",
    Q2   =  "Jan-Mar",
    PctQ1  =  "Oct-Dec",
    PctQ2   =  "Jan-Mar"
  )

# followed by code like this
### List Columns to Apply Coloring
columns <- c("PctQ1", "PctQ2")


### Apply Background Color ###
for (col in columns) {
  sites_connectivty  <-   sites_connectivty %>% 
    bg(j = col, bg = connect_color(sites_reporting[[col]]), part = "body")
}


#### merge column
sites_connectivty  <-   sites_connectivty %>%
  merge_at(i = 1, j=3:4, part = "header") %>% 
  merge_at(i = 1, j=5:6, part = "header")

#### merge vertical columns
sites_connectivty  <-   sites_connectivty %>%
  merge_v(j=1, part = "header") %>% 
  merge_v(j=2, part = "header")

#### Add vertical lines
sites_connectivty  <-   sites_connectivty %>%
  vline(j=c(1,2,4,6), part = "all")


#### the title
sites_connectivty  <-   sites_connectivty %>%
  add_header_row(values = "%age of sites uploading data in the respective databses",
                 colwidths = ncol(sites_reporting))

sites_connectivty

#### Report - Number of POC sites reporting in different databases by RRH ####
rrhsites_reporting <- EID_Dataset %>%
  group_by(REGION) %>% 
  reframe(
    Database = c("ALIS","Sympheous","LabXpert"),
    `No.Expected Sites`  =  c(
      sum(EID_Platform %in% c("GeneXpert", "16 Module GeneXpert", "m-PIMA"), na.rm = TRUE) + sum(is.na(EID_Platform)), # ALIS
      sum(EID_Platform == "m-PIMA", na.rm = TRUE),                                          # Sympheous
      sum(EID_Platform %in% c("GeneXpert", "16 Module GeneXpert"), na.rm = TRUE)            # LabXpert
    ),
    Q1 = c(
      n_distinct(HFacility[`ALIS-2024(Oct-Dec)` > 0]),
      n_distinct(HFacility[`mPima-2024(Oct-Dec)` > 0]),
      n_distinct(HFacility[`GxP-2024(Oct-Dec)` > 0])
    ),
    Q2  =  c(
      n_distinct(HFacility[`ALIS-2025(Jan-Mar)` > 0]),
      n_distinct(HFacility[`mPima-2025(Jan-Mar)` > 0]),
      n_distinct(HFacility[`GxP-2025(Jan-Mar)` > 0])
    ))
#### creating proportions
rrhsites_reportingdf   <-   rrhsites_reporting %>%
  rowwise() %>% 
  mutate(
    Props_Q1  =  paste0(Q1, "/", `No.Expected Sites`),
    Props_Q2  =  paste0(Q2, "/", `No.Expected Sites`),
    PctQ1     =   round((Q1/`No.Expected Sites`)*100,0),
    PctQ2     =   round((Q2/`No.Expected Sites`)*100,0)
  ) %>% 
  select(REGION, Database, Props_Q1, Props_Q2, PctQ1, PctQ2)

#### re arranging the data frame
rrhsites_rptPvt   <-  rrhsites_reportingdf %>% 
  pivot_wider(
    names_from = Database,
    values_from = c(Props_Q1,Props_Q2, PctQ1, PctQ2)
  ) %>% 
    select(REGION, Props_Q1_ALIS, Props_Q2_ALIS, Props_Q1_Sympheous, Props_Q2_Sympheous,
           Props_Q1_LabXpert, Props_Q2_LabXpert, PctQ1_ALIS, PctQ2_ALIS, PctQ1_Sympheous,
           PctQ2_Sympheous, PctQ1_LabXpert, PctQ2_LabXpert)
          
connect_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data > 50 & column_data <= 84, "yellow","red"))
}

# the table
tble_rrhsites_rptPvt <- flextable(rrhsites_rptPvt)

tble_rrhsites_rptPvt    <-   tble_rrhsites_rptPvt %>% 
  add_header_row(
    values = c(
      REGION   =  "RRH",
      "Proportion of ALIS sites with ALIS connectivity across Qtrs", "", 
      "Proportion of m-Pima sites with Sympheous connectivity across Qtrs", "",
      "Proportion of GxP sites with LabXpert connectivity across Qtrs", "",
      "%age of ALIS sites with ALIS connectivity across Qtrs", "", 
      "%age of m-Pima sites with Sympheous connectivity across Qtrs", "",
      "%age of GxP sites with LabXpert connectivity across Qtrs", ""
    )
  )

#### set header
tble_rrhsites_rptPvt    <-   tble_rrhsites_rptPvt %>% 
  set_header_labels(
    Database   =  "Database",
    `No.Expected Sites`  =  "No.Expected Sites",
    Props_Q1_ALIS   =  "Oct-Dec",
    Props_Q2_ALIS   =  "Jan-Mar",
    Props_Q1_Sympheous  =  "Oct-Dec",
    Props_Q2_Sympheous   =  "Jan-Mar",
    Props_Q1_LabXpert   =  "Oct-Dec",
    Props_Q2_LabXpert   =  "Jan-Mar",
    PctQ1_ALIS  =  "Oct-Dec",
    PctQ2_ALIS   =  "Jan-Mar",
    PctQ1_Sympheous   =  "Oct-Dec",
    PctQ2_Sympheous   =  "Jan-Mar",
    PctQ1_LabXpert  =  "Oct-Dec",
    PctQ2_LabXpert   =  "Jan-Mar"
  )

# followed by code like this
### List Columns to Apply Coloring
columns <- names(rrhsites_rptPvt)[which(names(rrhsites_rptPvt) == "PctQ1_ALIS"):which(names(rrhsites_rptPvt) == "PctQ2_LabXpert")]


### Apply Background Color ###
for (col in columns) {
  tble_rrhsites_rptPvt    <-   tble_rrhsites_rptPvt %>% 
    bg(j = col, bg = connect_color(rrhsites_rptPvt[[col]]), part = "body")
}

#### merge column
tble_rrhsites_rptPvt    <-   tble_rrhsites_rptPvt %>% 
  merge_at(i = 1, j=2:3, part = "header") %>% 
  merge_at(i = 1, j=4:5, part = "header") %>% 
  merge_at(i = 1, j=6:7, part = "header") %>% 
  merge_at(i = 1, j=8:9, part = "header") %>%
  merge_at(i = 1, j=10:11, part = "header") %>% 
  merge_at(i = 1, j=12:13, part = "header")

#### merge vertical columns
tble_rrhsites_rptPvt    <-   tble_rrhsites_rptPvt %>% 
  merge_v(j=1, part = "header") 

#### Add vertical lines
tble_rrhsites_rptPvt    <-   tble_rrhsites_rptPvt %>% 
  vline(j=c(1, 3, 5, 7, 9, 11, 13), part = "all")


#### the title
tble_rrhsites_rptPvt    <-   tble_rrhsites_rptPvt %>% 
  add_header_row(values = "%age of sites with connectivity in the respective databses",
                 colwidths = ncol(rrhsites_rptPvt))


tble_rrhsites_rptPvt

##################################################################
##################################################################
#### Disease multi testing ####
### Report - #  tests on GxP machines, proportions of tests ####
# Join GxP tests to POC Sites#
dfP_Sites <- TBGxP %>% 
  full_join(EIDGxP %>% 
              distinct(HFacility, Qtr, .keep_all = TRUE) %>% 
              select(Qtr, HFacility, GxPEID), 
            by = c("HFacility", "Qtr"))%>% 
  
  full_join(VLGxP %>% 
              distinct(HFacility, Qtr, .keep_all = TRUE) %>% 
              select(Qtr, HFacility, GxPVL), 
            by = c("HFacility", "Qtr")) %>% 
  
  full_join(HPVGxP %>% 
              distinct(HFacility, Qtr, .keep_all = TRUE) %>% 
              select(Qtr, HFacility, GxPHPV), 
            by = c("HFacility", "Qtr")) %>% 
  
  mutate(GxPEID = replace_na(GxPEID, 0),
         GxPVL = replace_na(GxPVL, 0),
         GxPHPV = replace_na(GxPHPV, 0),
         GxPTB = replace_na(GxPTB, 0))

# GxP Multi testing report
# add columns for target based on the number of modules
dfP_Sites <- dfP_Sites %>% 
  mutate(
    EWk  = module*3*5,
    EMon = module*3*21,
    EQtr = module*3*63)

# Add the number of tests performed
dfP_Sites <- dfP_Sites %>% 
  mutate(Total_tests = GxPEID+GxPVL+GxPHPV+GxPTB)

# Proportions of of each test category
dfP_Sites <- dfP_Sites %>% 
  mutate(
    Pct_EID = round(ifelse(is.nan(GxPEID / Total_tests), 0, (GxPEID / Total_tests) * 100), 1),
    Pct_VL = round(ifelse(is.nan(GxPVL / Total_tests), 0, (GxPVL / Total_tests) * 100), 1),
    Pct_HPV = round(ifelse(is.nan(GxPHPV / Total_tests), 0, (GxPHPV / Total_tests) * 100), 1),
    Pct_TB = round(ifelse(is.nan(GxPTB / Total_tests), 0, (GxPTB / Total_tests) * 100), 1)
  )

# Filtering the multi testing sites
dfP_Sites  <- dfP_Sites %>% 
  select("HFacility","District","RRH","Qtr","GxPEID","GxPVL","GxPHPV","GxPTB","module","EWk","EMon","EQtr","Total_tests","Pct_EID","Pct_VL","Pct_HPV","Pct_TB"
  )
## calculate total number of tests
multi_test <- dfP_Sites %>% 
  rowwise() %>% 
  mutate(no_tests = sum(c_across(`GxPEID`:`GxPTB`)> 0)) %>% 
  ungroup()
# filter out sites with more than 1 test
fn_multi_test <- multi_test %>% 
  filter(no_tests > 1)

#### Report
RRH_multiTest <- fn_multi_test %>% 
  filter(Qtr   == "Oct-Dec") %>% 
  group_by(RRH) %>% 
  summarise(
    Qtr   =  "Jan-Mar",
    No.TB  = sum(GxPTB),
    No.EID = sum(GxPEID),
    No.VL  = sum(GxPVL),
    No.HPV = sum(GxPHPV)
  ) %>% 
  rowwise() %>% 
  mutate(
    Total = sum(No.TB +No.EID+No.VL+No.HPV)) %>% 
  ungroup()

# National Multi testing
Nat.Multi <- RRH_multiTest %>% 
  summarise(
    RRH   = "National",
    Qtr   =  "Jan-Mar",
    No.TB  = sum(No.TB),
    No.EID = sum(No.EID),
    No.VL  = sum(No.VL),
    No.HPV = sum(No.HPV),
    Total  = sum(Total)
  )
# Adding total to RRH dataset
RRH_multiTest <- bind_rows(RRH_multiTest,Nat.Multi)

# Create table
RRH_MT <- flextable(RRH_multiTest)

RRH_MT    <-   RRH_MT %>% 
  add_header_row(values = "# of each test category by RRH",
                 colwidths = ncol(RRH_multiTest))

RRH_MT

#### Report - Number tested for each test category, summary ####
multi_tests   <-  fn_multi_test %>% 
  group_by(Qtr) %>% 
  summarise(
    GxPTB     =   sum(GxPTB),
    GxPEID    =   sum(GxPEID),
    GxPVL     =   sum(GxPVL),
    GxPHPV    =   sum(GxPHPV),
    Total     =   sum(Total_tests)
  )
# Re arrange the table
multi_testsPct   <-    multi_tests %>% 
  pivot_longer(
    cols = c(starts_with("GxP"),Total),
    names_to = "Test Category",
    values_to = "No.Tests"
  )
# pivot wider
multi_testsPvt    <-   multi_testsPct %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No.Tests
  ) %>% 
  select(`Test Category`,`Oct-Dec`,`Jan-Mar`)

#### rename the test categories
multi_testsPvt   <-   multi_testsPvt %>% 
  mutate(`Test Category`    =  recode(`Test Category`,
                                      "GxPTB"  =   "TB",
                                      "GxPEID"  =   "EID",
                                      "GxPVL"   =   "VL",
                                      "GxPHPV"    =    "HPV"))

#### the table
tbl_multi_testsPvt   <-  flextable(multi_testsPvt)

tbl_multi_testsPvt   <-   tbl_multi_testsPvt %>% 
  add_header_row(values = "No. of tests done by test category",
                 colwidths = ncol(multi_testsPvt))

tbl_multi_testsPvt
#### Report - Number of HFs performing each of the test category ####

#### Obtain the number of sites implementing multi-disease testing
GxP_Sites   <-   fn_multi_test %>%
  group_by(Qtr) %>% 
  summarise(
    No.Sites  =  n_distinct(HFacility)
  ) 
# Re arrange the table
GxP_SitesPvt  <-    GxP_Sites%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No.Sites
  )
#### value by qtr
SOct_Dec  <-  GxP_SitesPvt$`Oct-Dec`
SJan_Mar  <-  GxP_SitesPvt$`Jan-Mar` 


#### Obtain the number of sites performing each test
No.faci_gxp_tests <- fn_multi_test %>%
  group_by(Qtr) %>% 
  summarise(
    No.TB  = n_distinct(HFacility[GxPTB > 0]),
    No.EID = n_distinct(HFacility[GxPEID > 0]),
    No.VL  = n_distinct(HFacility[GxPVL > 0]),
    No.HPV = n_distinct(HFacility[GxPHPV > 0])
  )

# Re arrange the table
No.gxp_tests   <-    No.faci_gxp_tests%>% 
  pivot_longer(
    cols =   starts_with("No"),
    names_to = "Test Category",
    values_to = "No.Tests"
  )

# pivot wider
No.gxpPvt    <-   No.gxp_tests %>% 
  pivot_wider(
    id_cols =  `Test Category`,
    names_from = Qtr,
    values_from = No.Tests
  )
#### add percentages columns
GxPTests   <-  No.gxpPvt %>% 
  rowwise() %>% 
  mutate(
    Pct_Oct_Dec  = round((`Oct-Dec`/SOct_Dec)*100,0),
    Pct_Jan_Mar  =  round((`Jan-Mar`/SOct_Dec)*100,0)
  )

#### rename the test categories
GxPTests   <-   GxPTests %>% 
  mutate(`Test Category`    =  recode(`Test Category`,
                                      "No.TB"  =   "TB",
                                      "No.EID"  =   "EID",
                                      "No.VL"   =   "VL",
                                      "No.HPV"    =    "HPV"))

#### creating the table

Multi_summary <- flextable(GxPTests)

#### adding headers
Multi_summary   <-   Multi_summary %>% 
  add_header_row(
    values = c(
      `Test Category`   =  "Test Category",
      "No. sites performing the test in the respective quarters","",
      "%age of multi-testing sites performing the tests in the respective quarters",""
    )
  )
#### set header columns
Multi_summary   <-   Multi_summary %>% 
  set_header_labels(
    `Test Category`   =  "Test Category",
    `Jan-Mar`         =  "Jan-Mar",
    `Oct-Dec`         =  "Oct-Dec",
    `Pct_Oct_Dec`     =  "Oct_Dec",
    `Pct_Jan_Mar`     =  "Jan_Mar"
  )

Multi_summary   <-   Multi_summary %>% 
  # Merging columns 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertically for the first five columns in the header
Multi_summary   <-   Multi_summary %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
Multi_summary   <-   Multi_summary %>%  
  vline(j = c(1,3,5), part = "all")


#### Add header title
Multi_summary <- Multi_summary %>%
  add_header_row(
    values = "% of multi-disease testing sites performing the targeted multi-testing tests",
    colwidths = ncol(GxPTests) # Ensure colwidths matches Multi_summary
  )

Multi_summary
#### Report -  %ge of HFs performing each of the test category by RRH ####

#### Obtain the number of sites implementing multi-disease testing
RGxP_Sites   <-   fn_multi_test %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    No.Sites  =  n_distinct(HFacility),
    .groups = "drop"
  ) 
# Re arrange the table
RGxP_SitesPvt  <-    RGxP_Sites%>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No.Sites
  )

#### Obtain the number of sites performing each test
RNo.faci_gxp_tests <- fn_multi_test %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    No.TB  = n_distinct(HFacility[GxPTB > 0]),
    No.EID = n_distinct(HFacility[GxPEID > 0]),
    No.VL  = n_distinct(HFacility[GxPVL > 0]),
    No.HPV = n_distinct(HFacility[GxPHPV > 0])
  )

# Re arrange the table
RNo.gxp_tests   <-    RNo.faci_gxp_tests%>% 
  pivot_longer(
    cols =   starts_with("No"),
    names_to = "Test Category",
    values_to = "No.Tests"
  )

# pivot wider
RNo.gxpPvt    <-   RNo.gxp_tests %>% 
  pivot_wider(
    names_from = c(Qtr, `Test Category`),  # Columns to spread out
    values_from = No.Tests
  )

#### Joining the multi testing sites to sites testing specific tests
RGxP_SitesPvt   <-  RGxP_SitesPvt %>% 
  left_join(RNo.gxpPvt, by = "RRH")

#### Adding the percentages 
RGxP_Report   <-   RGxP_SitesPvt %>% 
  rowwise() %>% 
  mutate(across(
    .cols  =  contains("Jan-Mar_No"),
    .fns   =  ~ round((.x/`Jan-Mar`)*100,0),
    .names =  "Pct{.col}"
  )) %>% 
  mutate(across(
    .cols   =   contains("Oct-Dec_No"),
    .fns    =   ~ round((.x/`Oct-Dec`)*100,0),
    .names  =   "Pct{.col}"
  )) %>% 
  ungroup()

#### selecting columns of interest\
RGxP_Report   <-   RGxP_Report %>% 
  select(RRH,`Oct-Dec`,`Jan-Mar`,starts_with("PctOct-Dec"),starts_with("PctJan-Mar")
  )

#### creating the table

tbl_RGxP_Report <- flextable(RGxP_Report)

#### adding headers
tbl_RGxP_Report   <-   tbl_RGxP_Report %>% 
  add_header_row(
    values = c(
      RRH   =  "RRH",
      "No. of sites implementing multi-disease testing sites by Qtr","",
      "%age of multi-disease testing sites performing the respective tests in Qtr Oct-Dec","","","",
      "%age of multi-disease testing sites performing the respective tests in Qtr Jan-Mar","","",""
    )
  )
#### set header columns
tbl_RGxP_Report   <-   tbl_RGxP_Report %>% 
  set_header_labels(
    RRH   =  "RRH",
    `Oct-Dec`         =  "Oct-Dec",
    `Jan-Mar`         =  "Jan-Mar",
    `PctOct-Dec_No.TB`     =  "TB",
    `PctOct-Dec_No.EID`     =  "EID",
    `PctOct-Dec_No.VL`      =   "VL",
    `PctOct-Dec_No.HPV`     =  "HPV",
    `PctJan-Mar_No.TB`     =  "TB",
    `PctJan-Mar_No.EID`     =  "EID",
    `PctJan-Mar_No.VL`      =   "VL",
    `PctJan-Mar_No.HPV`     =  "HPV"
  )

# Merging columns 
tbl_RGxP_Report   <-   tbl_RGxP_Report %>% 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:7, part = "header") %>% 
  merge_at(i = 1, j = 8:11, part = "header")

# Merging vertically for the first five columns in the header
tbl_RGxP_Report   <-   tbl_RGxP_Report %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_RGxP_Report   <-   tbl_RGxP_Report %>% 
  vline(j = c(1,3,7,11), part = "all")


#### Add header title
tbl_RGxP_Report   <-   tbl_RGxP_Report %>% 
  add_header_row(
    values = "% of multi-disease testing sites performing the targeted multi-testing tests by RRH",
    colwidths = ncol(RGxP_Report) # Ensure colwidths matches Multi_summary
  )

tbl_RGxP_Report

#############################################################
#############################################################
#### Utilization Rate ####
#### Report - GxP National utilization rate ####
util_rateall  <-   fn_multi_test %>% 
  group_by(Qtr) %>% 
  summarise(
    No.sites  =  n(),
    Total_Tested  =  sum(Total_tests, na.rm = TRUE),
    Expected_Tests  =  sum(EQtr, na.rm = TRUE)
  ) %>% 
  mutate(
    Util_rate  =  round((Total_Tested/Expected_Tests)*100,0)
  ) %>% 
  arrange(desc(Qtr))

# the table
tble_util_rateall   <-  flextable(util_rateall)

#### Set header labels
tble_util_rateall <-  tble_util_rateall %>% 
  
  set_header_labels(
    Qtr       =   "Quarter",
    No.sites  =  "No. of reported multi-testing sites",
    Total_Tested =  "Total no. of samples tested",
    Expected_Tests =  "Expected number of samples to be tested",
    Util_rate      =  "GxP Utilization Rate"
  )

# Adding vertical lines to improve readability
tble_util_rateall <-  tble_util_rateall %>% 
  vline(j = c(1,2,3,4,5), part = "all")

tble_util_rateall   <-  tble_util_rateall %>% 
  add_header_row(values = "GxP Utilization Rate",
                 colwidths = ncol(util_rateall))

tble_util_rateall
#### Report - %age of health facilities >85% utilization rate ####
site_utl <- faci_util %>%
  filter(HFacility  !=  "BULUBA HOSPITAL") %>% 
  group_by(Qtr) %>% 
  summarise(
    Total    = n(),
    No_85    = sum(util_rate > 84, na.rm = TRUE),
    No_50_85 = sum(util_rate > 50 & util_rate <= 84, na.rm = TRUE),
    No_50    = sum(util_rate <= 50, na.rm = TRUE),
    Pct_85   = round((No_85 / Total) * 100,0),
    Pct_50_85 = round((No_50_85 / Total) * 100,0),
    Pct_50   = round((No_50 / Total) * 100,0)
  ) %>% 
  ungroup()

#### RE-ARRANGE THE DATA FRAME
site_utlPvt <- site_utl %>% 
  select(-c("Total", starts_with("No_"))) %>% 
    pivot_longer(
    cols = c("Pct_85", "Pct_50_85", "Pct_50"),
    names_to = "Group_Categories",
    values_to = "Percentage"
  )

#### renaming the column names
site_utlPvt  <-   site_utlPvt %>%
  mutate(Group_Categories = recode(Group_Categories,
                                   "Pct_85" = ">85%",
                                   "Pct_50_85" = "Btn 50%-84%",
                                   "Pct_50" = "<50%"
  ))


#### ensuring consistent naming
site_utlPvt  <-   site_utlPvt %>% 
  mutate(
    Group_Categories  =  factor(Group_Categories, levels = c(">85%", "Btn 50%-84%", "<50%")),
    Qtr                  =  factor(Qtr, levels = c("Oct-Dec","Jan-Mar")
  ))



# draw the chart
ggplot(site_utlPvt, aes(x = Group_Categories, y = Percentage, fill = Qtr)) +
  geom_col(position = position_dodge(width = 0.9)) +  # Ensure bars are properly dodged
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9),  # Align text with bars
            vjust = -0.3,  # Adjust text slightly above the bars
            color = "black", size = 4) + 
  labs(title = "% of GxP sites with >85% utilization rate", 
       x = "Group Categories", 
       y = "Percentage") +
  theme_minimal() +
  theme(
    legend.position = "top",  # You can adjust this if needed
    axis.text.x = element_text(size = 10)  
  )


#### Report - RRH %age of health facilities >85% utilization rate ####
RRHsite_utl <- faci_util %>%
  filter(HFacility  !=  "BULUBA HOSPITAL") %>% 
  group_by(RRH, Qtr) %>% 
  summarise(
    Total    = n(),
    No_85    =  sum(util_rate  >= 85),
    .groups = "drop"
  ) %>% 
  mutate(
    `age sites`  =  round((No_85/Total)*100,0)
  ) %>% 
      select(RRH,Qtr,`age sites`) %>% 
      
      pivot_wider(
        names_from = Qtr,
        values_from = `age sites`
      )

#### set the target coloring
util_color   <-  function(column_data){
  ifelse(column_data >= 85, "green",
         ifelse(column_data >= 50 & column_data < 85, "yellow","red"))
}

# the table
tble_RRHsite_utl   <-  flextable(RRHsite_utl)


#### Format the table
tble_RRHsite_utl  <-   tble_RRHsite_utl %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    "Reporting Period", ""
  ))
## set the labels
tble_RRHsite_utl  <-   tble_RRHsite_utl %>% 
  set_header_labels(
    RRH  =  "RRH",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec"
  )

tble_RRHsite_utl
### List Columns to Apply Coloring ###
columns <- c("Jan-Mar", "Oct-Dec")

### Apply Background Color ###
for (col in columns) {
  tble_RRHsite_utl  <-   tble_RRHsite_utl %>% 
    bg(j = col, bg = util_color(RRHsite_utl[[col]]), part = "body")
}


# Merging columns 
tble_RRHsite_utl  <-   tble_RRHsite_utl %>%
  merge_at(i = 1, j = 2:3, part = "header") 

# Merging vertically for the first five columns in the header
tble_RRHsite_utl  <-   tble_RRHsite_utl %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tble_RRHsite_utl  <-   tble_RRHsite_utl %>%
  vline(j = c(1, 2, 3), part = "all")


tble_RRHsite_utl  <-   tble_RRHsite_utl %>%
  add_header_row(values="%age of multi-testing sites with >= 85% utilization rate, by RRH", 
                 colwidths = ncol(RRHsite_utl))

tble_RRHsite_utl


#### Report Not complete - EID POC Coverage (#of EID POC testing sites, %age testing ####

#### A summary of designated POC sites
POC_SitesR    <-  POC_Sites %>% 
  group_by(REGION) %>% 
    summarise(
      N =  n()
    ) %>% 
    bind_rows(
      tibble(
        REGION   =  "NATIONAL",
        N        =  sum(.$N)
      )
    )

#### No. of reporting sites
EIDPoC_Sites <- EID_Dataset %>% 
  filter(!is.na(REGION)) %>% 
  group_by(REGION) %>% 
  summarise(
    `Oct-Dec`    =   sum(!is.na(`Highest Report-2024(Oct-Dec)`)),
    `Jan-Mar`    =   sum(!is.na(`Highest Report-2025(Jan-Mar)`))
    ) %>% 
  bind_rows(
    tibble(
      REGION   =  "NATIONAL",
      `Oct-Dec`  =  sum(.$`Oct-Dec`),
      `Jan-Mar`  =  sum(. $`Jan-Mar`)
    )
  )
#### rename RRH 
EIDPoC_Sites <- EIDPoC_Sites %>% 
  mutate(REGION = recode(REGION,
                         "FORTPORTAL" = "FORT PORTAL"))

#### Add columns of Reporting sites data frame to POC sites by RRH
POC_SitesR  <-  POC_SitesR %>% 
  left_join(EIDPoC_Sites, by = "REGION")

#### Add column on percentage 
POC_SitesRpt  <-  POC_SitesR %>% 
  rowwise() %>% 
  mutate(
   across(
     !c(N,REGION),
      ~ round(.x / N *100,0),
        .names = "Pct{.col}")
    )

## Calculating the coverage
#### obtaining referring and testing sites
## create a new column indicating wheter its testing it referring sites
EID_Data$Refer <- ifelse(EID_Data$SFacility == EID_Data$HFacility, "Testing","Referring")

## Number of sites referring samples to by POC Site
EID_refer <- EID_Data %>% 
  group_by(HFacility,Qtr) %>% 
  summarise(
    No.referring = n_distinct(SFacility)) %>% 
      pivot_wider(
        names_from = Qtr,
        values_from = No.referring
      )


## Refer by RRH
EID_RRHrefer <- EID_Data %>% 
  filter(!is.na(RRH) & Refer == "Referring") %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    No_to_POC = n_distinct(SFacility))
# National value
Nat.refer <- EID_RRHrefer %>% 
  summarise(
    RRH = "National",
    No_to_POC = sum(No_to_POC)
  )
EID_RRHrefer <- bind_rows(EID_RRHrefer,Nat.refer)

## Number sites referring samples to CPHL
EID_to_CPHL <- EIDCon %>% 
  group_by(RRH) %>% 
  summarise(
    No_CPHL = n_distinct(HFacility))

# National 
Nat.cphl <- EID_to_CPHL %>% 
  summarise(
    RRH = "National",
    No_CPHL = sum(No_CPHL)
  )

EID_to_CPHL <- bind_rows(EID_to_CPHL,Nat.cphl)

## Table with POC and Conventional sites
EID_to_CPHL <- EID_to_CPHL %>% 
  left_join(EID_RRHrefer, by = "RRH") 
# capitalize
EID_to_CPHL$RRH  <-  toupper(EID_to_CPHL$RRH)

##Q1 Report, adding report from last reporting period

RRH <- c("Arua","Entebbe","FortPortal","Gulu","Hoima","Jinja","Kabale","Kampala","Kayunga","Lira","Masaka","Mbale","Mbarara","Moroto","Mubende","Soroti","Yumbe","National")
Base  <- c(55,16,60,33,9,51,50,3,47,37,31,106,83,17,55,13,36,702)

# create a data frame
ReferQ1 <- data.frame(RRH,Base)
ReferQ1$RRH   <-   toupper(ReferQ1$RRH)

# current quarter
EID_to_CPHL <- EID_to_CPHL %>% 
  left_join(ReferQ1, by = "RRH")
# rearranging the columns

EID_to_CPHL <- EID_to_CPHL %>% 
  select("RRH","No_CPHL","Base","No_to_POC")

#### Report - EID POC Coverage 
EIDPoC_cov <- EIDPoC_Sites %>% 
  left_join(EID_to_CPHL, by = "RRH")

EIDPoC_cov <- EIDPoC_cov %>% 
  rename(
    "No. of designated POC Sites"      = "No_Sites",
    "No of the POC sites reporting lab test"   =  "No_testing_sites",
    "%age of POC sites reporting a test"     =  "Pct_Testing",
    "No of peripheral sites referring samples to CPHL"  = "No_CPHL",
    "Q1"                        = "No_to_POC"
  )

# Create the table

EID_Cov <- flextable(EIDPoC_cov)


EID_Cov
autofit(EID_Cov)

EID_Cov <- height_all(EID_Cov, height = 0.1)  # changinf height of rows

# Renaming header row
EID_Cov <- EID_Cov %>%
  add_header_row(values = c(
    "RRH",
    "No. of designated POC Sites",
    "No. of POC sites reporting lab tests",
    "% of POC sites reporting a test",
    "No. of peripheral sites referring samples to CPHL",
    "No. of Peripheral Sites Referring to POC Site, by Quarter",
    ""
  ))

# Setting the header labels
EID_Cov <- EID_Cov %>%
  set_header_labels(
    RRH = "RRH",
    `No. of designated POC Sites` = "No. of designated POC Sites",
    `No of the POC sites reporting lab test` = "No. of POC sites reporting lab tests",
    `%age of POC sites reporting a test` = "% of POC sites reporting a test",
    `No of peripheral sites referring samples to CPHL` = "No. of peripheral sites referring samples to CPHL",
    Base = "Base",
    Q1 = "Q1"
  )

# Merging columns 6 and 7 in the header row
EID_Cov <- EID_Cov %>%
  merge_at(i = 1, j = 6:7, part = "header") 

# Merging vertically for the first five columns in the header
EID_Cov <- EID_Cov %>% 
  merge_v(j = 1:5, part = "header") 

# Adding vertical lines to improve readability
EID_Cov <- EID_Cov %>%
  vline(j = c(1, 2, 5), part = "all")

#Font and sice
EID_Cov <- EID_Cov %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

EID_Cov

#### Report NOT COMPLETE - Facilities not identified as POC sites but with data in system ####
missg_sites  <-  EID_Dataset %>% 
  filter(is.na(`S/N`))

# the table
tbl_missg_sites   <-  flextable(missg_sites)
tbl_missg_sites





##########################################################
################################################################
#### Annex Section of the report ####
#### Report - number of POC VL tests performed in POC sites by facility #### 
# the table
table_VLPOC   <-  flextable(VL_Dataset)   

table_VLPOC
# format the report
table_VLPOC <- table_VLPOC %>%
  set_header_labels(
    RRH         = "RRH Region",
    HRegion     = "Health Region",
    District    = "District",
    HFacility   = "VL POC Site",
    ALIS        = "# VL POC tests reported in ALIS",
    Pima        =  "# of VL POC tests reported in sympheous (m-Pima) data base",
    GxP         =  "# of VL tests reported in LabXpert (GeneXpert) data base",
    Top_No      = "# Reported as VL tests performed in POC sites"
  ) 

# Borders and backgroud

# define style for border line
border_style = officer::fp_border(color="black", width=1)

# add border lines to table
table_VLPOC <- table_VLPOC %>%
  
  # Remove all existing borders
  border_remove() %>%  
  
  # add horizontal lines via a pre-determined theme setting
  theme_booktabs() %>% 
  
  # add vertical lines to separate Recovered and Died sections
  vline(part = "all", j = 1, border = border_style) %>%
  vline(part = "all", j = 2, border = border_style) %>%  
  vline(part = "all", j = 3, border = border_style) %>% 
  vline(part = "all", j = 4, border = border_style) %>% 
  vline(part = "all", j = 5, border = border_style) %>% 
  vline(part = "all", j = 6, border = border_style) %>% 
  vline(part = "all", j = 9, border = border_style)

##Bold
table_VLPOC <- table_VLPOC %>%
  fontsize(i = 1, size = 10, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  fontsize(i = NULL, size = 8, part = "body")           # adjust bold face of total row (row 7 of body)
# Back ground color
table_VLPOC <- table_VLPOC %>%
  bg(part = "body", bg = "gray95")  


table_VLPOC     <-   table_VLPOC %>% 
  add_header_row(values = "No of VL tests performed by site",
                 colwidths = ncol(VL_Dataset))

table_VLPOC
#### Report - %age of tests performed on PBFW by Health Facility ####
pbfw <- VL_Data %>% 
  group_by(HFacility,Yr,Qtr) %>% 
  summarise(
    Tests  = sum(!str_detect(result, regex("Error|Invalid", ignore_case = TRUE))), # Count rows containing 'Error' or 'Invalid'
    No_preg    = sum(replace_na(is_pregnant == "Y",FALSE)),
    No_bf     =  sum(replace_na(is_breastfeeding == "Y",FALSE))  
  )
#### adding the RRH to the list
pbfw_rrh <-  pbfw %>% 
  left_join(dhis2names, by = "HFacility")

# calculating the %age column and proportions in descending order by RRH 
pbfw_rrh_r  <-  pbfw_rrh %>% 
  group_by(RRH,HFacility,Qtr) %>% 
  summarise(
    No.Tests  =  sum(Tests, na.rm = TRUE),
    preg      =  sum(No_preg, na.rm = TRUE),
    bf        =  sum(No_bf, na.rm = TRUE)
  ) %>% 
  mutate(
    No_pbfw   =  preg + bf,
    `%pbfw`   = round((No_pbfw/No.Tests)*100,0)) %>% 
  select(RRH,HFacility,Qtr,`%pbfw`) %>% 
  arrange(RRH,desc(`%pbfw`))
## re-arrange of each quarter
pbfw_rrh_r_pvt   <-  pbfw_rrh_r %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = `%pbfw`
  ) %>% 
  filter(`Jan-Mar` < 50) %>% 
  select(RRH,HFacility, `Oct-Dec`, `Jan-Mar`)
  

#### the table 
tble_pbfw_rrh   <-  flextable(pbfw_rrh_r_pvt)

tble_pbfw_rrh  <-  tble_pbfw_rrh %>% 
  add_header_row(values = "%of of VL tests performed for pbfw",
                 colwidths = ncol(pbfw_rrh_r_pvt))
tble_pbfw_rrh
# Color the columns based on the targets
tble_pbfw_rrh <- tble_pbfw_rrh %>% 
  bg(j = "Oct-Dec", bg = ifelse(pbfw_rrh_r_pvt$`Oct-Dec` >= 85, "green",
                                ifelse(pbfw_rrh_r_pvt$`Oct-Dec` >= 50 & pbfw_rrh_r_pvt$`Oct-Dec` <= 84, "yellow", "red"))) %>%
  bg(j = "Jan-Mar", bg = ifelse(pbfw_rrh_r_pvt$`Jan-Mar` >= 85, "green",
                                ifelse(pbfw_rrh_r_pvt$`Jan-Mar` >= 50 & pbfw_rrh_r_pvt$`Jan-Mar` <= 84, "yellow", "red"))
  )


tble_pbfw_rrh

#### report by health facility ####
#### Report - Quarterly EID POC LabTAT by RRH ####
HQtrPoCTAT <- WklyTAT %>% 
  filter(Qtr   !=  "Jul-Sept") %>%
  group_by(RRH,HFacility, Yr, Qtr) %>% 
  summarise(
    No_samples  =  n(),
    QLabTAT = median(LabTAT, na.rm = TRUE),  # Ensure NA handling
    .groups = "drop"
  ) %>%
  arrange(Yr, Qtr)  # Ensure correct order


#### Pivot for period
pvt_HQtrPoCTAT   <-   HQtrPoCTAT %>% 
  pivot_wider(
    id_cols = c(RRH,HFacility),
    names_from = c(Yr,Qtr),
    values_from = c(No_samples,QLabTAT)
  )

#### set table coloring
HFrow_color <- function(column_data) {
  case_when(
    is.na(column_data) | column_data == "" ~ "grey",  # Handle blanks/NA as grey
    column_data == 0 ~ "green",
    column_data > 0 & column_data <= 3 ~ "yellow",
    TRUE ~ "red"
  )
}

### The table
tbl_pvt_HQtrPoCTAT  <-  flextable(pvt_HQtrPoCTAT)
tbl_pvt_HQtrPoCTAT

### Format the Table Headers ###
tbl_pvt_HQtrPoCTAT <- tbl_pvt_HQtrPoCTAT %>% 
  add_header_row(
    values = c(RRH      =  "RRH",
               HFacility   =  "Health Facility Name",
               "No. of reported samples tested by Qtr","",
               "LabTAT for respective quarters","")
  ) 

# the the header labels
tbl_pvt_HQtrPoCTAT <- tbl_pvt_HQtrPoCTAT %>% 
  set_header_labels(
    RRH = "RRH",
    HFacility   =  "Health Facility Name",
    `No_samples_2024_Oct-Dec` = "Oct-Dec",
    `No_samples_2025_Jan-Mar` = "Jan-Mar",
    `QLabTAT_2024_Oct-Dec` = "Oct-Dec TAT",
    `QLabTAT_2025_Jan-Mar` = "Jan-Mar TAT"
  )

### List Columns to Apply Coloring ###
columns <- c("QLabTAT_2024_Oct-Dec", "QLabTAT_2025_Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  if (col %in% names(pvt_RQtrPoCTAT)) {  # Ensure column exists before applying
    tbl_pvt_HQtrPoCTAT <- tbl_pvt_HQtrPoCTAT %>% 
      bg(j = col, bg = HFrow_color(pvt_HQtrPoCTAT[[col]]), part = "body")
  }
}

tbl_pvt_HQtrPoCTAT <- tbl_pvt_HQtrPoCTAT %>% 
# Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") %>% 
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertically for the first five columns in the header
tbl_pvt_HQtrPoCTAT <- tbl_pvt_HQtrPoCTAT %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tbl_pvt_HQtrPoCTAT <- tbl_pvt_HQtrPoCTAT %>% 
  vline(j = c(1,2, 4, 6), part = "all")


tbl_pvt_HQtrPoCTAT <- tbl_pvt_HQtrPoCTAT %>% 
  add_header_row(values = "EID POC Lab TAT by Site",
                 colwidths = ncol(pvt_HQtrPoCTAT))


tbl_pvt_HQtrPoCTAT

#### Report - %age of Sample results released in 1 day TAT by Site ####
HfPctTimely   <-   WklyTAT %>% 
  filter(Qtr   !=  "Jul-Sept") %>% 
  group_by(RRH,HFacility, Yr,Qtr) %>% 
  summarise(
    No_samples  = n(),
    No_1Day     =  sum(LabTAT == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    Pct_1DayTAT  =  round((No_1Day/No_samples)*100,0)
  )

#### Pivot the table
Pvt_HFPct   <-   HfPctTimely %>% 
  pivot_wider(
    id_cols = c(RRH,HFacility),
    names_from = c(Yr,Qtr),
    values_from = c(No_samples,Pct_1DayTAT)
  )

#### plan for color in table
siteapplyrow_color   <-  function(column_data){
  ifelse(is.na(column_data) | column_data == "","grey",
  ifelse(column_data >= 75, "green",
         ifelse(column_data > 50 & column_data < 75, "yellow","red")))
   }

#### The table
tble_SPctTimely   <-  flextable(Pvt_HFPct)

# format the table
tble_SPctTimely   <-   tble_SPctTimely %>% 
  add_header_row(
    values = c(RRH      =  "RRH", 
               HFacility   =  "Health Facility Name",
               "No. of reported samples tested by Qtr","",
               "%age of samples released within 0 Day TAT for respective quarters","")
  ) 

# Set the header
tble_SPctTimely   <-   tble_SPctTimely %>% 
  set_header_labels(
    RRH = "RRH",
    HFacility   =  "Health Facility Name",
    `No_samples_2024_Oct-Dec` = "Oct-Dec",
    `No_samples_2025_Jan-Mar` = "Jan-Mar",
    `Pct_1DayTAT_2024_Oct-Dec` = "Oct-Dec",
    `Pct_1DayTAT_2025_Jan-Mar` = "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Pct_1DayTAT_2024_Oct-Dec", "Pct_1DayTAT_2025_Jan-Mar")

### Apply Background Color ###
for (col in columns) {
  tble_SPctTimely   <-   tble_SPctTimely %>%
    bg(j = col, bg = siteapplyrow_color(Pvt_HFPct[[col]]), part = "body")
}

tble_SPctTimely   <-   tble_SPctTimely %>% 
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") %>% 
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertically for the first five columns in the header
tble_SPctTimely   <-   tble_SPctTimely %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")

# Adding vertical lines to improve readability
tble_SPctTimely   <-   tble_SPctTimely %>% 
  vline(j = c(1,2, 4, 6), part = "all")


tble_SPctTimely   <-   tble_SPctTimely %>% 
  add_header_row(values = "%age of samples released within 1 day by Site",
                 colwidths = ncol(Pvt_HFPct))


tble_SPctTimely


#### Report - 1st PCR by health facility ####
# total number of HEIs
hfpcr <- FirstPcr %>% 
  filter(Result %in% c("Positive","Negative")) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>% 
  group_by(RRH,HFacility, Qtr) %>% 
  summarise(
    No_HEI = n(),.groups = "drop") 

# Re-organize the data set
hfpcr_Pvt   <-  hfpcr %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_HEI
  ) 

#### add the columns on number tested timely
hfPcr2M <- FirstPcr %>%
  filter(Result %in% c("Positive","Negative")) %>% 
  filter(Qtr %in% c("Oct-Dec","Jan-Mar")) %>%
  group_by(RRH,HFacility, Qtr) %>% 
  summarise(
    No_timely = sum(Age >= 0 & Age < 2.1, na.rm = TRUE),
    .groups = "drop"
  )

## pivot the data frame to re arrange table
hfPcr2M_Pvt   <-  hfPcr2M %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_timely
  )

#### create the data frame to calculate %age of infants at 2month
hfPcr_Report <- hfpcr_Pvt %>% 
  left_join(hfPcr2M_Pvt, by = "HFacility")
### selecting colmuns of interest
hfPcr_Report  <-  hfPcr_Report %>% 
  select(RRH.x,HFacility,`Jan-Mar.x`,`Oct-Dec.x`,`Jan-Mar.y`,`Oct-Dec.y`)

# Pct 1st PCR at 2 months table
hfPcr_Report <- hfPcr_Report %>%  
  mutate(
    across(ends_with(".y"),
           ~ round(.x/get(sub(".y$",".x",cur_column()))*100,0),
           .names = "Pct_{.col}"
    )
  )
#### select columns of interest
hfPcr_Report <- hfPcr_Report %>% 
  select(RRH.x,HFacility,`Oct-Dec.x`,`Jan-Mar.x`,`Pct_Oct-Dec.y`,`Pct_Jan-Mar.y`)

# function to color the columns
timely_colors   <-  function(column_data){
  ifelse(column_data >= 75, "green",
         ifelse(column_data > 50 & column_data < 75, "yellow","red"))
}

# The table
Table_hf1st_PCR <- flextable(hfPcr_Report)
Table_hf1st_PCR
# format the table
Table_hf1st_PCR   <-  Table_hf1st_PCR %>% 
  add_header_row(
    values = c(
      RRH.x  =  "RRH",
      HFacility  =  "Facility Name",
      "No. of reported tests done","",
      "%age of HEI accesing timely 1st PCR", ""
    ))

Table_hf1st_PCR   <-  Table_hf1st_PCR %>% 
  set_header_labels(
    RRH.x  =  "RRH",
    HFacility  =  "Facility Name",
    `Oct-Dec.x`   =  "Oct-Dec",
    `Jan-Mar.x`   =  "Jan-Mar",
    `Pct_Oct-Dec.y`   =  "Oct-Dec",
    `Pct_Jan-Mar.y`   =  "Jan-Mar"
  )

### List Columns to Apply Coloring ###
columns <- c("Pct_Oct-Dec.y", "Pct_Jan-Mar.y")

### Apply Background Color ###
for (col in columns) {
  Table_hf1st_PCR   <-  Table_hf1st_PCR %>%
    bg(j = col, bg = timely_colors(hfPcr_Report[[col]]), part = "body")
}

Table_hf1st_PCR   <-  Table_hf1st_PCR %>%
  # Merging columns 
  merge_at(i = 1, j = 3:4, part = "header") %>% 
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertically for the first five columns in the header
Table_hf1st_PCR   <-  Table_hf1st_PCR %>%
  merge_v(j = 1, part = "header") %>%  
  merge_v(j = 2, part = "header")  
  
# Adding vertical lines to improve readability
Table_hf1st_PCR   <-  Table_hf1st_PCR %>%
  vline(j = c(1,2,4,6), part = "all")


Table_hf1st_PCR   <-  Table_hf1st_PCR %>%
  add_header_row(values = "%age of HEIs accessing timely EID by site",
                 colwidths = ncol(hfPcr_Report))

Table_hf1st_PCR

#### Report - Disease multi-testing sites by health facility ####
hf_multiTesting  <-     fn_multi_test %>% 
  filter(HFacility !=  "BULUBA HOSPITAL") %>% 
  filter(no_tests > 1) %>% 
  select(-c("module","EWk","EMon","EQtr","no_tests","Total_tests"))

#### Pivot table
hf_multiTestingPvt   <-  hf_multiTesting %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = c("GxPEID","GxPVL","GxPHPV","GxPTB","Pct_EID",
                    "Pct_VL","Pct_HPV","Pct_TB")
       )

#### Final report, arranging the columns
hf_mulT_report   <-   hf_multiTestingPvt %>% 
  select(RRH,District,HFacility,
         matches("GxP.*Oct-Dec"),
         matches("GxP.*Jan-Mar"),
         matches("Pct.*Oct-Dec"),
         matches("Pct.*Jan-Mar"))

#### The table
tbl_hf_mulT_report   <- flextable(hf_mulT_report)

### Format the Table Headers ###
tbl_hf_mulT_report   <-  tbl_hf_mulT_report %>% 
  add_header_row(
    values = c(RRH      =  "RRH",
               District =   "District",
               HFacility   =  "Health Facility Name",
               "No. of respective samples tested in Qtr Oct-Dec","","","",
               "No. of respective samples tested in Qtr Jan-Mar","","","",
               "% of respective samples tested in Qtr Oct-Dec","","","",
               "% of respective samples tested in Qtr Jan-Mar","","",""
         )) 

# the the header labels
tbl_hf_mulT_report   <-  tbl_hf_mulT_report %>% 
  set_header_labels(
    RRH      =  "RRH",
    District =   "District",
    HFacility   =  "Health Facility Name",
    `GxPEID_Oct-Dec` = "EID",
    `GxPVL_Oct-Dec` = "VL",
    `GxPHPV_Oct-Dec` = "HPV",
    `GxPTB_Oct-Dec` = "TB",
    `GxPEID_Jan-Mar` = "EID",
    `GxPVL_Jan-Mar` = "VL",
    `GxPHPV_Jan-Mar` = "HPV",
    `GxPTB_Jan-Mar` = "TB",
    `Pct_EID_Oct-Dec` = "EID",
    `Pct_VL_Oct-Dec` = "VL",
    `Pct_HPV_Oct-Dec` = "HPV",
    `Pct_TB_Oct-Dec` = "TB",
    `Pct_EID_Jan-Mar` = "EID",
    `Pct_VL_Jan-Mar` = "VL",
    `Pct_HPV_Jan-Mar` = "HPV",
    `Pct_TB_Jan-Mar` = "TB"
      )

# Merging columns 
tbl_hf_mulT_report   <-  tbl_hf_mulT_report %>% 
  merge_at(i = 1, j = 4:7, part = "header") %>% 
  merge_at(i = 1, j = 8:11, part = "header") %>% 
  merge_at(i = 1, j = 12:15, part = "header") %>% 
  merge_at(i = 1, j = 16:19, part = "header") 

# Merging vertically for the first five columns in the header
tbl_hf_mulT_report   <-  tbl_hf_mulT_report %>%  
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header") %>% 
  merge_v(j = 3, part = "header")

# Adding vertical lines to improve readability
tbl_hf_mulT_report   <-  tbl_hf_mulT_report %>% 
  vline(j = c(1,2,3,7,11,15,19), part = "all")


tbl_hf_mulT_report   <-  tbl_hf_mulT_report %>% 
  add_header_row(values = "Line list of Mult-testing sites, and samples tested",
                 colwidths = ncol(hf_mulT_report))


tbl_hf_mulT_report

#### Report - Pct tests performed in GxP site #####
fn_multi_test <- fn_multi_test %>% 
  group_by(RRH) %>% 
  summarise(
    TB = sum(GxPTB, na.rm = TRUE),
    VL = sum(GxPVL, na.rm = TRUE),
    EID = sum(GxPEID, na.rm = TRUE),
    HPV = sum(GxPHPV, na.rm = TRUE)
  ) %>%
  mutate(Total = TB + VL + EID + HPV) %>%
  bind_rows(
    summarise(
      fn_multi_test,
      RRH = "National",
      TB = sum(GxPTB, na.rm = TRUE),
      VL = sum(GxPVL, na.rm = TRUE),
      EID = sum(GxPEID, na.rm = TRUE),
      HPV = sum(GxPHPV, na.rm = TRUE),
      Total = sum(GxPTB, GxPVL, GxPEID, GxPHPV, na.rm = TRUE)
    ))
# To establish region with highest multi - test performance
RRH_by_test <- fn_multi_test %>%    
  mutate(
    Pct_TB = (TB/Total)*100,
    Pct_VL  = (VL/Total)*100,
    Pct_EID = (EID/Total)*100,
    Pct_HPV = (HPV/Total)*100
  )

# Pct of each test on the machine
national_totals <- fn_multi_test %>%
  summarise(
    RRH = "National",
    TB = sum(TB, na.rm = TRUE),
    VL = sum(VL, na.rm = TRUE),
    EID = sum(EID, na.rm = TRUE),
    HPV = sum(HPV, na.rm = TRUE)
  ) %>% 
  mutate(Total = TB + VL + EID + HPV)
# percent of each test 
pct_tests <- national_totals %>% 
  summarise(
    Pct_TB = (TB/Total)*100,
    Pct_VL  = (VL/Total)*100,
    Pct_EID = (EID/Total)*100,
    Pct_HPV = (HPV/Total)*100
  )

# The graph
# Create the data frame
GxP_tests <- data.frame(
  Category = c("TB", "VL", "EID", "HPV"),
  Percentage = c(pct_tests$Pct_TB, pct_tests$Pct_VL, pct_tests$Pct_EID, pct_tests$Pct_HPV)
)

# draw the pic chart

ggplot(GxP_tests, aes(x = Category, y = Percentage)) +
  geom_segment(aes(x = Category, xend = Category, y = 0, yend = Percentage), color = "grey") +
  geom_point(aes(color = Category), size = 8) +
  labs(title = "Proportion of test categories performed in GXP sites",
       x = "Category",
       y = "Percentage") +
  theme(
    axis.text.x = element_text(hjust = 1, size = 12,face = "bold"))+
  scale_color_manual(values = c("TB" = "#1f77b4", "VL" = "#ff7f0e", "EID" = "#2ca02c", "HPV" = "#d62728"
  ))

#### Utilization Rates ####
####  Report - Mpima Utilization rates ####
#### Cleaning mpima data sets 
## EID m-Pima
EIDPima$HFacility <- gsub("RRH", "Regional Referral Hospital", EIDPima$HFacility)
EIDPima$HFacility <- trimws(EIDPima$HFacility)

EPima <- EIDPima %>% 
  mutate(HFacility       =     recode(HFacility,
                                      # Old                          # New
                                      "Gulu RRH ART Clinic"       =      "Gulu Regional Referral Hospital",
                                      "Gulu RRH Lab"              =      "Gulu Regional Referral Hospital",
                                      "Katabi General Hospital"   =       "Katabi Military HC III",
                                      "Kisiizi Hospital"          =      "COU Kisiizi Hospital",
                                      "Kyanamukaka HC IV"         =      "Kyanamukaaka HC IV",
                                      "Kyegegwa HC IV"            =      "Kyegegwa Hospital",
                                      "Mukono Hospital"           =      "Mukono General Hospital",
                                      "Naguru Regional Referral Hospital" =  "China Uganda Friendship (Naguru) Regional Referral Hospital",
                                      "Patongo HC IV"            =        "Patongo HC III",
                                      "Rwamwanja HC IV"          =        "Rwamwanja HC III",
                                      "St Mary's Hospital Kasese" =        "Mt. St. Mary's Hospital-DOK",
                                      "St. Joseph Kitovu Hospital" =       "KITOVU HOSPITAL",
                                      "St.Joseph Kitgum Hospital"  =        "St. Joseph's Kitgum Hospital")
  )
# Obtain number tested by facility
EPima <- EPima %>% 
  group_by(HFacility) %>% 
  summarise(EPima = n())
# Making each letter capital
EPima$HFacility <- toupper(EPima$HFacility)


#### M-Pima VL
VLPima$HFacility <- gsub("RRH", "Regional Referral Hospital", VLPima$HFacility)
VLPima$HFacility <- trimws(VLPima$HFacility)

df_VLPima <- VLPima %>% 
  mutate(HFacility       =     recode(HFacility,
                                      # Old                          # New
                                      "Gulu RRH ART Clinic"       =      "Gulu Regional Referral Hospital",
                                      "Gulu RRH Lab"              =      "Gulu Regional Referral Hospital",
                                      "Katabi General Hospital"   =       "Katabi Military HC III",
                                      "Kisiizi Hospital"          =      "COU Kisiizi Hospital",
                                      "Kyanamukaka HC IV"         =      "Kyanamukaaka HC IV",
                                      "Kyegegwa HC IV"            =      "Kyegegwa Hospital",
                                      "Mukono Hospital"           =      "Mukono General Hospital",
                                      "Naguru Regional Referral Hospital" =  "China Uganda Friendship (Naguru) Regional Referral Hospital",
                                      "Patongo HC IV"            =        "Patongo HC III",
                                      "Rwamwanja HC IV"          =        "Rwamwanja HC III",
                                      "St Mary's Hospital Kasese" =        "Mt. St. Mary's Hospital-DOK",
                                      "St. Joseph Kitovu Hospital" =       "KITOVU HOSPITAL",
                                      "St.Joseph Kitgum Hospital"  =        "St. Joseph's Kitgum Hospital")
  )
# Obtain number tested by facility
df_VLPima <- df_VLPima %>% 
  group_by(HFacility) %>% 
  summarise(n_VLPima = n())
# Making each letter capital
df_VLPima$HFacility <- toupper(df_VLPima$HFacility)

#########################################################################
####Joing m-Pima to POC sites####
Pima_util <- POC_Sites %>% 
  left_join(df_VLPima, by = "HFacility") %>% 
  left_join(EPima, by = "HFacility") %>% 
  mutate(n_VLPima = replace_na(n_VLPima, 0),
         EPima    = replace_na(EPima, 0))

# Filter out MPima sites only 
Pima_util <- Pima_util %>% 
  filter(EID_Platform == "m-PIMA")

# Add 3 Columns "Expected number of tests for week, month, quarter
Pima_util <- Pima_util %>% 
  mutate(EWk   =  40,
         EMth  =  160,
         EQtr  =  480)
# Add the EID and VL tests
Pima_util <- Pima_util %>% 
  mutate(Total_Tests = n_VLPima + EPima)
# Calculate quarterly utilization rate
Pima_util <- Pima_util %>% 
  mutate(Qtrly_Util_Rate = round((Total_Tests/EQtr)*100, 1))
# Quarterly line list Report 
QPima_Util <- Pima_util %>% 
  select(HFacility,REGION,`IP/REGION`,n_VLPima,EPima,EQtr,Total_Tests,Qtrly_Util_Rate
  )
# Rename the column
QPima_Util <- QPima_Util %>% 
  rename(
    No.VL     = "n_VLPima",
    No.EID    = "EPima",
    Expected_Qtr = "EQtr",
    Util_Rate    =  "Qtrly_Util_Rate")

QPima_Util   <-  QPima_Util %>% 
  arrange(REGION,desc("Util_Rate"))

#### Report - mPima utilization rate by health facility ####
tbl_QPima_Util   <-  flextable(QPima_Util)
tbl_QPima_Util


#### Report - Summary %age of HFacilities with targeted mPima Utilization rate by RRH ####
Reg_pima_util <- QPima_Util %>% 
  group_by(REGION,`IP/REGION`) %>% 
  summarise(
    No.Sites = n(), 
    n75      = sum (Util_Rate > 74),
    n50_75   = sum(Util_Rate > 49 & Util_Rate < 75),
    n50      = sum(Util_Rate < 50)) %>% 
  ungroup()
# Adding a Total Row
total_summary <- Reg_pima_util %>% 
  summarise(
    REGION = "National",
    No.Sites = sum(No.Sites),
    n75      = sum(n75),
    n50_75   = sum(n50_75),
    n50      = sum(n50))
# Combine the groups total and regional report
Reg_pima_util <- bind_rows(Reg_pima_util,total_summary)
# Arange the Natioanl to be on top
Reg_pima_util <- Reg_pima_util %>%
  arrange(ifelse(REGION == "National", 0, 1))  # Places "Total" row first

## the table
tbl_Reg_pima_util   <-  flextable(Reg_pima_util)
tbl_Reg_pima_util

###################################################################
##################################################################
###################################################################
###################################################################
#### Error Rates ####
#### Report - VL mpima Error Rate BY HEALTH FACILITY ####
# VL mPima error rate   (mpima database)
# Error rate by health facility
VLPima_e_rate <- VLPima_data %>% 
  group_by(HFacility,Qtr) %>% 
  summarise(
    No_Error  = sum(Error == 0, na.rm = TRUE),
    N_Errors  = sum(Error > 0, na.rm = TRUE), 
      .groups  =  "drop") %>%  
  filter(!is.na(HFacility)) %>% 
  mutate(
    Total       = No_Error + N_Errors,
    error_rate  = round((N_Errors/Total)*100,1))

# Pivot the table for Period
VLPima_e_ratePvt   <-  VLPima_e_rate %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = c(No_Error,N_Errors,Total,error_rate)
  ) %>% 
  select(HFacility,`Total_Oct-Dec`,`Total_Jan-Mar`,`error_rate_Oct-Dec`,`error_rate_Jan-Mar`
         )

#### Add the regions columns
VLPima_Error   <-   VLPima_e_ratePvt %>% 
  left_join(POC_Sites, by = "HFacility") %>% 
    select(HFacility,REGION,`Total_Oct-Dec`,`Total_Jan-Mar`,`error_rate_Oct-Dec`,`error_rate_Jan-Mar`
           ) %>% 
          replace_na(list(`Total_Oct-Dec`=0,`Total_Jan-Mar`=0))

# the table
tble_VLPima_Error  <- flextable(VLPima_Error)

# format the report
#### Format the table
# Add the header row
tble_VLPima_Error <- tble_VLPima_Error %>% 
  add_header_row(
    values = c(
      HFacility    =  "Health Facility Name",
      REGION       =  "RRH Region",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_VLPima_Error <- tble_VLPima_Error %>% 
  set_header_labels(
    HFacility = "Health Facility Name",
    REGION = "RRH Region",
    `Total_Oct-Dec`  =  "Oct-Dec",
    `Total_Jan-Mar` = "Jan-Mar",
    `error_rate_Oct-Dec` = "Oct-Dec",
    `error_rate_Jan-Mar` = "Jan-Mar")

# Merging columns 
tble_VLPima_Error <- tble_VLPima_Error %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_VLPima_Error <- tble_VLPima_Error %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_VLPima_Error <- tble_VLPima_Error %>% 
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_VLPima_Error <- tble_VLPima_Error %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_VLPima_Error <- tble_VLPima_Error %>% 
  add_header_row(values = "VL m-Pima Error rate by Health Facility",
                 colwidths = ncol(VLPima_Error))

tble_VLPima_Error


#### Report - VLPima by RRH ####

# VL Error rate by RRH
RRH_VL_Error <- VLPima_data %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    N_tests   =  n(), 
    N_Errors  = sum(Error > 0, na.rm = TRUE)) %>% 
  filter(!is.na(RRH)) %>% 
  ungroup()

# re-organise the pivot 
RRH_VL_ErrorPvt  <-   RRH_VL_Error %>% 
  pivot_wider(
    id_cols = c(RRH),
    names_from = Qtr,
    values_from = c(N_tests,`N_Errors`)
  )
#### The national values
nat.RRH_VL_ErrorPvt  <-  RRH_VL_ErrorPvt %>% 
  summarise(
    RRH    =   "NATIONAL",
    `N_tests_Oct-Dec`    =  sum(`N_tests_Oct-Dec`,na.rm = TRUE),
    `N_tests_Jan-Mar`    =  sum(`N_tests_Jan-Mar`,na.rm = TRUE),
    `N_Errors_Oct-Dec`  =   sum(`N_Errors_Oct-Dec`,na.rm = TRUE),
    `N_Errors_Jan-Mar`  =   sum(`N_Errors_Jan-Mar`,na.rm = TRUE)
  )
# Combining the table RRH and National
RRH_VL_ErrorPvt   <-  rbind(RRH_VL_ErrorPvt,nat.RRH_VL_ErrorPvt)

### calculating the error rate by RRH and Qtr
RRH_Qtr_Err  <-  RRH_VL_ErrorPvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_tests_Oct-Dec`=0,`N_tests_Jan-Mar`=0))

#### The report table
RRH_Qtr_ErrReport  <-  RRH_Qtr_Err %>% 
  select(RRH,`N_tests_Oct-Dec`,`N_tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`) %>% 
    arrange(desc(RRH == "NATIONAL"))

# the table
tble_RRH_Qtr_ErrReport   <-  flextable(RRH_Qtr_ErrReport)
tble_RRH_Qtr_ErrReport
#### format the table report
# Add the header row
tble_RRH_Qtr_ErrReport <- tble_RRH_Qtr_ErrReport %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Region",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      "")
    )
# Set the header labels
tble_RRH_Qtr_ErrReport <- tble_RRH_Qtr_ErrReport %>% 
  set_header_labels(
    RRH    =  "RRH Region",
    `N_tests_Oct-Dec`  =  "Oct-Dec",
    `N_tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Merging columns 
tble_RRH_Qtr_ErrReport <- tble_RRH_Qtr_ErrReport %>% 
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertical HFacility and Region
tble_RRH_Qtr_ErrReport <- tble_RRH_Qtr_ErrReport %>% 
  merge_v(j = 1, part = "header") 


# Adding vertical lines to improve readability
tble_RRH_Qtr_ErrReport <- tble_RRH_Qtr_ErrReport %>% 
  vline(j = c(1, 3, 5), part = "all")

#Font and sice
tble_RRH_Qtr_ErrReport <- tble_RRH_Qtr_ErrReport %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_RRH_Qtr_ErrReport <- tble_RRH_Qtr_ErrReport %>% 
  add_header_row(values = "VL m-Pima Error rate by RRH Region",
                 colwidths = ncol(RRH_Qtr_ErrReport))

tble_RRH_Qtr_ErrReport

######################################################################################
#### EID mpima Error Rate ####
#### Report - EID m-Pima Error rate by health facility ####
EIDPima_e_rate <- Pima_data %>% 
  group_by(HFacility, Qtr) %>% 
  summarise(
    No_Error = sum(Error == 0, na.rm = TRUE),
    N_Errors = sum(Error > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(!is.na(HFacility)) %>% 
  mutate(
    Total = No_Error + N_Errors,
    error_rate = round((N_Errors / Total) * 100, 1)
  )

# Pivot the table for Period
EIDPima_e_ratePvt   <-  EIDPima_e_rate %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = c(No_Error,N_Errors,Total,error_rate)
  ) %>% 
  select(HFacility,`Total_Oct-Dec`,`Total_Jan-Mar`,`error_rate_Oct-Dec`,`error_rate_Jan-Mar`
  )

#### Add the regions columns
EIDPima_Error   <-   EIDPima_e_ratePvt  %>% 
  left_join(POC_Sites, by = "HFacility") %>% 
  select(HFacility,REGION,`Total_Oct-Dec`,`Total_Jan-Mar`,`error_rate_Oct-Dec`,`error_rate_Jan-Mar`
  ) %>% 
  replace_na(list(`Total_Oct-Dec`=0,`Total_Jan-Mar`=0))

# the table
tble_EIDPima_Error <- flextable(EIDPima_Error)

# format the report
#### Format the table
# Add the header row
tble_EIDPima_Error <- tble_EIDPima_Error %>% 
  add_header_row(
    values = c(
      HFacility    =  "Health Facility Name",
      REGION       =  "RRH Region",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_EIDPima_Error <- tble_EIDPima_Error %>%  
  set_header_labels(
    HFacility = "Health Facility Name",
    REGION = "RRH Region",
    `Total_Oct-Dec`  =  "Oct-Dec",
    `Total_Jan-Mar` = "Jan-Mar",
    `error_rate_Oct-Dec` = "Oct-Dec",
    `error_rate_Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_EIDPima_Error <- tble_EIDPima_Error %>%  
  bg(j = "error_rate_Oct-Dec", bg = ifelse(EIDPima_Error$`error_rate_Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "error_rate_Jan-Mar", bg = ifelse(EIDPima_Error$`error_rate_Jan-Mar` >= 5, "red", "green")) 


# Merging columns 
tble_EIDPima_Error <- tble_EIDPima_Error %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_EIDPima_Error <- tble_EIDPima_Error %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_EIDPima_Error <- tble_EIDPima_Error %>% 
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_EIDPima_Error <- tble_EIDPima_Error %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_EIDPima_Error <- tble_EIDPima_Error %>% 
  add_header_row(values = "EID m-Pima Error rate by Health Facility",
                 colwidths = ncol(VLPima_Error))

tble_EIDPima_Error

#### Report - EID m-PIMA error rate by RRH ####
# VL Error rate by RRH
RRH_EID_Error <- Pima_data %>% 
  filter(Qtr  !=  "Apri-Jun") %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    N_tests   =  n(), 
    N_Errors  = sum(Error > 0, na.rm = TRUE), .groups = "drop") %>% 
  filter(!is.na(RRH)) %>% 
  ungroup()

# re-organise the pivot 
RRH_EID_Error_ErrorPvt  <-   RRH_EID_Error %>% 
  pivot_wider(
    id_cols = c(RRH),
    names_from = Qtr,
    values_from = c(N_tests,`N_Errors`)
  )
#### The national values
nat.RRH_EID_ErrorPvt  <-  RRH_EID_Error_ErrorPvt %>% 
  summarise(
    RRH    =   "NATIONAL",
    `N_tests_Oct-Dec`    =  sum(`N_tests_Oct-Dec`,na.rm = TRUE),
    `N_tests_Jan-Mar`    =  sum(`N_tests_Jan-Mar`,na.rm = TRUE),
    `N_Errors_Oct-Dec`  =   sum(`N_Errors_Oct-Dec`,na.rm = TRUE),
    `N_Errors_Jan-Mar`  =   sum(`N_Errors_Jan-Mar`,na.rm = TRUE)
  )
# Combining the table RRH and National
RRH_EID_Error_ErrorPvt   <-  rbind(RRH_EID_Error_ErrorPvt,nat.RRH_EID_ErrorPvt)

### calculating the error rate by RRH and Qtr
RRH_EIDQtr_Err  <-  RRH_EID_Error_ErrorPvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_tests_Oct-Dec`=0,`N_tests_Jan-Mar`=0))

#### The report table
RRH_EIDQtr_ErrReport  <-  RRH_EIDQtr_Err %>% 
  select(RRH,`N_tests_Oct-Dec`,`N_tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`) %>% 
  arrange(desc(RRH == "NATIONAL"))

# the table
tble_RRH_EIDQtr_ErrReport   <-  flextable(RRH_EIDQtr_ErrReport)

#### format the table report
# Add the header row
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Region",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      "")
  )
# Set the header labels
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  set_header_labels(
    RRH    =  "RRH Region",
    `N_tests_Oct-Dec`  =  "Oct-Dec",
    `N_tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  bg(j = "Oct-Dec", bg = ifelse(RRH_EIDQtr_ErrReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(RRH_EIDQtr_ErrReport$`Jan-Mar` >= 5, "red", "green")) 

# Merging columns 
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertical HFacility and Region
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  merge_v(j = 1, part = "header") 


# Adding vertical lines to improve readability
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  vline(j = c(1, 3, 5), part = "all")

#Font and sice
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tble_RRH_EIDQtr_ErrReport <- tble_RRH_EIDQtr_ErrReport %>% 
  add_header_row(values = "EID m-Pima Error rate by RRH Region",
                 colwidths = ncol(RRH_Qtr_ErrReport))

tble_RRH_EIDQtr_ErrReport


########################################################################
###########################################################################
#### GxP Error rates ####
#### Report - EID GxP error rate by health facility ####
EIDGxP_Err <- EIDGxP %>% 
  group_by(RRH,HFacility,Qtr) %>% 
    summarise(
      N_Tests  =   sum(GxPEID),
      N_Errors  =  sum(Errors, na.rm = TRUE),
      .groups = "drop"
    )
# pivot the table to re-arrange by quarter
EIDGxP_ErrPvt <- EIDGxP_Err %>%
  pivot_wider(
    id_cols = RRH:HFacility,
    names_from = Qtr,
    values_from = c(N_Tests, N_Errors)
  )


# calculate the error rate
EIDGxP_ErrPvt   <-  EIDGxP_ErrPvt %>% 
mutate(
  `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
  `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))
    

# the table
EIDGxP_ErrReport   <-  EIDGxP_ErrPvt %>% 
  select(RRH,HFacility,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
         )

# create table
tble_EIDGxP_ErrReport <- flextable(EIDGxP_ErrReport)

#### Format the table
# Add the header row
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      HFacility   =  "Health Facility Name",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>%  
  set_header_labels(
    RRH    =  "RRH Regions",
    HFacility   =  "Health Facility Name",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>% 
  bg(j = "Oct-Dec", bg = ifelse(EIDGxP_ErrReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(EIDGxP_ErrReport$`Jan-Mar` >= 5, "red", "green")) 


# Merging columns 
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>% 
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_EIDGxP_ErrReport <- tble_EIDGxP_ErrReport %>% 
  add_header_row(values = "EID GxP Error rate by Health Facility",
                 colwidths = ncol(EIDGxP_ErrReport))

tble_EIDGxP_ErrReport

#### Report - %age of health facilities with < 5% EID GxP error rate ####
EGxP_ER  <-  EIDGxP_ErrReport %>%
  group_by(RRH) %>% 
  summarise(
    No.Sites  =  n(),
    `Oct-Dec`  = sum(`Oct-Dec`  < 5, na.rm = TRUE),
    `Jan-Mar`  = sum(`Jan-Mar`  < 5, na.rm = TRUE)
  ) %>% 
  ungroup()
#### Add national row
nat.EGxP_ER   <-  EGxP_ER %>% 
  summarise(
  RRH  =  "NATIONAL",
  No.Sites   =  sum(No.Sites),
  `Oct-Dec`  =  sum(`Oct-Dec`),
  `Jan-Mar`  =  sum(`Jan-Mar`)
  )
#### add to table
EGxP_ER   <-  rbind(EGxP_ER,nat.EGxP_ER)

# Include percentage of sites
EGxP_ER_Report   <-  EGxP_ER %>%
  rowwise() %>% 
  mutate(
    `Pct_Oct-Dec`   =  round((`Oct-Dec`/No.Sites)*100,1),
    `Pct_Jan-Mar`   =  round((`Jan-Mar`/No.Sites)*100,1)
  ) %>% 
  arrange(desc(RRH == "NATIONAL"))
    
 
# the table
tble_EGxP_ER_Report   <-  flextable(EGxP_ER_Report)

# Add the header row
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      No.Sites   =  "No. of GxP EID Testing Sites",
      "No. of sites with <5% EID GxP Error Rates",
      "",
      "% of sites with <5% EID GxP Error Rates",
      ""
    ))
# Set the header labels
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  set_header_labels(
    RRH    =  "RRH Regions",
    No.Sites   =  "No. of GxP EID Testing Sites",
    `Oct-Dec`  =  "Oct-Dec",
    `Jan-Mar` = "Jan-Mar",
    `Pct_Oct-Dec` = "Oct-Dec",
    `Pct_Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  bg(j = "Pct_Oct-Dec", bg = ifelse(EGxP_ER_Report$`Pct_Oct-Dec` >= 75, "green",
                                   ifelse(EGxP_ER_Report$`Pct_Oct-Dec` >= 50 & EGxP_ER_Report$`Pct_Oct-Dec` <= 74, "yellow", "red"))) %>%
  bg(j = "Pct_Jan-Mar", bg = ifelse(EGxP_ER_Report$`Pct_Jan-Mar` >= 75, "green",
                                   ifelse(EGxP_ER_Report$`Pct_Jan-Mar` >= 50 & EGxP_ER_Report$`Pct_Jan-Mar` <= 74, "yellow", "red"))
  )

# Merging columns 
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_EGxP_ER_Report <- tble_EGxP_ER_Report %>% 
  add_header_row(values = "%age of sites with <5% EID GxP error rate by RRH",
                 colwidths = ncol(EGxP_ER_Report))

tble_EGxP_ER_Report

#### Report - EID GxP Error rate by RRH ####
RRHEIDGxP_Err <- EIDGxP %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    N_Tests  =   sum(GxPEID),
    N_Errors  =  sum(Errors, na.rm = TRUE)
  )
# pivot the table to re-arrange by quarter
RRHEIDGxP_ErrPvt   <-  RRHEIDGxP_Err %>% 
  pivot_wider(
    id_cols = c(RRH),
    names_from = Qtr,
    values_from = c(N_Tests,N_Errors)
  )

# calculate the error rate
RRHEIDGxP_ErrPvt   <-  RRHEIDGxP_ErrPvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))


# the table
RRHEIDGxP_ErrPvtReport   <-  RRHEIDGxP_ErrPvt %>% 
  select(RRH,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
  )

# create table
tble_RRHEIDGxP <- flextable(RRHEIDGxP_ErrPvtReport)
tble_RRHEIDGxP
#### Format the table
# Add the header row
tble_RRHEIDGxP <- tble_RRHEIDGxP %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_RRHEIDGxP <- tble_RRHEIDGxP %>% 
  set_header_labels(
    RRH    =  "RRH Regions",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_RRHEIDGxP <- tble_RRHEIDGxP %>%  
  bg(j = "Oct-Dec", bg = ifelse(RRHEIDGxP_ErrPvtReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(RRHEIDGxP_ErrPvtReport$`Jan-Mar` >= 5, "red", "green")
     ) 


# Merging columns 
tble_RRHEIDGxP <- tble_RRHEIDGxP %>%   
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertical HFacility and Region
tble_RRHEIDGxP <- tble_RRHEIDGxP %>%  
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_RRHEIDGxP <- tble_RRHEIDGxP %>%  
  vline(j = c(1, 3, 5), part = "all")

#Font and sice
tble_RRHEIDGxP <- tble_RRHEIDGxP %>%  
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_RRHEIDGxP <- tble_RRHEIDGxP %>%  
  add_header_row(values = "EID GxP Error rate by RRH",
                 colwidths = ncol(RRHEIDGxP_ErrPvtReport))

tble_RRHEIDGxP

######################################################################
######################################################################
#### Report - VL GxP Error Rate by health facility ####
## Error rate by health facility
VLGxP_e_rate <- VLGxP %>% 
  group_by(RRH,HFacility,Qtr) %>% 
  summarise(
    N_Tests  =   sum(GxPVL),
    N_Errors  =  sum(Errors, na.rm = TRUE),
    .groups = "drop"
  )
# pivot the table to re-arrange by quarter
VLGxP_e_ratePvt   <-  VLGxP_e_rate %>% 
  pivot_wider(
    id_cols = c(RRH,HFacility),
    names_from = Qtr,
    values_from = c(N_Tests,N_Errors)
  )

# calculate the error rate
VLGxP_e_ratePvt   <-  VLGxP_e_ratePvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))


# the table
VLGxP_ErrorReport   <-  VLGxP_e_ratePvt %>% 
  select(RRH,HFacility,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
  )

# create table
tble_VLGxP_ErrorReport <- flextable(VLGxP_ErrorReport)

#### Format the table
# Add the header row
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      HFacility   =  "Health Facility Name",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>%  
  set_header_labels(
    RRH    =  "RRH Regions",
    HFacility   =  "Health Facility Name",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>% 
  bg(j = "Oct-Dec", bg = ifelse(VLGxP_ErrorReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(VLGxP_ErrorReport$`Jan-Mar` >= 5, "red", "green")
     ) 


# Merging columns 
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>% 
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_VLGxP_ErrorReport <- tble_VLGxP_ErrorReport %>% 
  add_header_row(values = "VL GxP Error rate by Health Facility",
                 colwidths = ncol(VLGxP_ErrorReport))

tble_VLGxP_ErrorReport

#### Report - %age of health facilities with < 5% EID GxP error rate ####
VGxP_ER  <-  VLGxP_ErrorReport %>% 
  group_by(RRH) %>% 
  summarise(
    No.Sites  =  n(),
    `Oct-Dec`  = sum(`Oct-Dec`  < 5, na.rm = TRUE),
    `Jan-Mar`  = sum(`Jan-Mar`  < 5, na.rm = TRUE)
  ) %>% 
  ungroup()
#### Add national row
nat.VGxP_ER   <-  VGxP_ER %>% 
  summarise(
    RRH  =  "NATIONAL",
    No.Sites   =  sum(No.Sites),
    `Oct-Dec`  =  sum(`Oct-Dec`),
    `Jan-Mar`  =  sum(`Jan-Mar`)
  )
#### add to table
VGxP_ER   <-  rbind(VGxP_ER,nat.VGxP_ER)

# Include percentage of sites
VGxP_ER_Report   <-  VGxP_ER %>%
  rowwise() %>% 
  mutate(
    `Pct_Oct-Dec`   =  round((`Oct-Dec`/No.Sites)*100,1),
    `Pct_Jan-Mar`   =  round((`Jan-Mar`/No.Sites)*100,1)
  ) %>% 
  arrange(desc(RRH == "NATIONAL"))


# the table
tble_VGxP_ER_Report   <-  flextable(VGxP_ER_Report)


# Add the header row
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      No.Sites   =  "No. of GxP VL Testing Sites",
      "No. of sites with <5% VL GxP Error Rates",
      "",
      "% of sites with <5% VL GxP Error Rates",
      ""
    ))
# Set the header labels
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  set_header_labels(
    RRH    =  "RRH Regions",
    No.Sites   =  "No. of GxP VL Testing Sites",
    `Oct-Dec`  =  "Oct-Dec",
    `Jan-Mar` = "Jan-Mar",
    `Pct_Oct-Dec` = "Oct-Dec",
    `Pct_Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  bg(j = "Pct_Oct-Dec", bg = ifelse(VGxP_ER_Report$`Pct_Oct-Dec` >= 75, "green",
                                    ifelse(VGxP_ER_Report$`Pct_Oct-Dec` >= 50 & VGxP_ER_Report$`Pct_Oct-Dec` <= 74, "yellow", "red"))) %>%
  bg(j = "Pct_Jan-Mar", bg = ifelse(VGxP_ER_Report$`Pct_Jan-Mar` >= 75, "green",
                                    ifelse(VGxP_ER_Report$`Pct_Jan-Mar` >= 50 & VGxP_ER_Report$`Pct_Jan-Mar` <= 74, "yellow", "red"))
  )

# Merging columns 
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_VGxP_ER_Report  <- tble_VGxP_ER_Report  %>% 
  add_header_row(values = "%age of sites with <5% VL GxP error rate by RRH",
                 colwidths = ncol(VGxP_ER_Report))

tble_VGxP_ER_Report


####  Report - VL GxP Error rate by RRH ####
RRH_VL_GxP_Error <- VLGxP %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    N_Tests  =   sum(GxPVL),
    N_Errors  =  sum(Errors, na.rm = TRUE),
    .groups = "drop"
  )
# pivot the table to re-arrange by quarter
RRH_VL_GxP_ErrorPvt   <-  RRH_VL_GxP_Error %>% 
  pivot_wider(
    id_cols = c(RRH),
    names_from = Qtr,
    values_from = c(N_Tests,N_Errors)
  )

# calculate the error rate
RRH_VL_GxP_ErrorPvt   <-  RRH_VL_GxP_ErrorPvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))


# the table
RRH_VL_GxP_ErrorPvtReport   <-  RRH_VL_GxP_ErrorPvt %>% 
  select(RRH,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
  )

# create table
tble_RRH_VL_GxP_ErrorPvtReport <- flextable(RRH_VL_GxP_ErrorPvtReport)

#### Format the table
# Add the header row
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>%  
  set_header_labels(
    RRH    =  "RRH Regions",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>% 
  bg(j = "Oct-Dec", bg = ifelse(RRH_VL_GxP_ErrorPvtReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(RRH_VL_GxP_ErrorPvtReport$`Jan-Mar` >= 5, "red", "green")
  ) 


# Merging columns 
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>%   
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertical HFacility and Region
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>%  
  vline(j = c(1, 3, 5), part = "all")

#Font and sice
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>%  
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_RRH_VL_GxP_ErrorPvtReport <- tble_RRH_VL_GxP_ErrorPvtReport %>%   
  add_header_row(values = "VL GxP Error rate by RRH",
                 colwidths = ncol(RRH_VL_GxP_ErrorPvtReport))

tble_RRH_VL_GxP_ErrorPvtReport


#################################################################
#################################################################
#### Report - HPV GxP Error rate by health facility ####
## Error rate by health facility
HPVGxP_e_rate <- HPVGxP %>% 
  group_by(RRH,HFacility,Qtr) %>% 
  summarise(
    N_Tests  =   sum(GxPHPV),
    N_Errors  =  sum(Errors, na.rm = TRUE),
    .groups = "drop"
  )
# pivot the table to re-arrange by quarter
HPVGxP_e_ratePvt   <-  HPVGxP_e_rate %>% 
  pivot_wider(
    id_cols = c(RRH,HFacility),
    names_from = Qtr,
    values_from = c(N_Tests,N_Errors)
  )

# calculate the error rate
HPVGxP_e_ratePvt   <-  HPVGxP_e_ratePvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))


# the table
HPVGxP_e_ratePvtReport   <-  HPVGxP_e_ratePvt %>% 
  select(RRH,HFacility,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
  )

# create table
tble_HPVGxP_Report <- flextable(HPVGxP_e_ratePvtReport)

#### Format the table
# Add the header row
tble_HPVGxP_Report <- tble_HPVGxP_Report %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      HFacility   =  "Health Facility Name",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_HPVGxP_Report <- tble_HPVGxP_Report %>%  
  set_header_labels(
    RRH    =  "RRH Regions",
    HFacility   =  "Health Facility Name",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_HPVGxP_Report <- tble_HPVGxP_Report %>% 
  bg(j = "Oct-Dec", bg = ifelse(HPVGxP_e_ratePvtReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(HPVGxP_e_ratePvtReport$`Jan-Mar` >= 5, "red", "green")
  ) 


# Merging columns 
tble_HPVGxP_Report <- tble_HPVGxP_Report %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_HPVGxP_Report <- tble_HPVGxP_Report %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_HPVGxP_Report <- tble_HPVGxP_Report %>% 
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_HPVGxP_Report <- tble_HPVGxP_Report %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_HPVGxP_Report <- tble_HPVGxP_Report %>% 
  add_header_row(values = "HPV GxP Error rate by Health Facility",
                 colwidths = ncol(HPVGxP_e_ratePvtReport))

tble_HPVGxP_Report


### Report - %age sites with < 5% HPV GxP error rate ####
HPVGxP_ER  <-  HPVGxP_e_ratePvt %>% 
  group_by(RRH) %>% 
  summarise(
    No.Sites  =  n(),
    `Oct-Dec`  = sum(`Oct-Dec`  < 5, na.rm = TRUE),
    `Jan-Mar`  = sum(`Jan-Mar`  < 5, na.rm = TRUE)
  ) %>% 
  ungroup()
#### Add national row
nat.HPVGxP_ER   <-  HPVGxP_ER %>% 
  summarise(
    RRH  =  "NATIONAL",
    No.Sites   =  sum(No.Sites),
    `Oct-Dec`  =  sum(`Oct-Dec`),
    `Jan-Mar`  =  sum(`Jan-Mar`),
    .groups = "drop"
  )
#### add to table
HPVGxP_ER   <-  rbind(HPVGxP_ER,nat.HPVGxP_ER)

# Include percentage of sites
HPVGxP_ER_Report   <-  HPVGxP_ER %>%
  rowwise() %>% 
  mutate(
    `Pct_Oct-Dec`   =  round((`Oct-Dec`/No.Sites)*100,1),
    `Pct_Jan-Mar`   =  round((`Jan-Mar`/No.Sites)*100,1)
  ) %>% 
  arrange(desc(RRH == "NATIONAL"))


# the table
tble_HPVGxP_ER_Report   <-  flextable(HPVGxP_ER_Report)


# Add the header row
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      No.Sites   =  "No. of GxP HPV Testing Sites",
      "No. of sites with <5% HPV GxP Error Rates",
      "",
      "% of sites with <5% HPV GxP Error Rates",
      ""
    ))
# Set the header labels
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>% 
  set_header_labels(
    RRH    =  "RRH Regions",
    No.Sites   =  "No. of GxP HPV Testing Sites",
    `Oct-Dec`  =  "Oct-Dec",
    `Jan-Mar` = "Jan-Mar",
    `Pct_Oct-Dec` = "Oct-Dec",
    `Pct_Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>% 
  bg(j = "Pct_Oct-Dec", bg = ifelse(HPVGxP_ER_Report$`Pct_Oct-Dec` >= 75, "green",
                                    ifelse(HPVGxP_ER_Report$`Pct_Oct-Dec` >= 50 & HPVGxP_ER_Report$`Pct_Oct-Dec` <= 74, "yellow", "red"))) %>%
  bg(j = "Pct_Jan-Mar", bg = ifelse(HPVGxP_ER_Report$`Pct_Jan-Mar` >= 75, "green",
                                    ifelse(HPVGxP_ER_Report$`Pct_Jan-Mar` >= 50 & HPVGxP_ER_Report$`Pct_Jan-Mar` <= 74, "yellow", "red"))
  )

# Merging columns 
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>%
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_HPVGxP_ER_Report   <- tble_HPVGxP_ER_Report   %>%
  add_header_row(values = "%age of sites with <5% HPV GxP error rate by RRH",
                 colwidths = ncol(HPVGxP_ER_Report))

tble_HPVGxP_ER_Report

#### HPV GxP Error rate by RRH ####
RRH_HPV_GxP_Error <- HPVGxP %>%
  group_by(RRH,Qtr) %>% 
  summarise(
    N_Tests  =   sum(GxPHPV),
    N_Errors  =  sum(Errors, na.rm = TRUE),
    .groups = "drop"
  )
# pivot the table to re-arrange by quarter
RRH_HPV_GxP_ErrorPvt   <-  RRH_HPV_GxP_Error %>% 
  pivot_wider(
    id_cols = c(RRH),
    names_from = Qtr,
    values_from = c(N_Tests,N_Errors)
  )

# calculate the error rate
RRH_HPV_GxP_ErrorPvt   <-  RRH_HPV_GxP_ErrorPvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))


# the table
RRH_HPV_GxP_ErrorPvtReport   <-  RRH_HPV_GxP_ErrorPvt %>% 
  select(RRH,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
  )

# create table
tble_RRH_HPV_Rt <- flextable(RRH_HPV_GxP_ErrorPvtReport)

#### Format the table
# Add the header row
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>%  
  set_header_labels(
    RRH    =  "RRH Regions",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>% 
  bg(j = "Oct-Dec", bg = ifelse(RRH_HPV_GxP_ErrorPvtReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(RRH_HPV_GxP_ErrorPvtReport$`Jan-Mar` >= 5, "red", "green")
  ) 


# Merging columns 
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>%  
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertical HFacility and Region
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>%  
  vline(j = c(1, 3, 5), part = "all")

#Font and sice
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_RRH_HPV_Rt <- tble_RRH_HPV_Rt %>%  
  add_header_row(values = "HPV GxP Error rate by RRH",
                 colwidths = ncol(RRH_HPV_GxP_ErrorPvtReport))

tble_RRH_HPV_Rt


######################################################################
######################################################################
#### Report - TB GxP Error Rate by health facility ####
TBGxP_e_rate <- TBGxP %>% 
  group_by(RRH,HFacility,Qtr) %>% 
  summarise(
    N_Tests  =   sum(GxPTB),
    N_Errors  =  sum(Errors, na.rm = TRUE),
    .groups = "drop"
  )
# pivot the table to re-arrange by quarter
TBGxP_e_ratePvt   <-  TBGxP_e_rate %>% 
  pivot_wider(
    id_cols = c(RRH,HFacility),
    names_from = Qtr,
    values_from = c(N_Tests,N_Errors)
  )

# calculate the error rate
TBGxP_e_ratePvt   <-  TBGxP_e_ratePvt %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))


# the table
TBGxP_e_ratePvtReport   <-  TBGxP_e_ratePvt %>% 
  select(RRH,HFacility,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
  )

# create table
tble_TBGxP_Error <- flextable(TBGxP_e_ratePvtReport)

#### Format the table
# Add the header row
tble_TBGxP_Error <- tble_TBGxP_Error %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      HFacility   =  "Health Facility Name",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_TBGxP_Error <- tble_TBGxP_Error %>%  
  set_header_labels(
    RRH    =  "RRH Regions",
    HFacility   =  "Health Facility Name",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_TBGxP_Error <- tble_TBGxP_Error %>%
  bg(j = "Oct-Dec", bg = ifelse(TBGxP_e_ratePvtReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(TBGxP_e_ratePvtReport$`Jan-Mar` >= 5, "red", "green")
  ) 


# Merging columns 
tble_TBGxP_Error <- tble_TBGxP_Error %>%
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_TBGxP_Error <- tble_TBGxP_Error %>%
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_TBGxP_Error <- tble_TBGxP_Error %>%
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_TBGxP_Error <- tble_TBGxP_Error %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_TBGxP_Error <- tble_TBGxP_Error %>%
  add_header_row(values = "TB GxP Error rate by Health Facility",
                 colwidths = ncol(TBGxP_e_ratePvtReport))

tble_TBGxP_Error

### Report - Percentage of health facilities with < 5% TB GxP error rate ####
TBGxP_ER  <-  TBGxP_e_ratePvtReport %>% 
  group_by(RRH) %>% 
  summarise(
    No.Sites  =  n(),
    `Oct-Dec`  = sum(`Oct-Dec`  < 5, na.rm = TRUE),
    `Jan-Mar`  = sum(`Jan-Mar`  < 5, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ungroup()
#### Add national row
nat.TBGxP_ER    <-  TBGxP_ER  %>% 
  summarise(
    RRH  =  "NATIONAL",
    No.Sites   =  sum(No.Sites),
    `Oct-Dec`  =  sum(`Oct-Dec`),
    `Jan-Mar`  =  sum(`Jan-Mar`)
  )
#### add to table
TBGxP_ER   <-  rbind(TBGxP_ER,nat.TBGxP_ER)

# Include percentage of sites
TBGxP_ER_Report   <-  TBGxP_ER %>%
  rowwise() %>% 
  mutate(
    `Pct_Oct-Dec`   =  round((`Oct-Dec`/No.Sites)*100,1),
    `Pct_Jan-Mar`   =  round((`Jan-Mar`/No.Sites)*100,1)
  ) %>% 
  arrange(desc(RRH == "NATIONAL"))


# the table
tble_TBGxP_ER_Report  <-  flextable(TBGxP_ER_Report)


# Add the header row
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      No.Sites   =  "No. of TB VL Testing Sites",
      "No. of sites with <5% TB GxP Error Rates",
      "",
      "% of sites with <5% TB GxP Error Rates",
      ""
    ))
# Set the header labels
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>% 
  set_header_labels(
    RRH    =  "RRH Regions",
    No.Sites   =  "No. of GxP TB Testing Sites",
    `Oct-Dec`  =  "Oct-Dec",
    `Jan-Mar` = "Jan-Mar",
    `Pct_Oct-Dec` = "Oct-Dec",
    `Pct_Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>%  
  bg(j = "Pct_Oct-Dec", bg = ifelse(TBGxP_ER_Report$`Pct_Oct-Dec` >= 75, "green",
                                    ifelse(TBGxP_ER_Report$`Pct_Oct-Dec` >= 50 & TBGxP_ER_Report$`Pct_Oct-Dec` <= 74, "yellow", "red"))) %>%
  bg(j = "Pct_Jan-Mar", bg = ifelse(TBGxP_ER_Report$`Pct_Jan-Mar` >= 75, "green",
                                    ifelse(TBGxP_ER_Report$`Pct_Jan-Mar` >= 50 & TBGxP_ER_Report$`Pct_Jan-Mar` <= 74, "yellow", "red"))
  )

# Merging columns 
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>%  
  merge_at(i = 1, j = 3:4, part = "header") %>%
  merge_at(i = 1, j = 5:6, part = "header")

# Merging vertical HFacility and Region
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>%  
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>%  
  vline(j = c(1, 2, 4,6), part = "all")

#Font and sice
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>%  
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_TBGxP_ER_Report  <- tble_TBGxP_ER_Report  %>%  
  add_header_row(values = "%age of sites with <5% TB GxP error rate by RRH",
                 colwidths = ncol(TBGxP_ER_Report))

tble_TBGxP_ER_Report


#### report - TB GxP Error rate by RRH ####
RRH_GxP_Error <- TBGxP %>% 
  group_by(RRH,Qtr) %>% 
  summarise(
    N_Tests  =   sum(GxPTB),
    N_Errors  =  sum(Errors, na.rm = TRUE),
    .groups = "drop"
  )
# pivot the table to re-arrange by quarter
RRH_GxP_ErrorPvt   <-  RRH_GxP_Error %>% 
  pivot_wider(
    id_cols = c(RRH),
    names_from = Qtr,
    values_from = c(N_Tests,N_Errors)
  )

# calculate the error rate
RRH_GxP_ErrorPvt    <-  RRH_GxP_ErrorPvt  %>% 
  mutate(
    `Oct-Dec`   =   round((`N_Errors_Oct-Dec`/`N_Tests_Oct-Dec`)*100,1),
    `Jan-Mar`   =   round((`N_Errors_Jan-Mar`/`N_Tests_Jan-Mar`)*100,1)
  ) %>% 
  replace_na(list(`N_Tests_Oct-Dec`=0,`N_Tests_Jan-Mar`=0))


# the table
RRH_GxP_ErrorPvtReport   <-  RRH_GxP_ErrorPvt  %>% 
  select(RRH,`N_Tests_Oct-Dec`,`N_Tests_Jan-Mar`,`Oct-Dec`,`Jan-Mar`
  )

# create table
tble_RRH_TBReport <- flextable(RRH_GxP_ErrorPvtReport)

#### Format the table
# Add the header row
tble_RRH_TBReport <- tble_RRH_TBReport %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Regions",
      "Test Volumes by Quarter",
      "",
      "Error Rate by Quarter",
      ""
    ))
# Set the header labels
tble_RRH_TBReport <- tble_RRH_TBReport %>% 
  set_header_labels(
    RRH    =  "RRH Regions",
    `N_Tests_Oct-Dec`  =  "Oct-Dec",
    `N_Tests_Jan-Mar` = "Jan-Mar",
    `Oct-Dec` = "Oct-Dec",
    `Jan-Mar` = "Jan-Mar")

# Color the columns based on the facility without lab tests
tble_RRH_TBReport <- tble_RRH_TBReport %>% 
  bg(j = "Oct-Dec", bg = ifelse(RRH_GxP_ErrorPvtReport$`Oct-Dec` >= 5, "red", "green")) %>% 
  bg(j = "Jan-Mar", bg = ifelse(RRH_GxP_ErrorPvtReport$`Jan-Mar` >= 5, "red", "green")
  ) 


# Merging columns 
tble_RRH_TBReport <- tble_RRH_TBReport %>%  
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header")

# Merging vertical HFacility and Region
tble_RRH_TBReport <- tble_RRH_TBReport %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_v(j = 2, part = "header")


# Adding vertical lines to improve readability
tble_RRH_TBReport <- tble_RRH_TBReport %>%  
  vline(j = c(1, 3, 5), part = "all")

#Font and sice
tble_RRH_TBReport <- tble_RRH_TBReport %>%   
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")
## add tile
tble_RRH_TBReport <- tble_RRH_TBReport %>%  
  add_header_row(values = "TB GxP Error rate by RRH",
                 colwidths = ncol(RRH_GxP_ErrorPvtReport))

tble_RRH_TBReport


########################################################################
##########################################################################
#### Report - RRH All tests, Error rate by RRH summary ####
all_error <- RRH_GxP_ErrorPvtReport %>% 
  select(RRH, `Oct-Dec`, `Jan-Mar`) %>% 
  left_join(RRHEIDGxP_ErrPvtReport %>% select(RRH, `Oct-Dec`, `Jan-Mar`), by = "RRH") %>% 
  left_join(RRH_VL_GxP_ErrorPvtReport %>% select(RRH, `Oct-Dec`, `Jan-Mar`), by = "RRH") %>% 
  left_join(RRH_HPV_GxP_ErrorPvtReport %>% select(RRH, `Oct-Dec`, `Jan-Mar`), by = "RRH") %>% 
  left_join(RRH_Qtr_ErrReport %>% select(RRH, `Oct-Dec`, `Jan-Mar`), by = "RRH") %>% 
  left_join(RRH_EIDQtr_ErrReport %>% select(RRH, `Oct-Dec`, `Jan-Mar`), by = "RRH"
            )

#### Define the coloring threshold of the columns
apply_bg_color  <-  function(column_data){
  ifelse(is.na(column_data),"grey",
    ifelse(column_data <=5.0,"green", "red"))
}

# the table
tble_all_error    <-   flextable(all_error)

## indicate column data of interest
columns   =  c("Oct-Dec.x","Jan-Mar.x","Oct-Dec.y","Jan-Mar.y","Oct-Dec.x.x","Jan-Mar.x.x","Oct-Dec.y.y",  
               "Jan-Mar.y.y","Oct-Dec.x.x.x","Jan-Mar.x.x.x","Oct-Dec.y.y.y","Jan-Mar.y.y.y"
               )

# apply the background color using loop
for (col in columns) {
  tble_all_error    <-  tble_all_error %>% 
    bg(j = col, bg = apply_bg_color(all_error[[col]]))
}

# Table headings
tble_all_error    <-  tble_all_error %>% 
  add_header_row(
    values = c(
      RRH    =  "RRH Region",
      "TB GxP",
      "",
      "EID GxP",
      "",
      "VL GxP",
      "",
      "HPV GxP",
      "",
      "VL m-Pima",
      "",
      "EID m-Pima",
      "")
  )
# Set the header labels
tble_all_error    <-  tble_all_error %>%
  set_header_labels(
    RRH    =  "RRH Region",
    `Oct-Dec.x`  =  "Oct-Dec",
    `Jan-Mar.x` = "Jan-Mar",
    `Oct-Dec.y` = "Oct-Dec",
    `Jan-Mar.y` = "Jan-Mar",
    `Oct-Dec.x.x`  =  "Oct-Dec",
    `Jan-Mar.x.x` = "Jan-Mar",
    `Oct-Dec.y.y` = "Oct-Dec",
    `Jan-Mar.y.y` = "Jan-Mar",
    `Oct-Dec.x.x.x` = "Oct-Dec",
    `Jan-Mar.x.x.x` = "Jan-Mar",
    `Oct-Dec.y.y.y` = "Oct-Dec",
    `Jan-Mar.y.y.y` = "Jan-Mar"
        )
  
# Merging columns 
tble_all_error    <-  tble_all_error %>%
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header") %>% 
  merge_at(i = 1, j = 6:7, part = "header") %>% 
  merge_at(i = 1, j = 8:9, part = "header") %>% 
  merge_at(i = 1, j = 10:11, part = "header") %>% 
  merge_at(i = 1, j = 12:13, part = "header")

# Merging vertical HFacility and Region
tble_all_error    <-  tble_all_error %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tble_all_error    <-  tble_all_error %>%
  vline(j = c(1, 3, 5,7,9,11,13), part = "all")

#Font and sice
tble_all_error    <-  tble_all_error %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tble_all_error    <-  tble_all_error %>% 
  add_header_row(values = "Error rate by Test and RRH",
                 colwidths = ncol(all_error))

tble_all_error
  
############################################################
###############################################################
#### Report - Quarterly Error rates ####
#### create TB GxP data frame
TBGxPall   <-  TBGxP_e_ratePvt %>% 
  mutate(
    Test_Type  =  "TBGxP"
  )%>% 
  ungroup()

#### create  HPV data frame
HPVGxPall    <-  HPVGxP_e_ratePvt %>% 
  mutate(
    Test_Type   =  "HPVGxP"
  )%>% 
  ungroup()

#### create VL data frame
VLGxPall   <-   VLGxP_e_ratePvt %>% 
  mutate(
    Test_Type  =  "VLGxP"
  )%>% 
  ungroup()

#### Create EID GxP data frame
EIDGxPall   <-   EIDGxP_ErrPvt %>% 
  mutate(
    Test_Type   =  "EIDGxP"
  ) %>% 
  ungroup()

#### Combine all GxP error rates
allGxPE_Rate  <-  rbind(TBGxPall,HPVGxPall,VLGxPall,EIDGxPall)

#### create GxP error rate data frame
GxP_all   <-   allGxPE_Rate %>% 
  group_by(Test_Type) %>% 
  summarise(
    TOct_Dec   =   sum(`N_Tests_Oct-Dec`, na.rm = TRUE),
    TJan_Mar   =   sum(`N_Tests_Jan-Mar`, na.rm = TRUE),
    EOct_Dec   =   sum(`N_Errors_Oct-Dec`, na.rm = TRUE),
    EJan_Mar   =   sum(`N_Errors_Jan-Mar`, na.rm = TRUE)
  )

### CALCULATE ERROR RATE
GxP_all   <-   GxP_all %>% 
      mutate(
        `Oct-Dec`   =   round((EOct_Dec/TOct_Dec)*100,1),
        `Jan-Mar`   =   round((EJan_Mar/TJan_Mar)*100,1)
      ) %>% 
     select(Test_Type,`Oct-Dec`,`Jan-Mar`)
#### Re-organise the data frame
GxP_allPvt   <-   GxP_all %>% 
  pivot_longer(
    cols = c(`Oct-Dec`,`Jan-Mar`),
    names_to = "Qtr",
    values_to = "E_Rate"
  )

#### Create EID m-Pima daa frame
EIDmPima <- EIDPima_e_rate %>%
  group_by(Qtr) %>% 
  summarise(
    E_Rate = round(sum(N_Errors) / sum(Total) * 100, 1),
    .groups = "drop") %>% 
  mutate(
    Test_Type  =  "EIDmPima",
    Qtr        = factor(Qtr, levels = c("Oct-Dec", "Jan-Mar")) # Ensure correct order
  ) %>% 
  filter(Qtr %in% c("Oct-Dec", "Jan-Mar")) %>% 
  arrange(Qtr) %>% # Sort by factor order
  select(Test_Type,Qtr,E_Rate)

#### Create VL m-Pima data frame
VLmPima   <-  VLPima_e_rate %>% 
group_by(Qtr) %>% 
  summarise(
    E_Rate = round(sum(N_Errors) / sum(Total) * 100, 1),
    .groups = "drop") %>% 
  mutate(
    Test_Type  =  "VLmPima",
    Qtr        =  factor(Qtr, levels = c("Oct-Dec", "Jan-Mar"))
  ) %>% 
  filter(Qtr %in% c("Oct-Dec", "Jan-Mar")) %>% 
  arrange(Qtr) %>% 
  select(Test_Type,Qtr,E_Rate)

#### Combine all data frame to report error rate for all
all_err <-  rbind(GxP_allPvt,EIDmPima,VLmPima)


#### The graph

# Ensure Qtr has the correct order
all_err$Qtr <- factor(all_err$Qtr, levels = c("Oct-Dec", "Jan-Mar"))


#### the graph
ggplot(data = all_err, aes(fill = Qtr, y = E_Rate, x = Test_Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = E_Rate), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) + # Correct text positioning
  labs(
    title = "Error Rates by Test Type Across Quarters",
    x = "Test Type",
    y = "Error Rate (%)"
  ) +
  scale_fill_manual(values = c("Oct-Dec" = "steelblue", "Jan-Mar" = "orange")) + # Optional custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


###########################################################
###############################################################
#### Report - mPima - Most common error codes ####
#### Report - m-Pima Most common EID Error codes ####
EIDPima_Codes  <-  Pima_data %>% 
  select(HFacility,District,RRH,Error,Qtr) %>% 
    mutate("Test_type"   =  "EIDPima")

#### The analysis common errors 
EIDmPima_ErrC   <-  EIDPima_Codes %>% 
  group_by(Qtr,Error) %>% 
  summarise(
    No_Error_occurences  =  n(),
    .groups = "drop"  # Ensures that grouping is removed after summarizing
  ) 
#### re arrange the data set
EIDmPima_ErrPvt    <-  EIDmPima_ErrC %>% 
  filter(Error !=  "0") %>% 
  pivot_wider(
    id_cols = c(Error),
    names_from = Qtr,
    values_from = No_Error_occurences
  )

#### total error
nat.EIDmPima_Err <- EIDmPima_ErrPvt %>% 
  summarise(
    Error = "TOTAL",
    across(where(is.numeric), sum, na.rm = TRUE)
  )

# Add a row for "TOTAL"
nat.EIDmPima_Err <- nat.EIDmPima_Err %>% 
  mutate(Error = "TOTAL") %>% 
  select(Error, everything())


# Calculate percentages by dividing each value by the total for its quarter
EIDmPima_ErrPvt_pct <- EIDmPima_ErrPvt %>% 
  mutate(
    across(where(is.numeric), 
           ~ round((.x / nat.VLmPima_Err[[cur_column()]]) * 100, 1)
    )) %>% 
  arrange(desc(!!last(colnames(select(.,where(is.numeric))))))

#### include column on error names
EIDmPima_Report  <-  EIDmPima_ErrPvt_pct %>% 
  left_join(mPima_error, by = c("Error" = "Error_Code")
      ) %>% 
  select(Error,Error_Reason,Error_category,where(is.numeric))


#### the table
tbl_EIDmPima_ErrPvt  <-  flextable(EIDmPima_Report)

tbl_EIDmPima_ErrPvt  <-  tbl_EIDmPima_ErrPvt %>% 
  add_header_row(values = "Common Error Codes (EID)",
                 colwidths = ncol(EIDmPima_Report))
tbl_EIDmPima_ErrPvt
#### Report - %age of sites with indicated EID error codes ####
# Calculate distinct HFacilities in the whole dataset
Total_Hfs <- n_distinct(EIDPima_Codes$HFacility)

# Calculate the number of health facilities with errors (where Error is not "0")
Total_Errors <- EIDPima_Codes %>% 
  filter(Error != "0") %>% 
  summarise(n = n_distinct(HFacility)) %>% 
  pull(n)

# Grouped summarization with Total_Hfs added
No_EIDHfs <- EIDPima_Codes %>% 
  group_by(Qtr, Error) %>% 
  summarise(
    No_HFs_witherror = n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  mutate(
    Total_Hfs = Total_Hfs,
    Total_Errors = Total_Errors,
    `Pct_of_all_HFs` = round((No_HFs_witherror / Total_Hfs) * 100, 0),
    `%_sites_with_error` = round((No_HFs_witherror / Total_Errors) * 100, 0)
  )

#### Re-organize the data frame
No_EIDHfsPvt    <-  No_EIDHfs %>% 
  filter(Error  != "0") %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = c("No_HFs_witherror","Total_Hfs","Total_Errors","Pct_of_all_HFs",   
                    "%_sites_with_error")
  ) %>% 
  select(Error,`Total_Hfs_Oct-Dec`,`No_HFs_witherror_Oct-Dec`,`Total_Errors_Oct-Dec`,`Pct_of_all_HFs_Oct-Dec`,
         `%_sites_with_error_Oct-Dec`)

#### Add Error meaning
No_EIDHfReport  <-   No_EIDHfsPvt %>% 
  left_join(mPima_error, by = c("Error" = "Error_Code")
  ) %>% 
  select(Error,Error_Reason,Error_category,where(is.numeric))

#### the table
tble_No_EIDHfsPvt   <-  flextable(No_EIDHfReport)

# Table headings
tble_No_EIDHfsPvt    <-  tble_No_EIDHfsPvt %>% 
  add_header_row(
    values = c(
      Error    =  "Error Code",
      "Error_Reason"        =   "Error Description",
      "Error_category"      =   "Error Category",
      "Total_Hfs_Oct-Dec"   =   "# of reporting sites (Oct-Dec)",
      "No_HFs_witherror_Oct-Dec"  =   "No. of sites with indicated Error (Oct-Dec)",
      "Total_Errors_Oct-Dec"      =   "Total No. of sites reporting Error (Oct-Dec)",
      "Pct_of_all_HFs_Oct-Dec"    =   "%age of total number of sites reporting Error",
      "%_sites_with_error_Oct-Dec"  =  "% of the sites reporting an Error occurence with the indicated Error"
    )
  )

# Set the header labels
tble_No_EIDHfsPvt    <-  tble_No_EIDHfsPvt %>%
  set_header_labels(
    Error    =  "Error Code",
    "Error_Reason"        =   "Error Description",
    "Error_category"      =   "Error Category",
    "Total_Hfs_Oct-Dec"   =   "# of reporting sites (Oct-Dec)",
    "No_HFs_witherror_Oct-Dec"  =   "No. of sites with indicated Error (Oct-Dec)",
    "Total_Errors_Oct-Dec"      =   "Total No. of sites reporting Error (Oct-Dec)",
    "Pct_of_all_HFs_Oct-Dec"    =   "%age of total number of sites reporting Error",
    "%_sites_with_error_Oct-Dec"  =  "% of the sites reporting an Error occurence with the indicated Error"
  )

# Merging columns 


# Merging vertical HFacility and Region
tble_No_EIDHfsPvt    <-  tble_No_EIDHfsPvt %>%
  merge_v(j = 1:ncol(No_EIDHfReport), part = "header") 

# Adding vertical lines to improve readability
tble_No_EIDHfsPvt    <-  tble_No_EIDHfsPvt %>%
  vline(j = 1:ncol(No_EIDHfReport), part = "all")

#Font and sice
tble_No_EIDHfsPvt    <-  tble_No_EIDHfsPvt %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tble_No_EIDHfsPvt    <-  tble_No_EIDHfsPvt %>%
  add_header_row(values = "Proportion of sites with the respective EID m-Pima errors",
                 colwidths = ncol(No_EIDHfReport))


tble_No_EIDHfsPvt

#################################################################
#### Report -   VL m-Pima most common errors ####
VLPima_Codes    <-  VLPima_data %>% 
  select(HFacility,District,RRH,Error,Qtr)%>% 
  mutate("Test_type"   =  "VLPima")

#### The analysis common errors 
VLmPima_ErrC   <-  VLPima_Codes %>% 
  group_by(Qtr,Error) %>% 
  summarise(
    No_Error_occurences  =  n(),
    .groups = "drop"  # Ensures that grouping is removed after summarizing
  ) 

#### re arrange the data set
VLmPima_ErrPvt    <-  VLmPima_ErrC %>% 
  filter(Error !=  "0") %>% 
  pivot_wider(
    id_cols = c(Error),
    names_from = Qtr,
    values_from = No_Error_occurences
  )

#### total error
nat.VLmPima_Err <- VLmPima_ErrPvt %>% 
  summarise(
    Error = "TOTAL",
    across(where(is.numeric), sum, na.rm = TRUE)
  )

# Add a row for "TOTAL"
nat.VLmPima_Err <- nat.VLmPima_Err %>% 
  mutate(Error = "TOTAL") %>% 
  select(Error, everything())


# Calculate percentages by dividing each value by the total for its quarter
VLmPima_ErrPvt_pct <- VLmPima_ErrPvt %>% 
  mutate(
    across(
      where(is.numeric), 
      ~ round((.x / nat.VLmPima_Err[[cur_column()]]) * 100, 1)
    )) %>% 
  arrange(desc(!!last(colnames(select(.,where(is.numeric))))))

#### Include Error meaning
VLmPima_Report   <-   VLmPima_ErrPvt_pct %>% 
  left_join(mPima_error, by = c("Error" = "Error_Code")
) %>% 
  select(Error,Error_Reason,Error_category,where(is.numeric))

### the table
tbl_VLmPima_ErrPvt   <-  flextable(VLmPima_Report)

# including title
tbl_VLmPima_ErrPvt  <-  tbl_VLmPima_ErrPvt %>% 
  add_header_row(values = "Common VL EID Error Codes (VL)",
                 colwidths = ncol(VLmPima_Report))
tbl_VLmPima_ErrPvt
#### Report - %age of sites with indicated VL error codes ####
VLTotal_Hfs <- n_distinct(VLPima_Codes$HFacility)

# Calculate the number of health facilities with errors (where Error is not "0")
VTotal_Errors <- VLPima_Codes %>% 
  filter(Error != "0") %>% 
  summarise(n = n_distinct(HFacility)) %>% 
  pull(n)

# Grouped summarization with Total_Hfs added
No_VLHfs <- VLPima_Codes %>% 
  group_by(Qtr, Error) %>% 
  summarise(
    No_HFs_witherror = n_distinct(HFacility),
    .groups = "drop"
  ) %>% 
  mutate(
    Total_Hfs = Total_Hfs,
    Total_Errors = Total_Errors,
    `Pct_of_all_HFs` = round((No_HFs_witherror / Total_Hfs) * 100, 0),
    `%_sites_with_error` = round((No_HFs_witherror / Total_Errors) * 100, 0)
  )

#### Re-organize the data frame
No_VLHfsPvt    <-  No_VLHfs %>% 
  filter(Error  != "0") %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = c("No_HFs_witherror","Total_Hfs","Total_Errors","Pct_of_all_HFs",   
                    "%_sites_with_error")
  ) %>% 
  select(Error,`Total_Hfs_Oct-Dec`,`No_HFs_witherror_Oct-Dec`,`Total_Errors_Oct-Dec`,`Pct_of_all_HFs_Oct-Dec`,
         `%_sites_with_error_Oct-Dec`)

#### include the column for error description
No_VLHfReport   <-   No_VLHfsPvt %>% 
  left_join(mPima_error, by = c("Error" = "Error_Code")
    ) %>% 
      select(Error,Error_Reason,Error_category,where(is.numeric))

#### the table
tble_No_VLHfsPvt   <-  flextable(No_VLHfReport)

# Table headings
tble_No_VLHfsPvt    <-  tble_No_VLHfsPvt %>% 
  add_header_row(
    values = c(
      Error    =  "Error Code",
      "Error_Reason"        =   "Error Description",
      "Error_category"      =   "Error Category",
      "Total_Hfs_Oct-Dec"   =   "# of reporting sites (Oct-Dec)",
      "No_HFs_witherror_Oct-Dec"  =   "No. of sites with indicated Error (Oct-Dec)",
      "Total_Errors_Oct-Dec"      =   "Total No. of sites reporting Error (Oct-Dec)",
      "Pct_of_all_HFs_Oct-Dec"    =   "%age of total number of sites reporting Error",
      "%_sites_with_error_Oct-Dec"  =  "% of the sites reporting an Error occurence with the indicated Error"
    )
  )

# Set the header labels
tble_No_VLHfsPvt    <-  tble_No_VLHfsPvt %>% 
  set_header_labels(
    Error    =  "Error Code",
    "Error_Reason"        =   "Error Description",
    "Error_category"      =   "Error Category",
    "Total_Hfs_Oct-Dec"   =   "# of reporting sites (Oct-Dec)",
    "No_HFs_witherror_Oct-Dec"  =   "No. of sites with indicated Error (Oct-Dec)",
    "Total_Errors_Oct-Dec"      =   "Total No. of sites reporting Error (Oct-Dec)",
    "Pct_of_all_HFs_Oct-Dec"    =   "%age of total number of sites reporting Error",
    "%_sites_with_error_Oct-Dec"  =  "% of the sites reporting an Error occurence with the indicated Error"
  )

# Merging columns 


# Merging vertical HFacility and Region
tble_No_VLHfsPvt    <-  tble_No_VLHfsPvt %>% 
  merge_v(j = 1:ncol(No_VLHfReport), part = "header") 

# Adding vertical lines to improve readability
tble_No_VLHfsPvt    <-  tble_No_VLHfsPvt %>% 
  vline(j = 1:ncol(No_VLHfReport), part = "all")

#Font and sice
tble_No_VLHfsPvt    <-  tble_No_VLHfsPvt %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tble_No_VLHfsPvt    <-  tble_No_VLHfsPvt %>% 
  add_header_row(values = "Proportion of sites with the respective VL m-Pima errors",
                 colwidths = ncol(No_VLHfReport))


tble_No_VLHfsPvt

#################################################################
#################################################################
#### Report - Most common number of occurrences GxP Error Codes ####
GxPErrorC     <-   GxPError %>% 
  group_by(Error,Qtr) %>%
  summarise(
          N    =   sum(`No.of occurance`), .groups =  "drop")

# Re-arrange the data frame
GxPErrPvt   <- GxPErrorC %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = N
  )
# calculate the total
All_err <-  GxPErrPvt %>%
  summarise(
  Error  =   "Total",
  `Jan-Mar`  =  sum(`Jan-Mar`,na.rm = TRUE),
  `Oct-Dec`  =  sum(`Oct-Dec`, na.rm = TRUE)
  )

#### Pull the totals
T_Oct_Dec  <-  All_err %>% pull(`Oct-Dec`)
T_Jan_Mar  <-  All_err  %>% pull(`Jan-Mar`)

# Add the data frames
GxPErrPvt   <-  rbind(GxPErrPvt,All_err)

### add proportions
GxPErrPvt <- GxPErrPvt %>%
  rowwise() %>%
  mutate(
    `%Oct-Dec` = ifelse(is.na(`Oct-Dec`) | is.na(T_Oct_Dec), NA, round((`Oct-Dec` / T_Oct_Dec) * 100, 0)),
    `%Jan-Mar` = ifelse(is.na(`Jan-Mar`) | is.na(T_Jan_Mar), NA, round((`Jan-Mar` / T_Jan_Mar) * 100, 0))
  ) %>%
  ungroup()


#### add the Error meanings
GxPErrReport  <-   GxPErrPvt %>%
  left_join(gxp_error, by = c("Error" = "Error Code")) %>% 
    select(Error,Error_Category,`Possible Causes`,`Oct-Dec`,`Jan-Mar`,`%Oct-Dec`,`%Jan-Mar`) %>% 
     # split the is total
  mutate(is_total =  ifelse(Error == "Total", TRUE,FALSE)) %>% 
      arrange(desc(`%Jan-Mar`)) %>% 
      # ENSURE TOTAL STAYS AT THE BOTTOM
      arrange(is_total) %>% 
        select(-is_total)
  
#### the table
tbl_GxPErrorCpvt   <-  flextable(GxPErrReport)
tbl_GxPErrorCpvt
# format the table
tbl_GxPErrorCpvt   <-  tbl_GxPErrorCpvt %>% 
  add_header_row(
    values = c(
      Error   =  "Error Code",
      Error_Category   =  "Error Category",
      `Possible Causes`   =  "Possible Causes",
      "No. of occurences of the respective errors in the indicated reporting period",
      "",
      "%of of the occurence of the respective errors in the indicated period",
      ""
    ))
# setting the header
tbl_GxPErrorCpvt   <-  tbl_GxPErrorCpvt %>% 
  set_header_labels(
    Error   =  "Error Code",
    Error_Category   =  "Error Category",
    `Possible Causes`   =  "Possible Causes",
    `Oct-Dec`  =  "Oct-Dec",
    `Jan-Mar` =  "Jan-Mar",
    `%Oct-Dec`  =  "%Oct-Dec",
    `%Jan-Mar`  =  "%Jan-Mar"
  )
# Merging columns
tbl_GxPErrorCpvt   <-  tbl_GxPErrorCpvt %>% 
  merge_at(i = 1, j = 4:5, part = "header") %>% 
  merge_at(i = 1, j = 6:7, part = "header")

# Merging vertical HFacility and Region
tbl_GxPErrorCpvt   <-  tbl_GxPErrorCpvt %>% 
  merge_v(j = 1:3, part = "header") 

# Adding vertical lines to improve readability
tbl_GxPErrorCpvt   <-  tbl_GxPErrorCpvt %>% 
  vline(j = c(1,2,3,5,7), part = "all")

#Font and sice
tbl_GxPErrorCpvt   <-  tbl_GxPErrorCpvt %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tbl_GxPErrorCpvt   <-  tbl_GxPErrorCpvt %>% 
  add_header_row(values = "Common Reasons for GXP Error and Extent of Occurence",
                 colwidths = ncol(GxPErrReport))

tbl_GxPErrorCpvt

#### Report - %age of health facilities with the errors ####
PctHFs   <-  GxPError %>% 
  left_join(dhis2names, by = c("Site"  =  "HFacility")) # add the RRH columns

# Calculate total number of unique sites with the respective errors
Total_sites <- PctHFs %>% 
  group_by(Error,Qtr) %>% 
    summarise(No.sites  =  n(),
              .groups = "drop")

#### No. sites by Quarter
Total_sitesPvt   <-   Total_sites %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No.sites
  )

#### obtain the total number of sites by quarter
GxPErrorSites <- GxPError %>%
  group_by(Qtr) %>%
  summarise(
    No_of_Sites = n_distinct(Site)
    )

#### re arrange the table to allow further analysis
GxPErrorSites_wide <- GxPErrorSites %>%
  pivot_wider(names_from = Qtr, values_from = No_of_Sites)


#### calculating the proportions
Total_sitesPvt  <-  Total_sitesPvt %>% 
  mutate(
    `%ageOct-Dec`   =  round((`Oct-Dec`/GxPErrorSites_wide$`Oct-Dec`)*100,0),
    `%ageJan-Mar`   =  round((`Jan-Mar`/GxPErrorSites_wide$`Jan-Mar`)*100,0)
  ) %>% 
    arrange(desc(`%ageJan-Mar`))

#### the table
tbl_Total_sitesPvt   <-  flextable(Total_sitesPvt)
 
#### format the table 
tbl_Total_sitesPvt  <-  tbl_Total_sitesPvt %>% 
  add_header_row(values = c(
    Error  =  "Error",
    "No. of sites in the Periods", "",
    "%age of sites in the reporting period", ""
  ))

## set the labels
tbl_Total_sitesPvt  <-  tbl_Total_sitesPvt %>% 
  set_header_labels(
    Error  =  "Error",
    `Jan-Mar`    =   "Jan-Mar",
    `Oct-Dec`    =    "Oct-Dec",
    `%ageOct-Dec`    =   "Jan-Mar",
    `%ageJan-Mar`    =    "Oct-Dec"
      )

# Merging columns 
tbl_Total_sitesPvt  <-  tbl_Total_sitesPvt %>% 
    merge_at(i = 1, j = 2:3, part = "header") %>% 
    merge_at(i = 1, j = 4:5, part = "header")

# Merging vertically for the first five columns in the header
tbl_Total_sitesPvt  <-  tbl_Total_sitesPvt %>% 
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_Total_sitesPvt  <-  tbl_Total_sitesPvt %>% 
  vline(j = c(1, 3, 5), part = "all")


tbl_Total_sitesPvt  <-  tbl_Total_sitesPvt %>%  
  add_header_row(values="%age of sites with the indicated GxP error code", 
                 colwidths = ncol(Total_sitesPvt))

tbl_Total_sitesPvt

#### Report - RRH sites with the indicated error code ####
RRHTotal_sites <- PctHFs %>% 
  group_by(RRH,Error,Qtr) %>% 
  summarise(No.sites  =  n(),
            .groups = "drop")

#### No. sites by Quarter
RRHTotal_sitesPvt   <-   RRHTotal_sites %>% 
  pivot_wider(
    names_from = c(Error,Qtr),
    values_from = No.sites 
  )

#### Report - No. of sites with the most common errors by RRH 
RRHError_Rpt <-  RRHTotal_sitesPvt %>% 
  select(RRH,matches("5007"),matches("5006"), matches("2124"), matches("5017")
         ,matches("2014"),matches("2008"),matches("2126"),matches("2037"),matches("4011")
       )

#### The table
tbl_RRHError_Rpt   <-  flextable(RRHError_Rpt)  

# format the table
tbl_RRHError_Rpt   <-  tbl_RRHError_Rpt %>% 
  add_header_row(
    values = c(
      "RRH",
      "No. of sites reporting 5007 (Probe Check failures) error ", "",                 # spanning 2 columns
      "No. of sites reporting 2124 (Communication loss)", "",  # spanning 2 columns
      "No. of sites reporting 5017 (Other)", "",    # spanning 2 columns
      "No. of sites reporting 2014 (Temperature)",                 # spanning 2 columns
      "No. of sites reporting 2037", "",    # spanning 2 columns
      "No. of sites reporting 4011 (User)",""
      ))

# setting the header
tbl_RRHError_Rpt   <-  tbl_RRHError_Rpt %>%
  set_header_labels(
    RRH   =  "RRH",
    `5007_Jan-Mar`    =   "Jan-Mar",
    `5007_Oct-Dec`    =   "Oct-Dec", 
    `2124_Jan-Mar`     =  "Jan-Mar",
    `2124_Oct-Dec`     =   "Oct-Dec",
    `5017_Jan-Mar`     =   "Jan-Mar",
    `5017_Oct-Dec`     =    "Oct-Dec",
    `2014_Jan-Mar`     =    "Oct-Dec",
    `Jan-Mar_2014`     =    "Jan-Mar",
    `2037_Jan-Mar`     =    "Oct-Dec",
    `2037_Oct-Dec`     =    "Jan-Mar",
    `4011_Jan-Mar`     =    "Jan-Mar",
    `4011_Oct-Dec`     =    "Jan-Mar"
  )

# Merging columns
tbl_RRHError_Rpt   <-  tbl_RRHError_Rpt %>%
  merge_at(i = 1, j = 2:3, part = "header") %>%
  merge_at(i = 1, j = 4:5, part = "header") %>%
  merge_at(i = 1, j = 6:7, part = "header") %>%
    merge_at(i = 1, j = 9:10, part = "header") %>% 
  merge_at(i = 1, j = 11:12, part = "header") 
  

# Merging vertical HFacility and Region
tbl_RRHError_Rpt   <-  tbl_RRHError_Rpt %>%
  merge_v(j = 1, part = "header") 

# Adding vertical lines to improve readability
tbl_RRHError_Rpt   <-  tbl_RRHError_Rpt %>%
  vline(j = c(1,3,5,7,8,10,12), part = "all")

#Font and sice
tbl_RRHError_Rpt   <-  tbl_RRHError_Rpt %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tbl_RRHError_Rpt   <-  tbl_RRHError_Rpt %>%
  add_header_row(values = "No. of sites with the respective GxP Error Codes by RRH",
                 colwidths = ncol(RRHError_Rpt))

tbl_RRHError_Rpt

#### Report - GxP Error rate by health facility #####
HFgxPError    <-   GxPError %>% 
  group_by(Site,Qtr,Error) %>%
  summarise(
    N_Error_occurences    =   sum(`No.of occurance`), .groups =  "drop")

# re arrange the dataset
HFgxPErrRate    <-   HFgxPError %>% 
  group_by(Site,Qtr) %>% 
  summarise(
    N_Err_occurences  =  sum(N_Error_occurences, na.rm = TRUE),
    .groups = "drop"
  )
#### arrange the data frame for Qtr columns
HFgxPErrPvt   <-  HFgxPErrRate %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = N_Err_occurences
  )

#### Add the vol of tests performed by site
hf_tests <- TBGxP %>% 
  full_join(EIDGxP %>% 
              distinct(HFacility, Qtr, .keep_all = TRUE) %>% 
              select(Qtr, HFacility, GxPEID), 
            by = c("HFacility", "Qtr"))%>% 
  
  full_join(VLGxP %>% 
              distinct(HFacility, Qtr, .keep_all = TRUE) %>% 
              select(Qtr, HFacility, GxPVL), 
            by = c("HFacility", "Qtr")) %>% 
  
  full_join(HPVGxP %>% 
              distinct(HFacility, Qtr, .keep_all = TRUE) %>% 
              select(Qtr, HFacility, GxPHPV), 
            by = c("HFacility", "Qtr")) %>% 
  
  mutate(GxPEID = replace_na(GxPEID, 0),
         GxPVL = replace_na(GxPVL, 0),
         GxPHPV = replace_na(GxPHPV, 0),
         GxPTB = replace_na(GxPTB, 0)) %>% 
  
  mutate(Total_Tests   =  (GxPTB + GxPEID + GxPVL + GxPHPV)) %>% 
  
  select(RRH,HFacility,Qtr,Total_Tests)

### re-organise with pivot table
hf_testsC   <-  hf_tests %>% 
  group_by(HFacility,Qtr) %>% 
  summarise(
    No_tests  =  sum(Total_Tests, na.rm = TRUE),
    .groups = "drop"
  )
## the pivot
hf_testsPvt   <-  hf_testsC %>% 
  pivot_wider(
    names_from = Qtr,
    values_from = No_tests
  )

#### Add the volume of tests performed by site
HFgxPErrRpt  <-  HFgxPErrPvt %>% 
  full_join(hf_testsPvt, by = c("Site" = "HFacility")
  ) %>% 
  mutate(across(`Jan-Mar.x`:`Jan-Mar.y`, ~ replace_na(.x, 0))) %>% 
    mutate(
    `%Oct-Dec`    =   round((`Oct-Dec.x`/`Oct-Dec.y`)*100,0),
    `%Jan-Mar`    =   round((`Jan-Mar.x`/`Jan-Mar.y`)*100,0)
  ) 

# Add RRH column
HFgxPErrRptfinal  <-  HFgxPErrRpt %>% 
  left_join(dhis2names, by = c("Site"   =  "HFacility")) %>% 
        filter(`%Jan-Mar` > 5) %>% 
          select(RRH,District,Site,`Oct-Dec.y`,`Jan-Mar.y`,`Oct-Dec.x`
                 ,`Jan-Mar.x`,`%Oct-Dec`,`%Jan-Mar`) %>% 
                arrange(RRH)

# The table
tbl_HFgxPErrRpt  <-  flextable(HFgxPErrRptfinal)

# format the table
tbl_HFgxPErrRpt   <-  tbl_HFgxPErrRpt %>% 
  add_header_row(
    values = c(
      RRH    =    "RRH",
      District   =  "District",
      Site     =   "GeneXpert Site",
      "No. of tests performed","",
      "No. of error occurences against tests performed", "",
      "%age of error occurences against tests performed", ""
      ))
# setting the header
tbl_HFgxPErrRpt   <-  tbl_HFgxPErrRpt %>%  
  set_header_labels(
    RRH    =    "RRH",
    District   =  "District",
    Site     =   "GeneXpert Site",
    `Oct-Dec.y`   =   "Oct-Dec",
    `Jan-Mar.y`    =  "Jan-Mar",
    `Oct-Dec.x`    =   "Oct-Dec",
    `Jan-Mar.x`    =   "Jan-Mar",
    `%Oct-Dec`    =   "Jan-Mar",
    `%Jan-Mar`    =   "Oct-Dec")
        
# Merging columns
tbl_HFgxPErrRpt   <-  tbl_HFgxPErrRpt %>%
  merge_at(i = 1, j = 4:5, part = "header") %>% 
  merge_at(i = 1, j = 6:7, part = "header") %>%
  merge_at(i = 1, j = 8:9, part = "header")

# Merging vertical HFacility and Region
tbl_HFgxPErrRpt   <-  tbl_HFgxPErrRpt %>%
  merge_v(j = 1:3, part = "header")
  

# Adding vertical lines to improve readability
tbl_HFgxPErrRpt   <-  tbl_HFgxPErrRpt %>%
  vline(j = c(1,2,3,5,7,9), part = "all")

#Font and sice
tbl_HFgxPErrRpt   <-  tbl_HFgxPErrRpt %>%
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tbl_HFgxPErrRpt   <-  tbl_HFgxPErrRpt %>%
  add_header_row(values = "Line list of GxP sites with >5% of error occurences against the total tests done in Jan-March period",
                 colwidths = ncol(HFgxPErrRptfinal))

tbl_HFgxPErrRpt

#### Report - Financial implications on Number of error occurrences ####
t_occurences  <-  HFgxPErrRptfinal %>% 
  summarise(
    Period    =  "No. of GxP Occurences",
    `GxP cartridge Unit Cost` =  "8",
    `Oct-Dec`  =  sum(`Oct-Dec.x`),
    `Jan-Mar`  =  sum(`Jan-Mar.x`)
  ) 

#### change unit cost to numeric
t_occurences$`GxP cartridge Unit Cost`  <- as.numeric(t_occurences$`GxP cartridge Unit Cost`)

#### calculate the cost amount lost
t_occurences   <-  t_occurences %>% 
  mutate(
    Oct_Dec  =  `Oct-Dec` *  `GxP cartridge Unit Cost`,
    Jan_Mar  =  `Jan-Mar` *  `GxP cartridge Unit Cost`
  )
#### the table
tbl_t_occurences  <-  flextable(t_occurences)

tbl_t_occurences  <-  tbl_t_occurences %>% 
  add_header_row(values = "Financial implications of GxP Error Occurences",
                 colwidths = ncol(t_occurences))

tbl_t_occurences

#### Report - Financial implications of occurrences by RRH ####
RRHt_occurences  <-  HFgxPErrRptfinal %>%
  filter(!is.na(RRH)) %>% 
  group_by(RRH) %>% 
  summarise(
    `GxP cartridge Unit Cost` =  "8",
    `Oct-Dec`  =  sum(`Oct-Dec.x`),
    `Jan-Mar`  =  sum(`Jan-Mar.x`)
  ) 
#### change unit cost to numeric
RRHt_occurences$`GxP cartridge Unit Cost`  <- as.numeric(RRHt_occurences$`GxP cartridge Unit Cost`)

#### calculate the final implications
RRHt_occurences   <- RRHt_occurences %>% 
  rowwise() %>% 
  mutate(
    Oct_Dec  =  `Oct-Dec` * `GxP cartridge Unit Cost`,
    Jan_Mar   =  `Jan-Mar` * `GxP cartridge Unit Cost`
  )

#### the table
tbl_RRHt_occurences  <-  flextable(RRHt_occurences)

tbl_RRHt_occurences  <-  tbl_RRHt_occurences %>% 
  add_header_row(values = c(
    RRH  =  "RRH",
    `GxP cartridge Unit Cost`  =  "GxP cartridge Unit Cost",
    "No of error occurences by Qtr","",
    "Financial implications by Qtr",""
  ))

tbl_RRHt_occurences  <-  tbl_RRHt_occurences %>% 
  set_header_labels(
    RRH  =  "RRH",
    `GxP cartridge Unit Cost`  =  "GxP cartridge Unit Cost",
    `Oct-Dec`   =  "Oct-Dec",
    `Jan-Mar`   =  "Jan-Mar",
    Oct_Dec   =  "Oct-Dec",
    Jan_Mar   =  "Jan-Mar"
   )

# Merging columns
tbl_RRHt_occurences  <-  tbl_RRHt_occurences %>% 
  merge_at(i = 1, j = 3:4, part = "header") %>% 
  merge_at(i = 1, j = 5:6, part = "header") 

# Merging vertical HFacility and Region
tbl_RRHt_occurences  <-  tbl_RRHt_occurences %>% 
  merge_v(j = 1:2, part = "header")


# Adding vertical lines to improve readability
tbl_RRHt_occurences  <-  tbl_RRHt_occurences %>% 
  vline(j = c(1,2,4,6), part = "all")

#Font and sice
tbl_RRHt_occurences  <-  tbl_RRHt_occurences %>% 
  fontsize(i = 1:2, size = 9, part = "header") %>% 
  bold(i = 1:2, bold = TRUE, part = "header") %>% 
  fontsize(i = NULL, size = 8, part = "body")

## add tile
tbl_RRHt_occurences  <-  tbl_RRHt_occurences %>%
  add_header_row(values = "Financial implications of GxP Error Occurences by RRH",
                 colwidths = ncol(RRHt_occurences))

tbl_RRHt_occurences




######################################################################
######################################################################
#### Regression Analysis ####

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)

# Step 1: Import the data
# Replace with your actual Excel file path
df <- read_excel("your_file.xlsx", sheet = 1)

# Step 2: Clean and prepare the data
# Step 1: Ensure correct variable types
regress_df <- EID_Data %>%
  mutate(
    Gender = as.factor(Gender),
    `Breastfeeding?` = as.factor(`Breastfeeding?`),
    `PMTCT Antenatal` = as.factor(`PMTCT Antenatal`),
    `PMTCT Delivery` = as.factor(`PMTCT Delivery`),
    `PMTCT Post Natal` = as.factor(`PMTCT Post Natal`),
    age_in_months = as.numeric(age_in_months)
  )

# Step 2: View summary of the data (optional)
summary(regress_df)

# Step 3: Fit a linear regression model
model <- lm(
  age_in_months ~ Gender + `Breastfeeding?` + `PMTCT Antenatal` + `PMTCT Delivery` + `PMTCT Post Natal`,
  data = regress_df
)

# Step 4: Summarize the model
summary(model)
# Step 4: Run stepwise model selection (optional)
step_model <- stepAIC(model, direction = "both")
summary(step_model)

# Step 5: Plot diagnostics (optional)
par(mfrow = c(2, 2))
plot(model)

# Step 6: Export results (if needed)
# write.csv(summary(model)$coefficients, "regression_output.csv")




######################################################################
######################################################################
#### create reports ####
#### Main Report ####
POCRpt <- read_docx()

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_EID_VL_Done)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(EIDPOC)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_POCVL_tests)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_EID_PropR)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(pbfw_table)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(Table_1st_PCR)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(Table_Pcr12m_Report)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(Table_1PCR_12)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_QtrPoCTAT)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_pvt_RQtr)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RPctTimely)


###
POCRpt <- POCRpt %>% 
  body_add_flextable(sites_connectivty)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_rrhsites_rptPvt)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_rrhsites_reporting)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(RRH_MT)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_multi_testsPvt)


###
POCRpt <- POCRpt %>% 
  body_add_flextable(Multi_summary)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_RGxP_Report)


###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_util_rateall)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(RRH_util)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RRHsite_utl)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_EGxP_ER_Report)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RRHEIDGxP)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_VGxP_ER_Report)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RRH_VL_GxP_ErrorPvtReport)


###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_HPVGxP_ER_Report)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RRH_HPV_Rt)


###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_TBGxP_ER_Report)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RRH_TBReport)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RRH_Qtr_ErrReport)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_RRH_EIDQtr_ErrReport)

###
POCRpt <- POCRpt %>% 
  body_add_flextable(tble_all_error)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_GxPErrorCpvt)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_Total_sitesPvt)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_RRHError_Rpt)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_t_occurences)
###
POCRpt <- POCRpt %>% 
  body_add_flextable(tbl_RRHt_occurences)






#### Save the document
print(POCRpt, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/r - reports/POCQ2.docx")

#### adds Report ####
POCRptadd <- read_docx()

###
POCRptadd <- POCRptadd %>% 
  body_add_flextable(tble_rrhsites_rptPvt)

#### Save the document
print(POCRptadd, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/r - reports/POCaddQ2.docx")


#### Annex  1  ####
POCAnnex1  <- read_docx() 

#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(table_VLPOC)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_EID_Dataset)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(Table_hf1st_PCR)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_pbfw_rrh)

#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_SPctTimely)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tbl_pvt_HQtrPoCTAT)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tbl_hf_mulT_report)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(Fac_util)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_EIDGxP_ErrReport)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_VLGxP_ErrorReport)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_HPVGxP_Report)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_TBGxP_Error)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_VLPima_Error)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tble_EIDPima_Error)
#### 
POCAnnex1 <- POCAnnex1 %>% 
  body_add_flextable(tbl_HFgxPErrRpt)


#### Save the document
print(POCAnnex1, target = "D:/CPHL-MOH/Running Grants/coag/CoAg Project 2/COP24 (fy24-25)/Report/Q2/Data/POC/r - reports/POCAnnexQ2.docx")


#### IP POC Additional reporting ####
# Create a Word document
POC_extra <- read_docx()

# Number of EID tests done reported
POC_extra <- POC_extra %>%
  body_add_flextable(Facility_EIDPOC)


# Number of VL tests done reported
POC_extra <- POC_extra %>%
  body_add_flextable(Table_VL_Dataset)

# Weekly reporting summary
POC_extra <- POC_extra %>%
  body_add_flextable(wkly_report)

# HF with < 25% weekly uploads
POC_extra <- POC_extra %>%
  body_add_flextable(T_HF_wkly_all_25)


# HF with < 25% - 50% weekly uploads
POC_extra <- POC_extra %>%
  body_add_flextable(T_HF_wkly_all_50)


# Save the document
print(POC_extra, target = "C:/Users/HP/Desktop/POC_IP_Up.docx")


# Create a Word document
POC_extra_1 <- read_docx()
# weekly uploaods
POC_extra_1 <- POC_extra_1 %>%
  body_add_flextable(wkly_report)

# Save the document
print(POC_extra_1, target = "C:/Users/HP/Desktop/POC_IP_Up_1.docx")










