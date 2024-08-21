library(openxlsx)#allows you to load excel files and all tabs
library(dplyr)
library(tidyr)
library(stringr)#allow you to check string lengths in cells
library(httr)



###FUNCTIONS###

`%notin%` <- Negate(`%in%`)

####ACTIONS TO BDS FORMAT

BDSdata <- read.xlsx(here::here("01_data/03_priv/BDS - Wide.xlsx"), startRow =2)#Reads in BDS

BDSlong <- BDSdata %>%
  pivot_longer(cols = Barking.and.Dagenham:England_2, names_to = 'LA and Regions',
               values_to = 'Values') %>%
  filter (Years %notin% c("-","","Trend","Change","Rank","Target","Distance","Quartile","Deviation National","% YoY change","%3yr change","2010 Target","Target 2011","NA - Distance",is.na(0)))#removes these lines with matching entries in year column

#Creating Vectors:
columns_to_delete <- c("Quartile.1.(A)",	"Quartile.2.(B)",	"Quartile.3.(C.)", "Quartile.4.(D)","X181","line","Data.item")#vector to store columns to delete
#Underscore_delete <-c("_1", "_2", "_3", "_4", "_5", "_6", "_7" ,"_8" ,"_9" ,"_10")


BDSlong<-BDSlong %>% select(!columns_to_delete, -starts_with("X")) %>% #delete lines in vector from column
  mutate('LA and Regions'= str_replace_all(`LA and Regions`,"\\."," ")) %>% #replaces dots in LA and Regions with spaces
  mutate('LA and Regions'= str_replace_all(`LA and Regions`,"  ","\\. ")) %>%   #sorts out St.Helens issue!
  mutate(Short.Desc = (str_remove(Short.Desc,"(_[0-9]{1,2})")))#removes underscore and number from Short code

LACharacteristics <- read.xlsx(here::here("01_data/03_priv/LA code list.xlsx"))#Reads in LA code list

BDSlong<-BDSlong %>% left_join(LACharacteristics,by=c('LA and Regions'='LA.Name'))# Joins data frames on LA Names

BDSlong$CombinedCode<-paste(BDSlong$LA.Number,BDSlong$Short.Desc,BDSlong$Years, sep="_") #concatenates short code, year and LA num


#BDSlong_NA<-BDSlong %>% filter(is.na(Region))#creates an unwritten data frame to check BDSlong for NA mismatches from join


#Create link to live file and pull data into BDS Long - do this next....

#Ofsted<-read.xlsx(createLink(link="https://educationgovuk.sharepoint.com/sites/sdg/c/WorkplaceDocuments/BETA_Interventions/Data/", "Latest%20Ofsted%20Overall%20Judgement.xlsx", skip=!overwrite, overwrite=FALSE,
 #           methods=getOption("createLink/args/methods", c("windows-ntfs-symlink",
  #        "windows-shortcut")), ...))


#Ofsted <- file.path("//educationgovuk.sharepoint.com/sites/sdg/c/WorkplaceDocuments/BETA_Interventions/Data/Latest%20Ofsted%20Overall%20Judgement.xlsx")  # Create file path
#OfstedJudgements<-read.xlsx(url(Ofsted,sheets("Latest Ofsted"),startRow=16))
#get(Ofsted, authenticate("name", "password","ntlm"),
 #   write_disk("tempfile.xlsx", overwrite = TRUE))
#df <- read.xlsx("tempfile.xlsx", sheets("Latest Ofsted"), startRow = 16)
# BDSlong$OfstedJudgement

#copyFromSharePoint <- function(fileName)
#{       cmd <- paste("curl --max-time 7200 --connect-timeout 7200 --ntlm --user","",
#                     "--download-file educationgovuk.sharepoint.com/sites/sdg/c/WorkplaceDocuments/BETA_Interventions/Data/",fileName,
#                     "OneDrive - Department for Education/MyRCode/LAIT modernisation project",fileName, sep = " ")       system(cmd)
#}

#copyFromSharePoint("Latest%20Ofsted%20Overall%20Judgement.xlsx")










write.xlsx(BDSlong,"BDS_Long.xlsx")#Write first pivot to help check outputs


#Regionfilter

BDSlongregion <- BDSlong %>%
  filter(`LA and Regions` %in% c('North.East', 'North.West','Yorkshire.and.the.Humber','East.Midlands','West.Midlands','East.of.England','London','Inner.London','Outer.London','South.East','South.West'))

write.xlsx(BDSlongregion, "BDS_Regions_Only_long.xlsx")

#EnglandFilter

BDSNational <- BDSlong %>%
  filter(`LA and Regions` =='England')

write.xlsx(BDSNational, "National_Only_long.xlsx")

#LAFilter

BDSLAOnly <- BDSlong %>%
  filter(`LA and Regions` %notin% c('England','North.East', 'North.West','Yorkshire.and.the.Humber','East.Midlands','West.Midlands','East.of.England','London','Inner.London','Outer.London','South.East','South.West','England.2'))

write.xlsx(BDSLAOnly, "LA_Only_long.xlsx")

#England2Filter

BDSNational2 <- BDSlong %>%
  filter(`LA and Regions` =='England.2')

write.xlsx(BDSNational2, "National2_Only_long.xlsx")
