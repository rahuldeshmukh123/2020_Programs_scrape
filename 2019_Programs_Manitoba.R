
library(tidyverse)
library(rvest)
library(RSelenium)
library(XML)
#setwd("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs")

source("2019_Programs_Functions.R")


#### Reading in helper data sets
colleges <- read_rds("All_colleges.rds")
colleges %>% 
  filter(grepl("Manitoba", prov_terr))  %>% 
  mutate(member = substr(record_id, 8,12),
         member = ifelse(member != "", T, F)) %>% 
  as.data.frame()


#                                        institution_name   member
#1                           Assiniboine Community College    TRUE
#2   louis riel school division arts and technology centre   FALSE
#3                     manitoba emergency services college   FALSE
#4             Manitoba Institute of Trades and Technology    TRUE
#5                     r.b. russell vocational high school   FALSE
#6 Red River College of Applied Arts, Science & Technology    TRUE
#7               st. james-assiniboia continuing education   FALSE
#8                            Université de Saint-Boniface    TRUE
#9                         University College of the North    TRUE


###########################################
#########         Manitoba        ##########
###########################################


fprof <- makeFirefoxProfile(list(
  browser.download.folderList = 2L,  
  browser.download.manager.showWhenStarting = FALSE,
  browser.helperApps.neverAsk.openFile = "text/csv",
  browser.helperApps.neverAsk.saveToDisk = "text/csv",
  browser.helperApps.alwaysAsk.force = FALSE,
  browser.download.manager.showAlertOnComplete = FALSE,
  browser.download.manager.closeWhenDone = TRUE )
)

rD <- rsDriver(browser = "firefox", 
               verbose = FALSE)

remDr <- rD$client

program_scrapes = list.files("2019_Programs_Manitoba")
#source the above files
#print(Instittution$instituition_name)
program_scrapes %>% 
  map(~ source(paste0("2019_Programs_Manitoba/", .)))


program_datas = list.files("programs/Manitoba", pattern = "programs.rds")
Manitoba_programs = program_datas %>% 
  map(~ read_rds(paste0("programs/Manitoba/", .))) %>% 
  bind_rows()



