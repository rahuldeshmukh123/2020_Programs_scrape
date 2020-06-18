
library(tidyverse)
library(rvest)
library(RSelenium)
library(XML)
#setwd("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs")

source("2019_Programs_Functions.R")


#### Reading in helper data sets
colleges <- read_rds("All_colleges.rds")
colleges %>% 
  filter(grepl("Alberta", prov_terr))  %>% 
  mutate(member = substr(record_id, 8,12),
         member = ifelse(member != "", T, F)) %>% 
  as.data.frame()


#             institution_name                    member
#             alberta school of radiation therapy FALSE
#                  Alberta University of the Arts  TRUE
#                                    banff centre FALSE
#                              Bow Valley College  TRUE
#                               burman university FALSE
#                   Centre collégial de l’Alberta  TRUE
#                     concordia lutheran seminary FALSE
#                 Grande Prairie Regional College  TRUE
#                        grant macewan university FALSE
#                                  Keyano College  TRUE
#                                Lakeland College  TRUE
#                              Lethbridge College  TRUE
#                            Medicine Hat College  TRUE
#                          mount royal university FALSE
#                                NorQuest College  TRUE
# Northern Alberta Institute of Technology (NAIT)  TRUE
#                          Northern Lakes College  TRUE
#                                    Olds College  TRUE
#                                 Portage College  TRUE
#                                Red Deer College  TRUE
# Southern Alberta Institute of Technology (SAIT)  TRUE
#                       yellowhead tribal college FALSE



###########################################
#########         Alberta        ##########
###########################################


program_scrapes = list.files("2019_Programs_Alberta")
#source the above files
#print(Instittution$instituition_name)
program_scrapes %>% 
  map(~ source(paste0("2019_Programs_Alberta/", .)))


program_datas = list.files("programs/Alberta", pattern = "programs.rds")
Atlantic_programs = program_datas %>% 
  map(~ read_rds(paste0("programs/Alberta/", .))) %>% 
  bind_rows()


program_datas = list.files("programs/Territories", pattern = "programs.rds")
Terr_programs = program_datas %>% 
  map(~ read_rds(paste0("programs/Territories/", .))) %>% 
  bind_rows()

Atl_terr_programs = Atlantic_programs %>% 
  bind_rows(Terr_programs)
