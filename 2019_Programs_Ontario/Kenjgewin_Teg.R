#############################
##### Kenjgewin
## Type of website:


Institution <- CIConnexName("Kenjgewin")
webPage <- read_webPage('https://www.kenjgewinteg.ca/post-secondary-programs---current.html')

programs = tibble(Detail = webPage %>% 
                    html_nodes("div.wsite-multicol") %>%
                    html_nodes("td.wsite-multicol-col") %>% 
                    html_text() %>% 
                    str_split("\n\n\r\n\n\n[[:digit:]]\\.") %>% 
                    unlist()) %>% 
  separate(Detail, c("Program", "Credential"), sep =  "CREDENTIAL:") %>% 
  filter(!is.na(Credential)) %>% 
  separate(Credential, c("Credential", "Duration"), sep = "PROGRAM LENGTH:") %>% 
  separate(Duration, c("Duration", "Start_date"), sep = "PROGRAM START:|START DATE:") %>% 
  separate(Start_date, c("Start_date", "Tuition"), sep = "FEES/TUITION:") %>% 
  separate(Tuition, c("Tuition", "Description"), sep = "start\\)|change\\)|textbooks") %>% 
  mutate(Campus = NA,
         url = NA,
         WIL = NA,
         institution_name = Institution$institution_name,
         prov_terr = Institution$prov_terr,
         record_id = Institution$record_id,
         member = Institution$member)
  

save_out_standard_file(programs, Institution$institution_name, "Ontario")
