#############################
##### ontario college of art and design
## Type of website: Hidden behind scripts


Institution <- CIConnexName("ontario college of art and design")
webPage <- read_webPage("https://www.ocadu.ca/admissions/programs")

remDr$open()


remDr$navigate("https://www.ocadu.ca/services/records-and-registration/program-guides?search=cadn")

program_details = remDr$findElements(using = 'xpath', '/html/body/div/div/div/div/div[3]/div/div/div/div')
program_urls = remDr$findElements(using = 'xpath', '/html/body/div/div/div/div/div[3]/div/div/div/div/div/a')
program_details = tibble(detail = lapply(program_details, function(x) {x$getElementText()}) %>% unlist()) %>% 
  filter(grepl("DOWNLOAD PDF$", detail)) %>% 
  mutate(url = lapply(program_urls, function(x) {x$getElementAttribute('href')}) %>% unlist())

program_details = program_details %>% 
  separate(detail, c("Credential", "Year", "Program", "download"), sep = "\n") %>% 
  filter(Credential != "Minor") %>% 
  mutate(Year = as.numeric(substr(Year,1,4))) %>% 
  filter(Year == max(Year, na.rm = T))

programs = program_details %>% 
  select(Program, Credential, url) %>% 
  mutate(institution_name = Institution$institution_name,
         prov_terr = Institution$prov_terr,
         record_id = Institution$record_id,
         member = Institution$member,
         Campus = NA,
         Duration = NA,
         Description = NA,
         WIL = NA)
    
    
  
remDr$close()


save_out_standard_file(programs, Institution$institution_name, "Ontario")
