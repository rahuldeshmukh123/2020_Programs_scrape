#############################
##### Kenjgewin
## Type of website:


Institution <- CIConnexName("Kenjgewin")
webPage <- read_webPage('https://www.kenjgewinteg.ca/post-secondary-programs---current.html')

programs = webPage %>% 
  html_nodes("div.wsite-multicol-table-wrap") %>% 
  html_nodes("table") %>% 
  html_table(fill = T) %>% 
  bind_rows()

## start here


save_out_standard_file(programs, Institution$institution_name, "Ontario")
