#############################
##### TARA
## Type of website: No structure - manual entry


Institution <- CIConnexName("audio recording academy")
webpage = read_webPage("http://tara.ca/programs/")

Description = webPage %>% 
  html_nodes("div.programs-text") %>% 
  html_nodes('p') %>% 
  html_text() %>% 
  clean_tags() %>% 
  paste(., collapse = " ")


programs = create_program_df(Institution, 
                             url = "http://tara.ca/programs/", 
                             Program = "Applied Audio Recording", 
                             Credential = "Diploma", 
                             Campus = "TARA", 
                             Duration = "30 weeks", 
                             Description = Description, 
                             WIL = NA)


courses = tibble(Code = NA,
                 Name = webPage %>% html_nodes('div.program-text') %>% 
                   html_nodes("div.container") %>% 
                   html_nodes('h4') %>% 
                   html_text(),
                 Description = webPage %>% html_nodes('div.program-text') %>% 
                   html_nodes("div.container") %>% 
                   html_nodes('p') %>% 
                   html_text() %>% 
                   clean_tags() %>% 
                   paste(., collapse = " "))


course_eval(courses, Institution$institution_name, Program = "Applied Audio Recording", "http://tara.ca/programs/", noDescription = F)

save_out_standard_file(programs, Institution$institution_name, "Ontario")
