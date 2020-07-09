#############################
##### ridgetown college
## Type of website: 


Institution <- CIConnexName("ridgetown college")
webPage <- read_webPage("https://www.ridgetownc.com/future/programs.cfm")

program_urls <- tibble(Program = get_detail_id(webPage%>% html_nodes('div'), "subnav") %>% html_nodes('ul') %>% html_nodes('li') %>% html_nodes('a') %>% html_text(),
                       url = get_detail_id(webPage%>% html_nodes('div'), "subnav") %>% html_nodes('ul') %>% html_nodes('li') %>% html_nodes('a') %>% html_attr('href')) %>% 
  filter(grepl("programs", url)) %>% 
  mutate(url = paste0("https://www.ridgetownc.com/future/", url))


credentials <- tibble(Program = get_detail_id(webPage%>% html_nodes('div'), "subnav") %>% html_nodes('ul') %>% html_nodes('li') %>% html_text()) %>% 
  mutate(Credential = zoo::na.locf(ifelse(grepl("^- ", Program), Program, NA), na.rm = F)) %>% 
  mutate(Program = str_trim(Program))

program_urls <- program_urls %>% 
  left_join(credentials, by = "Program") %>% 
  mutate(Credential = gsub("- | -", "", Credential))

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    Description = webPage %>% 
      html_nodes("div.container") %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    Duration = webPage %>% 
      html_nodes("div.container") %>% 
      html_nodes('p.intake') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    coursePage = read_webPage(gsub(".cfm", "_courses.cfm", program_urls$url[url]))
    
    
    
    WIL = NA
    tryCatch({
      courses = tibble(Code = NA,
                       Name = get_detail_id(coursePage %>% html_nodes("div"), "content") %>% html_nodes('li') %>% html_nodes('a') %>% html_text() %>% clean_tags())
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = "University of Guelph - Ridgetown Campus", Duration = Duration, Description = Description, WIL = WIL)
    
    
    
    
    
    if(!exists("programs")){
      programs = program_df
    } else {
      programs = programs %>% bind_rows(program_df)
    }
    #tryCatch({
    #  if(!exists("Fees")){
    #    Fees = Program_tuition
    #  } else {
    #    Fees = Fees %>% bind_rows(Program_tuition)
    #  }
    #}, error = function(e){cat("Tuition ERROR :", conditionMessage(e), "\n")})
  }, error = function(e){cat("Overall error ERROR :", conditionMessage(e), "\n")})
}   




save_out_standard_file(programs, Institution$institution_name, "Ontario")