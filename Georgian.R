#############################
##### Georgian
## Type of website:


Institution <- CIConnexName("Georgian")
webPage <- read_webPage('https://www.georgiancollege.ca/academics/full-time-programs/')

program_urls <- webPage %>% 
  html_nodes("section.body") %>% 
  html_nodes("table") %>% 
  html_table(fill = T) %>% 
  bind_rows() %>% 
  set_names(c("Code", "Program", "Campus", "Topic", "WIL")) %>% 
  mutate(Campus = clean_tags(Campus)) %>% 
  filter(Code != "Code") %>% 
  mutate(url = webPage %>% 
           html_nodes("section.body") %>% 
           html_nodes("table") %>% 
           html_nodes("tr") %>% 
           #html_nodes("td.sorting_1") %>% 
           html_nodes("a") %>% 
           html_attr("href") %>% 
           unique())



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = webPage %>% 
      html_nodes("section.outline-section")
    
    ids = details %>% 
      html_attr("id")
    
    description = details[grepl("program-description", ids)] %>% 
      html_text() %>% 
      paste(., collapse = " ") %>% 
      clean_tags()
    
    courses = details[grepl("courses", ids)] %>% 
      html_nodes("ul.list-unstyled") %>% 
      html_nodes("li") %>% 
      html_text()
    
    
    tryCatch({
      courses = tibble(courses = courses) %>% 
        separate(courses, c("Code", "Name"), sep = " - ") %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    details = details[grepl("general-info", ids)] %>% 
      html_nodes('div.row') %>% 
      html_text() %>% 
      gsub("^\n[[:space:]]*|\n[[:space:]]*$", "", .)
    
    details = tibble(detail = details) %>% 
      separate(detail, c("Category", "Detail"), sep = "\n[[:space:]]*")
    
    if(is.na(WIL)){
      WIL = details %>% filter(grepl("Work integrated", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
      if(WIL == ""){
        WIL = program_urls$WIL[url]
      }
    }
    
    
    Credential = details %>% filter(grepl("credential", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Duration = details %>% filter(grepl("duration", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Campus = details %>% filter(grepl("campus", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    
    if(Campus == ""){
      Campus = program_urls$Campus[url]
    }
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = description, WIL = WIL)
    
    
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

programs <- programs %>% 
  mutate(WIL = ifelse(WIL == "No", NA, WIL))

save_out_standard_file(programs, Institution$institution_name, "Ontario")
