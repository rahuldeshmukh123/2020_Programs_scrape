#############################
##### Fleming
## Type of website:


Institution <- CIConnexName("Fleming")
webPage <- read_webPage('https://flemingcollege.ca/programs/a-z')
contin_ed <- read_webPage('https://flemingcollege.ca/continuing-education/certificates')

program_urls <- tibble(Program = webPage %>% 
                         html_nodes('div.program-list-content') %>% 
                         html_nodes('div.row') %>% 
                         html_nodes('a') %>% 
                         html_text(),
                       url = webPage %>% 
                         html_nodes('div.program-list-content') %>% 
                         html_nodes('div.row') %>% 
                         html_nodes('a') %>% 
                         html_attr('href') %>% 
                         paste0('https://flemingcollege.ca', .))

contin_ed <- tibble(Program = contin_ed %>% 
                      html_nodes('div.program-list-content') %>% 
                      html_nodes('div.row') %>% 
                      html_nodes('a') %>% 
                      html_text(),
                    url = contin_ed %>% 
                      html_nodes('div.program-list-content') %>% 
                      html_nodes('div.row') %>% 
                      html_nodes('a') %>% 
                      html_attr('href') %>% 
                      paste0('https://flemingcollege.ca', .))

program_urls <- program_urls %>% 
  bind_rows(contin_ed)


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = webPage %>% 
      html_nodes(clean_css_space("div.program-info tab-pane fade in active"))
    
    Credential = details %>% html_nodes("div.program-credential") %>% html_nodes("strong") %>% html_text() %>% unlist() %>% paste(., collapse = " ") %>% clean_tags()
    Duration = details %>% html_nodes("span.program-credential-length") %>% html_text() %>% unlist() %>% paste(., collapse = " ") %>% clean_tags()
    Campus  = details %>% html_nodes("div.program-location") %>% html_text() %>% unlist() %>% paste(., collapse = " ") %>% clean_tags()
    #Tuition = details %>% html_nodes("div.program-tuition") %>% html_text() %>% unlist() %>% paste(., collapse = " ") %>% clean_tags()
    
    description = webPage %>% 
      html_nodes('div.box.shadow-2dp') %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      unlist() %>% 
      paste(., collapse = " ") %>% 
      clean_tags()
    
    tuition = webPage %>% 
      html_nodes("div.ui-collapse-panel-body") %>% 
      html_nodes("table.table.table-striped.table-bordered.small") %>% 
      html_table(fill = T) 
    
    if(exists("Program_tuition")){
      remove(Program_tuition)
    }
    
    tryCatch({
      
      names(tuition) = webPage %>% 
        html_nodes("div.ui-collapse-panel-body") %>% html_nodes('h5') %>% html_text() %>% 
        rep(., each = length(tuition)/2) %>% 
        paste(., rep(1:(length(tuition)/2), 2),sep = "_")
      
      tuition = tuition %>% 
        bind_rows(., .id = "names")
      
      Program_tuition = tuition %>% 
        pivot_longer(cols = -c(Amount, names)) %>% 
        filter(!is.na(value)) %>% 
        mutate(Amount = ifelse(is.na(Amount), value, Amount),
               value = ifelse(value == Amount, NA, value),
               names = gsub("_[[:digit:]]", "", names)) %>% 
        rename(studentType = names) %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = read_webPage(paste0(program_urls$url[url], "/curriculum"))
    WIL = NA
    
    tryCatch({
      courses_df = tibble(Code = courses %>% 
                         html_nodes('div.section') %>% 
                         html_nodes('div.course-container') %>% 
                         html_nodes('h5') %>% 
                         html_attr('id'),
                       Name = courses %>% 
                         html_nodes('div.section') %>% 
                         html_nodes('div.course-container') %>% 
                         html_nodes('h5') %>% 
                         html_text() %>% 
                         clean_tags(),
                       Description = courses %>% 
                         html_nodes('div.section') %>% 
                         html_nodes('div.course-container') %>% 
                         html_nodes('div.curriculum-data') %>% 
                         html_text() %>% 
                         clean_tags())
      WIL = course_eval(courses_df, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = description, WIL = WIL)
    
    
    if(!exists("programs")){
      programs = program_df
    } else {
      programs = programs %>% bind_rows(program_df)
    }
    tryCatch({
      if(!exists("Fees")){
        Fees = Program_tuition
      } else {
        Fees = Fees %>% bind_rows(Program_tuition)
      }
    }, error = function(e){cat("Tuition ERROR :", conditionMessage(e), "\n")})
  }, error = function(e){cat("Overall error ERROR :", conditionMessage(e), "\n")})
}   

programs <- programs %>% 
  mutate(Credential = ifelse(grepl("continuing-education", url), "Continuing education", Credential))

save_out_standard_file(programs, Institution$institution_name, "Ontario")
