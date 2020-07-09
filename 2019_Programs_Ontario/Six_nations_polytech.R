#############################
##### Six Nations
## Type of website: 


Institution <- CIConnexName("Six Nations")
webPage <- read_webPage("https://www.snpolytechnic.com/programs-courses")


program_urls <- tibble(Program = webPage %>% 
                         html_nodes("div.view-content") %>% 
                         html_nodes('ul') %>% 
                         html_nodes("li") %>% 
                         html_nodes("a") %>% 
                         html_text(),
                       url = webPage %>% 
                         html_nodes("div.view-content") %>% 
                         html_nodes('ul') %>% 
                         html_nodes("li") %>% 
                         html_nodes("a") %>% 
                         html_attr('href') %>% 
                         paste0('https://www.snpolytechnic.com', .))

tuition_df = read_webPage("https://www.snpolytechnic.com/admissions/fees") 
tuition_fees = tuition_df %>% 
  html_nodes(clean_css_space("div.white-blocks equal-blocks two")) %>% 
  html_nodes("div.block")

tryCatch({
  tuition_df = tibble(Type = tuition_df %>% 
                        html_nodes(clean_css_space("div.white-blocks equal-blocks two")) %>% 
                        html_nodes("div.block") %>% 
                        html_nodes("h2") %>% 
                        html_text() %>% 
                        clean_tags(),
                      Fee =  tuition_fees[1]%>% 
                        html_nodes("p") %>% 
                        html_text() %>% 
                        clean_tags())
}, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = tibble(Detail = webPage %>% 
                       html_nodes("div.inside") %>% 
                       html_nodes('div.panel-pane.pane-entity-field') %>% 
                       html_nodes("div.field") %>% 
                       html_text() %>% 
                       clean_tags()) %>% 
      filter(!grepl("apply now", Detail, ignore.case = T)) %>% 
      mutate(Category = webPage %>% 
                       html_nodes("div.inside") %>% 
                       html_nodes('div.panel-pane.pane-entity-field') %>% 
                       html_nodes("h3") %>% 
                       html_text() %>% 
                       clean_tags()) %>% 
      select(Category, Detail)
    
    Duration = get_detail_df(details, "Length", " AND ")
    Campus = get_detail_df(details, "Campus", " AND ")
    Credential = get_detail_df(details, "Credential", " AND ")
    
    Description = webPage %>% 
      html_nodes(clean_css_space("div.field field--name-body field--type-text-with-summary field--label-above")) %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    courses = tibble(Name = webPage %>% 
                       html_nodes("div.fos") %>% 
                       html_nodes('div.term') %>% 
                       html_nodes('li') %>% 
                       html_nodes('a') %>% 
                       html_text(),
                     url = webPage %>% 
                       html_nodes("div.fos") %>% 
                       html_nodes('div.term') %>% 
                       html_nodes('li') %>% 
                       html_nodes('a') %>% 
                       html_attr('href') %>% 
                       paste0('https://www.snpolytechnic.com', .))
    
    WIL = NA
    tryCatch({
      print("Working on courses")
      course_df = tibble(Code = NA, Name = NA, Description = NA)
      for(node in 1:nrow(courses)){
        coursePage = read_webPage(courses$url[node])
        node_df = tibble(Code = coursePage %>% html_nodes(clean_css_space("div.field field--name-field-course-code field--type-text field--label-above")) %>% html_text %>% clean_tags() %>% paste(., collapse = " "),
                         Name = courses$Name[node],
                         Description = coursePage %>% html_nodes(clean_css_space("div.field field--name-body field--type-text-with-summary field--label-above")) %>% html_text %>% clean_tags() %>% paste(., collapse = " "))
        course_df = course_df %>% 
          bind_rows(node_df) %>% 
          filter(!is.na(Code))
      }
      
      WIL = course_eval(course_df, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    
    
    tryCatch({
      
      if(grepl("college", Credential, ignore.case = T)){
        Program_tuition = tuition_df %>% 
          filter(grepl("college", Type, ignore.case = T)) %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url])
      } else if(grepl("bachelor", Credential, ignore.case = T)){
        Program_tuition = tuition_df %>% 
          filter(grepl("university", Type, ignore.case = T)) %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url])
      }
      
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = Description, WIL = WIL)
    
    
    
    
    
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




save_out_standard_file(programs, Institution$institution_name, "Ontario")
