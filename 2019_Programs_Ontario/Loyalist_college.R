#############################
##### Loyalist
## Type of website:


Institution <- CIConnexName("Loyalist")
webPage <- read_webPage("https://www.loyalistcollege.com/programs-and-courses/full-time-programs/")

program_urls = tibble(Program = webPage %>% 
                        html_nodes('div.programs') %>% 
                        html_nodes('li') %>% 
                        html_text(),
                      url = webPage %>% 
                        html_nodes('div.programs') %>% 
                        html_nodes('li') %>% 
                        html_nodes('a') %>% 
                        html_attr('href') %>% 
                        paste0('https://www.loyalistcollege.com', .))


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    div_ids = webPage %>% 
      html_nodes("div") %>% 
      html_attr("id")
    
    details = webPage %>% 
      html_nodes('div')
    details = details[grepl('program-data', div_ids)]
    
    details = tibble(detail = details %>% 
                       html_nodes("ul") %>% 
                       html_nodes("li") %>% 
                       html_text() %>% 
                       clean_tags()) %>% 
      mutate(detail = gsub("([a-z])([A-Z])", "\\1|\\2", detail)) %>% 
      separate(detail, c("Category", "Detail"), sep = "\\|")
                     
    Credential = details %>% filter(grepl("credential", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " AND ")                 
    Campus = details %>% filter(grepl("location", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " AND ")                 
    Credential = details %>% filter(grepl("year|semester", Category, ignore.case = T)) %>% select(Category) %>% unlist() %>% paste(., collapse = " AND ")                   

    div_ids = webPage %>% html_nodes("div") %>% html_attr("id")
    Description = webPage %>% html_nodes("div") 
    Description = Description[grepl("description", div_ids)] %>% html_text() %>% clean_tags() %>% paste(., collapse = " ")
    
    
    tryCatch({
      
      webPage %>% 
        html_nodes("div.tab-view.costs") %>% 
        html_nodes("ul") %>% 
        html_nodes("li") %>% 
        html_text()
      
      Program_tuition = tibble(detail = webPage %>% 
                                 html_nodes("div.tab-view.costs") %>% 
                                 html_nodes("ul") %>% 
                                 html_nodes("li") %>% 
                                 html_text()) %>%
        separate(detail, c("Category", "Fee"), sep = ":[[:space:]]*") %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    

    
    
    WIL = NA
    tryCatch({
      
      courses = tibble(Name = webPage %>% 
                         html_nodes("div.tab-view.courses") %>% 
                         html_nodes("ul") %>% 
                         html_nodes("ul") %>% 
                         html_nodes("li.courseName") %>% 
                         html_text(),
                       Description = webPage %>% 
                         html_nodes("div.tab-view.courses") %>% 
                         html_nodes("ul") %>% 
                         html_nodes("ul") %>% 
                         html_nodes("li.courseDescription") %>% 
                         html_text() %>% 
                         clean_tags())%>% 
        separate(Name, c("Code", "Name"), sep = "[[:space:]]")
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
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
