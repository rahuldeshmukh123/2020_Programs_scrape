#############################
##### Seneca
## Type of website: 


Institution <- CIConnexName("Seneca")
webPage <- read_webPage("https://www.senecacollege.ca/programs/alphabetical.html")

program_urls <- get_detail_id(webPage %>% html_nodes("div.list") %>% html_nodes('div'), id = 'programs-landing-page-parsys_list')

program_urls <- tibble(Program = program_urls %>%  html_nodes("ul.seneca-list") %>% html_nodes("li.foundation-list-item") %>% html_nodes("a") %>% html_text(),
                       url = program_urls %>% html_nodes("ul.seneca-list") %>% html_nodes("li.foundation-list-item") %>% html_nodes("a") %>% html_attr('href') %>% paste0("https://www.senecacollege.ca", .)) 


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    
    details = tibble(detail = webPage %>% html_nodes('div.container.program-summary') %>% html_nodes('div.row') %>% html_nodes("div") %>% html_text())
    details = details %>% 
      mutate(detail = gsub("^\n[[:space:]]*", "", detail),
             detail = sub("\n", "|", detail)) %>% 
      separate(detail, c("Category", "Detail"), sep = "\\|[[:space:]]*") %>% 
      mutate(Detail = clean_tags(Detail))
    
    
    Campus = get_detail_df(details, category = "Location", separation = " AND ")
    Duration = get_detail_df(details, category = "duration", separation = " AND ")
    Credential = get_detail_df(details, category = "credential", separation = " AND ")
    
    Description = webPage %>% html_nodes("div.text.parbase") %>% html_nodes('p') %>% html_text() %>% clean_tags() %>% paste(., collapse = " ")
    
    coursesPage = read_webPage(gsub(".html", "/courses.html#pre-content_menu", program_urls$url[url]))
    
    WIL = NA
    tryCatch({
      
      courses = coursesPage %>% 
        html_nodes("div.table-component.table-responsive") %>% 
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows()
      
      courses = courses[1:2] %>% 
        set_names(c("Code", "Name"))
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    tuitionPage = read_webPage(gsub(".html", "/costs.html#pre-content_menu", program_urls$url[url]))
    
    
    tryCatch({
      
      
      
      Program_tuition = tibble(domestic_tuition = tuitionPage %>% html_nodes("p.tuition-domestic") %>% html_text() %>% clean_tags() %>% paste(., collapse = " "),
                               intl_tuition = tuitionPage %>% html_nodes("p.tuition-international") %>% html_text() %>% clean_tags() %>% paste(., collapse = " ")) %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
      
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


