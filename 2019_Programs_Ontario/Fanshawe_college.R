#############################
##### Fanshawe
## Type of website:


Institution <- CIConnexName("Fanshawe")
webPage <- read_webPage('https://www.fanshawec.ca/programs-and-courses')

program_urls <- tibble(Program = webPage %>% 
                         html_nodes(gsub(" ", ".", "div.program-and-courses view view-programs-and-courses-search-api- view-id-programs_and_courses_search_api_ view-display-id-program_courses_page js-view-dom-id-8237af98560bf25e28df03beb7f203ddf666f08d97ee879e8a114210bdbd6186")) %>% 
                         html_nodes("div.view-content") %>% 
                         html_nodes("div.views-row") %>% 
                         html_nodes("a") %>% 
                         html_text(),
                       url = webPage %>% 
                         html_nodes(gsub(" ", ".", "div.program-and-courses view view-programs-and-courses-search-api- view-id-programs_and_courses_search_api_ view-display-id-program_courses_page js-view-dom-id-8237af98560bf25e28df03beb7f203ddf666f08d97ee879e8a114210bdbd6186")) %>% 
                         html_nodes("div.view-content") %>% 
                         html_nodes("div.views-row") %>% 
                         html_nodes("a") %>% 
                         html_attr("href") %>% 
                         paste0("https://www.fanshawec.ca", .))



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    tibble(Category = webPage %>% 
             html_nodes(gsub(" ", ".", "div.view view-program-displays view-id-program_displays")) %>% 
             html_nodes("div.view-content") %>% 
             html_nodes("span") %>% 
             html_text(),
           Detail = webPage %>% 
             html_nodes(gsub(" ", ".", "div.view view-program-displays view-id-program_displays")) %>% 
             html_nodes("div.view-content") %>% 
             html_nodes("div.field-content") %>% 
             html_text())
    
    details = webPage %>% 
      html_nodes(gsub(" ", ".", "div.view view-program-displays view-id-program_displays")) %>% 
      html_nodes("div.views-row") %>% 
      html_nodes("div")
    
    if(exists("details_tbl")){
      remove(details_tbl)
    }
    
    for(i in 1:length(details)){
      detail_tbl = tibble(Category = details[i] %>% 
                            html_nodes("span") %>% 
                            html_text(),
                          Detail = details[i] %>% 
                            html_nodes("div.field-content") %>% 
                            html_text())
      
      if(!exists("details_tbl")){
        details_tbl = detail_tbl
      } else {
        details_tbl = details_tbl %>% 
          bind_rows(detail_tbl)
      }
      
    }
    
    headings = webPage %>% 
      html_nodes("div.view.view-program-displays.view-id-program_displays.view-display-id-pr_overview_bl") %>% 
      html_nodes("div.offering-headings") %>% 
      html_text()
    
    details = webPage %>% 
      html_nodes("div.view.view-program-displays.view-id-program_displays.view-display-id-pr_overview_bl") %>% 
      html_nodes("div.view-content") %>% 
      html_nodes("div.views-row") %>% 
      html_nodes("div.field-content") %>% 
      html_text()
    
    intake_details = tibble(Category = rep(headings, length(details)/length(headings)),
                            Detail = details)

        
    credential = details_tbl %>% filter(grepl("credential", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    campus = intake_details %>% filter(grepl("location", Category, ignore.case = T)) %>% select(Detail) %>% summarize(Detail = paste0(Detail, collapse = " AND ")) %>% unlist() 
    duration = details_tbl %>% filter(grepl("duration", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    
    
    courses = read_webPage(paste0(program_urls$url[url], "#group_courses"))
    
    
    WIL = NA
    tryCatch({
      courses = courses %>% 
        html_nodes("div.course_listing") %>% 
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows() %>% 
        
        
        
        ### continue here
        
        

      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = credential, Campus = campus, Duration = duration, Description = description, WIL = WIL)
    
    
    if(!exists("programs")){
      programs = program_df
    } else {
      programs = programs %>% bind_rows(program_df)
    }
    tryCatch({
      if(!exists("Fees")){
        Fees = Program_tuition%>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url])
      } else {
        Fees = Fees %>% bind_rows(Program_tuition%>% 
                                    mutate(Institution = Institution$institution_name,
                                           Program = program_urls$Program[url],
                                           Program_url = program_urls$url[url]))
      }
    }, error = function(e){cat("Tuition ERROR :", conditionMessage(e), "\n")})
  }, error = function(e){cat("Overall error ERROR :", conditionMessage(e), "\n")})
}   


save_out_standard_file(programs, Institution$institution_name, "Ontario")
                         
  