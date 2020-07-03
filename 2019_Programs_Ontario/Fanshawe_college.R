#############################
##### Fanshawe
## Type of website:


Institution <- CIConnexName("Fanshawe")
webPage <- read_webPage('https://www.fanshawec.ca/programs-and-courses')

program_urls <- tibble(Program = webPage %>% 
                         html_nodes(gsub(" ", ".", "div.program-and-courses view view-programs-and-courses-search-api- view-id-programs_and_courses_search_api_ view-display-id-program_courses_page")) %>% 
                         html_nodes("div.view-content") %>% 
                         html_nodes("div.views-row") %>% 
                         html_nodes("a") %>% 
                         html_text(),
                       url = webPage %>% 
                         html_nodes(gsub(" ", ".", "div.program-and-courses view view-programs-and-courses-search-api- view-id-programs_and_courses_search_api_ view-display-id-program_courses_page")) %>% 
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
    
    description = webPage %>% 
      html_nodes(clean_css_space("div.group_overview.block.block-ctools.block-entity-view--node")) %>% 
      html_nodes("article") %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")

        
    credential = details_tbl %>% filter(grepl("credential", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    campus = intake_details %>% filter(grepl("location", Category, ignore.case = T)) %>% select(Detail) %>% summarize(Detail = paste0(Detail, collapse = " AND ")) %>% unlist() 
    duration = details_tbl %>% filter(grepl("duration", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    
    
    courses = read_webPage(paste0(program_urls$url[url], "#group_courses"))
    
    
    
    WIL = NA
    tryCatch({
      courses_df = courses %>% 
        html_nodes("div.course_listing") %>% 
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows() %>% 
        filter(X1 != "") %>% 
        filter(!grepl("level|^Take|^Students Must", X1, ignore.case = T)) %>% 
        mutate(index = zoo::na.locf(ifelse(grepl("^[A-Z]{3}|Program Residency|Program residency|program resi", X1), X1, NA), na.rm = F)) %>% 
        filter(!is.na(index)) %>% 
        pivot_longer(cols = 1:(ncol(.) - 1)) %>% 
        select(-name) %>% 
        distinct() %>% 
        group_by(index) %>% 
        filter(!is.na(value)) %>% 
        mutate(name = row_number()) %>% 
        pivot_wider(id_cols = index, names_from = name, values_from = value) %>% 
        ungroup() %>% 
        select(-c(index, 3)) %>% 
        set_names(c("Code", "Name", "Description"))
      
      
      WIL = course_eval(courses_df, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    
    tuition = read_webPage(gsub("next", "domestic/fees/next", program_urls$url[url]))
    
    if(exists("Program_tuition")){
      remove(Program_tuition)
    }
    
    tryCatch({
 
      
      Program_tuition = tuition %>% 
        html_nodes('div.block') %>% 
        html_nodes("div.clearfix") %>% 
        html_nodes("table.fees-tbl") %>% 
        html_table(fill = T) %>% 
        bind_rows() %>% 
        mutate() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               studentType = "Domestic")

      
    }, error = function(e){cat("domestic tuition ERROR :", conditionMessage(e), "\n")})
    
    tuition = read_webPage(gsub("next", "international/fees/next", program_urls$url[url]))
    
    tryCatch({

      intlTuition = tuition %>% 
        html_nodes('div.block') %>% 
        html_nodes("div.clearfix") %>% 
        html_nodes("table.fees-tbl") %>% 
        html_table(fill = T) %>% 
        bind_rows() %>% 
        mutate() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               studentType = "International")
      
      
      if(!exists('Program_tuition')){
        Program_tuition = intlTuition
      } else {
        Program_tuition = Program_tuition %>% 
          bind_rows(intlTuition)
      }
      
    }, error = function(e){cat("intl tuition ERROR :", conditionMessage(e), "\n")})
      

      
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = credential, Campus = campus, Duration = duration, Description = description, WIL = WIL)
    
    
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
                         
  