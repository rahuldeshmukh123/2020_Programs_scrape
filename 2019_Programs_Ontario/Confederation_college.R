#############################
##### Confederation
## Type of website:


Institution <- CIConnexName("Confederation")
webPage <- read_webPage('https://www.confederationcollege.ca/programs-courses/full-time-programs?')

program_urls <- tibble(Program = webPage %>% 
                         html_nodes("div.jsfilter-row") %>% 
                         html_text(),
                       url = webPage %>% 
                         html_nodes("div.jsfilter-row") %>% 
                         html_nodes("a") %>% 
                         html_attr("href") %>% 
                         paste0("https://www.confederationcollege.ca", .),
                       campus = webPage %>% 
                         html_nodes("div.jsfilter-row") %>%
                         html_attr("data-campuses"),
                       credential = webPage %>% 
                         html_nodes("div.jsfilter-row") %>%
                         html_attr("data-credential"),
                       international = webPage %>% 
                         html_nodes("div.jsfilter-row") %>%
                         html_attr("data-field-international"),
                       articulation_agr = webPage %>% 
                         html_nodes("div.jsfilter-row") %>%
                         html_attr("data-has-articulation-agreement"))



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = tibble(category = webPage %>% 
                       html_nodes("div.program-details-block.program-sidebar") %>% 
                       html_nodes("li") %>% 
                       html_nodes("div.label") %>% 
                       html_text(),
                     detail = webPage %>% 
                       html_nodes("div.program-details-block.program-sidebar") %>% 
                       html_nodes("li") %>% 
                       html_nodes("span") %>% 
                       html_text())
    
    program_urls$campus[url] = if(program_urls$campus[url] == "") details %>% filter(grepl("location", category, ignore.case = T)) %>% select(detail) %>% unlist() %>% paste(., collapse = ";") else program_urls$campus[url]
    program_urls$credential[url] = if(program_urls$credential[url] == "") details %>% filter(grepl("credential", category, ignore.case = T)) %>% select(detail) %>% unlist() %>% paste(., collapse = ";") else program_urls$credential[url]
    duration = details %>% filter(grepl("duration", category, ignore.case = T)) %>% select(detail) %>% unlist() %>% paste(., collapse = ";")
    
    description = webPage %>% 
      html_nodes("div.content") %>% 
      html_nodes("div.field.field-name-body.field-type-text-with-summary.field-label-hidden") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    courseWebPage = read_webPage(paste0(program_urls$url[url], "/courses"))
    WIL = NA
    tryCatch({
      courses = courseWebPage %>% 
        html_nodes("div.block.block-system") %>% 
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows() %>% 
        select(1:3) %>% 
        set_names(c("Code", "Detail", "Hours")) %>% 
        mutate(Code = ifelse(Code == "", NA, Code),
               Detail = ifelse(Detail == "", NA, Detail),
               Code = zoo::na.locf(Code, na.rm = F)) %>% 
        filter(!is.na(Code)) %>% 
        filter(!is.na(Detail)) %>% 
        filter(!grepl("^Choose ", Detail, ignore.case = T)) %>% 
        group_by(Code) %>% 
        mutate(Names = row_number()) %>% 
        filter(Names < 3) %>% 
        mutate(Names = ifelse(Names == 2, "Description", "Name")) %>% 
        pivot_wider(id_cols = Code, names_from = Names, values_from = Detail)
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    tuitionWebPage = read_webPage(paste0(program_urls$url[url], "/tuition-fees"))
    intlTuitionWebPage = read_webPage(paste0(program_urls$url[url], "/intl-tuition-fees"))
    
    
    tryCatch({
      tuition = tuitionWebPage %>% 
        html_nodes("div.year-container")
      if(exists("Program_tuition")){
        remove(Program_tuition)
      }
      for(set in 1:length(tuition)){
        tuitionYearDf = tibble(Category = tuition[[set]] %>% 
                           html_nodes("div.year-breakdown") %>% 
                           html_nodes("span.field-label") %>% 
                           html_text() %>% 
                             gsub("^\n[[:space:]]*", "", .),
                         Value = tuition[[set]] %>% 
                           html_nodes("div.year-breakdown") %>% 
                           html_nodes("span.field-value") %>% 
                           html_text() %>% 
                           gsub("^\n[[:space:]]*", "", .),
                         Year = set) %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url],
                 studentType = "Domestic")
        if(!exists('Program_tuition')){
          Program_tuition = tuitionYearDf
        } else {
          Program_tuition = Program_tuition %>% 
            bind_rows(tuitionYearDf)
        }
      }

    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    
    tryCatch({
      tuition = intlTuitionWebPage %>% 
        html_nodes("div.year-container")
      for(set in 1:length(tuition)){
        tuitionYearDf = tibble(Category = tuition[[set]] %>% 
                                 html_nodes("div.year-breakdown") %>% 
                                 html_nodes("span.field-label") %>% 
                                 html_text() %>% 
                                 gsub("^\n[[:space:]]*", "", .),
                               Value = tuition[[set]] %>% 
                                 html_nodes("div.year-breakdown") %>% 
                                 html_nodes("span.field-value") %>% 
                                 html_text() %>% 
                                 gsub("^\n[[:space:]]*", "", .),
                               Year = set) %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url],
                 studentType = "International")
        if(!exists('Program_tuition')){
          Program_tuition = tuitionYearDf
        } else {
          Program_tuition = Program_tuition %>% 
            bind_rows(tuitionYearDf)
        }
      }
      
    }, error = function(e){cat("intl-tuition ERROR :", conditionMessage(e), "\n")})
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$credential[url], Campus = program_urls$campus[url], Duration = duration, Description = description, WIL = WIL)
    
    
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
