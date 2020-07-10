#############################
##### Assiniboine
## Type of website: Direct links through css - shaky
## Can use some thinking about the course capture -various formats

Institution <- CIConnexName("Assiniboine")

webPage = read_webPage("https://assiniboine.net/programs")

program_urls = tibble(Program = webPage %>% 
                        html_nodes("div.views-field.views-field-nothing") %>% 
                        html_text(),
                      Credential = webPage %>% 
                        html_nodes("div.views-field.views-field-field-program-credential") %>% 
                        html_text(),
                      url = webPage %>% 
                        html_nodes("div.views-field.views-field-nothing") %>% 
                        html_nodes('a') %>% 
                        html_attr('href') %>% 
                        paste0('https://assiniboine.net', .)
                        )











for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    
    courses = get_detail_id(webPage %>% html_nodes('div'), "courses-&-costs", attribute = 'data-tab-id') %>% 
      html_nodes(clean_css_space("table")) %>% 
      html_table(fill = T)
    
    WIL = NA
    
    tryCatch({
      courses = courses[lapply(courses, function(x) grepl("[A-Z]{4}-[[:digit:]]{4,}|Courses|COURSES", paste(x, collapse = ""))) %>% unlist()]
      courses = courses %>% 
        bind_rows() 
      
      if(grepl("Course Title", names(courses) %>% paste(collapse = ""))){
        courses = courses[1:2] %>% 
          set_names("Name", "Code") %>% 
          mutate(Description = NA)
      } else {
        courses = courses %>% 
          bind_rows() %>% 
          set_names("Name", "Credits", "Elective", "Distance", "PLAR") %>% 
          mutate(Name = gsub("\\)|\\(", "\\|", Name)) %>% 
          separate(Name, c("Name", "Code", "Description"), sep = "\\|") %>% 
          mutate(Elective = paste("Elective", Elective, sep = ":"),
                 Distance = paste("Distance", Distance, sep = ":"),
                 PLAR = paste("PLAR", PLAR, sep = ":")) %>% 
          unite("Description_2", Credits:PLAR, sep = ";") %>% 
          unite("Description", c(Description_2, Description), sep = ";")
      }
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("No courses for", program_urls$Program[url], "\n")})
    
    
    
    Credential = webPage %>% 
      html_nodes('div.node__content') %>% 
      html_nodes(clean_css_space('div.field field--name-field-program-credential field--type-entity-reference field--label-inline')) %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " AND ")
    
    Duration = webPage %>% 
      html_nodes('div.node__content') %>% 
      html_nodes(clean_css_space('div.field field--name-field-program-length field--type-string field--label-inline')) %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " AND ")
    
    Credits = webPage %>% 
      html_nodes('div.node__content') %>% 
      html_nodes(clean_css_space('div.field field--name-field-credits field--type-string field--label-inline')) %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " AND ")
    
    Campus = webPage %>% 
      html_nodes('div.node__content') %>% 
      html_nodes(clean_css_space('div.field field--name-field-locations field--type-entity-reference field--label-above')) %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " AND ")
    
    Extra_dets = webPage %>% 
      html_nodes('div.node__content') %>% 
      html_nodes(clean_css_space('div.field field--name-field-study-options field--type-entity-reference field--label-inline')) %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " AND ")
    
    
    Load = webPage %>% 
      html_nodes('div.node__content') %>% 
      html_nodes(clean_css_space('div.field field--name-field-course-load field--type-list-string field--label-inline')) %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " AND ")
    
    
    
    Description = webPage %>% 
      html_nodes(clean_css_space('div.clearfix text-formatted field field--name-field-body field--type-text-with-summary field--label-hidden field__item')) %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    
    
    
    tuition = webPage %>% 
               html_nodes(clean_css_space('div.clearfix text-formatted field field--name-field-courses-costs field--type-text-long field--label-hidden field__item')) %>% 
               html_nodes('p') %>% 
               html_text() %>% 
      gsub("([[:digit:]])(\\.)", "\\1\\|", .) %>% 
      str_split("\\|") %>% 
      unlist()

    intl_tuition =  webPage %>% 
      html_nodes(clean_css_space('div.clearfix text-formatted field field--name-field-courses-costs field--type-text-long field--label-hidden field__item')) %>% 
      html_nodes('table') %>% 
      html_table(fill = T)
    
    
    tryCatch({
      
      Program_tuition = tibble(Category = tuition) %>% 
        separate(Category, c("name", "value"), sep = "\\$") %>% 
        filter(!is.na(value)) %>% 
        bind_rows(tibble(value = webPage %>% 
                           html_nodes(clean_css_space('div.clearfix text-formatted field field--name-field-courses-costs field--type-text-long field--label-hidden field__item')) %>% html_text() %>% paste(., collapse = ""),
                         name = "All text")) %>% 
        mutate(Year_Semester = "Domestic_unstructured",
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               institution = Institution$institution_name,
               prov_terr = Institution$prov_terr,
               record_id = Institution$record_id)
      
      
    }, error = function(e){cat("No tuition for", program_urls$Program[url], "\n")})
    
    
    tryCatch({
      
      intl_program_tuition = intl_tuition %>% 
        bind_rows() %>% 
        set_names(c("Year_Semester", names(.)[2:ncol(.)])) %>% 
        mutate_if(is.numeric,as.character, is.factor, as.character, is.integer, as.character) %>% 
        pivot_longer(cols = 2:ncol(.)) %>% 
        mutate(Year_Semester = paste0("International", Year_Semester),
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               institution = Institution$institution_name,
               prov_terr = Institution$prov_terr,
               record_id = Institution$record_id)
      
      if(exists('Program_tuition')){
        Program_tuition = Program_tuition %>% 
          bind_rows(intl_program_tuition)
      } else {
        Program_tuition = intl_program_tuition
      }
      
      
    }, error = function(e){cat("No tuition for", program_urls$Program[url], "\n")})
    
    
    
      
     
      
    
    
    
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


save_out_standard_file(programs, Institution$institution_name, "Manitoba")
