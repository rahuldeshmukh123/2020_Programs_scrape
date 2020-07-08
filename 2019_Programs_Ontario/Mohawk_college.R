#############################
##### Mohawk
## Type of website: Really excellent - well structured


Institution <- CIConnexName("Mohawk")
webPage <- read_webPage("https://www.mohawkcollege.ca/programs/search")

program_urls = webPage %>% 
  html_nodes('div.jsfilter-content-container') %>% 
  html_nodes('div')


### Getting all possible variables in case they are useful in the future
program_urls = tibble(Program = program_urls %>% html_nodes("a") %>% html_text(),
                  url = program_urls %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://www.mohawkcollege.ca", .),
                  Topic = program_urls %>% html_attr('data-field-home-theme'),
                  Duration = program_urls %>% html_attr('data-field-program-length'),
                  Credential = program_urls %>% html_attr('data-field-accreditation'),
                  Campus = program_urls %>% html_attr('data-field-program-campus'),
                  Status = program_urls %>% html_attr('data-field-program-status'),
                  WIL = program_urls %>% html_attr('data-field-co-op-option'),
                  Intl_students = program_urls %>% html_attr('data-field-international-students'),
                  Start_month = program_urls %>% html_attr('data-field-session-start'),
                  Fast_track = program_urls %>% html_attr('data-field-fast-track'))



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    Description = webPage %>% 
      html_nodes(clean_css_space('div.field field--name-field-program-highlights field--type-text-long field--label-above')) %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(collapse = " ")


    Tuition = webPage %>% 
      html_nodes("div.tuition-and-fees--table") %>% 
      html_nodes("table") %>% 
      html_table(fill = T)
    
    tryCatch({
      
      
      
      Program_tuition = Tuition %>% 
        bind_rows() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = webPage %>% 
      html_nodes(clean_css_space("div.field.field--name-field-course-overview.field--type-text-long.field--label-above")) %>% 
      html_nodes("div.field__item") %>% 
      html_nodes("iframe") %>% 
      html_attr("src")
    
    
    
    WIL = NA
    tryCatch({
      
      courses = read_webPage(courses) %>% 
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows()
      
      courses = courses[1:2] %>% 
        set_names(c("Code", "Name"))
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    if(is.na(WIL)){
      WIL = program_urls$WIL[url]
    }
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = Description, WIL = WIL)
    
    
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






