#############################
##### Northern
## Type of website: d


Institution <- CIConnexName("Northern college")
webPage <- read_webPage("http://www.northernc.on.ca/programs/")


program_urls = tibble(Program = webPage %>% html_nodes("div.cs-content") %>% html_nodes("a") %>% html_text(),
                      url = webPage %>% html_nodes("div.cs-content") %>% html_nodes("a") %>% html_attr("href"))


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = tibble(detail = webPage %>% 
                       html_nodes("div.x-tab-pane.active") %>% 
                       html_nodes('p') %>% 
                       html_text()) %>% 
      mutate(detail = sub("\n", "|", detail, fixed = T)) %>% 
      separate(detail, c("Category", "Detail"), sep = "\\|")
    
    Credential = webPage %>% 
      html_nodes("li.x-block-grid-item") %>% 
      html_text()
    
    Credential = gsub("(^.*Credential Earned\n)([[:alpha:]]*| *)*(.*$)", "\\2", Credential[1]) %>% clean_tags()
    
    Duration = get_detail_df(details, "Length")
    Description = get_detail_df(details, "description")
    Campus = get_detail_df(details, "program codes", separation = " AND ")
    
    div_ids = webPage %>% html_nodes("div") %>% html_attr("id")
    
    courses = webPage %>% html_nodes("div")
    courses_urls = courses[grepl("x-legacy-panel-3", div_ids)] %>% html_nodes("p") %>% html_nodes("a") %>% html_attr("href")
    courses_urls_length = length(courses_urls)
    courses_urls = courses_urls %>% unique()
    
    course_appendix_full = tibble(Code = NA,
                             Name = NA)
    
    tryCatch({
      for(course_url in 1:length(courses_urls)){
        course_appendix = read_webPage(courses_urls[course_url]) %>% html_nodes("div")
        course_appendix = course_appendix[grepl("x-legacy-panel-3", course_appendix %>% html_attr("id"))] %>% html_nodes("p")
        course_appendix = course_appendix[1:courses_urls_length]
        course_appendix = tibble(detail = course_appendix %>% html_text() %>% str_split("\n") %>% unlist())
        
        course_appendix = course_appendix %>% 
          mutate(detail = gsub("([A-Z]{2}[[:digit:]]{4})([[:space:]]*)([A-Z)])", "\\1|\\3", detail)) %>% 
          separate(detail, c("Code", "Name"), sep = "\\|")
        
        course_appendix_full = course_appendix_full %>% 
          bind_rows(course_appendix) %>% 
          filter(!is.na(Name))
        
      }
    }, error = function(e){cat("corse appendix ERROR :", conditionMessage(e), "\n")})

    
    WIL = NA
    tryCatch({
      courses = tibble(detail = courses[grepl("x-legacy-panel-3", div_ids)] %>% html_nodes("p") %>% html_text() %>% str_split("\n") %>% unlist()) %>%  
        mutate(detail = gsub("([A-Z]{2}[[:digit:]]{4})([[:space:]]*)([A-Z)])", "\\1|\\3", detail)) %>% 
        separate(detail, c("Code", "Name"), sep = "\\|")
      
      if(nrow(course_appendix_full) > 1){
        courses = course_appendix_full %>% 
          bind_rows(courses)
      }
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T) 
      
    }, error = function(e){cat("corsee  ERROR :", conditionMessage(e), "\n")})
    
    tuition_page = gsub("/$", "-tuition", program_urls$url[url])
    

    
    
    tryCatch({
      
      Tuition = read_webPage(tuition_page) %>% 
        html_nodes("div.entry-content.content") %>% 
        html_nodes("table") %>% 
        html_table(fill = T)
      
      Program_tuition = Tuition %>% 
        bind_rows() %>% 
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
