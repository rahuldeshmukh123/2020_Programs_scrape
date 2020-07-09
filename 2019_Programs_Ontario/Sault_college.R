#############################
##### Sault
## Type of website:


Institution <- CIConnexName("Sault")
webPage <- read_webPage("https://www.saultcollege.ca/Programs/AZindex.asp")


div_ids <- webPage %>% html_nodes("div") %>% html_attr('id')


details = webPage %>% html_nodes("div")
program_urls = tibble(Program = details[grepl('leftCol4M', div_ids)] %>% html_nodes("li") %>% html_nodes("a") %>% html_text() %>% clean_tags(),
                      url = details[grepl('leftCol4M', div_ids)] %>% html_nodes("li") %>% html_nodes("a") %>% html_attr('href') %>% paste0("https://www.saultcollege.ca", .),
                      Credential = details[grepl('leftCol4M', div_ids)] %>% html_nodes("li") %>% html_text() %>% clean_tags()) %>% 
  mutate(Credential = gsub(paste(Program, collapse = "|"), "", Credential)) %>% 
  separate(Credential, c("Credential", "Duration"), sep = "\\(") %>% 
  mutate(Duration = gsub("\\)", "", Duration))



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    
    Description = get_detail_id(webPage %>% html_nodes('div'), id = 'leftCol4M') %>% html_nodes('p') %>% html_text() %>% clean_tags() %>% paste(., collapse = " ")
    
    coursesPage = read_webPage(gsub("overview", "study", program_urls$url[url]))
    courses = get_detail_id(coursesPage %>% html_nodes('div'), id = 'leftCol4M') %>% html_nodes('table') %>% html_table(fill = T)
        
    
    WIL = NA
    tryCatch({
      
      courses = courses %>% 
        bind_rows()
      
      courses = courses[1:2] %>% 
        set_names(c("Code", "Name"))
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = NA, Duration = program_urls$Duration[url], Description = Description, WIL = WIL)
    
    
    if(!exists("programs")){
      programs = program_df
    } else {
      programs = programs %>% bind_rows(program_df)
    }
    #tryCatch({
    #  if(!exists("Fees")){
    #    Fees = Program_tuition
    #  } else {
    #    Fees = Fees %>% bind_rows(Program_tuition)
    #  }
    #}, error = function(e){cat("Tuition ERROR :", conditionMessage(e), "\n")})
  }, error = function(e){cat("Overall error ERROR :", conditionMessage(e), "\n")})
}   




save_out_standard_file(programs, Institution$institution_name, "Ontario")


