#############################
##### Niagara
## Type of website: Really excellent - well structured


Institution <- CIConnexName("Niagara college")
webPage <- read_webPage("https://www.niagaracollege.ca/academics/programs/")

details = webPage %>% html_nodes("ul.filtered-programs") %>% html_nodes("li") %>% html_nodes("a")

#Capturing details for potential future use
program_urls <- tibble(Program = details %>% html_text(),
                       url = details %>% html_attr('href') %>% paste0("https://www.niagaracollege.ca", .),
                       Campus = details %>% html_attr('data-campus'),
                       Topic = details %>% html_attr('data-interest'),
                       Credential = details %>% html_attr('data-credential'),
                       FTPT = details %>% html_attr('data-type-of-study'),
                       School = details %>% html_attr('data-school'),
                       Start_date = details %>% html_attr('data-start-date'))



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = tibble(detail = webPage %>% 
                       html_nodes(clean_css_space("div.r details animated fadeInDown")) %>% 
                       html_nodes("ul") %>% 
                       html_nodes("li") %>% 
                       html_text() %>% 
                       clean_tags()) %>% 
      separate(detail, c("Category", "Detail"), sep = ":")
    
    if(program_urls$Credential[url] == ""){
      program_urls$Credential[url] = details %>% filter(grepl("Credential", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " AND ")
    }
    if(program_urls$Campus[url] == ""){
      program_urls$Campus[url] = details %>% filter(grepl("Campus", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " AND ")
    }
    
    Duration = details %>% filter(grepl("Delivery", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " AND ")
    
    Description = webPage %>% 
      html_nodes("div.summary") %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    
    #PDF tuitiion schedule
    
    courses = webPage %>% 
      html_nodes("div.curriculum") %>% 
      html_nodes("table") %>% 
      html_table(fill = T)
      
    WIL = NA
    tryCatch({
      
      courses = courses %>% 
        plyr::rbind.fill()
      
      courses = courses[1:2] %>% 
        set_names(c("Code", "Name"))
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    

    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = Duration, Description = Description, WIL = WIL)
    
    
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
