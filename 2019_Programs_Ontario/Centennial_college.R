#############################
##### Centennial
## Type of website:

Institution <- CIConnexName("Centennial")

webPage = read_webPage("https://www.centennialcollege.ca/programs-courses/full-time/?areasofstudy")

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)
remDr$navigate("https://www.centennialcollege.ca/programs-courses/full-time/?areasofstudy")

program_urls <- remDr$findElements(using = "xpath", '/html/body/div[2]/main/section[2]/span/div/div/div[2]/div[2]/div/div[2]/div/div[1]/div[2]/div/div/div/a')
credentials = remDr$findElements(using = "xpath", '/html/body/div[2]/main/section[2]/span/div/div/div[2]/div[2]/div/div[2]/div/div[1]/div[2]/div/div/div[2]')


program_urls <- tibble(Program = unlist(lapply(program_urls, function(x) {x$getElementText()})),
                       url = unlist(lapply(program_urls, function(x) {x$getElementAttribute("href")})),
                       Credential = unlist(lapply(credentials, function(x) {x$getElementText()})))

remDr$close()


remove(details)
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = webPage %>% 
      #html_nodes(xpath = '/html/body/div[2]/main/section[1]/div/article/div/div')
      html_nodes('div.program-single-summary') %>% 
      html_nodes('ul') %>% 
      html_text() %>% 
      str_split("\r\n") %>% 
      unlist() %>% 
      gsub("^[[:space:]]*", "", .) 
    
    details = tibble(details = details) %>% 
      separate(details, c("category", "detail"), sep = ":") %>% 
      mutate(category = ifelse(grepl("Length", category, ignore.case = T), "Duration",
                               ifelse(grepl("Location", category, ignore.case = T), "Campus", category))) %>% 
      filter(!is.na(detail))
    
    duration = details %>% filter(category == "Duration") %>% select(detail) %>% unlist()
    campus = details %>% filter(category == "Campus") %>% select(detail) %>% unlist()
    program_code = details %>% filter(grepl("code", category, ignore.case = T)) %>% select(detail) %>% unlist()
    
    description = webPage %>% 
      html_nodes('div.program-info.current-open') %>% 
      html_text()
    

    tuition = webPage %>% 
      html_nodes('table.responsive') %>% 
      html_table(fill = T) 
    
    tryCatch({
    
      names(tuition) = tuition %>% 
       map(~ ifelse(length(grep("Tuition", names(.) %>% unlist())) > 0, "keep", "drop")) %>% 
        unlist()
      
      
      Program_tuition = tuition[["keep"]] %>% 
        bind_rows() %>% 
        mutate(Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               institution = Institution$institution_name,
               prov_terr = Institution$prov_terr,
               record_id = Institution$record_id)
       
    
    }, error = function(e){cat("No tuition for", program_urls$Program[url], "\n")})
    
    courses = webPage %>% 
      html_nodes('table.tbl.responsive') %>% 
      html_table(fill = T) %>% 
      bind_rows()
    
    WIL = if(length(grepl("Pratique|Practicum|Field placement|Placement|Work experience|Co-op|coop|Apprentice|internship|field practice|clinical practice|clinical work|work term| stage |capstone", courses, ignore.case = T)) > 0) {"WIL"} else {NA}
    
    tryCatch({
      
      WIL_2 = course_eval(courses[,1:2] %>% set_names("Code", "Name"), Institution$institution_name, program_urls$Program[url], program_urls$url[url])
      
      if(!is.na(WIL_2)){
        WIL = WIL_2
      }
      
    }, error = function(e){cat("No courses for", program_urls$Program[url], "\n")})
    
    

    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = campus, Duration = duration, Description = description, WIL = WIL)
    
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
