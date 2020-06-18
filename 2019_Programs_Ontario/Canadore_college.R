#############################
##### Canadore
## Type of website:

Institution <- CIConnexName("Canadore")

webPage = read_webPage("https://www.canadorecollege.ca/programs/search")

program_urls = webPage %>% 
  html_nodes("div.col-xs-12") %>% 
  html_nodes("h2") %>% 
  html_text()


rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)
remDr$navigate("https://www.canadorecollege.ca/programs/search")

remove("program_urls")
for(i in 1:40){
  tryCatch({
    if(i != 1){
      next_button = remDr$findElement(using = 'xpath', '/html/body/div[2]/div/div[2]/div/div/div[2]/div/div/div[3]/div/nav/div/ul/li[10]/a')
      next_button$clickElement()
    }
  
  
    programs = remDr$findElements(using = 'xpath', '//*[@id="hits"]/div/div/div/div/a')
    programs = tibble(Program = unlist(lapply(programs, function(x) {x$getElementText()})),
                      url = unlist(lapply(programs, function(x) {x$getElementAttribute('href')})))
    if(!exists("program_urls")){
      program_urls = programs
    } else {
      program_urls = program_urls %>% bind_rows(programs)
        
    }

  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")}) 
}

remDr$close()


program_urls = program_urls %>% 
  separate(Program, c("Program", "FT_PT"), sep = "[[:space:]]\\|[[:space:]]")

program_urls$WIL <- NA
remove(details)
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = webPage %>% 
      html_nodes("div.program-info.hidden-xs.hidden-sm") %>% 
      html_text() %>% 
      gsub("^\r\n[[:space:]]*\r\n[[:space:]]*", "", .) %>% 
      str_split("\r\n[[:space:]]*\r\n[[:space:]]*") %>% 
      unlist()
    
    details = tibble(detail = details) %>% 
      separate(detail, c("Category", "Value"), sep = ":")
    
    Campus = details$Value[grepl("Campus", details$Category, ignore.case = T)]
    Description = webPage %>% html_nodes("div.program-details") %>% html_nodes("p") %>%  html_text() %>% paste(collapse = " ")
    CreDuration = webPage %>% html_nodes("h3.h5.program-duration") %>%  html_text()
    

    tryCatch({
      CreDuration = tibble(CreDuration = CreDuration) %>% 
        separate(CreDuration, c("Duration", "Credential"), sep = "[[:space:]]–[[:space:]]")
      
      CreDuration = c(CreDuration$Duration[1], CreDuration$Credential[1])
      
    }, error = function(e){cat("Credential and Duration ERROR :", conditionMessage(e), "\n")})
    
    Credential = CreDuration[length(CreDuration)]
    Duration =  CreDuration[1]
    
    courses = webPage %>% html_nodes("div.info-collapse.collapse") %>% html_nodes("table.table.curriculum")
    #https://community.rstudio.com/t/how-to-skip-empty-table-while-scraping-with-rvest-html-table/27597/2
    
    tryCatch({
      courses = courses %>% 
        html_table() %>% 
        bind_rows() %>% 
        mutate(CodeName = gsub("([A-Z]{3}[[:digit:]]{3})", "\\1\\|", X1)) %>% 
        separate(CodeName, c("Code", "Name"), sep = "\\|")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url])
      
    }, error = function(e){cat("Courses ERROR :", conditionMessage(e), "\n")})
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = Description, WIL = WIL)
    
    tryCatch({
      Program_tuition = webPage %>% html_nodes("div.info-collapse") %>% html_nodes("div.table-responsive") %>%  html_nodes("table.table") %>% html_table() %>% bind_rows()
      Program_tuition = Program_tuition %>% 
        pivot_longer(cols = -1) %>% 
        mutate(Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               institution = Institution$institution_name,
               prov_terr = Institution$prov_terr,
               record_id = Institution$record_id)
      
      
    }, error = function(e){cat("Tuition ERROR :", conditionMessage(e), "\n")})
    
    
    
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
