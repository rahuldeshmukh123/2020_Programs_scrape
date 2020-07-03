#############################
##### Humber
## Type of website:


Institution <- CIConnexName("Humber")
#webPage <- read_webPage('https://humber.ca/search/full-time.html')

remDr$open(silent = T)
url = 'https://humber.ca/search/full-time.html'
remDr$navigate(url)
Sys.sleep(3)
doc = htmlParse(remDr$getPageSource()[[1]])
program_urls = readHTMLTable(doc) %>% 
  bind_rows()

programUrls = getNodeSet(doc, '/html/body/div[1]/section/div[2]/div[3]/div/div/div/div/table/tbody/tr/td[1]/a/@href')


program_urls = program_urls %>% 
  mutate(url = programUrls %>% unlist())


url = 'https://humber.ca/search/ce/programs.html'
remDr$navigate(url)
Sys.sleep(3)
doc = htmlParse(remDr$getPageSource()[[1]])
ce_program_urls = readHTMLTable(doc) %>% 
  bind_rows()

programUrls = getNodeSet(doc, '/html/body/div[1]/section/div[2]/div[3]/div/div/div/div/table/tbody/tr/td[1]/a/@href')


ce_program_urls = ce_program_urls %>% 
  mutate(url = programUrls %>% unlist())


remDr$close()

program_urls <- program_urls %>% 
  set_names(c("Program", "Code", "Credential", "Duration", "Campus", "url")) %>% 
  bind_rows(ce_program_urls %>% 
              set_names("Program", "Code", "Credential", "url")) %>% 
  arrange(url) %>% 
  mutate(dup = ifelse(lag(url) == url, 1, NA))

program_urls <- program_urls %>% 
  filter(is.na(dup)) %>% 
  select(- dup)
  


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    if(!is.na(Duration)){
      
      details = webPage %>% 
        html_nodes("div.program-info") %>% 
        html_nodes("span.program-credential") %>% 
        html_text()
      
      details = tibble(detail = details) %>% 
        separate(detail, c("Category", "Detail"), sep = ":") %>% 
        mutate(Detail = clean_tags(Detail))
      
      courses = webPage %>% 
        html_nodes("div.curriculum-content-wrapper") %>% 
        html_nodes('div.curriculum') %>%
        html_nodes("div.curr-heading") %>% 
        html_text() %>% 
        clean_tags()
      
      WIL = NA
      tryCatch({
        courses = tibble(courses = courses) %>% 
          separate(courses, c("Code", "Name"), sep = ": ") %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url])
        
        
        WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
        
        
      }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
      
      
      tuition = webPage %>% 
        html_nodes("table.fee-table") %>% 
        html_table(fill = T)
      
      tryCatch({
        Program_tuition = tuition %>% 
          map(~ set_names(., .[1,])) %>% 
          map(~ mutate(., Semester = zoo::na.locf(ifelse(grepl("^semester", .[,1], ignore.case = T), .[,1], NA)))) %>% 
          bind_rows() %>% 
          pivot_longer(-Semester) %>% 
          filter(name != Semester) %>% 
          filter(!is.na(value)) %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url])
        
        
      }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
      
      
      
    } else {
      description = webPage %>% 
        html_nodes("div.container-fluid") %>% 
        html_nodes('p') %>% 
        html_text() %>% 
        clean_tags() %>% 
        paste(., collapse = " ")
      
      courses = webPage %>% 
        html_nodes(clean_css_space("table.table ce-curriculum ce-courses-click responsive-custom table-hover")) %>% 
        html_table(fill = T) %>% 
        bind_rows()
      
      
      WIL = NA
      tryCatch({
        courses = tibble(courses = courses[,1]) %>% 
          separate(courses, c("Code", "Name"), sep = ": ") %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url]) %>% 
          filter(!is.na(Name))

                
        
        WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
        
        
      }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
      
      program_urls$Credential[url] = paste(program_urls$Credential[url], "Continuing education", sep = "_")
    }
    

    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = description, WIL = WIL)
    
    
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