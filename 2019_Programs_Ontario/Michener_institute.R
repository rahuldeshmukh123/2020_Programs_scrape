#############################
##### Michener Institute
## Type of website: 


Institution <- CIConnexName("Michener Institute")
webPage <- "https://michener.ca/academic-programs/programs/"

remDr$open()
remDr$navigate(webPage)


program_urls = remDr$findElements(using = 'xpath', '/html/body/div[2]/div/div/div/div[2]/main/div[4]/section[1]/ul/li/a')
program_urls = tibble(Program = unlist(lapply(program_urls, function(x) {x$getElementText()})),
                      url = unlist(lapply(program_urls, function(x) {x$getElementAttribute("href")})))
remDr$close()


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = tibble(Category = webPage %>% 
                       html_nodes('div.program-info--box.program-info--box__glance') %>% 
                       html_nodes('h3') %>% 
                       html_text() %>% 
                       clean_tags(),
                     Detail = webPage %>% 
                       html_nodes('div.program-info--box.program-info--box__glance') %>% 
                       html_nodes('p') %>% 
                       html_text() %>% 
                       clean_tags())
    
    Credential = get_detail_df(details, "Credential", " AND ")
    Duration = get_detail_df(details, "length", " AND ")
    
    Description = webPage %>% 
      html_nodes('div.accordion') %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    link_ids = webPage %>% 
      html_nodes('div.accordion') %>% 
      html_nodes('p') %>% 
      html_nodes('a') %>% 
      html_attr('title')
    
    coursePage = webPage %>% 
      html_nodes('div.accordion') %>% 
      html_nodes('p') %>% 
      html_nodes('a')
    
    coursePage = read_webPage(coursePage[grepl("curriculum page", link_ids, ignore.case = T)] %>% html_attr('href'))
    
    
    WIL = NA
    tryCatch({
      courses = tibble(Code = coursePage %>% 
                         html_nodes("div.entry-content") %>% 
                         html_nodes("h3") %>% 
                         html_text() %>% 
                         clean_tags()) %>% 
        mutate(Code = gsub("([[:digit:]])([A-Z])", "\\1|\\2", Code)) %>% 
        separate(Code, c("Code", "Name"), sep = "[[:space:]]*-[[:space:]]*|\\|")
      
      #get nodes from coursePage %>% html_nodes("div.entry-content") 
      #put in df, identify those with h3, group by and number
      #paste down to include all inbetween p/ul etc. into rows for each h3
        
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = Description, WIL = WIL)
    
    
    
    
    
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

