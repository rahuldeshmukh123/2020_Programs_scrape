#############################
##### George Brown
## Type of website:


Institution <- CIConnexName("George Brown")
webPage <- read_webPage('https://www.georgebrown.ca/programs/program-finder')

webPage %>% 
  html_nodes("tr.program-row") %>% 
  html_nodes("a") %>% 
  html_text()


remDr$open(silent = T)

url <- "https://www.georgebrown.ca/programs/program-finder"
remDr$navigate(url)


for(i in 1:6){
  print(i)
  if(exists('program_urls') & i == 1){
    remove(program_urls)
  }
  tryCatch({
    
    doc = htmlParse(remDr$getPageSource()[[1]])
    programTable = readHTMLTable(doc) %>% 
      bind_rows() %>% 
      filter(!is.na(`Program Name`)) %>% 
      select(-c(V1:V3)) %>% 
      mutate(index = rep(1:(nrow(.)/2), each = 2)) %>% 
      pivot_longer(cols = 1:3) %>% 
      filter(!is.na(value)) %>% 
      group_by(index, name) %>% 
      mutate(index_2 = row_number(),
             name = ifelse(index_2 == 2, paste(name, index_2, sep = "_"), name)) %>% 
      select(-index_2) %>% 
      pivot_wider(id_cols = index, names_from = name, values_from = value) %>% 
      mutate(`Program Name` = gsub("Add program| to Program Comparison.*$", "", `Program Name`))
    
    programUrls = getNodeSet(doc, '//*[@id="block-program-finder-content"]/div/div/table/tbody/tr/td/div/div[3]/a[1]/@href')
    programTable = programTable %>% 
      ungroup() %>% 
      mutate(url = programUrls %>% unlist())
    
    if(exists('program_urls')){
      program_urls = program_urls %>% 
        bind_rows(programTable)
    } else {
      program_urls = programTable
    }
    
    #Accounting for the appearance of "First" and "Previous" buttons that bumps the "Next" button up 2 spots
    if(i == 1){
      webElem <- remDr$findElement(using = "xpath", '/html/body/div[2]/div[1]/div/div[2]/main/div/div/article/div/div[2]/div[2]/article/div/div/div/div/div/nav/ul/li[5]/a/span[2]')  
    } else {
      webElem <- remDr$findElement(using = "xpath", '/html/body/div[2]/div[1]/div/div[2]/main/div/div/article/div/div[2]/div[2]/article/div/div/div/div/div/nav/ul/li[7]/a/span[2]')  
    }
   webElem$clickElement()
    Sys.sleep(3)
    
  }, error = function(e){cat("Maxed out pages :", conditionMessage(e), "\n")})
}

remDr$close()

program_urls <- program_urls %>% 
  select(-index) %>% 
  mutate(url = paste0("https://www.georgebrown.ca", url))

program_urls <- program_urls %>% 
  set_names(c("Program", "Credential", "International", "Description", "url"))

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    
    details = webPage %>% 
      html_nodes("section.program-general-details") %>% 
      html_nodes("li") %>% 
      html_text() %>% 
      clean_tags()
    
    details = tibble(detail = details) %>% 
      separate(detail, c("Category", "Detail"), sep = ":")
    
    Credential = details %>% filter(grepl("credential", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Duration = details %>% filter(grepl("duration", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Tuition = details %>% filter(grepl("tuition", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Campus = details %>% filter(grepl("location", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    
    description = webPage %>% 
      html_nodes("section.full-description") %>% 
      html_nodes('div') %>% 
      html_text() %>% 
      paste(., collapse = " ") %>% 
      clean_tags()
    
    courses = webPage %>% 
      html_nodes("section.courses") %>% 
      html_nodes('table') %>% 
      html_table(fill = T)
    
    WIL = NA
    tryCatch({
      courses = courses %>% 
        bind_rows() %>% 
        set_names(c("Code", "Name")) %>%
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
       
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    tryCatch({
      Program_tuition = tibble(Tuition = Tuition,
                               Institution = Institution$institution_name,
                               Program = program_urls$Program[url],
                               Program_url = program_urls$url[url])
      
    }, error = function(e){cat("intl-tuition ERROR :", conditionMessage(e), "\n")})
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = description, WIL = WIL)
    
    
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
