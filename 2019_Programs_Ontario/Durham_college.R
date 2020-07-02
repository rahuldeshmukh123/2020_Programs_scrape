#############################
##### Durham
## Type of website:


Institution <- CIConnexName("Durham")
webPage <- read_webPage('https://durhamcollege.ca/programs-and-courses')

program_urls <- webPage %>%
  html_nodes("div.twelve.columns.table-scroll") %>% 
  html_nodes("table") %>% 
  html_table(fill = T) %>% 
  bind_rows() %>% 
  select(1:4) %>% 
  set_names(c("Program", "Duration", "Credential", "Campus")) %>% 
  mutate(url = webPage %>%
           html_nodes("div.twelve.columns.table-scroll") %>%
           html_nodes("table") %>% 
           html_nodes("td") %>% 
           html_nodes("a") %>% 
           html_attr("href"))


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    description = webPage %>% 
      html_nodes("p.TEXTPROGRAMPAGES") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    courses = read_webPage(paste0(program_urls$url[url], "#tabCourses")) %>% 
      html_nodes("div.small-12.medium-6.columns.end") %>% 
      html_nodes("li") %>% 
      html_text()
    
    WIL = NA
    tryCatch({
      courses = tibble(Name = courses,
                       Code = NA) 
        
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    Program_tuition = read_webPage(paste0(program_urls$url[url], "#tabCosts")) %>% 
      html_nodes("table.program-costs-table.lines.table-design") %>% 
      html_table(fill = T) %>% 
      bind_rows()
    
    
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = description, WIL = WIL)
    
    
    if(!exists("programs")){
      programs = program_df
    } else {
      programs = programs %>% bind_rows(program_df)
    }
    tryCatch({
      if(!exists("Fees")){
        Fees = Program_tuition%>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls$Program[url],
                 Program_url = program_urls$url[url])
      } else {
        Fees = Fees %>% bind_rows(Program_tuition%>% 
                                    mutate(Institution = Institution$institution_name,
                                           Program = program_urls$Program[url],
                                           Program_url = program_urls$url[url]))
      }
    }, error = function(e){cat("Tuition ERROR :", conditionMessage(e), "\n")})
  }, error = function(e){cat("Overall error ERROR :", conditionMessage(e), "\n")})
}   


save_out_standard_file(programs, Institution$institution_name, "Ontario")
