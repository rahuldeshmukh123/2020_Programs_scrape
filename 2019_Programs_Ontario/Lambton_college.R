#############################
##### Lambton
## Type of website:


Institution <- CIConnexName("Lambton")
webPage <- read_webPage("https://www.lambtoncollege.ca/custom/LambtonApps/Programs/ProgramListAZ.aspx")

program_urls <- tibble(Program = webPage %>% 
                         html_nodes(clean_css_space("div.col-xs-12 col-sm-9 pull-right")) %>% 
                         html_nodes("li") %>% 
                         html_text(),
                       url = webPage %>% 
                         html_nodes(clean_css_space("div.col-xs-12 col-sm-9 pull-right")) %>% 
                         html_nodes("li") %>% 
                         html_nodes("a") %>% 
                         html_attr("href") %>% 
                         paste0("https://www.lambtoncollege.ca", .))


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = webPage %>% 
      html_nodes(clean_css_space("div.col-md-4 info-items")) %>% 
      html_nodes("div.items-wrapper") %>% 
      html_text() %>% 
      str_split("\r\n[[:space:]]*") %>% 
      unlist()
    
    Campus = details[grepl("Location", details)] %>% paste(., collapse = " AND ")
    Credential = details[grepl("certificate|bachelor|degree|diploma", details, ignore.case = T)] %>% paste(., collapse = " AND ")
    Duration = details[grepl("year|semester", details, ignore.case = T)] %>% paste(., collapse = " AND ")
    
    Description = webPage %>% 
      html_nodes(clean_css_space("section.main-content col-md-8")) %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = ' ')
    
    Tuition = read_webPage(paste0(program_urls$url[url], "&Type=TF")) %>% 
      html_nodes("table.fees") %>% 
      html_table(fill = T)
    
    tryCatch({
      
    
      
      Program_tuition = Tuition %>% 
        bind_rows() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = read_webPage(paste0(program_urls$url[url], "&Type=CL")) %>% 
      html_nodes("div.row.program-page") %>% 
      html_nodes("div")
    div_ids = courses %>% 
      html_attr('id')
    
    courses = courses[grepl('courselist', div_ids, ignore.case = T)]
    
    WIL = NA
    tryCatch({
      
      courses = tibble(Code = courses %>% 
                         html_nodes("div.toggle-content") %>% 
                         html_nodes('h4') %>% 
                         html_text(),
                       Description = courses %>% 
                         html_nodes("div.toggle-content") %>% 
                         html_nodes('p') %>% 
                         html_text()) %>% 
        separate(Code, c("Code", "Name"), sep = ":[[:space:]]*")
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
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
