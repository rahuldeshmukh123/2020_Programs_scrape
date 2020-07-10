#############################
##### Manitoba Institute of Trades and Technology
## Type of website: 

Institution <- CIConnexName("Manitoba Institute of Trades and Technology")

webPage = "https://mitt.ca/programs"

remDr$open()
remDr$navigate(webPage)

program_urls_list <- remDr$findElements(using = 'xpath', '/html/body/div[1]/div[2]/div[3]/div[2]/div/div[1]/div/section[1]/ul/li/a')
program_urls_list <- lapply(program_urls_list, function(x) {x$getElementAttribute("href")}) %>% unlist()
remDr$close()

program_urls <- tibble(Program = NA,
                       url = NA)
for(url in 1:length(program_urls_list)){
  webPage = read_webPage(program_urls_list[url])
  
  program_urls = program_urls %>% 
    bind_rows(tibble(Program = webPage %>% html_nodes("ul.resultsList.group") %>% html_nodes('a') %>% html_text(),
                     url = webPage %>% html_nodes("ul.resultsList.group") %>% html_nodes('a') %>% html_attr('href'))) %>% 
    filter(!is.na(Program))
}

program_urls <- program_urls %>% 
  mutate(url = paste0("https://mitt.ca", url))


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    Description = webPage %>% 
      html_nodes("section.group") %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    details = tibble(Category = webPage %>% 
                       html_nodes("div.atAGlanceGroup") %>% 
                       html_nodes("section.canadianStudentContent") %>% 
                       html_nodes('ul.atAGlanceContainer') %>% 
                       html_nodes('li') %>% 
                       html_text() %>% 
                       gsub("^\r\n[[:space:]]*|\r\n[[:space:]]*$", "", .)) %>% 
      separate(Category, c("Category", "Detail"), sep = "\r\n[[:space:]]*")
      
    
    Duration = get_detail_df(details, "Duration")
    Credential = get_detail_df(details, "Credential")
    
    Program_tuition = tibble(Tuition = get_detail_df(details, "tuition"),
                             Books = get_detail_df(details, "Books"),
                             Additional = get_detail_df(details, "additional student fees"),
                             Program = program_urls$Program[url],
                             Program_url = program_urls$url[url],
                             institution = Institution$institution_name,
                             prov_terr = Institution$prov_terr,
                             record_id = Institution$record_id)
    
    coursePage = read_webPage(paste0(program_urls$url[url],'/course-information'))
    
    WIL = get_details_from_node_table(get_detail_node_table(webPage %>% html_nodes('section.group') %>% html_children()), nodeType_filter = "div", nodeText_filter = "Work practicum")
    
    
    
    tryCatch({
      courses = tibble(Course = coursePage %>% html_nodes('ul.courseList') %>% html_nodes('li') %>% html_text() %>% gsub("^\r\n[[:space:]]*", "", .)) %>% 
        separate(Course, c("Name", "Description", "Code", "Hours", "Other"), sep = "\r\n[[:space:]]*") %>% 
        mutate(Code = gsub("Course Code:[[:space:]]*|[[:space:]]*Course Requirements.*$", "", Code)) %>% 
        select(Code, Name, Description)
      ## Start here - organize course data
      
      WIL = paste(WIL, course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F), sep = " AND ")
      if(WIL == " AND NA" ){
        WIL = NA
      }
      
      
    }, error = function(e){cat("No courses for", program_urls$Program[url], "\n")})
    
    
    tryCatch({
      
      Program_tuition = webPage %>% html_nodes('table.wp-block-table') %>% html_table(fill = T) %>% bind_rows()
      names(Program_tuition)[1] = "studentType"
      Program_tuition = Program_tuition %>% 
        mutate(Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               institution = Institution$institution_name,
               prov_terr = Institution$prov_terr,
               record_id = Institution$record_id)
      
      
    }, error = function(e){cat("No tuition for", program_urls$Program[url], "\n")})
    
    
    
    
    
    
    
    
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = NA, Duration = Duration, Description = Description, WIL = WIL)
    
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


save_out_standard_file(programs, Institution$institution_name, "Manitoba")
