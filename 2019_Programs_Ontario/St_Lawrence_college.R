#############################
##### St. Lawrence
## Type of website: Some structure, hinges on direct connections


Institution <- CIConnexName("St. Lawrence")
webPage <- read_webPage("https://www.stlawrencecollege.ca/programs-and-courses/full-time/program-list/")


program_urls <- tibble(url = webPage %>% 
                         html_nodes('div.bodyCopy') %>% 
                         html_nodes('a') %>% 
                         html_attr('href') %>% 
                         unique() %>% 
                         paste0("https://www.stlawrencecollege.ca", .))


if(exists("Program_contacts_full")){
  remove(Program_contacts_full)
}
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    overall_url = gsub("([[:alpha:]]|-)*(/$)", "", program_urls$url[url])
    
    Program = webPage %>% html_nodes("div.innerTitle") %>% html_nodes("h2") %>% html_text() %>% clean_tags() %>% str_trim()
    Description = webPage %>% html_nodes("div.innerText") %>% html_nodes("p") %>% html_text() %>% clean_tags() %>% paste(., collapse = " ")
    
    detail_categories = webPage %>% html_nodes(clean_css_space("div.borderedComponent paddedComponent paddedComponentRight")) %>% html_nodes("div.innerText.printTwoColumns") %>% html_nodes('h4') %>% html_text() %>% gsub("\\(|\\)","", .)
    details = webPage %>% html_nodes(clean_css_space("div.borderedComponent paddedComponent paddedComponentRight")) %>% html_nodes("div.innerText.printTwoColumns") %>% html_text() %>% gsub("^\r\n[[:space:]]*|\\(|\\)", "", .) %>% gsub(paste0("(", paste0(detail_categories, collapse = "|"), ")"), "\\1\\|", .) %>% gsub("([a-z])([A-Z])", "\\1\r\n   \\2", .) %>% 
      strsplit("\\|[[:space:]]*\r\n") %>% unlist() %>% gsub("^[[:space:]]*", "", .) %>% paste(., collapse = "|") %>% str_split("\r\n[[:space:]]{1,}") %>% unlist()
    details = tibble(Category = details) %>% 
     separate(Category, c("Category", "Detail"), sep = "\\|")
    
    
    Credential = get_detail_df(details, "Credential", " AND ")
    Duration = get_detail_df(details, "Duration", " AND ")
    Campus = get_detail_df(details, "Campus", " AND ")
    
    courses = webPage %>% 
      html_nodes("div.innerText") %>% 
      html_nodes("div.programOutline.printTwoColumns") %>% 
      html_nodes('a') %>%
      html_attr('href') %>% 
      paste0('https://www.stlawrencecollege.ca', .)

    
    previous_program = programs$Program[ncol(programs)]
    WIL = NA
    if(Program != previous_program) {
      tryCatch({
        course_df = tibble(Code = NA, Name = NA)
        print("Working on courses")
        for(node in 1:length(courses)){
          coursePage = read_webPage(courses[node])
          coursePage = get_detail_id(coursePage %>% html_nodes('div'), id ="pageContent" )
          Code = coursePage %>% html_nodes('div.bodyCopy') %>% html_nodes("p") %>% html_nodes("strong") %>% html_text()
          Code = Code[1]
          Name = coursePage %>% html_nodes("div.page-header") %>% html_nodes('h1') %>% html_text() %>% clean_tags()
          course_page_df = tibble(Code = Code,
                                  Name = Name)
          
          course_df <- course_df %>% 
            bind_rows(course_page_df) %>% 
            filter(!is.na(Name))
          
        }
        
        
        WIL = course_eval(course_df, Institution$institution_name, Program = Program, overall_url, noDescription = T)
        
      }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
      
    } else {
      print("Captured courses previously")
    }
    
    
    
    
    tryCatch({
      
      Program_contact_div_id = webPage %>% html_nodes("div.innerText.printTwoColumns") %>% html_text()
      Program_contact_div = webPage %>% html_nodes("div.innerText.printTwoColumns")
      Program_contact_div = Program_contact_div[grepl("Program Contact", Program_contact_div_id)]
      
      
      Program_contact = tibble(detail = Program_contact_div %>% html_text() %>% clean_tags()) %>% 
        mutate(Institution = Institution$institution_name,
               Program = Program,
               Program_url = program_urls$url[url])
      
      if(exists("Program_contacts_full")){
        Program_contacts_full = Program_contacts_full %>% 
          bind_rows(Program_contact)
      } else {
        Program_contacts_full = Program_contact
      }
      
    }, error = function(e){cat("contact ERROR :", conditionMessage(e), "\n")})
    
    
    
    
    
    
    #PDF tuition fees
    
    
    program_df = create_program_df(Institution, 
                                   url = overall_url, 
                                   Program = Program, 
                                   Credential = Credential, 
                                   Campus = Campus, 
                                   Duration = Duration, 
                                   Description = Description, 
                                   WIL = WIL)
    
    
    
    
    
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


programs <- programs  %>% 
  group_by(url, Program)  %>% 
  summarize(institution_name = Institution$institution_name,
            prov_terr = Institution$prov_terr,
            record_id = Institution$record_id,
            member = Institution$member,
            Credential = paste(unique(Credential), collapse = " AND "),
            Campus = paste(unique(Campus), collapse = " AND "),
            Duration = paste(unique(Duration), collapse = " AND "),
            Description = paste(unique(Description), collapse = " "),
            WIL = paste(unique(WIL), collapse = " AND ")) %>% 
  mutate(Duration = gsub(" AND $", "", Duration),
         Campus = gsub(" AND $", "", Campus),
         Credential = gsub(" AND $", "", Credential),
         WIL = gsub(" AND $|NA AND NA", "", WIL)) %>% 
  ungroup()

save_out_standard_file(programs, Institution$institution_name, "Ontario")
write_csv(Program_contacts_full, paste0("programs/Ontario/", clean_string(Institution$institution_name), "_program_contacts.csv"))
