#############################
##### Sheridan
## Type of website: 


Institution <- CIConnexName("Sheridan")
webPage <- read_webPage("https://academics.sheridancollege.ca/programs/")

program_urls = tibble(Program = webPage %>% 
                        html_nodes('div.program-list') %>% 
                        html_nodes('li') %>% 
                        html_nodes('a') %>% 
                        html_text(),
                      url = webPage %>% 
                        html_nodes('div.program-list') %>% 
                        html_nodes('li') %>% 
                        html_nodes('a') %>% 
                        html_attr('href') %>% 
                        paste0('https://academics.sheridancollege.ca', .))



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    Credential = webPage %>% 
      html_nodes("div.col-sm-12.plans-list") %>% 
      html_nodes('ul.plan-item') %>% 
      html_nodes('li.row') %>% 
      html_nodes("div.row") %>% 
      html_nodes('h4') %>% 
      html_text()
    
    details = webPage %>% 
      html_nodes("div.col-sm-12.plans-list") %>% 
      html_nodes('ul.plan-item') %>% 
      html_nodes('li.row') 
    
    programs_df_cred <- tibble(Credential = NA, Duration = NA, Campus = NA)
    tryCatch({
      for(cred in 1:length(Credential)){
        #print(cred)
        details_tbl = tibble(Detail = details[cred] %>% 
                               html_nodes("div.row") %>% 
                               html_nodes('ul.small.list-sep')%>% 
                               html_nodes('li') %>% 
                               html_text())%>% 
          mutate(Category = ifelse(grepl("time", Detail), "PTFT",
                                   ifelse(grepl("yrs|semester", Detail), "Duration",
                                          ifelse(grepl("Program code", Detail), "Program_Code", NA)))) %>% 
          bind_rows(tibble(Detail = details[cred] %>% 
                             html_nodes('table') %>% 
                             html_table(fill = T) %>% 
                             bind_rows() %>% 
                             pivot_longer(1:ncol(.)) %>% 
                             filter(grepl('campus', value, ignore.case = T)|grepl("location", name, ignore.case = T)) %>% 
                             select(value) %>% 
                             mutate(value = clean_tags(value)) %>% 
                             distinct() %>% 
                             unlist() %>% 
                             paste(., collapse = ' AND '),
                           Category = "Campus")) %>% 
          select(Category, Detail) %>% 
          bind_rows(tibble(Category = "Credential",
                           Detail = Credential[cred]))
        
        programs_df_cred <- programs_df_cred %>% 
          bind_rows(tibble(Credential = get_detail_df(details_tbl, "Credential"),
                           Duration = get_detail_df(details_tbl, "Duration", separation = "|"),
                           Campus = get_detail_df(details_tbl, "Campus"))) %>% 
          filter(!is.na(Credential)) %>% 
          group_by(Credential) %>% 
          summarize(Duration = paste(unique(Duration), collapse = " AND "),
                    Campus = paste(unique(Campus), collapse = " AND ")) %>% 
          mutate(Duration = gsub(" AND $", "", Duration),
                 Campus = gsub(" AND $", "", Campus))
        
        
      }
    }, error = function(e){cat("details ERROR :", conditionMessage(e), "\n")})

    
    
    Description = webPage %>% 
      html_nodes("div.col-sm-9") %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
  
    
    
    
    
    
    
    coursesPage = read_webPage(paste0(program_urls$url[url], "/courses"))
    
    WIL = NA
    tryCatch({
      
      courses = coursesPage %>% 
        html_nodes(clean_css_space("div.col-xs-12 col-sm-9 main")) %>% 
        html_nodes("table") %>% 
        html_table(fill = T) %>% 
        bind_rows()
      
      courses = courses[1:2] %>% 
        set_names(c("Code", "Name"))
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    tuitionPage = read_webPage(paste0(program_urls$url[url], "/financial/domestic"))
    
    
    tryCatch({
      
      Program_tuition = tibble(Credential = tuitionPage %>% 
                                 html_nodes("div.col-sm-12.plans-list") %>% 
                                 html_nodes('ul.plan-item') %>% 
                                 html_nodes('li.row') %>% 
                                 html_nodes("div.row") %>% 
                                 html_nodes('h4') %>% 
                                 html_text(),
                               Tuition = tuitionPage %>% 
                                 html_nodes("div.col-sm-12.plans-list") %>% 
                                 html_nodes('ul.plan-item') %>% 
                                 html_nodes('li.row') %>% 
                                 html_nodes("div.row") %>% 
                                 html_nodes("div.col-xs-12.col-sm-4.text-sm-right") %>% 
                                 html_text() %>% 
                                 clean_tags()) %>% 
        group_by(Credential) %>% 
        summarize(Tuition = paste(unique(Tuition), collapse = " AND ")) %>% 
        mutate(Tuition = gsub(" AND $", "", Tuition)) %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      

      }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
      
      
  
    for(program_iteration in 1:nrow(programs_df_cred)){
      program_df_iteration = create_program_df(Institution, url = paste(program_urls$url[url], program_iteration, sep = "_"), Program = program_urls$Program[url], Credential = programs_df_cred$Credential[program_iteration], Campus = programs_df_cred$Campus[program_iteration], Duration = programs_df_cred$Duration[program_iteration], Description = Description, WIL = WIL)
    
      if(program_iteration == 1){
        program_df = program_df_iteration
      } else {
        program_df = program_df %>% 
          bind_rows(program_df_iteration)
      }
    }
    
     
    
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
