#############################
##### St. Clair
## Type of website: 


Institution <- CIConnexName("St. Clair")
webPage <- read_webPage("https://www.stclaircollege.ca/programs")

program_urls <- tibble(Program = webPage %>% 
                         html_nodes("div.program") %>% 
                         html_nodes("a") %>% 
                         html_text() %>% 
                         clean_tags(),
                       url = webPage %>% 
                         html_nodes("div.program") %>% 
                         html_nodes("a") %>% 
                         html_attr('href') %>% 
                         paste0('https://www.stclaircollege.ca', .))




for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    details = webPage %>% 
      html_nodes("div.view-content")
    
    #Grabbing some details for potential future use
    Topic = details %>% html_nodes("div.progInfo--details__school") %>% html_text() %>% clean_tags() %>% paste(., collapse = " AND ")
    New = details %>% html_nodes("div.progInfo--details__new") %>% html_text() %>% clean_tags() %>% paste(., collapse = " AND ")
    Start_month = details %>% html_nodes("div.progInfo--details__start") %>% html_text() %>% clean_tags() %>% paste(., collapse = " AND ")
    Program_code = details %>% html_nodes("div.progInfo--group") %>% html_nodes('div.progInfo--details__code') %>% html_text() %>% clean_tags() %>% paste(., collapse = " AND ")
    Program_status = details %>% html_nodes("div.progInfo--group") %>% html_nodes('div.progInfo--details__status') %>% html_text() %>% clean_tags() %>% paste(., collapse = " AND ")
    Description = webPage %>% 
      html_nodes(clean_css_space("div.clearfix text-formatted field field--name-field-prog-overview field--type-text-with-summary field--label-hidden field__item")) %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    tryCatch({
      Program_contact = tibble(Name = details %>% html_nodes("div.progInfo--details__contact") %>% html_nodes(clean_css_space('div.field field--name-field-contact-name field--type-string field--label-hidden field__item')) %>% html_text() %>% clean_tags() %>% paste(., collapse = " "),
                               Number = details %>% html_nodes("div.progInfo--details__contact") %>% html_nodes(clean_css_space('div.field field--name-field-contact-number field--type-telephone field--label-hidden field__item')) %>% html_text() %>% clean_tags() %>% paste(., collapse = " "),
                               Email = details %>% html_nodes("div.progInfo--details__contact") %>% html_nodes(clean_css_space('div.contact--email')) %>% html_text() %>% clean_tags() %>% paste(., collapse = " ")) %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
      if(exists("Program_contacts_full")){
        Program_contacts_full = Program_contacts_full %>% 
          bind_rows(Program_contact)
      } else {
        Program_contacts_full = Program_contact
      }
      
    }, error = function(e){cat("contact ERROR :", conditionMessage(e), "\n")})
    
    
   
    
    
    #Currently only using the below
    Duration = details %>% html_nodes("div.progInfo--details__length") %>% html_text() %>% clean_tags() %>% paste(., collapse = " AND ")
    Credential = gsub("^.*- |[[:space:]]*$", "", Duration)
    Duration = gsub("(^.*)( - .*$)", "\\1", Duration)
    Campus = details %>% html_nodes("div.progInfo--group") %>% html_nodes('div.progInfo--details__campus') %>% html_text() %>% clean_tags() %>% paste(., collapse = " AND ")
    
    
    courses = webPage %>% 
      html_nodes(clean_css_space("table.table table-responsive-sm table--courselistings")) %>% 
      html_table(fill = T)
    
    
    WIL = NA
    tryCatch({
      courses = courses %>% 
        plyr::rbind.fill()
      
      courses = courses[1:2] %>% 
        set_names(c("Code", "Name"))
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    
    
    #PDF tuition fees
    
    
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

#Starting to save out contact details
write_csv(Program_contacts_full, paste0("programs/Ontario/", clean_string(Institution$institution_name), "_program_contacts.csv"))
