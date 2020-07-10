#############################
##### louis riel
## Type of website: Direct links through css - shaky


Institution <- CIConnexName("louis riel")

webPage = read_webPage("https://www.lrsd.net/schools/ATC/ProgramsAndServices/Pages/default.aspx")


program_urls = tibble(Program = webPage %>% html_nodes("span.headertitle.headermarker") %>% html_nodes('a') %>% html_text(),
                      url = webPage %>% html_nodes("span.headertitle.headermarker") %>% html_nodes('a') %>% html_attr('href') %>% paste0('https://www.lrsd.net', .))

program_urls = program_urls[4:nrow(program_urls),]

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    
    Description = webPage %>% html_nodes("div.pageContent") %>% html_nodes('p') %>% html_text() %>% clean_tags() %>% paste(., collapse = " ")
    
    Credential = get_detail_node_table(webPage %>% html_nodes("div.pageContent") %>% html_nodes("div.ms-rtestate-field") %>% html_children() ) %>% 
      get_details_from_node_table(., "h2", "Certification")
    
    
   details = webPage %>% 
     html_nodes('table.ms-rteTable-1') %>% 
     html_table(fill = T)
   
   WIL = NA
   
    
    
    tryCatch({
      
      details = details %>% 
        bind_rows() %>% 
        set_names(c("Category", "Detail"))
      
      WIL = get_detail_df(details, category = "Work experience", separation = " AND ")
      
      
      
      Program_tuition = details %>% 
        mutate(Year_Semester = "Domestic_unstructured",
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               institution = Institution$institution_name,
               prov_terr = Institution$prov_terr,
               record_id = Institution$record_id)
      
      
    }, error = function(e){cat("No tuition for", program_urls$Program[url], "\n")})
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = NA, Duration = NA, Description = Description, WIL = WIL)
    
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
