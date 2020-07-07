#############################
##### La cite
## Type of website:


Institution <- CIConnexName("La cit")
webPage <- read_webPage("https://www.collegelacite.ca/programmes/")

program_urls = url = webPage %>% 
  html_nodes("div.programme-list") %>% 
  html_nodes("table.table.table-striped.table-hover") %>% 
  html_nodes("td") %>% 
  html_nodes("a") %>% 
  html_attr("href")

program_urls = program_urls[grepl('programmes', program_urls)]


program_urls <- webPage %>% 
  html_nodes("div.programme-list") %>% 
  html_nodes("table.table.table-striped.table-hover") %>% 
  html_table(fill = T) %>% 
  plyr::rbind.fill() %>% 
  select(c(Programme, Campus, `Frais\n\t\t\t\t\t\t\tpar étape`, Durée, `Stage COOP`)) %>% 
  rename(Tuition_per_semester = `Frais\n\t\t\t\t\t\t\tpar étape`,
         Duration = Durée,
         WIL = `Stage COOP`,
         Program = Programme) %>% 
  mutate(url = paste0("https://www.collegelacite.ca", program_urls))

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    Credential = NA
    tryCatch({
      categories = webPage %>% 
        html_nodes("section.general") %>% 
        html_nodes("span") %>% 
        html_text()
      
      categories = categories[!grepl("^[[:space:]]*$", categories)]
      
      details = webPage %>% 
        html_nodes("section.general") %>% 
        html_nodes("div") %>% 
        html_text() %>% 
        clean_tags() %>% 
        paste(., collapse = " ") %>% 
        str_split(paste(categories, collapse = "|")) %>% 
        unlist()
      
      details = tibble(categories = categories, 
                       details = details[2:length(details)])
      
      Credential = details %>% filter(grepl("titre d", categories, ignore.case = T)) %>% select(details) %>% unlist() %>% paste(., collapse = " ")
    
    }, error = function(e){cat("details ERROR :", conditionMessage(e), "\n")})
    
    
      
    courses = webPage %>% 
      html_nodes('table.cursus') %>% 
      html_table(fill = T)
    
    WIL = NA
    tryCatch({
      courses = courses %>% 
        bind_rows() %>% 
        select(starts_with('Cours')) %>% 
        rename(Code = Cours) %>% 
        group_by(Code) %>% 
        unite("Name", - Code) %>% 
        separate(Name, c("Name", "Description"), sep = "\n[[:space:]]*") %>% 
        mutate(Name = gsub("NA_", "", Name))
      
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    
    if(is.na(WIL)){
      WIL = program_urls$WIL[url]
    }
    
    fees = webPage %>% 
      html_nodes("table.frais") %>% 
      html_table(fill = T)
    
    Program_tuition = tibble(Institution = Institution$institution_name,
             Program = program_urls$Program[url],
             Program_url = program_urls$url[url],
             Tuition_per_semester = program_urls$Tuition_per_semester[url])
    
    tryCatch({
      
      fee_names = webPage %>% 
        html_nodes("section.frais") %>% 
        html_nodes("h3") %>% 
        html_text()
      
      fee_names = fee_names[1:length(fees)]
      names(fees) = fee_names
      
      Program_tuition = fees %>% 
        bind_rows(.id = "studentType") %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url],
               Tuition_per_semester = program_urls$Tuition_per_semester[url])
      
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = description, WIL = WIL)
    
    
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
