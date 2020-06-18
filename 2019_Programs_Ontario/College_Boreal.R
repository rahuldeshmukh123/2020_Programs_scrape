#############################
##### Collège Boréal
## Type of website:

Institution <- CIConnexName("Boréal")


########## Continuing education #############


webPage = read_webPage("http://continue.collegeboreal.ca/continuing-education/programs/")


program_nodes <- webPage %>% 
  html_nodes(gsub(" ", "\\.", "dl")) %>% 
  html_text()

program_urls <- tibble(program_urls = program_nodes) %>% 
  separate(program_urls, c("Program", "Code"), sep = "[[:space:]]{2,}") %>% 
  filter(Code != "Code") %>% 
  mutate(Code = stringr::str_trim(Code)) %>% 
  filter(nchar(Code) < 6) %>% 
  mutate(url = webPage %>% 
           html_nodes(gsub(" ", "\\.", "dl")) %>% 
           html_nodes("a.details-programme") %>% 
           html_attr("href"))
  
program_urls$WIL <- NA
remove(details)
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    details = webPage %>% 
      html_nodes("dl.titre-competence") %>% 
      html_text()
    
    details = tibble(details = details) %>% 
      separate(details, c("category","detail", "category2", "detail2"), sep = "[[:space:]]{2,}" ) %>% 
      pivot_longer(cols = 1:4) %>% 
      mutate(name = gsub("2", "", name),
             id = rep(1:(nrow(.)/2), each = 2)) %>% 
      pivot_wider(names_from = name) %>% 
      select(-id)
      
    credential = details %>% filter(grepl("credential", category, ignore.case = T)) %>% select(detail) %>% unlist()
    campus = details %>% filter(grepl("location", category, ignore.case = T)) %>% select(detail) %>% unlist()
    online = details %>% filter(grepl("training mod", category, ignore.case = T)) %>% select(detail) %>% unlist()  

    description = webPage %>% 
      html_nodes("div.large-8.medium-8.columns.first") %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    courses = webPage %>% 
      html_nodes("div.instances") %>% 
      html_nodes("table") %>% 
      html_table() %>% 
      bind_rows()
    
    tryCatch({
      
      courses = courses[1] %>% 
        separate(1, c("Code", "Name", "Description"), sep = "[[:space:]]{2,}")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
    }, error = function(e){cat("Courses ERROR :", conditionMessage(e), "\n")})
    
    

      
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = credential, Campus = campus, Duration = NA, Description = description, WIL = WIL)
    
    
    
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


###### programs for newcomers #############


webPage = read_webPage("https://www.collegeboreal.ca/futurs-etudiants/informations-par-profil/je-suis-nouvel-arrivant")


program_nodes <- webPage %>% 
  html_nodes("div.accordion__item") 

program_urls = tibble(Program = program_nodes %>%
                        html_nodes("button") %>% 
                        html_text(),
                      Description = program_nodes %>% 
                        html_nodes("div.accordion__content") %>% 
                        html_text(),
                      Campus = NA,
                      Credential = NA,
                      Tuition = NA,
                      Duration = NA,
                      url = NA)

details = program_nodes %>% 
  html_nodes("div.accordion__content") 


for(prog in 1:nrow(program_urls)){
  Campus_prog = details[prog] %>% 
    html_nodes("p") %>% 
    html_text()
  
  url_prog = details[prog] %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  details_prog = details[prog] %>% 
    html_nodes("li") %>% 
    html_text()
  
  Campus_prog = Campus_prog[grepl("Campus", Campus_prog, ignore.case = T)]
  duration_prog = details_prog[grepl("Durée", details_prog, ignore.case = T)]
  credential = details_prog[grepl("titre de comp", details_prog, ignore.case = T)]
  tuition_prog = details_prog[grepl("frais de scolari", details_prog, ignore.case = T)]
  
  
  tryCatch({
    program_urls$Campus[prog] = Campus_prog %>% paste(., collapse = " ")
  }, error = function(e){cat("Campus error ERROR :", conditionMessage(e), "\n")})
  tryCatch({
    program_urls$Duration[prog] = duration_prog %>% paste(., collapse = " ")
  }, error = function(e){cat("Duration error ERROR :", conditionMessage(e), "\n")})
  tryCatch({
    program_urls$Credential[prog] = credential %>% paste(., collapse = " ")
  }, error = function(e){cat("Credential error ERROR :", conditionMessage(e), "\n")})
  tryCatch({
    program_urls$Tuition[prog] = tuition_prog %>% paste(., collapse = " ")
  }, error = function(e){cat("Tuition error ERROR :", conditionMessage(e), "\n")})
  tryCatch({
    program_urls$url[prog] = url_prog
  }, error = function(e){cat("Tuition error ERROR :", conditionMessage(e), "\n")})
}

#Removing TEF and TEF details
program_urls = program_urls[1:(grep("TEF", program_urls$Program)-1),]

program_df = create_program_df(Institution, url = program_urls$url, Program = program_urls$Program, Credential = program_urls$Credential, Campus = program_urls$Campus, Duration = program_urls$Duration, Description = program_urls$Description, WIL = NA)

programs = programs %>% 
  bind_rows(program_df)


  ################ Programs ##############

webPage = read_webPage("https://www.collegeboreal.ca/")


program_urls = webPage %>% 
  html_nodes(xpath = '/html/body/div[1]/header/div/nav/div[2]/ul[1]/li[1]/ul') %>% 
  html_nodes("li.panel-content__item") %>% 
  html_nodes("a") %>% 
  html_attr("href")

for(url in 1:length(program_urls)){
  print(url)
  webPage = read_webPage(paste0("https://www.collegeboreal.ca/", program_urls[[url]]))
  program_web = webPage %>% 
    html_nodes("ul.choice-card-menu__list") %>% 
    html_nodes("li")
  
  tryCatch({
    program_urls_detail = tibble(
      campus = program_web %>% 
        html_attr("data-campus"),
      
      credential = program_web %>% 
        html_attr("data-credentials"),
      
      mode = program_web %>% 
        html_attr("data-mode"),
      
      program_url = program_web %>% 
        html_nodes("a") %>% 
        html_attr("href"),
      
      Program = program_web %>% 
        html_nodes("a") %>% 
        html_text()
      
    )
    
  
  if(!exists("program_urls_all")){
    program_urls_all = program_urls_detail
  } else {
    program_urls_all = program_urls_all %>% bind_rows(program_urls_detail)
  }
    
    if(url == length(program_urls)) {
      program_urls = program_urls_all
      remove(program_urls_all)
      remove(program_urls_detail)
    } 
    
  }, error = function(e){cat("ERROR :", url, "\n")})  
  
  
}

program_urls = program_urls %>% 
  rename(url = program_url) %>% 
  mutate(url = paste0("https://www.collegeboreal.ca/", url))

#32 previous programs

remove(details)
for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    
    webPage = read_webPage(program_urls$url[url])
    
    description = webPage %>% 
      html_nodes("div.content-text__main ") %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
  details = webPage %>% 
    html_nodes("div.to-note__item")
  
  program_details = tibble(category = rep(NA, length(details)),
                           detail = rep(NA, length(details)))
  
  for(item in 1:length(details)){
    
    category = details[item] %>% 
      html_nodes("strong") %>% 
      html_text()
    
    detail = details[item] %>% 
      html_nodes("div") %>% 
      html_text()

      tryCatch({
        program_details$category[item] = category %>% paste(., collapse = " ")
      }, error = function(e){cat("category error ERROR :", conditionMessage(e), "\n")})
      tryCatch({
        program_details$detail[item] = detail %>% paste(., collapse = " ")
      }, error = function(e){cat("detail error ERROR :", conditionMessage(e), "\n")})
    
    if(item == length(details)){
      details = program_details
      remove(program_details)
    }
      
    }
    
    
  credential = if(is.na(program_urls$credential[url]))   {details %>% filter(grepl("titre de comp", category, ignore.case = T)) %>% select(detail) %>% unlist()} else {program_urls$credential[url]}
  campus = if(is.na(program_urls$campus[url]))   {details %>% filter(grepl("campus", category, ignore.case = T)) %>% select(detail) %>% unlist()} else {program_urls$campus[url]}
  mode = if(is.na(program_urls$mode[url]))   {details %>% filter(grepl("mode", category, ignore.case = T)) %>% select(detail) %>% unlist()} else {program_urls$mode[url]}  
  duration = details %>% filter(grepl("DURÉE", category, ignore.case = T)) %>% select(detail) %>% unlist()
    
    tryCatch({
      
      courses = courses[1] %>% 
        separate(1, c("Code", "Name", "Description"), sep = "[[:space:]]{2,}")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
    }, error = function(e){cat("Courses ERROR :", conditionMessage(e), "\n")})
    
    
    courses = webPage %>% 
      html_nodes("table.program__course-table.accordion.sub_accordion_tab") %>% 
      html_table(fill = T)
    
    tryCatch({
      
      courses = courses %>% 
        bind_rows()
      
      courses = courses[1:2] %>% 
        set_names(c("Code", "Name")) %>% 
        separate(Name, c("Name", "Description"), sep = "[[:space:]]{2,}")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = T)
      
    }, error = function(e){cat("Courses ERROR :", conditionMessage(e), "\n")})
    
    
    tuition = webPage %>% 
      html_nodes("div.accordion__content-column") %>% 
      html_nodes("p") %>% 
      html_text()
    
    Program_tuition = tibble(Institution = Institution$institution_name,
                             Program = program_urls$Program[url],
                             Program_url = program_urls$url[url],
                             tuition = tuition)
    
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = credential, Campus = campus, Duration = duration, Description = description, WIL = WIL)
    
    
    
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


programs = programs %>% 
  filter(url != "https://www.collegeboreal.ca/")

save_out_standard_file(programs, Institution$institution_name, "Ontario")
