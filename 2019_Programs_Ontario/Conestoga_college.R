#############################
##### Conestoga
## Type of website:


Institution <- CIConnexName("Conestoga")

#### FT programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/fulltime/programalphalisting.jsp#A")

program_urls <- webPage %>% 
  html_nodes("table.full.hover.program_listing") %>% 
  html_table(fill = T) %>% 
  map(~ mutate(., across(where(is.integer), as.character))) %>% 
  bind_rows() %>% 
  mutate(url = webPage %>% 
           html_nodes("table.full.hover.program_listing") %>%
           html_nodes("td") %>% 
           html_nodes("a") %>% 
           html_attr('href') %>% 
           paste0("https://www.conestogac.on.ca",.)) %>% 
  set_names(c("Program", "Duration", "Credential", "Campus", "url"))

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    
    webPage = read_webPage(program_urls$url[url])
    
    description = webPage %>% 
      html_nodes("div.maincol.programinfo") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    Program_tuition = webPage %>% 
      html_nodes("table.full.program-fees") %>% 
      html_table(fill = T)
    
    tryCatch({
      Program_tuition = Program_tuition %>% 
        bind_rows() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = webPage %>% 
      html_nodes("table.full.coursedata") %>% 
      html_table(fill = T)
    
    tryCatch({
      courses = courses %>%
        bind_rows() %>% 
        set_names(c("Code", "Name")) %>% 
        separate(Name, c("Name", "Description"), sep = "[[:space:]]{2,}")

      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = description, WIL = WIL)
    
    
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




#### Accelerated programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/fulltime/acceleratedprograms")



program_urls <- webPage %>% 
  html_nodes("table.full.hover.program_listing") %>% 
  html_table(fill = T) %>% 
  map(~ mutate(., across(where(is.integer), as.character))) %>% 
  bind_rows() %>% 
  mutate(url = webPage %>% 
           html_nodes("table.full.hover.program_listing") %>%
           html_nodes("td") %>% 
           html_nodes("a") %>% 
           html_attr('href')) %>% 
  set_names(c("Program", "Duration", "Credential", "Start_date", "Campus", "url"))

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    #remove_old_programs(url)
    
    
    webPage = read_webPage(program_urls$url[url])
    
    description = webPage %>% 
      html_nodes("div.maincol.programinfo") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    Program_tuition = webPage %>% 
      html_nodes("table.full.program-fees") %>% 
      html_table(fill = T)
    
    tryCatch({
      Program_tuition = Program_tuition %>% 
        bind_rows() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = webPage %>% 
      html_nodes("table.full.coursedata") %>% 
      html_table(fill = T)
    
    tryCatch({
      courses = courses %>%
        bind_rows() %>% 
        set_names(c("Code", "Name")) %>% 
        separate(Name, c("Name", "Description"), sep = "[[:space:]]{2,}")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = description, WIL = WIL)
    
    
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




#### Co-op programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/fulltime/programsearchresults.jsp?SearchKeyword=co-op")



program_urls <- webPage %>% 
  html_nodes("table.full.hover.program_listing") %>% 
  html_table(fill = T) %>% 
  map(~ mutate(., across(where(is.integer), as.character))) %>% 
  bind_rows() %>% 
  mutate(url = webPage %>% 
           html_nodes("table.full.hover.program_listing") %>%
           html_nodes("td") %>% 
           html_nodes("a") %>% 
           html_attr('href') %>% 
           paste0("https://www.conestogac.on.ca/", .)) %>% 
  set_names(c("Program", "Duration", "Credential", "Start_date", "Campus", "url"))

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    #remove_old_programs(url)
    
    
    webPage = read_webPage(program_urls$url[url])
    
    description = webPage %>% 
      html_nodes("div.maincol.programinfo") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    Program_tuition = webPage %>% 
      html_nodes("table.full.program-fees") %>% 
      html_table(fill = T)
    
    tryCatch({
      Program_tuition = Program_tuition %>% 
        bind_rows() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = webPage %>% 
      html_nodes("table.full.coursedata") %>% 
      html_table(fill = T)
    
    tryCatch({
      courses = courses %>%
        bind_rows() %>% 
        set_names(c("Code", "Name")) %>% 
        separate(Name, c("Name", "Description"), sep = "[[:space:]]{2,}")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      if(is.na(WIL)){
        WIL = "COOP"
      }
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    if(length(WIL) < 1){
      WIL = "COOP"
    
    }
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = description, WIL = WIL)
    
    
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




#### Graduate Cert programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/graduate-certificates/")



program_urls <- webPage %>% 
  html_nodes("table.full.hover.program_listing") %>% 
  html_table(fill = T) %>% 
  map(~ mutate(., across(where(is.integer), as.character))) %>% 
  bind_rows() %>% 
  mutate(url = webPage %>% 
           html_nodes("table.full.hover.program_listing") %>%
           html_nodes("td") %>% 
           html_nodes("a") %>% 
           html_attr('href') %>% 
           paste0("https://www.conestogac.on.ca/", .)) %>% 
  set_names(c("Program", "Duration", "Credential", "Campus", "url"))

for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    #remove_old_programs(url)
    
    
    webPage = read_webPage(program_urls$url[url])
    
    description = webPage %>% 
      html_nodes("div.maincol.programinfo") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    Program_tuition = webPage %>% 
      html_nodes("table.full.program-fees") %>% 
      html_table(fill = T)
    
    tryCatch({
      Program_tuition = Program_tuition %>% 
        bind_rows() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = webPage %>% 
      html_nodes("table.full.coursedata") %>% 
      html_table(fill = T)
    
    tryCatch({
      courses = courses %>%
        bind_rows() %>% 
        set_names(c("Code", "Name")) %>% 
        separate(Name, c("Name", "Description"), sep = "[[:space:]]{2,}")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      if(is.na(WIL)){
        WIL = "COOP"
      }
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    if(length(WIL) < 1){
      WIL = "COOP"
      
    }
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = program_urls$Credential[url], Campus = program_urls$Campus[url], Duration = program_urls$Duration[url], Description = description, WIL = WIL)
    
    
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








#### Degree programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/degrees/")



program_urls <- webPage %>% 
  html_nodes("table.full.hover.program_listing") %>% 
  html_table(fill = T) %>% 
  map(~ mutate(., across(where(is.integer), as.character))) %>% 
  bind_rows() %>% 
  filter(Campus != "Campus") %>% 
  mutate(url = webPage %>% 
           html_nodes("table.full.hover.program_listing") %>%
           html_nodes("td") %>% 
           html_nodes("a") %>% 
           html_attr('href') %>% 
           paste0("https://www.conestogac.on.ca/", .)) %>% 
  set_names(c("Program", "Campus", "url"))


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    #remove_old_programs(url)
    
    
    webPage = read_webPage(program_urls$url[url])
    
    details = webPage %>% 
      html_nodes("dl.programdetails")
    
    details = tibble(category = details %>% html_nodes("dt") %>% html_text(),
                     detail = details %>% html_nodes("dd") %>% html_text())
    
    
    credential = details %>% filter(grepl("credential", category, ignore.case = T)) %>% select(detail) %>% unlist()
    campus = details %>% filter(grepl("campus", category, ignore.case = T)) %>% select(detail) %>% unlist()
    
    
    description = webPage %>% 
      html_nodes("div.maincol.programinfo") %>% 
      html_text() %>% 
      paste(., collapse = " ")
    
    Program_tuition = webPage %>% 
      html_nodes("table.full.program-fees") %>% 
      html_table(fill = T)
    
    tryCatch({
      Program_tuition = Program_tuition %>% 
        bind_rows() %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    courses = webPage %>% 
      html_nodes("table.full.coursedata") %>% 
      html_table(fill = T)
    
    tryCatch({
      courses = courses %>%
        bind_rows() %>% 
        set_names(c("Code", "Name")) %>% 
        separate(Name, c("Name", "Description"), sep = "[[:space:]]{2,}")
      
      WIL = course_eval(courses, Institution$institution_name, program_urls$Program[url], program_urls$url[url], noDescription = F)
      
      if(is.na(WIL)){
        WIL = "COOP"
      }
      
    }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
    
    if(length(WIL) < 1){
      WIL = "COOP"
      
    }
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = credential, Campus = campus, Duration = NA, Description = description, WIL = WIL)
    
    
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



#### Corporate programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/corporate-training/")

program_urls = webPage %>% 
  html_nodes("div.maincol-wide") %>% 
  html_nodes("a") %>% 
  html_attr("href")

for(url in 1:length(program_urls)){
  print(url)
  tryCatch({
    webPage <- read_webPage(program_urls[[url]])
    
    program_urls_programs = webPage %>% 
      html_nodes("div.maincol") %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    for(url in 1:length(program_urls_programs)){
      tryCatch({
        webPage <- read_webPage(program_urls_programs[[url]])
        description = webPage %>% 
          html_nodes("div.maincol-wide") %>% 
          html_nodes("p") %>% 
          html_text() %>% 
          paste(., collapse = " ")
        
        Program = webPage %>% 
          html_nodes("div.maincol-wide") %>% 
          html_node("h1") %>% 
          html_text()
        
        program_df = create_program_df(Institution, url = program_urls_programs[[url]], Program = Program, Credential = "Corporate training", Campus = NA, Duration = NA, Description = description, WIL = NA)
        
        
        
        if(!exists("programs")){
          programs = program_df
        } else {
          programs = programs %>% bind_rows(program_df)
        }
        
        
      }, error = function(e){cat("suburl error ERROR :", conditionMessage(e), "\n")})
    }
    
  
  }, error = function(e){cat("Overall error ERROR :", conditionMessage(e), "\n")})
}

#### Apprenticeships programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/trades/")

program_urls = webPage %>% 
  html_nodes("div.button.left") %>% 
  html_nodes("a") %>% 
  html_attr("href")

program_urls = program_urls[grepl("subsector", program_urls)]

for(url in 1:length(program_urls)){
  webPage = read_webPage(program_urls[[url]])
  
  program_urls_programs <- webPage %>% 
    html_nodes("table.full") %>% 
    html_table(fill = T) %>% 
    map(~ mutate(., across(where(is.integer), as.character))) %>% 
    map(~ set_names(., gsub("Industrial Programs", "Program Title", names(.)))) %>% 
    bind_rows() %>%
    filter(`Program Title` != `Credential`) %>% 
    #bind_rows(webPage %>% 
    #            html_nodes("table.full.hover.program_listing") %>% 
    #            html_table(fill = T) %>% 
    #            map(~ mutate(., across(where(is.integer), as.character))) %>% 
    #            bind_rows()) %>% 
    mutate(url = webPage %>% 
             html_nodes("table.full") %>%
             html_nodes("td") %>% 
             html_nodes("a") %>% 
             html_attr("href") %>% 
             paste0("https://www.conestogac.on.ca", .))

    
  
  
  
  for(url in 1:nrow(program_urls_programs)){
    print(url)
    
    tryCatch({
      #remove_old_programs(url)
      
      
      webPage = read_webPage(program_urls_programs$url[url])
      
      details = webPage %>% 
        html_nodes("dl.programdetails")
      
      details = tibble(category = details %>% html_nodes("dt") %>% html_text(),
                       detail = details %>% html_nodes("dd") %>% html_text())
      
      
      credential = details %>% filter(grepl("credential", category, ignore.case = T)) %>% select(detail) %>% unlist() %>% paste(., "- Apprenticeship")
      campus = details %>% filter(grepl("campus", category, ignore.case = T)) %>% select(detail) %>% unlist()
      
      
      description = webPage %>% 
        html_nodes("div.maincol.programinfo") %>% 
        html_text() %>% 
        paste(., collapse = " ")
      
      Program_tuition = webPage %>% 
        html_nodes("table.full.program-fees") %>% 
        html_table(fill = T)
      
      tryCatch({
        Program_tuition = Program_tuition %>% 
          bind_rows() %>% 
          mutate(Institution = Institution$institution_name,
                 Program = program_urls_programs$Program[url],
                 Program_url = program_urls_programs$url[url])
      }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
      
      courses = webPage %>% 
        html_nodes("table.full.coursedata") %>% 
        html_table(fill = T)
      
      tryCatch({
        courses = courses %>%
          bind_rows() %>% 
          set_names(c("Code", "Name")) %>% 
          separate(Name, c("Name", "Description"), sep = "[[:space:]]{2,}")
        
        WIL = course_eval(courses, Institution$institution_name, program_urls_programs$Program[url], program_urls_programs$url[url], noDescription = F)
        
        if(is.na(WIL)){
          WIL = "COOP"
        }
        
      }, error = function(e){cat("courses ERROR :", conditionMessage(e), "\n")})
      
      if(length(WIL) < 1){
        WIL = "COOP"
        
      }
      
      program_df = create_program_df(Institution, url = program_urls_programs$url[url], Program = program_urls_programs$Program[url], Credential = credential, Campus = campus, Duration = NA, Description = description, WIL = WIL)
      
      
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
  
  
}




#### FAST programs ####
webPage <- read_webPage("https://www.conestogac.on.ca/fast-programs/")

program_urls = tibble(Program = webPage %>% 
                        html_nodes("div.full") %>% 
                        html_nodes('div') %>% 
                        html_nodes("h2") %>% 
                        html_text(),
                      Description = webPage %>% 
                        html_nodes("div.full") %>% 
                        html_nodes('div') %>% 
                        html_nodes("div.panel.accordian") %>% 
                        html_text(),
                      url = webPage %>% 
                        html_nodes("div.full") %>% 
                        html_nodes('div') %>% 
                        html_nodes("div.panel.accordian") %>% 
                        html_nodes("a") %>% 
                        html_attr("href"))
  
program_df = create_program_df(Institution, url = program_urls$url, Program = program_urls$Program, Credential = NA, Campus = NA, Duration = NA, Description = program_urls$Description, WIL = NA)


if(!exists("programs")){
  programs = program_df
} else {
  programs = programs %>% bind_rows(program_df)
}
#### Continuing ed programs ####
webPage <- read_webPage("https://continuing-education.conestogac.on.ca/programs")

program_urls = tibble(url = webPage %>% 
                        html_nodes("a.btn.btn-light.btn-block") %>% 
                        html_attr("href") %>% 
                        paste0("https://continuing-education.conestogac.on.ca/", .),
                      Program = webPage %>% 
                        html_nodes("a.btn.btn-light.btn-block") %>% 
                        html_nodes("span.d-block.text-truncate") %>% 
                        html_text())



for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    webPage <- read_webPage(program_urls$url[url])
    
    credential = webPage %>% 
      html_nodes("div.hero-banner") %>% 
      html_nodes("span.text-white.text-shadow.text-uppercase") %>% 
      html_text() %>% 
      gsub("^[[:space:]]*|[[:space:]]*$", "", .) %>% 
      paste(., "- continuing education")
    
    description = webPage %>% 
      html_nodes('div.mr-xl-3') %>% 
      html_text()
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = credential, Campus = NA, Duration = NA, Description = description, WIL = NA)
    
    
    if(!exists("programs")){
      programs = program_df
    } else {
      programs = programs %>% bind_rows(program_df)
    }
    
  }, error = function(e){cat("Overall error ERROR :", conditionMessage(e), "\n")})
}    




#######################


programs = programs %>% 
  distinct()

save_out_standard_file(programs, Institution$institution_name, "Ontario")
