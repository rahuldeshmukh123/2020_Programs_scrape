#############################
##### First Nations Technical Institute
## Type of website:


Institution <- CIConnexName("First Nations Technical Institute")
ind_deg = read_webPage('https://fnti.net/standalone-indigenous-degrees')
college = read_webPage('https://fnti.net/college-programs.php')
univ_prog = read_webPage('https://fnti.net/university-3')



ind_deg = tibble(Program = ind_deg %>% 
                   html_nodes("div.page") %>% 
                   html_nodes("table") %>% 
                   html_nodes("a") %>% 
                   html_text(),
                 url = ind_deg %>% 
                   html_nodes("div.page") %>% 
                   html_nodes("table") %>% 
                   html_nodes("a") %>% 
                   html_attr('href')) %>% 
  mutate(Credential = "Indigenous degree")
  

college = tibble(Program = college %>% 
                   html_nodes(xpath = '//*[@id="content"]/div/table') %>% 
                   html_nodes("a") %>% 
                   html_text(),
                 url = college %>% 
                   html_nodes(xpath = '//*[@id="content"]/div/table') %>% 
                   html_nodes("a") %>% 
                   html_attr('href')) %>% 
  mutate(Credential = "College")

univ_prog = tibble(Program = univ_prog %>% 
                   html_nodes(xpath = '//*[@id="content"]/table') %>% 
                   html_nodes("a") %>% 
                   html_text(),
                 url = univ_prog %>% 
                   html_nodes(xpath = '//*[@id="content"]/table') %>% 
                   html_nodes("a") %>% 
                   html_attr('href')) %>% 
  mutate(Credential = "University program")

program_urls <- ind_deg %>% 
  bind_rows(college) %>% 
  bind_rows(univ_prog)


for(url in 1:nrow(program_urls)){
  print(url)
  tryCatch({
    remove_old_programs(url)
    
    webPage = read_webPage(program_urls$url[url])
    
    ### Getting details by indexing by html tag "id"
    ### First need to get the inner container to get the ids for the content
    
    divisions = webPage %>% 
      html_nodes("div.page") %>% 
      html_nodes("div")
    
    ids = divisions %>% 
      html_attr('id')
    
    content = divisions[grepl('inner_content_container', ids)] %>%
      html_nodes('div')
    
    ids = content %>% 
      html_attr('id')
    
    ### Getting details from content
      
    details = content[grepl('^content', ids)] %>% 
      html_nodes('p') %>% 
      html_text()
    
    details = tibble(detail = details) %>% 
      separate(detail, c("Category", "Detail"), sep = ":")
    
    ### Getting partner institution in case it becomes pertinent in the future
    
    partner_inst = content[grepl('^content', ids)] %>% 
      html_nodes('p.p2') %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    #### Getting description
    
    para = content %>% 
      html_nodes("p")
    
    paraeids = para %>% 
      html_attr('paraeid')
    
    description = para[!is.na(paraeids)] %>% 
      html_text() %>% 
      clean_tags() %>% 
      paste(., collapse = " ")
    
    
    Credential = details %>% filter(grepl("type", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Duration = details %>% filter(grepl("length", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Tuition = details %>% filter(grepl("tuition", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    Campus = details %>% filter(grepl("location", Category, ignore.case = T)) %>% select(Detail) %>% unlist() %>% paste(., collapse = " ")
    
    tryCatch({
      if(exists("Program_tuition")){
        remove(Program_tuition)
      }
      
      Program_tuition = tibble(Tuition = Tuition)  %>% 
        mutate(Institution = Institution$institution_name,
               Program = program_urls$Program[url],
               Program_url = program_urls$url[url])
      
      
    }, error = function(e){cat("tuition ERROR :", conditionMessage(e), "\n")})
    
    program_df = create_program_df(Institution, url = program_urls$url[url], Program = program_urls$Program[url], Credential = Credential, Campus = Campus, Duration = Duration, Description = description, WIL = WIL)
    
    
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
