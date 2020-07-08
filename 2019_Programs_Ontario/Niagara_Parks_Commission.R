#############################
##### niagara parks commission
## Type of website: One program only - not well structured


Institution <- CIConnexName("niagara parks commission")
webPage <- read_webPage("https://www.niagaraparks.com/school-of-horticulture/")



program_urls = tibble(Program = "Horticultural Diploma",
                      Credential = "Horticultural Diploma",
                      url = "https://www.niagaraparks.com/school-of-horticulture/",
                      Description = webPage %>% html_nodes(clean_css_space('div.et_pb_text et_pb_module et_pb_bg_layout_light et_pb_text_align_left et_pb_text_3')) %>% html_text() %>% clean_tags() %>% paste(., collapse = " "),
                      Duration = webPage %>% html_nodes(clean_css_space("div.et_pb_text et_pb_module et_pb_bg_layout_light et_pb_text_align_left  et_pb_text_4")) %>% html_text() %>% clean_tags() %>% paste(., collapse = " "),
                      Campus = "Niagara Parks Botanical Garden",
                      WIL = "Experiential Learning")
detail = webPage %>% 
  html_nodes("div.et_pb_row") %>% 
  html_nodes('li') %>% 
  html_nodes('div.faq__answer') %>% 
  html_text() %>% 
  clean_tags()

detail = detail[grepl("^course", detail, ignore.case = T)]

courses = tibble(Name = webPage %>% 
                   html_nodes("div.et_pb_row") %>% 
                   html_nodes('li') %>% 
                   html_nodes('a.faq__toggle') %>% 
                   html_text()) %>% 
  filter(!grepl("\\?$", Name)) %>% 
  mutate(detail = detail) %>%
  mutate(detail = gsub("([[:digit:]]{2}[[:space:]]*)([A-Z])", "\\1|\\2", detail)) %>% 
  separate(detail, c("Code", "Description"),sep = "\\|" ) %>% 
  mutate(Code = gsub("(Course Code)(:*)([[:space:]]*)", "", Code, ignore.case = T))

program_urls$WIL = course_eval(courses, Institution$institution_name, program_urls$Program, program_urls$url, noDescription = F)
  

Fees = tibble(Fees = webPage %>% html_nodes(xpath = '//*[@id="et_builder_outer_content"]/div/div[3]/div[2]/div[2]/div/p[3]/strong') %>% html_text() %>% paste(., collapse = " "))%>% 
  mutate(Institution = Institution$institution_name,
         Program = program_urls$Program,
         Program_url = program_urls$url)


programs = create_program_df(Institution, url = program_urls$url, Program = program_urls$Program, Credential = program_urls$Credential, Campus = program_urls$Campus, Duration = program_urls$Duration, Description = program_urls$Description, WIL = program_urls$WIL)

save_out_standard_file(programs, Institution$institution_name, "Ontario")
