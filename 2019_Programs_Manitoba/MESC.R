#############################
##### manitoba emergency services college
## Type of website: One program only - fairly manual grab

Institution <- CIConnexName("manitoba emergency services college")

webPage = read_webPage("http://www.firecomm.gov.mb.ca/mesc_courses_pfpp.html")

programs = create_program_df(Institution,
                                 Program = get_detail_id(webPage %>% html_nodes("div"), "mainContent_wide") %>% html_nodes('h3') %>% html_text() %>% clean_tags() %>% paste(., collapse = ""),
                                 url = "http://www.firecomm.gov.mb.ca/mesc_courses_pfpp.html",
                                 Description = get_detail_id(webPage %>% html_nodes("div"), "mainContent_wide") %>% html_nodes('p') %>% html_text() %>% clean_tags() %>% paste(., collapse = " "),
                                 Credential = "Diploma",
                                 Campus = "Manitoba Emergency Serices College",
                                 Duration = "10 months",
                                 WIL = NA)



Fees = tibble(Total_tuition = "$20000",
              Program = get_detail_id(webPage %>% html_nodes("div"), "mainContent_wide") %>% html_nodes('h3') %>% html_text() %>% clean_tags() %>% paste(., collapse = ""),
              url = "http://www.firecomm.gov.mb.ca/mesc_courses_pfpp.html",
              institution = Institution$institution_name,
              prov_terr = Institution$prov_terr,
              record_id = Institution$record_id)


save_out_standard_file(programs, Institution$institution_name, "Manitoba")
