library(rvest)
library(tidyverse)
setwd("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees")
Members <- read_csv("temp/Members.csv") %>% 
  distinct() %>% 
  arrange(Member)

write_csv(Members, "temp/Members_for_check.csv")

urlFiles <- list.files("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp", pattern = "csv")



strToDf <- function(string, columnsToProduce = 2, columnNames = c("Category", "Value")){
  stringDf = tibble(Values = string) %>% 
    mutate(Categories = rep(1:columnsToProduce, (nrow(.)/columnsToProduce))) %>% 
    mutate(Groupings = rep(1:(nrow(.)/columnsToProduce), each = columnsToProduce)) %>% 
    spread(Categories, Values ) %>% 
    select(-Groupings) %>% 
    set_names(columnNames)
  
  return(stringDf)
}





#Algonquin

url <- "https://www.algonquincollege.com/ro/pay/fee-estimator/?campus=ALL&startterm=2019F&residency=canadian&programFees=1288X01FWO#programDetail"


website <- read_html(url)

website %>% 
  html_nodes("*")  %>% 
  html_attr("class") %>% 
  unique()


tibble(feeTitle = website %>% 
         html_nodes("span.feeTitle") %>% 
         html_text(),
       feeValue = website %>% 
         html_nodes("span.feeValue") %>% 
         html_text(),
       Program = website %>% 
         html_nodes("div.feeProgramTitle") %>% 
         html_text())



ACPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/Algonquin-Program-Codes-(Crawl-Run)---2019-08-28T181212Z.csv")
ACProgramCodes <- gsub(" .*","", ACPrograms$Col)

target <- "international"
target <- "canadian"

if(exists("Fees")){
  remove(Fees)
}

for(prog in 1:length(ACProgramCodes)) {
  url <- paste0("https://www.algonquincollege.com/ro/pay/fee-estimator/?campus=ALL&startterm=2019F&residency=", target, "&programFees=", ACProgramCodes[prog] ,"#programDetail")
  print(prog)

    website <- read_html(url)
    progUrl <- website %>% 
      html_nodes(xpath = '//*[@id="feeEstimator"]/div[2]/div[1]/h2/a') %>% 
      html_attr("href")
    
    d <- tibble(feeTitle = website %>% 
             html_nodes("span.feeTitle") %>% 
             html_text(),
           feeValue = website %>% 
             html_nodes("span.feeValue") %>% 
             html_text(),
           progName = ACPrograms$`Program Name 1`[prog],
           url = progUrl,
           ScrapeProgram = website %>% 
             html_nodes("div.feeProgramTitle") %>% 
             html_text(),
           progUrl = ACPrograms$`Program Name 1_link`[prog])
    if(!exists("Fees")){
      Fees <- d
    } else {
      Fees <- rbind(Fees, d)
    }
    
}


write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Fall_", target, "_Algonquin_Tuition_Fees.csv"))
d <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Fall_", target, "_Algonquin_Tuition_Fees.csv"))


### Durham

DurhamPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Durham_1of2-(Crawl-Run)---2019-08-29T113704Z.csv")
remove(Fees)
for(prog in 1:length(DurhamPrograms$Program_link)) {
  print(prog)
  url <- paste0(DurhamPrograms$Program_link[prog], "#tabCosts")
  website <- read_html(url)
  
  websiteData <- website %>% 
    html_nodes("table")%>% 
    html_text() %>% 
    .[grepl("YEAR ONE FEES", .) == T] %>% 
    strsplit(split = "\r\n")
  
  
  if(length(websiteData) > 0){
    websiteData <- unlist(websiteData)
    websiteData <- websiteData[websiteData != ""]
    print(length(websiteData))
    
    if(length(websiteData) == 15) {
      #split(websiteData, ceiling(seq_along(websiteData)/3))
      websiteData <- split(websiteData, ceiling(seq_along(websiteData)/3))
      websiteFees <- as_tibble(websiteData) %>% 
        gather(category, n, 2:5) %>% 
        spread(`1`, n) %>% 
        select(c(`YEAR ONE FEES`, `Domestic`, `International`)) %>%  
        mutate(Program = DurhamPrograms$Program[prog],
               ProgramUrl = DurhamPrograms$Program_link[prog])
      
          if(!exists("Fees")){
            Fees <- websiteFees
          } else {
            Fees <- rbind(Fees, websiteFees)
          }
      
      } else {
        #split(websiteData, ceiling(seq_along(websiteData)/3))
        websiteData <- split(websiteData, ceiling(seq_along(websiteData)/2))
        websiteFees <- as_tibble(websiteData) %>% 
          gather(category, n, 2:5) %>% 
          spread(`1`, n) %>% 
          select(c(`YEAR ONE FEES`, `Domestic`)) %>% 
          mutate(`International` = NA,
                 Program = DurhamPrograms$Program[prog],
                 ProgramUrl = DurhamPrograms$Program_link[prog])
        
          if(!exists("Fees")){
            Fees <- websiteFees
          } else {
            Fees <- rbind(Fees, websiteFees)
          }
     }
    }
}
Fees <- Fees %>% 
  filter(!grepl("confederation", url))
write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_First_Year_Durham_Tuition_Fees.csv"))



#George Brown
GBPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_George_Brown_1of2-(Crawl-Run)---2019-08-29T174309Z.csv")

website %>% 
  html_nodes("*")  %>% 
  html_attr("id") %>% 
  unique()

website %>% 
  html_nodes("*")  %>% 
  html_attr("class") %>% 
  unique()

remove(Fees)
for(prog in 1:length(GBPrograms$Program)){
  print(prog)
  url <- paste0(GBPrograms$Program_link[prog], "#tuitionContent")
  website <- read_html(url)
  
  
  websiteFees <- tibble(feeValue = website %>% 
                          html_nodes(gsub(" ", "\\.", "div.col-lg-9 col-md-12 col-sm-12 tab-pane-inner"))%>% 
                          html_text() %>% 
                          .[grep("\\$", .)] %>% 
                          strsplit(split = "\r\n") %>% 
                          unlist() %>% 
                          .[4] %>%
                          gsub(" |\\*", "", .),
                        Program = GBPrograms$Program[prog],
                        ProgramUrl = GBPrograms$Program_link[prog]
  )
  if(!exists("Fees")){
    Fees <- websiteFees
  } else {
    Fees <- rbind(Fees, websiteFees)
  }
}

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_First_Year_George_Brown_Tuition_Fees.csv"))


#Humber

HumberPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Humber_1of2-(Crawl-Run)---2019-08-29T184525Z.csv")

remove(Fees)
for(prog in 145:length(HumberPrograms$Program)){
  
  print(prog)
  
  url <- HumberPrograms$Program_link[prog]
  website <- read_html(url)
  websiteData <- website %>% 
    html_nodes("table.fee-table") %>% 
    html_text() %>% 
    strsplit(split = "(?<=Year [[:digit:]] - Term [[:digit:]])(?=[D])|(?<=Level [[:digit:]])(?=[D])|(?<=N/A)(?=[A-Z])|(?<=Semester [[:digit:]])(?=[D])|(?<=[[:digit:]][C])(?=Domestic)|(?<=[[:digit:]]{2})(?=[A-Z])|(?<=[a-z])(?=[A-Z])|(?<=\\*)(?=[A-Z])|(?<=[a-z])(?=\\$)|(?<=[[:digit:]])(?=\\$)|(?<=\\))(?=\\$)", perl = T) %>% 
    unlist()
  
  if(length(websiteData) > 0) {
    print(paste("processing:", prog))
    
    websiteData <- as_tibble(split(websiteData, ceiling(seq_along(websiteData)/21))) 
    colnames(websiteData) <- paste(websiteData[1,], letters[1:ncol(websiteData)], sep = "_")
    
    
    websiteData <- websiteData[-1,] %>% 
      gather(category, data, 1:ncol(.)) 
    
    
    websiteDataCats <- websiteData %>%
      filter(grepl("[[:alpha:]]", data)) %>% 
      filter(data %in% c("Domestic Fee", "International Fee*")) %>% 
      filter(data != "N/A") %>% 
      left_join(websiteData %>%
                  filter(grepl("[[:alpha:]]", data)) %>% 
                  filter(!data %in% c("Domestic Fee", "International Fee*")) %>% 
                  filter(data != "N/A") %>% 
                  rename(subData = data),
                by = "category") %>% 
      arrange(data, category) %>% 
      cbind(websiteData %>% 
              filter(grepl("[[:digit:]]|N/A", data)) %>% 
              mutate(order = rep.int(c(1,2), nrow(.)/2 )) %>% 
              arrange(order) %>% 
              select(-c(category, order)) %>% 
              rename(feeData = data)) %>% 
      #spread(category, feeData) %>% 
      rename(studentType = data,
             feeType = subData)
    
    websiteFees <- websiteDataCats %>% 
      mutate(Program = HumberPrograms$Program[prog],
             ProgramUrl = HumberPrograms$Program_link[prog])
    
    if(!exists("Fees")){
      Fees <- websiteFees
    } else {
      Fees <- rbind(Fees, websiteFees)
    }
  }
  
}

#write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_First_Year_Humber_Tuition_Fees_pt_3.csv"))

#Fees <- read_csv("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_First_Year_Humber_Tuition_Fees_pt_1.csv") %>% 
#  rbind(read_csv("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_First_Year_Humber_Tuition_Fees_pt_2.csv")) %>% 
#  rbind(read_csv("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_First_Year_Humber_Tuition_Fees_pt_3.csv"))

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_First_Year_Humber_Tuition_Fees_Complete.csv"))


#LaCite

url <- "https://www.collegelacite.ca/programmes/?utm_campaign=rev5159&utm_medium=menu&utm_source=internal"
webPage <- read_html(url)

Link <- webPage %>% 
  html_nodes("table.table-striped") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  tibble::enframe() %>% 
  filter(grepl("programmes", value)) %>% 
  mutate(link = paste0("https://www.collegelacite.ca", value)) 


websiteData <- webPage %>% 
  html_nodes("table.table-striped") %>% 
  html_text() %>% 
  gsub("([[:alpha:]]|[[:digit:]])(\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t)([[:alpha:]]|[[:digit:]])", "\\1\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t_\\3", .) %>% 
  gsub("Distance\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\tToronto", "Distance Toronto", .) %>% 
  gsub("Distance\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\tOttawa", "Distance Ottawa", .) %>% 
  gsub("Hawkesbury\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\tOrléans", "Hawkesbury/Orléans", .) %>% 
  gsub("Orléans\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\tOttawa", "Orléans/Ottawa", .) %>% 
  gsub("Ottawa\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\tToronto", "Ottawa/Toronto", .) %>% 
  gsub("(\t|\n)( )(\t|\n)", "\\1_ _\\3", .) %>% 
  gsub("\t", "", .) %>% 
  gsub("\n{1,}", "_", .) %>%
  gsub("Frais_par étape", "Frais par étape", .) %>% 
  strsplit("_") %>% 
  sapply(function(x) split(x, ceiling(seq_along(x)/8)))


websiteData <- websiteData %>% 
  bind_rows()

Fees <- websiteData[-c(1:8),] %>% 
  mutate(sortOrder = rep(1:(nrow(.)/8), each = 8)) %>% 
  gather(category, data, 2:(ncol(.)-1)) %>% 
  arrange(sortOrder) %>% 
  mutate(category = rep(1:(nrow(.)/8), each = 8)) %>% 
  spread(`1`, data) %>% 
  select(c(sortOrder, category, Programme, Durée, `Frais par étape`, `Stage COOP`, Campus, `Automne 2020`, `Hiver 2020`, `Printemps 2020`))%>% 
  filter(!is.na(Programme))%>% 
  cbind(Link %>% 
          select(link))


urls <- unique(Fees$link)
remove(CiteFees)
for(prog in 1:length(urls)){
  print(prog)
  url <- urls[[prog]]
  
  webPage <- read_html(url)
  
  programName = webPage %>% 
    html_node(xpath = '//*[@id="p_p_id_Programme_WAR_WebDynamiqueportlet_"]/div/div/h1/a') %>% 
    html_text()

    
  Fees <- webPage %>% 
    html_nodes("table.frais") %>% 
    html_table(fill = T)
  
  if(length(Fees) > 0){
    Fees <- Fees %>% 
      purrr::map(~ set_names(., c("Category", .[1,2:ncol(.)]))) %>% 
      purrr::map(~ filter(., Category != "")) %>% 
      bind_rows(.id = "studentType") %>% 
      mutate(studentType = ifelse(studentType == 1, "Canadian", "International")) %>% 
      gather("Term", "feeValue", 3:ncol(.)) %>% 
      mutate(url = url,
             Program = programName)
    
    if(exists("CiteFees")){
      CiteFees <- bind_rows(CiteFees, Fees)
    } else {
      CiteFees <- Fees
    }
    
  }
  
}


write_csv(CiteFees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_La_Cite_Tuition_Fees.csv")



### Lambton

LambtonPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Lambton_1of2-(Crawl-Run)---2019-08-30T171457Z.csv")
LambtonPrograms$Programs_link[1]

LambtonPrograms$Programs_link[grepl("International", LambtonPrograms$Programs_link, ignore.case = T)]

remove(Fees)
for(prog in 1:length(LambtonPrograms$Programs)) {
  print(prog)
  
  if(!grepl("International", LambtonPrograms$Programs_link[prog])) {
  
            url <- paste0(LambtonPrograms$Programs_link[prog], "&type=TF")
            #url <- paste0(LambtonPrograms$Programs_link[24], "&type=TF")
            possibleError <- tryCatch(
              read_html(url),
              error = function(e) e
            )
            
            if(!inherits(possibleError, "error")){
          
              webPage <- read_html(url)
              
              webPage <- webPage %>% 
                html_nodes("#contentwrapper") %>% 
                html_nodes("div.row") %>% 
                html_nodes("section") %>% 
                html_nodes("table") %>% 
                html_text()
              
              
              if(length(webPage) > 0){
                print(paste("Processing:", LambtonPrograms$Programs[prog]))
                websiteData <- webPage  %>% 
                  gsub("(Term|Year|Terms)( [[:digit:]])", "\\2", .) %>% 
                  gsub("([[:digit:]])(,[[:space:]])([[:digit:]])", "\\1&\\3", .) %>% 
                  gsub("([[:digit:]]\\&[[:digit:]])(,[[:space:]])([[:digit:]])", "\\1&\\3", .) %>% 
                  gsub("([[:digit:]])([[:space:]]|)(\\$)", "\\1\\3", .) %>% 
                  gsub("([[:digit:]])([[:digit:]])(\\$)", "\\1 \\2\\3", .) %>% 
                  strsplit("(?<=[a-z])(?=[[:digit:]])|(?<=[[:space:]])(?=[[:digit:]])|(?<=\\.)(?=[[:alpha:]])|(?<=[[:digit:]])(?=\\$)|(?<=Year)(?=Amount)|(?<=Term)(?=Amount)|(?<=[a-z])(?=[A-Z])|(?<=[a-z])(?=\\$)|(?<=TBD)(?=Total)|(?<=[[:space:]])(?=\\$)", perl = T)
                
                websiteData <- websiteData %>% 
                  unlist() %>% 
                  .[!grepl("Breakdown", ., ignore.case = T)] %>% 
                  split(., ceiling(seq_along(.)/2)) %>% 
                  as_tibble() %>% 
                  gather(category, data, 2:ncol(.)) %>% 
                  spread(`1`, data) %>% 
                  rename(Program = category) %>% 
                  mutate(Program = LambtonPrograms$Programs[prog],
                         ProgramUrl = LambtonPrograms$Programs_link[prog]) %>% 
                  mutate(Time_period = colnames(.)[3])
                  
                
                colnames(websiteData) <- c("Program", "Amount", "Num_Time", "ProgramUrl", "Time_period")
                
                if(!exists("Fees")){
                  Fees <- websiteData
                } else {
                  Fees <- rbind(Fees, websiteData)
                }
              }
            }
  }
 }

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Lambton_Tuition_Fees.csv")


#remove(Fees)
for(prog in 1:length(LambtonPrograms$Programs)) {
  print(prog)
  
  if(!grepl("International", LambtonPrograms$Programs_link[prog])) {
    url <- paste0(gsub("Full-Time|Part-Time", "International", LambtonPrograms$Programs_link[1]), "&type=TF")
  } else {
    url <- paste0(LambtonPrograms$Programs_link[prog], "&type=TF")
  }
    
    url <- paste0(LambtonPrograms$Programs_link[prog], "&type=TF")
    #url <- paste0(LambtonPrograms$Programs_link[24], "&type=TF")
    
    
    webPage <- read_html(url)
    
    webPage <- webPage %>% 
      html_nodes("#contentwrapper") %>% 
      html_nodes("div.row") %>% 
      html_nodes("section") %>% 
      html_nodes("table") %>% 
      html_text()
    
    
    if(length(webPage) > 0){
      print(paste("Processing:", LambtonPrograms$Programs[prog]))
      websiteData <- webPage  %>% 
        gsub("(Term|Year|Terms)( [[:digit:]])", "\\2", .) %>% 
        gsub("([[:digit:]])(,[[:space:]])([[:digit:]])", "\\1&\\3", .) %>% 
        gsub("([[:digit:]]\\&[[:digit:]])(,[[:space:]])([[:digit:]])", "\\1&\\3", .) %>% 
        gsub("([[:digit:]])([[:space:]]|)(\\$)", "\\1\\3", .) %>% 
        gsub("([[:digit:]])([[:digit:]])(\\$)", "\\1 \\2\\3", .) %>% 
        strsplit("(?<=[a-z])(?=[[:digit:]])|(?<=[[:space:]])(?=[[:digit:]])|(?<=\\.)(?=[[:alpha:]])|(?<=[[:digit:]])(?=\\$)|(?<=Year)(?=Amount)|(?<=Term)(?=Amount)|(?<=[a-z])(?=[A-Z])|(?<=[a-z])(?=\\$)|(?<=TBD)(?=Total)|(?<=[[:space:]])(?=\\$)", perl = T)
      
      websiteData <- websiteData %>% 
        unlist() %>% 
        .[!grepl("Breakdown", ., ignore.case = T)] %>% 
        split(., ceiling(seq_along(.)/2)) %>% 
        as_tibble() %>% 
        gather(category, data, 2:ncol(.)) %>% 
        spread(`1`, data) %>% 
        rename(Program = category) %>% 
        mutate(Program = LambtonPrograms$Programs[prog],
               ProgramUrl = LambtonPrograms$Programs_link[prog],
               Intl = "International student") %>% 
        mutate(Time_period = colnames(.)[3])
      
      
      colnames(websiteData) <- c("Program", "Amount", "Num_Time", "ProgramUrl", "Time_period")
      
      if(!exists("Fees")){
        Fees <- websiteData
      } else {
        Fees <- rbind(Fees, websiteData)
      }
    }
  }


	
write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Lambton_Tuition_Fees.csv")




#Loyalist
LoyalistPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Loyalist_1of2-(Crawl-Run)---2019-08-30T184547Z.csv")


remove(Fees)
for(prog in 1:length(LoyalistPrograms$Program)){
  print(prog)
  url <- LoyalistPrograms$Program_link[prog]
  
  webPage <- read_html(url)
  
  websiteData <- webPage %>% 
    html_nodes("div.tab-view.costs") %>% 
    html_text() 
  
  if(length(websiteData) > 0) {
    print(paste0("Processing: ", LoyalistPrograms$Program[prog]))
    websiteData <- websiteData %>%
      strsplit("\n") %>% 
      unlist()
    
    #websiteData[grepl("\\$", websiteData)]
    
    websiteData <- tibble(categoryAmount = websiteData[grepl("\\$", websiteData)],
                          Program = LoyalistPrograms$Program[prog],
                          ProgramUrl = LoyalistPrograms$Program_link[prog])
    
    if(!exists("Fees")){
      Fees <- websiteData
    } else {
      Fees <- rbind(Fees, websiteData)
    }
    
  }
  
}

FeesProcessed <- Fees %>% 
  separate(categoryAmount, into = c("Category", "Amount"), sep = ":")


write_csv(FeesProcessed, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Loyalist_Tuition_Fees.csv")


### Michener
MichTui <- pdftools::pdf_text("https://michener.ca/wp-content/uploads/2019/07/WebSite_Tuition_Fees_2019_20_2.pdf") %>%
  strsplit("\n")

MichNonTuiFees <- MichTui[[7]]

MichTui <- MichTui[2:(length(MichTui) - 1)]

names(MichTui) <- c("Full-Time", "Part-Time", "Textbooks", "Professional-exams")

FTNames <- MichTui[[1]][grepl("Fall     Fall", MichTui[[1]])] %>% gsub("[[:space:]]{1,}", "_", .) %>% gsub("^_", "", .) %>% strsplit("_") 
FTSubNames <- MichTui[[1]][grepl("uition  Ancillary ", MichTui[[1]])] %>% gsub("[[:space:]]{1,}", "_", .) %>% gsub("^_", "", .) %>% strsplit("_") 

FTData <- MichTui[[1]][grepl("\\$[[:digit:]]", MichTui[[1]])] %>% 
  gsub("([[:digit:]])(                                                                                  )(\\$)", "\\1_NA_NA_NA_NA_NA_NA_\\3", .) %>% 
  gsub("([[:digit:]])(                                                 )(\\$)" , "\\1_NA_NA_NA_\\3", .) %>% 
  gsub("([a-z])([[:space:]])([A-Z])", "\\1-\\3", .) %>% 
  gsub("[[:space:]]{1,}", "_", .) %>% 
  gsub("^_", "", .) %>% strsplit("_")


FTData <- lapply(FTData, function(x) if (length(x) == 11) c("Medical Laboratory Science", x) else x)

SomeNames <- paste(unlist(FTNames), unlist(FTSubNames), sep = "_")

FTData <- FTData %>% 
  do.call(rbind.data.frame, .) %>% 
  magrittr::set_colnames(c("Program", "Year", SomeNames[1:2], "Fall Total", SomeNames[3:length(SomeNames)], "Total")) %>% 
  gather(Category, Data, 3:ncol(.)) %>% 
  mutate(Year = paste("Year", Year, sep = "_"),
         Data = as.numeric(gsub("\\$|,", "", Data))) %>% 
  spread(Year, Data)

as_tibble(FTData)

NonTuiFees <- MichNonTuiFees[8:23] %>% gsub("[[:space:]]{2,}", "", .) %>% gsub("\r", "", .)

NonTuiFees <- tibble(Category = NonTuiFees[grepl("[[:alpha:]]", NonTuiFees) & !grepl("Varies", NonTuiFees)],
                     Data = NonTuiFees[grepl("[[:digit:]]|Varies", NonTuiFees)])

write_csv(FTData, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Michener_Tuition_Fees.csv")


####Mohawk
MohawkPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Mohawk_1of2-(Crawl-Run)---2019-09-03T155604Z.csv")

remove(Fees)
for(prog in 94:length(MohawkPrograms$Program)) {
  print(prog)
  
  url <- MohawkPrograms$Program_link[prog]
  
  webPage <- read_html(url)
  webPageData <- webPage %>% 
    html_nodes("div.tuition-and-fees--table") %>% 
    html_nodes("table") %>% 
    html_table()
  
  webPageDataNames <- webPage %>% 
    html_nodes("div.tuition-and-fees--table") %>% 
    html_nodes("h3") %>% 
    html_text()
  
  if(length(webPageDataNames) == 0){
    
    webPageDataNames <- webPage %>% 
      html_nodes("div.field__item") %>% 
      html_nodes("h3") %>% 
      html_text()
    
    webPageDataNames <- webPageDataNames[4:length(webPageDataNames)]
  }
  
  if(length(webPageData) > 0 ){
    print(length(webPageData))
    
    colIndex <- ncol(webPageData[[1]])
    
    if(length(webPageData) == 2) {
      colnames(webPageData[[1]]) <- gsub("\n|\t", "", colnames(webPageData[[1]]))
      colnames(webPageData[[2]]) <- gsub("\n|\t", "", colnames(webPageData[[2]]))
      
      urlFees <- webPageData[[1]] %>% 
        as_tibble(.name_repair = "unique") %>% 
        rename(Category = `...1`) %>% 
        mutate(studentType = gsub("([[:digit:]]){4}( - )([[:digit:]]){4} ", "", webPageDataNames[[1]])) %>% 
        rbind(webPageData[[2]] %>% 
                as_tibble(.name_repair = "unique") %>% 
                rename(Category = `...1`) %>% 
                mutate(studentType = gsub("([[:digit:]]){4}( - )([[:digit:]]){4} ", "", webPageDataNames[[2]]))) %>% 
        gather(Semester, Fee, 2:colIndex) %>% 
        mutate(Program = MohawkPrograms$Program[prog],
               ProgramUrl = MohawkPrograms$Program_link[prog])
      
    } else if(length(webPageData) == 1) {
      urlFees <- webPageData[[1]] %>% 
        as_tibble(.name_repair = "unique") 
      
      colnames(urlFees)[1] <- "Category"
      
      urlFees <- urlFees %>% 
        #rename(Category = `...1`) %>% 
        mutate(studentType = gsub("([[:digit:]]){4}( - )([[:digit:]]){4} ", "", webPageDataNames[[1]])) %>% 
        gather(Semester, Fee, 2:colIndex) %>% 
        mutate(Program = MohawkPrograms$Program[prog],
               ProgramUrl = MohawkPrograms$Program_link[prog])
      
      
    }
    
    
    
    if(!exists("Fees")) {
      Fees <- urlFees
    } else {
      Fees <- rbind(Fees, urlFees)
    }
  } 
  
  
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Mohawk_Tuition_Fees.csv")



#Niagara
NiagTui <- pdftools::pdf_text("https://www.niagaracollege.ca/fees-finances/wp-content/uploads/sites/13/Fee-Schedule-2019-2020-6182019.pdf") %>%
  strsplit("\n")

NiagTuiData <- unlist(NiagTui)[grepl("(P0)([[:digit:]]){3}", unlist(NiagTui))] %>% 
  #gsub("(2019-20|2019/20)([[:space:]])", "\\1", .) %>% 
  gsub("([[:space:]]){1,}(\\$|See programs|2019-20 Intake|2019/20 - No|New Program Code)", "_\\2", .) %>% 
  gsub("([[:digit:]])([[:space:]]){1,}", "\\1_", .) %>% 
  #rentice	2019/20
  strsplit("_")


#https://www.r-bloggers.com/converting-a-list-to-a-data-frame/
require(devtools)
source_gist(4676064)

NiagTuiData <- as.data.frame(NiagTuiData) %>% 
  filter(!grepl("[[:alpha:]]", Col3)) %>% 
  filter(!grepl("[[:alpha:]]", Col4))



NiagTuiHeaders <- unlist(NiagTui)[grepl("Fall Intake", unlist(NiagTui))][1] %>% 
  gsub("Fall Intake Niagara College 2019-2020 Tuition & Fees", "Fall Intake_Niagara College 2019-2020 Tuition & Fees", .) %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("\r", "", .) %>% 
  strsplit("_") %>% 
  unlist()

NiagTuiSubHeaders <- unlist(NiagTui)[grepl("Plan Code", unlist(NiagTui))][1] %>% 
  gsub("Niagara College Program Name", "Program-Name", .) %>% 
  gsub("Plan Code", "Program-ID", .) %>% 
  gsub("[[:space:]]{1,}", "_", .) %>% 
  gsub("\r", "", .) %>% 
  strsplit("_") %>% 
  unlist()

colnames(NiagTuiData) <- paste(c(NiagTuiHeaders[1:2], rep(NiagTuiHeaders[3:6], each = 5)), NiagTuiSubHeaders, sep = "_")

NiagTuiData %>% 
  gather(YearCat, Fee, 3:ncol(.)) %>% 
  separate(YearCat, c("Year", "Category"), sep = "_") %>% 
  write_csv("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Niagara_Tuition_Fees.csv")



#Northern


#Campus Codes
#CH – Contact North Haileybury
#CK – Contact North Kirkland Lake
#CN – Contact North Porcupine
#HL – Haileybury
#KL – Kirkland Lake
#JB – Moosonee (James Bay Education Centre)
#PC – Timmins
#OL – Open Learning
NorthernPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Northern_2of2-(Crawl-Run)---2019-09-03T200128Z.csv")

NorthernPrograms <- NorthernPrograms %>% 
  filter(!is.na(Tuition_Link_link))
remove(Fees)
for(prog in 21:length(NorthernPrograms$Program)){
  print(prog)
  url <- NorthernPrograms$Tuition_Link_link[prog]
  
  webPage <- read_html(url)
  
  webPageNames <- webPage %>% 
    html_nodes("div.entry-content") %>% 
    html_nodes("strong") %>% 
    html_text()
  
  
  
  webPageData <- webPage %>% 
    html_nodes("table") %>% 
    html_table() 
  
  if(length(webPageData) > 0) {
    print(paste("Processing: ", NorthernPrograms$Program[prog]))
    
    names(webPageData) <- webPageNames[grepl("Year", webPageNames)]
    
    if( is.na(names(webPageData) )){
      names(webPageData) = paste("Dataset", 1:length(webPageData), sep = "_")
    }
    
    
    webPageData <- webPageData %>% 
      bind_rows(, .id = "Year")
    
    if(colnames(webPageData)[1] == colnames(webPageData)[2]){
      webPageData <- webPageData[,2:ncol(webPageData)]
    }
    
    webPageData <- webPageData %>% 
      select(-starts_with("Total Due")) %>% 
      gather(Category, Fee, 4:ncol(.)) %>% 
      mutate(Year = gsub(" ","_", Year)) %>% 
      mutate(Program = NorthernPrograms$Program[prog],
             ProgramUrl = NorthernPrograms$Tuition_Link_link[prog])
    
    
    if ( !exists("Fees") ) {
      Fees <- webPageData
    } else {
      Fees <- rbind(Fees, webPageData)
    }
    
    
  }
}



Fees <- Fees %>% 
  select(-c(url, Credential, Duration)) %>% 
  left_join(NorthernPrograms %>% 
              rename(ProgramUrl = Tuition_Link_link) %>% 
              select(url, Credential, Duration, ProgramUrl),
            by = "ProgramUrl")

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Northern_Tuition_Fees.csv")



#Sault
SaultPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Sault_1of2-(Crawl-Run)---2019-09-04T123407Z.csv")
remove(Fees)
for(prog in 1:length(SaultPrograms$Program)){
  url <- gsub("overview*", "fees", SaultPrograms$Program_link[prog])
  print(prog)
  
  webPage <- read_html(url)
  webPageData <- webPage %>% 
    html_nodes("div") %>% 
    html_nodes("#FeesTable") %>% 
    html_nodes("table") %>% 
    html_table()
  
  if(length(webPageData) > 0){
    print(paste("Processing:", SaultPrograms$Program[prog]))
    webPageData <- webPageData %>% 
      bind_cols() %>% 
      gather(Category, Fees) %>% 
      mutate(Type = ifelse(!grepl("\\$", Fees), Fees, NA)) %>% 
      mutate(Type = zoo::na.locf(Type),
             Category = gsub("[[:digit:]]", "", Category)) %>% 
      filter(!Fees %in% unique(Type)) %>% 
      mutate(Program = SaultPrograms$Program[prog],
             ProgramUrl = SaultPrograms$Program_link[prog])
    
    if(!exists("Fees")){
      Fees <- webPageData
    } else {
      Fees <- rbind(Fees, webPageData)
    }
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Sault_Tuition_Fees.csv")


#Seneca
SenecaPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Seneca_1of2-(Crawl-Run)---2019-09-04T132223Z.csv")
remove(Fees)

for(prog in 1:length(SenecaPrograms$Program)){
  url <- gsub("\\.html", "/costs\\.html", SenecaPrograms$Program_link[prog])
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
  
      webPage <- read_html(url)
      webPageData <- webPage %>% 
        html_nodes("div.program-costs") %>% 
        html_nodes("p") %>% 
        html_text() %>% 
        gsub("\n|\t", "", .)
      
      Categories <- webPage %>% 
        html_nodes("div.program-costs") %>% 
        html_nodes("p") %>% 
        html_attr("class")
      
      if(length(webPageData) > 0){
        print(paste("Processing:", SenecaPrograms$Program[prog]))
        webPageData <- tibble(Category = Categories,
                              Fees = webPageData) %>% 
          filter(!is.na(Category)) %>% 
          mutate(Program = SenecaPrograms$Program[prog],
                 ProgramUrl = SenecaPrograms$Program_link[prog])
        
        if(!exists("Fees")){
          Fees <- webPageData
        } else {
          Fees <- rbind(Fees, webPageData)
        }
      }
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Seneca_Tuition_Fees.csv")


# Seneca International

url <- "https://www.senecacollege.ca/international/fees/international-program-fees.html"


webPage <- read_html(url)

urls <- webPage %>% 
  html_nodes("div.tabs") %>% 
  html_nodes("a") %>% 
  html_attr("href")

urls <- urls[5:length(urls)]

remove(Fees)

for(link in 1:length(urls)){
  url <- paste0("https://www.senecacollege.ca/", urls[link])
  print(link)
  
  
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    webPageData <- webPage %>% 
      html_nodes("div.table") %>% 
      html_nodes("table") %>% 
      html_table()
    

    
    if(length(webPageData) > 0){
      print(paste("Processing:", urls[link]))
      
      webPageData <- webPageData[[1]] %>% 
        rename(Category = Campus) %>% 
        gather(Campus, FeeValue, 2:ncol(.)) %>% 
        mutate(Program = gsub("(/international/fees/international-program-fees/)(.*)(\\.html)", "\\2", urls[link]),
               ProgramUrl = urls[link])
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
  
}

Fees <- 
  Fees %>% 
  mutate(Program =gsub("(international/fees/international-program-fees/)(.*)(\\.html)", "\\2", ProgramUrl),
         StudentType = "International")

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Seneca_Tuition_Fees_Intl_Students.csv")


#Sheridan

url <- "https://academics.sheridancollege.ca/programs/"


webPage <- read_html(url)
urls <- webPage %>% 
  html_nodes("div.filter-list-container") %>% 
  html_nodes("ul") %>% 
  html_nodes("li") %>% 
  html_nodes("a") %>% 
  html_attr("href")


remove(Fees)
for(link in 1:length(urls)){
  url <- paste0("https://academics.sheridancollege.ca", urls[link], "/financial/domestic")
  print(link)
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    webPageData <- webPage %>% 
      html_nodes("div.col-sm-12") %>% 
      html_nodes("ul.plan-item") %>% 
      html_text() %>% 
      gsub("(Notes)(\r\n            )", "\\1-", .) %>% 
      gsub("(\n|\r|([[:space:]]{2,})){1,}", "_", .) %>% 
      gsub("^_", "", .) %>% 
      strsplit("_") %>% 
      unlist()
    
    
    
    if(length(webPageData) > 0){
      print(paste("Processing:", urls[link]))
      
      if(length(grep("Part-Time", webPageData)) > 0){
        webPageData <- c(webPageData[1:grep("Part-Time", webPageData)], "Part-Time", webPageData[(grep("Part-Time", webPageData) + 1): length(webPageData)])
      }
      
      if(length(grep("yrs|semester|year", webPageData, ignore.case = T)) == 0){
        webPageData <- c(webPageData[1:grep("Part-Time|Full-time|Full-Time|Part-time", webPageData)], webPageData[grep("Part-Time|Full-time|Full-Time|Part-time", webPageData)], webPageData[(grep("Part-Time|Full-time|Full-Time|Part-time", webPageData) + 1): length(webPageData)])
      }
      
      
      webPageData <- webPageData %>% 
        split(., ceiling(seq_along(.)/6)) %>% 
        bind_rows() %>% 
        gather(Category, Data) %>% 
        mutate(Index = rep(6:1, (nrow(.)/6)),
               eduLevel = zoo::na.locf(ifelse(Index == 6, Data, NA))) %>% 
        #select(-c(Category)) %>% 
        filter(Data != eduLevel) %>% 
        spread(Index, Data) %>% 
        select(-Category) %>% 
        magrittr::set_colnames(c("Credential", "Notes","Tuition","ProgramCode","Duration","FT_PT")) %>% 
        mutate(Program = gsub("/programs/","", urls[link]),
               ProgramUrl = paste0("https://academics.sheridancollege.ca", urls[link]))
      

      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
 }
write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Sheridan_Tuition_Fees.csv")


#Sheridan International
remove(Fees)
for(link in 1:length(urls)){
  url <- paste0("https://academics.sheridancollege.ca", urls[link], "/financial/international")
  print(link)
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    webPageData <- webPage %>% 
      html_nodes("div.col-sm-12") %>% 
      html_nodes("ul.plan-item") %>% 
      html_text() %>% 
      gsub("(Notes)(\r\n            )", "\\1-", .) %>% 
      gsub("(\n|\r|([[:space:]]{2,})){1,}", "_", .) %>% 
      gsub("^_", "", .) %>% 
      strsplit("_") %>% 
      unlist()
    
    
    
    if(length(webPageData) > 0){
      print(paste("Processing:", urls[link]))
      
      if(length(grep("Part-Time", webPageData)) > 0){
        webPageData <- c(webPageData[1:grep("Part-Time", webPageData)], "Part-Time", webPageData[(grep("Part-Time", webPageData) + 1): length(webPageData)])
      }
      
      if(length(grep("yrs|semester|year", webPageData, ignore.case = T)) == 0){
        webPageData <- c(webPageData[1:grep("Part-Time|Full-time|Full-Time|Part-time", webPageData)], webPageData[grep("Part-Time|Full-time|Full-Time|Part-time", webPageData)], webPageData[(grep("Part-Time|Full-time|Full-Time|Part-time", webPageData) + 1): length(webPageData)])
      }
      
      
      webPageData <- webPageData %>% 
        split(., ceiling(seq_along(.)/6)) %>% 
        bind_rows() %>% 
        gather(Category, Data) %>% 
        mutate(Index = rep(6:1, (nrow(.)/6)),
               eduLevel = zoo::na.locf(ifelse(Index == 6, Data, NA))) %>% 
        #select(-c(Category)) %>% 
        filter(Data != eduLevel) %>% 
        spread(Index, Data) %>% 
        select(-Category) %>% 
        magrittr::set_colnames(c("Credential", "Notes","Tuition","ProgramCode","Duration","FT_PT")) %>% 
        mutate(Program = gsub("/programs/","", urls[link]),
               ProgramUrl = paste0("https://academics.sheridancollege.ca", urls[link]))
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}

Fees <- Fees %>% 
  mutate(StudentType = "International")

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Sheridan_Tuition_Fees_Intl_Students.csv")



# Six Nations

SixNationsPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Six_Nations_1of2-(Crawl-Run)---2019-09-04T155852Z.csv")
remove(Fees)

url <- "https://www.snpolytechnic.com/program/bachelor-arts-ogwehoweh-languages-mohawk-language-stream"

for(prog in 1:length(SixNationsPrograms$Program)){
  url <- SixNationsPrograms$Program_link[prog]
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    webPageData <- webPage %>% 
      html_nodes("div.field__items") %>% 
      html_nodes("p") %>% 
      html_text()
    
    durationData <- webPage %>% 
      html_nodes("div.field--name-field-program-length") %>% 
      html_text() %>% 
      gsub("(\n)([[:space:]])*", "", .)
    
    webPageData <- webPageData[grepl("Total", webPageData, ignore.case = T)]

    
    if(length(webPageData) > 0){
      
      webPageData <- tibble(TotalFees = webPageData,
                            Duration = durationData,
                            Program = SixNationsPrograms$Program[prog],
                            ProgramUrl = SixNationsPrograms$Program[prog])
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Six_Nations_Tuition_Fees.csv")


##St. Clair
"http://www.stclaircollege.ca/programs/postsec/docs/fees/2019-Tuition-Fee-Sheet.pdf"

StClairTui <- pdftools::pdf_text("http://www.stclaircollege.ca/programs/postsec/docs/fees/2019-Tuition-Fee-Sheet.pdf") %>%
  strsplit("\n") %>% 
  unlist()

StClairData <- StClairTui[!grepl("Program Information|Tuition", StClairTui)]
StClairHeaders <- StClairTui[grepl("Program Information|Tuition", StClairTui)]

require(devtools)
source_gist(4676064)


StClairData <- StClairData %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("([A-Z])([[:space:]])(\\$)", "\\1_\\2", .) %>% 
  gsub("([[:digit:]])([[:space:]])(\\$)", "\\1_\\2", .) %>% 
  strsplit("_") %>% 
  as.data.frame() %>% 
  mutate(Index = c(4:1, 5:nrow(.))) %>% 
  arrange(Index) %>% 
  mutate(Semester = ifelse(grepl("S|W|F", Col2),  as.character(Col2), "Total"),
         Col2 = ifelse(grepl("S|W|F", Col2), NA,  as.character(Col2))) %>% 
  mutate(Col8 = ifelse(!is.na(Col2),  as.character(Col7),  as.character(Col8)),
         Col7 = ifelse(!is.na(Col2),  as.character(Col6),  as.character(Col7)),
         Col6 = ifelse(!is.na(Col2),  as.character(Col5),  as.character(Col6)),
         Col5 = ifelse(!is.na(Col2),  as.character(Col4),  as.character(Col5)),
         Col4 = ifelse(!is.na(Col2),  as.character(Col3),  as.character(Col4)),
         Col3 = ifelse(!is.na(Col2),  as.character(Col2),  as.character(Col3)),
         Program = zoo::na.locf(ifelse(Semester == "19F", as.character(Col1), NA))) %>% 
  mutate(Col8 = ifelse(Program == "International Bus Mgt-Logistic" & Semester != "Total",  as.character(Col7),  as.character(Col8)),
         Col7 = ifelse(Program == "International Bus Mgt-Logistic" & Semester != "Total",  as.character(Col6),  as.character(Col7)),
         Col6 = ifelse(Program == "International Bus Mgt-Logistic" & Semester != "Total",  as.character(Col5),  as.character(Col6)),
         Col5 = ifelse(Program == "International Bus Mgt-Logistic" & Semester != "Total",  as.character(Col4),  as.character(Col5)),
         Col4 = ifelse(Program == "International Bus Mgt-Logistic" & Semester != "Total",  as.character(Col3),  as.character(Col4)),
         Col3 = ifelse(Program == "International Bus Mgt-Logistic" & Semester != "Total",  as.character(Col2),  as.character(Col3))) %>% 
  select(c(Program, Semester, Col3:Col8)) %>% 
  magrittr::set_colnames(c("Program", "Semester", "DomesticTuition", "DomesticTotal","IntlTuition","IntlTotal","USTuition","USTotal"))

write_csv(StClairData %>% filter(Semester != "Total"), "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_St_Clair_Tuition_Fees.csv")




#Carlton Trail
CarltonTrailPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Carlton_Trail_1of2-(Crawl-Run)---2019-09-05T131012Z.csv")
remove(Fees)
length(CarltonTrailPrograms$Program_link)
length(unique(CarltonTrailPrograms$Program_link))

CarltonTrailPrograms <- CarltonTrailPrograms %>% 
  distinct(Program_link, Program, Campus)


require(devtools)
source_gist(4676064)


for(prog in 1:length(CarltonTrailPrograms$Program)){
  url <- CarltonTrailPrograms$Program_link[prog]
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes("div.pricingNotes") %>% 
      html_nodes("p") %>% 
      html_text()
    
    
    if(length(webPageData) > 0){
      print(paste("Processing:", CarltonTrailPrograms$Program[prog]))
      
      webPageData <- webPageData %>% 
        strsplit(":|-") %>% 
        as.data.frame() 
      
      colnames(webPageData) <- paste0("Col", 1:ncol(webPageData))
      
      if(ncol(webPageData) > 1){
        webPageData <- webPageData %>% 
          mutate(timeFrame = zoo::na.locf(ifelse(grepl("Semester|Year", Col1, ignore.case = T), as.character(Col1), NA), na.rm = F),
                 timeFrame = ifelse(is.na(timeFrame), "No Semester given", timeFrame)) %>% 
          filter(Col1 != timeFrame) %>% 
          filter(!is.na(Col2)) %>% 
          mutate(Program = CarltonTrailPrograms$Program[prog],
                 ProgramUrl = CarltonTrailPrograms$Program_link[prog],
                 Campus = CarltonTrailPrograms$Campus[prog])
        
        if(!exists("Fees")){
          Fees <- webPageData
        } else {
          Fees <- rbind(Fees, webPageData)
        }
      }
      

      

    }
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Carlton_Trails_Tuition_Fees.csv")


#Col Mathieu

#should be a manual grab

url <- "https://www.collegemathieu.sk.ca/info/techniques-en-administration-des-affaires.html"

#Carlton Trail
ColMathieuPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Col_Mathieu_1of2-(Crawl-Run)---2019-09-05T163212Z.csv")
remove(Fees)
length(ColMathieuPrograms$Program_link)
length(unique(ColMathieuPrograms$Program_link))

ColMathieuPrograms <- ColMathieuPrograms %>% 
  distinct(Program_link, Program)

for(prog in 1:length(ColMathieuPrograms$Program)){
  url <- ColMathieuPrograms$Program_link[prog]
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes("table") %>% 
      html_table(fill = T)
    
    if(length(webPageData) <= 3){
      print(paste("Processing:", ColMathieuPrograms$Program[prog]))
      
      overview <- webPageData[[1]]
      
      if(ncol(overview) == 6){
        
        if(!TRUE %in% grepl("Durée", overview[1,], ignore.case = T)) {
          overview <- overview %>% 
            mutate(Duration = NA)
          overview <- overview[,c(1:2,7,3:6)]
        } else if(!TRUE %in% grepl("Admission", overview[1,], ignore.case = T)) {
          overview <- overview %>% 
            mutate(Admission = ifelse(grepl("Admission", X6), gsub("(\\))(.*$)", "\\1", X6), NA),
                   X6 = ifelse(!is.na(Admission), gsub("(.*\\))(.*$)", "\\2", X6), X6))
          overview <- overview[,c(1:5,7,6)]
          
          #"Admission : 35$ (non remboursable)\n\n\t\t\t**Scolarité : 6123,50$\n\t\t\t**Livres : 1450$"
          
        }
        
      }
      
      colnames(overview) <- c("Credential", "CreditNumber", "Duration", "Composition", "Method", "Admission", "Tuition")
      overview <- overview[2:nrow(overview),] %>% 
        mutate(Admission = gsub("(\\))(\n\n\t\t\t)(\\+)", "\\1_\\3", Admission)) %>% 
        mutate(Tuition = gsub("\\*", "", Tuition),
               Tuition = gsub("(Scolarité: Scolarité : |)([[:digit:]])([[:space:]])*([[:digit:]]{1,})(\\$)(\n\n\t\t\t)(Livres|Autre)", "\\1\\2\\3\\4\\5_\\7", Tuition ),
               CreditNumber = ifelse(!grepl("\n\n\t\t\t|\n\t\t\t", Credential), gsub("\n\n\t\t\t|\n\t\t\t", "", CreditNumber), CreditNumber),
               Duration = ifelse(!grepl("\n\n\t\t\t|\n\t\t\t", Credential), gsub("\n\n\t\t\t|\n\t\t\t", "", Duration), Duration),
               Composition = ifelse(!grepl("\n\n\t\t\t|\n\t\t\t", Credential), gsub("\n\n\t\t\t|\n\t\t\t", "", Composition), Composition),
               Method = ifelse(!grepl("\n\n\t\t\t|\n\t\t\t", Credential), gsub("\n\n\t\t\t|\n\t\t\t", "", Method), Method),
               Admission = ifelse(!grepl("\n\n\t\t\t|\n\t\t\t", Credential), gsub("\n\n\t\t\t|\n\t\t\t", "", Admission), Admission),
               Tuition = ifelse(!grepl("\n\n\t\t\t|\n\t\t\t", Credential), gsub("\n\n\t\t\t|\n\t\t\t", "", Tuition), Tuition)) %>% 
        separate_rows(Credential, CreditNumber,  Duration,  Composition, Tuition, sep = "\n\n\t\t\t|\n\t\t\t") %>% 
        separate(Tuition, c("Total", "Other"), sep = "_") %>% 
        mutate(Program = ColMathieuPrograms$Program[prog],
               ProgramUrl = ColMathieuPrograms$Program_link[prog])
      
      if(!exists("Fees")){
        Fees <- overview
      } else {
        Fees <- rbind(Fees, overview)
      }
      
      
      
    } else if (length(webPageData) < 3 & length(webPageData) > 0){
      print(webPageData)
    }
    
    
    
  }
}


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_College_Mathieu_Tuition_Fees.csv")







#Cumberland

url <- "https://www.cumberlandcollege.sk.ca/index.php?id=7"
webPage <- read_html(url)

urls <- webPage %>% 
  html_nodes("div.menu_item") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  paste0("https://www.cumberlandcollege.sk.ca", .)


url <- "https://www.cumberlandcollege.sk.ca/index.php?id=46"

remove(Fees)
for(url in 1:length(urls)){
  print(url)
  webPage <- read_html(urls[url])
  
  Program <- webPage %>% 
    html_nodes("h1") %>% 
    html_text()
  
  webPage %>% 
    html_nodes("b")
  
  webPageData <- webPage %>% 
    html_nodes(xpath = "/html/body/div[2]/div[9]") %>% 
    html_text() %>% 
    strsplit("\n") %>% 
    unlist()
  
  if(length(webPageData) > 0) {
    print(paste("Processing:", urls[url]))
    programData <- tibble(Program = Program,
                          TuitionData = webPageData[grepl("Tuition:", webPageData)],
                          ProgramUrl = urls[url])
    if(!exists("Fees")){
      Fees = programData
    } else {
      Fees = rbind(Fees, programData)
    }
  }

  
}

FeesClean <- Fees %>% 
  mutate(TuitionData = gsub("\\*", ":", TuitionData),
         TuitionData = gsub("([[:digit:]])([A-Z])", "\\1:\\2", TuitionData)) %>% 
  separate(TuitionData, c("Category", "feeValue","Category2", "feeValue2", "Category3", "feeValue3", "Category4", "feeValue4", "Category5", "feeValue5", "Category6", "feeValue6"), ":") %>% 
  mutate(Tuition = ifelse(Category == "Tuition", feeValue, NA),
         Books = ifelse(Category2 == "Books & Supplies", feeValue2, NA),
         studentFees = ifelse(Category3 == "Student Fees", feeValue3, NA),
         Health = ifelse(Category4 == "Health & Dental Plan", feeValue4, NA)) %>% 
  select(-c(Category:feeValue6)) %>% 
  gather(Category, feeValue, Tuition:Health)


write_csv(FeesClean, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Cumberland_Tuition_Fees.csv")

#Great Plains
GPPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Great_Plains_1of4-(Crawl-Run)---2019-09-06T170938Z.csv") %>% 
  rename(Program = `Field Content`,
         Program_link = `Field Content_link`) %>% 
  rbind(read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Great_Plains_2of4-(Crawl-Run)---2019-09-06T170948Z.csv") %>% 
          rename(Program = `Field Content`,
                 Program_link = `Field Content_link`)) %>% 
  rbind(read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Great_Plains_3of4-(Crawl-Run)---2019-09-06T171117Z.csv"))

write_csv(GPPrograms, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/Great_Plains_links.csv")


remove(Fees)
for(prog in 1:length(GPPrograms$Program_link)){
  url <- GPPrograms$Program_link[prog]
  print(prog)
  
  webPage <- read_html(url)%>% 
    html_nodes("div.program__info")
  
  webPageLabels <- webPage %>% 
    html_nodes("div.program__info_label") %>% 
    html_text()
  
  webPageData <- webPage %>% 
    html_nodes("div.program__info_content") %>% 
    html_text()
  
  webPageLocationData <- webPage %>% 
    html_nodes("div.program__info_multiple") %>% 
    html_text()
  
  if(length(webPageLabels) > 6){
    print(paste("Proceeding with:", GPPrograms$Program[prog]))
    webPageData <- tibble(Labels = webPageLabels[c(1:2, 4:7)],
                          Data = webPageData[c(1:6)],
                          Program = GPPrograms$Program[prog],
                          ProgramUrl = GPPrograms$Program_link[prog])
    
    if(!exists("Fees")){
      Fees <- webPageData
    } else {
      Fees <- rbind(Fees, webPageData)
    }
  }
  
}


FeesClean <- Fees %>%
  spread(Labels, Data)

write_csv(FeesClean, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Great_Plains_Tuition_Fees.csv")

#Northwest

NorthwestPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Northwest_1of2-(Crawl-Run)---2019-09-06T175408Z.csv")
remove(Fees)
for(prog in 1:length(NorthwestPrograms$Program_link)){
  url <- NorthwestPrograms$Program_link[prog]
  print(prog)
  
  
  webPage <- read_html(url)
  
  webPageDeriv <- webPage %>% 
    html_nodes("div.location-block") 
  
  if(length(webPageDeriv) > 0){
    print(paste("Proceeding with:", NorthwestPrograms$Program[prog]))
    
    webPageDeriv <- webPageDeriv %>% 
      html_text() %>% 
      gsub("[[:space:]]{2,}", "", .) %>% 
      strsplit("(?<=[[:punct:]]|[[:digit:]]|[a-z])(?=[A-Z])", perl = T) %>% 
      unlist()
    
    ProgFees <- tibble(Data = webPageDeriv,
                       Program = NorthwestPrograms$Program[prog],
                       ProgramUrl = NorthwestPrograms$Program_link[prog])
    if(!exists("Fees")){
      Fees <- ProgFees
    } else {
      Fees <- rbind(Fees, ProgFees)
    }
    
    
  } else {
    
    webPageDeriv <- webPage %>% 
      html_nodes("div.prog-desc") 
    
    if(length(webPageDeriv) > 0) {
      print("Moving on")
      webPageDeriv <- webPageDeriv%>% 
        html_text() %>% 
        strsplit("\r") %>% 
        unlist()
      
      webPageDeriv <- webPageDeriv[grepl("tuition", webPageDeriv, ignore.case = T)]
      
      if(length(webPageDeriv) > 0) {
        print(paste("Moving on and proceeding with:", NorthwestPrograms$Program[prog]))
        webPageDeriv %>% 
          strsplit("(?<=[[:punct:]]|[[:digit:]]|[a-z])(?=[A-Z])", perl = T) %>% 
          unlist()
        
        ProgFees <- tibble(Data = webPageDeriv,
                           Program = NorthwestPrograms$Program[prog],
                           ProgramUrl = NorthwestPrograms$Program_link[prog])
        if(!exists("Fees")){
          Fees <- ProgFees
        } else {
          Fees <- rbind(Fees, ProgFees)
        }
      } else {
        print(url)
      }
      
    }
    
  }
}


FeesClean <- Fees %>%
  separate(Data, c("Category", "Value"), sep = ":")

write_csv(FeesClean, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Northwest_Tuition_Fees.csv")

#Northlands
#Individual pdfs a la :https://trainnorth.ca/ckfinder/userfiles/files/BachSc_pb.pdf
Northlands <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Northlands_2of2-(Crawl-Run)---2019-09-06T185642Z.csv")

Northlands <- Northlands %>% 
  mutate(ProgramBulletin_link = gsub(".*;", "", ProgramBulletin_link)) %>% 
  mutate(ProgramBulletin_link = gsub("^[[:space:]]*", "", ProgramBulletin_link))



remove(Fees)
for(pdf in 1:length(Northlands$ProgramBulletin_link)){
  print(pdf)
  if(grepl("pdf", Northlands$ProgramBulletin_link[pdf])) {
    url = Northlands$ProgramBulletin_link[pdf]
    dataset = pdftools::pdf_text(url) %>% 
      strsplit("\n") %>% 
      unlist()
    dataset = dataset[grepl("Tuition", dataset, ignore.case = T)] 
    
    if(length(dataset) > 0 ){
      print(paste("Adding:", Northlands$Program[pdf]))
      dataset = dataset %>% 
        strsplit("[[:space:]]{2,}") %>% 
        unlist()
      
      dataset = tibble(Data = dataset,
                       Program = Northlands$Program[pdf],
                       ProgramUrl = Northlands$url[pdf])
      
      if(!exists("Fees")){
        Fees <- dataset
      } else {
        Fees <- rbind(Fees, dataset)
      }
    }
  }
}

FeesClean <- Fees %>% 
  mutate(Data = gsub("^[[:space:]]*", "", Data),
         Data = gsub("([[:digit:]])( )(Books/Fees)(: )(\\$1,040)", "\\1_\\3:\\5", Data)) %>% 
  separate_rows(Data, sep = "_") %>% 
  separate(Data, c("Category", "FeeValue"), sep = ":") %>% 
  separate(FeeValue, c("FeeValue", "TimeFrame"), sep = "/") %>% 
  filter(!is.na(FeeValue))



write_csv(FeesClean, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Northlands_Tuition_Fees.csv")

#Parkland

Parkland <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Parkland_1of2-(Crawl-Run)---2019-09-06T192916Z.csv")
remove(Fees)


for(prog in 1:length(Parkland$Program)){
  print(prog)
  url <- Parkland$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes("div.row-fluid") %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      unlist()
    
    webPageData <- webPageData[grepl("Tuition", webPageData, ignore.case = T)] 
    
    
    if(length(webPageData) > 0 ) {
      print(paste("Proceeding with:", Parkland$Program[prog]))
      webPageData <- webPageData %>% 
        strsplit("(?<=[[:digit:]]|[a-z])(?=[A-Z])", perl = T) %>% 
        unlist()
      
      webPageData = tibble(Data = webPageData,
                           Program = Parkland$Program[prog],
                           ProgramUrl = Parkland$Program_link[prog])
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
      
    }
  }
  
}

FeesClean <- Fees %>% 
  mutate(Data = gsub("^[[:space:]]*", "", Data)) %>% 
  #separate_rows(Data, sep = "_") %>% 
  separate(Data, c("Category", "CategoryValue"), sep = ":")# %>% 
#separate(FeeValue, c("FeeValue", "TimeFrame"), sep = "/") %>% 
#filter(!is.na(FeeValue))

write_csv(FeesClean, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Parkland_Tuition_Fees.csv")




#SIIT

SIIT <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_SIIT_1of2-(Crawl-Run)---2019-09-09T133017Z.csv")
remove(Fees)
url <- "http://siit.ca/programs/power-engineering/"

for(prog in 1:length(SIIT$Program)){
  print(prog)
  url <- SIIT$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes("div.et_pb_text_inner") %>% 
      html_nodes("table") %>% 
      html_table()
    
    if(length(webPageData) > 0 ) {
      names(webPageData) <- seq_along(webPageData)
      TuiPos <- unlist(lapply(webPageData, function(x) if(length(grep("Tuition", colnames(x))) > 0) 1 else NA))
      TuiPos <- Filter(Negate(is.na), TuiPos)
      
      webPageData <- webPageData[names(TuiPos)] 
      if(length(webPageData) > 0){
        
        print(paste("Proceeding with:", SIIT$Program[prog]))
        
        webPageDataNames <- webPage %>% 
          html_nodes("div.et_pb_text_inner") %>% 
          html_nodes("h6") %>% 
          html_text()
        if(!grepl("U-Pass", webPageDataNames[1])){
          names(webPageData) <- webPageDataNames[1:length(webPageData)]
        } else {
          names(webPageData) <- letters[1:length(webPageData)]
        }
        
        
        columnNumbers <- unlist(lapply(webPageData, function(x) ncol(x)))
        
        webPageData <- webPageData[columnNumbers %in% columnNumbers[1]] %>% 
          #unlist() %>% 
          bind_rows(.id = "name") %>% 
          filter(!grepl("Please note", Tuition)) %>% 
          gather( category, feeValue, 2:ncol(.)) %>% 
          mutate(Program = SIIT$Program[prog],
                 ProgramUrl = SIIT$Program_link[prog])
        
        


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_SIIT_Tuition_Fees.csv")



# Sask Poly
SaskPoly <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Saskatchewan_Polytechnic_1of2-(Crawl-Run)---2019-09-09T150933Z.csv")
remove(Fees)



url <- "https://saskpolytech.ca/programs-and-courses/programs/Aboriginal-Policing-Preparation.aspx"

remove(Fees)
for(prog in 1:length(SaskPoly$Program)){
  print(prog)
  url <- SaskPoly$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage  <- read_html(url)
    headers <- webPage %>% 
      html_nodes("div.programsectionheader") %>% 
      html_nodes("h2") %>% 
      html_text()
    
    dataPoints <- webPage %>% 
      html_nodes("div.detail-row") %>% 
      html_text()
    
    names(dataPoints) <- headers
    
    dataPoints <- dataPoints[grep("Tuition|Fees", names(dataPoints), ignore.case = T)]
    
    if(length(dataPoints) > 0 ) {
      webPageData <- tibble(data = dataPoints,
                            program = SaskPoly$Program[prog],
                            programUlr = SaskPoly$Program_link[prog])
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
    
    
  }
}


FeesClean <- Fees %>% 
  separate(data, letters[1:6], "\n") %>% 
  mutate(f = ifelse(grepl("\\$", b), e, f),
         e = ifelse(grepl("\\$", b), d, e),
         d = ifelse(grepl("\\$", b), c, d),
         c = ifelse(grepl("\\$", b), b, c),
         b = ifelse(grepl("\\$", b), NA, b)) %>% 
  select(-c(a,b)) %>% 
  separate(c, paste("time", letters[1:4], sep = "_"), sep = "(?<=[[:digit:]])(?=[A-Z])") %>% 
  mutate(d = ifelse(!grepl("\\$", d), NA, d),
         e = ifelse(!grepl("\\$", e), NA, e),
         f = ifelse(!grepl("\\$", f), NA, f)) %>% 
  gather(Category, Value, time_a:f) %>% 
  separate(Value, c("Time", "feeValue"), " - ") %>% 
  mutate(feeValue = ifelse(grepl("\\$", Time), Time, feeValue),
         Time = ifelse(grepl("\\$", Time), NA, Time)) %>% 
  select(-Category)


write_csv(FeesClean, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_SaskPoly_Tuition_Fees.csv")


#Southeast
Southeast <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Southeast_1of2-(Crawl-Run)---2019-09-09T160825Z.csv")

url <- "https://www.southeastcollege.org/courses/health-information-management/"

remove(Fees)
for(prog in 1:length(Southeast$Program)){
  print(prog)
  url <- Southeast$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    
    webText <- webPage %>% 
      html_nodes("p") %>% 
      html_text()
    
    webPageData <- tibble(Tuition = webText[grepl("Tuition", webText, ignore.case = T)],
                          Materials = webText[grepl("Materials", webText, ignore.case = T)],
                          program = Southeast$Program[prog],
                          programUlr = Southeast$Program_link[prog])
    
    if(!exists("Fees")){
      Fees <- webPageData
    } else {
      Fees <- rbind(Fees, webPageData)
    }
  }
  
  
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Southeast_Tuition_Fees.csv")

#AU Arts

url <- "https://www.auarts.ca/current-students/pay-tuition-and-fees/undergraduate-tuition-and-fees"

webPage <- read_html(url)

Undergraduate <- webPage %>% 
  html_nodes("table") %>% 
  html_table()



FullTui <- Undergraduate[[1]]

colnames(FullTui) <- gsub("\n|\r|\t", "", colnames(FullTui))
colnames(FullTui) <- paste(colnames(FullTui), FullTui[1,])
colnames(FullTui)[1] <- "FeeCat"
FullTui <- FullTui[-1,]
FullTui <- FullTui %>% 
  gather(StudentType, feeValue, 2:5) %>% 
  spread(FeeCat, feeValue) %>% 
  mutate(Connector = paste("Connect", row.names(.), sep = "_"))


Fees <- Undergraduate[[5]] %>% 
  mutate(X1 = gsub(" -.*", "", X1))

Fees <- Fees[1:7,]%>% 
  mutate(Connector = "Connect") %>% 
  spread(X1, X2)

Fees <- Fees %>% 
  mutate(Connector = "Connect_1") %>% 
  bind_rows(Fees %>% 
              mutate(Connector = "Connect_2")) %>% 
  bind_rows(Fees %>% 
              mutate(Connector = "Connect_3")) %>% 
  bind_rows(Fees %>% 
              mutate(Connector = "Connect_4"))


ProgramSpe <- Undergraduate[[4]] %>% 
  gather(category, value, 2:ncol(.)) %>% 
  mutate(value = as.numeric(gsub("\\$|,", "", value))) %>% 
  group_by(`Major/Area of Study`) %>% 
  summarize(meanProgramSpecific = mean(value, na.rm = T))


ProgramSpe <- ProgramSpe %>% 
  mutate(Connector = "Connect_1") %>% 
  bind_rows(ProgramSpe %>% 
              mutate(Connector = "Connect_2")) %>% 
  bind_rows(ProgramSpe %>% 
              mutate(Connector = "Connect_3")) %>% 
  bind_rows(ProgramSpe %>% 
              mutate(Connector = "Connect_4")) %>% 
  left_join(FullTui, by = "Connector") %>% 
  left_join(Fees, by = "Connector") %>% 
  gather(Category, feeValue, c(2,5:ncol(.))) %>% 
  arrange(`Major/Area of Study`) %>% 
  mutate(timePeriod = gsub("(.* per )(.*)", "\\2", StudentType),
         StudentType = gsub("(.*)( per .*)", "\\1", StudentType)) %>% 
  select(-Connector) %>% 
  spread(StudentType, feeValue)

url <- "https://www.auarts.ca/current-students/pay-tuition-and-fees/graduate-tuition-and-fees"

webPage <- read_html(url)

Graduate <- webPage %>% 
  html_nodes("table") %>% 
  html_table()



FullTui <- Graduate[[1]]

FullTui <- FullTui %>% 
  gather(timePeriod, feeValue, c(2,4)) %>% 
  select(-Occurance) %>% 
  spread(`Graduate student profile`, feeValue) %>% 
  mutate(Major = "Graduate Degree",
         Category = "TOTAL FEES") %>% 
  select(Major, Category, timePeriod:`International student status`)

ProgramSpe <- ProgramSpe %>% 
  bind_rows(FullTui)

  
write_csv(ProgramSpe, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_AU_Arts_Tuition_Fees.csv")


#Bow Valley

#in addition to the below, there is good data on individual program websites
#Get individual data + url - not enough matches

url <- "https://bowvalleycollege.ca/admissions/tuition-and-fees"
webPage <- read_html(url)
webPageData <- webPage %>% 
  html_nodes("div.col-xs-12") %>% 
  html_nodes("table") %>% 
  html_table()


webPageHeaders <- webPage %>% 
  html_nodes("div.col-xs-12") %>% 
  html_nodes("h5") %>% 
  html_text()

names(webPageData) <- webPageHeaders[2:5]

write_csv(webPageData[[1]], "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Bow_Valley_Tuition_Fees.csv")


Institution = "Bow_Valley"

url <- "https://bowvalleycollege.ca/programs-courses-search#f:Programs=[Business]"

library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)

XpathPossibleError <- function(xpathValue){
  tryCatch(
    remDr$findElement(using = "xpath", xpathValue),
    error = function(e) e
  )
}

remDr$navigate(url)
Sys.sleep(2)
webElem <- remDr$findElement(using = "xpath", '/html/body/div[1]/div/div/div[2]/div/div[2]/div/div[2]/div/div[4]/div[2]/ul/li[3]/a')
webElem$clickElement()

webElem <- remDr$findElements(using = "css", "div.program-template-container")
programs <- sapply(webElem, function(x){x$getElementText()}) %>% 
  unlist() %>% 
  strsplit("\n") %>% 
  purrr::map(~ tibble(Credential = .[1],
                      Program = .[2],
                      Description = .[3])) %>% 
  bind_rows()

webElem <- remDr$findElements(using = "css", "a.headerLink")

programs$urls <-sapply(webElem, function(x){x$getElementAttribute("href")}) %>% 
  unlist()

webElem <- remDr$findElement(using = "xpath", '/html/body/div[1]/div/div/div[2]/div/div[2]/div/div[2]/div/div[4]/div[5]/ul/li[2]/a')
webElem$clickElement()
Sys.sleep(2)

webElem <- remDr$findElements(using = "css", "div.program-template-container")
programs <- programs %>% 
  bind_rows(sapply(webElem, function(x){x$getElementText()}) %>% 
              unlist() %>% 
              strsplit("\n") %>% 
              purrr::map(~ tibble(Credential = .[1],
                                  Program = .[2],
                                  Description = .[3])) %>% 
              bind_rows())

webElem <- remDr$findElements(using = "css", "a.headerLink")

programs$urls <- c(programs$urls[!is.na(programs$urls)], sapply(webElem, function(x){x$getElementAttribute("href")}) %>% 
                     unlist())

remDr$close()

d <- programs %>% 
  select(Program, urls) %>% 
  rename(url = urls) %>% 
  mutate(Tuition = NA)

remDr <- rD$client
remDr$open(silent = T)

for(prog in 1:nrow(d)){
  print(prog)
  
  url <- d$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    remDr$navigate(url)
    Sys.sleep(sample(3:4, 1))
    print("Up to 4 seconds")
    
    introElementErrorCheck = suppressMessages(XpathPossibleError('/html/body/div[1]/div/div/div[2]/div/section[2]/div/div/div/div[3]/div[4]/div/div/p'))
    if(!inherits(introElementErrorCheck, "error")) {
      webElem <- remDr$findElement(using = "xpath", '/html/body/div[1]/div/div/div[2]/div/section[2]/div/div/div/div[3]/div[4]/div/div/p')
      Tuition <- webElem$getElementText() %>% 
        unlist()
      
      if(length(Tuition) > 0){
        d$Tuition[prog] <- Tuition
      }
    }
  }
}

tuition <- d %>% 
  filter(Tuition != "") %>% 
  mutate(Tuition = gsub("International", "\\|International", Tuition)) %>% 
  separate(Tuition, c("Domestic", "International"), sep = "\\|") %>% 
  mutate(Domestic = gsub("^.*: CAD ", "", Domestic),
         International = gsub("^.*: CAD ", "", International))

write_csv(tuition, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Bow_Valley_Tuition_Fees.csv")


#Grande Prairie
require(devtools)
source_gist(4676064)

GraPraTui <- pdftools::pdf_text("https://www.gprc.ab.ca/files/forms_documents/1920_Estimated_Program_Costs.pdf") %>%
  strsplit("\n") %>% 
  unlist() %>% 
  strsplit("\r") %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("([[:digit:]])([[:space:]])([[:digit:]])", "\\1_\\3", .) %>% 
  strsplit("_")

lapply(GraPraTui, function(x) if(length(x) != 7) print(x))

GraPraTuiColNames <- unlist(GraPraTui[2])

GraPraTui <- GraPraTui[c(3:42,45:(length(GraPraTui)-1))]


GraPraTui %>% 
  bind_cols() %>% 
  gather(category, value) %>% 
  mutate(category_2 = rep(c("Program", "Year", "Start_date", "End_date", "Tuition", "Fees", "Books"), nrow(.)/7)) %>% 
  spread(category_2, value) %>% 
  mutate(category = as.numeric(gsub("V", "", category)),
         Program = ifelse(Program == "", NA, Program)) %>% 
  arrange(category) %>% 
  mutate(Program = zoo::na.locf(Program)) %>% 
  select(-category) %>% 
  gather(category, value, c(1, 3, 6)) %>% 
  separate_rows(Year, sep = ",") %>% 
  select(Program, Year, Start_date, End_date, category, value) %>% 
  write_csv("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Grande_Prairie_Tuition_Fees.csv")



#Keyano

KeyanoTuiAll <- pdftools::pdf_text( "https://www.keyano.ca/en/student-services/resources/Documents/Credit-Calendars/18.19-CreditCalendar.pdf")

KeyanoTuiAll <- KeyanoTuiAll[grepl("FEES", KeyanoTuiAll)]
KeyanoTui <- KeyanoTuiAll[6:7]

KeyanoTui <- KeyanoTui %>% 
  strsplit("(?<=[0-9]|varies|[A-Z])(\r\n)", perl = T) 

FullTime <- KeyanoTui[[1]]
FullTime <- FullTime[c(9:10, 12:29)] %>% #Add items 31:33 later 
  gsub("([[:digit:]])(              )(\\$)", "\\1_NA_\\3", .) %>% 
  gsub("([[:digit:]])( )(\\$)", "\\1_\\3", .) %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("^ ", "", .) %>% 
  gsub("\r\n", "", .) %>% 
  strsplit("_")



sapply(FullTime, function(x) length(x))
FullTime[[18]] <- FullTime[[18]][3:12]
FullTime[[9]][1] <- paste(FullTime[[9]][1], FullTime[[10]][1])
FullTime[[10]] <- FullTime[[10]][2:11]

for(prog in 1:length(FullTime)){
  if(length(FullTime[[prog]]) == 9){
    FullTime[[prog]] <- c(FullTime[[prog]], NA)
    #print(prog)
  }
}

names(FullTime) <- 1:length(FullTime)

FullTime <- FullTime %>% 
  bind_rows() %>% 
  gather(Category, value, 1:ncol(.)) %>% 
  mutate(Category = as.numeric(Category)) %>% 
  mutate(Category2 = rep(c("Program", "Tuition_per_term", "SA_Fee", "SA_Building_Fee", "Rec_Athl", "Registration", "Health_dental","Tech", "Total", "Lab_fee_per_course"), nrow(.)/10)) %>% 
  spread(Category2, value) %>% 
  select(-Category) %>% 
  gather(Category, feeValue, c(1:2, 4:ncol(.))) %>% 
  arrange(Program)


PartTime <- KeyanoTui[[2]]
Trades <- PartTime[c(35:45)] %>% 
  gsub("([[:digit:]])( )(\\$)", "\\1_\\3", .) %>%  
  gsub("\r\n", "", .) %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("^ ", "", .) %>%
  strsplit("_")
PartTime <- PartTime[c(8:27)] %>% 
  gsub("([[:digit:]])( )(\\$)", "\\1_\\3", .) %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("^ ", "", .) %>% 
  gsub("\r\n", "", .) %>% 
  strsplit("_")


sapply(PartTime, function(x) length(x))

for(prog in 1:length(PartTime)){
  if(length(PartTime[[prog]]) == 7){
    PartTime[[prog]] <- c(PartTime[[prog]], NA)
    #print(prog)
  }
}
names(PartTime) <- 1:length(PartTime)

PartTime <- PartTime %>% 
  bind_rows() %>% 
  gather(Category, value, 1:ncol(.)) %>% 
  mutate(Category = as.numeric(Category)) %>% 
  mutate(Category2 = rep(c("Program", "Per_credit", "SA_Fee", "SA_Building_Fee", "Rec_Athl", "Registration","Tech", "Lab_fee_per_course"), nrow(.)/8)) %>% 
  spread(Category2, value) %>% 
  select(-Category) %>% 
  gather(Category, feeValue, c(1:2, 4:ncol(.))) %>% 
  arrange(Program)



sapply(Trades, function(x) length(x))

names(Trades) <- 1:length(Trades)

Trades <- Trades %>% 
  bind_rows() %>% 
  gather(Category, value, 1:ncol(.)) %>% 
  mutate(Category = as.numeric(Category)) %>% 
  mutate(Category2 = rep(c("Program", "Tuition", "SA_Fee", "SA_Building_Fee", "Rec_Athl", "Registration","Health_dental", "Tech", "Materials", "Total"), nrow(.)/10)) %>% 
  spread(Category2, value) %>% 
  select(-Category) %>% 
  gather(Category, feeValue, c(1:2, 4:ncol(.))) %>% 
  arrange(Program)

KeyanoTui <- FullTime %>% 
  mutate(ProgType = "Full Time") %>% 
  bind_rows(PartTime %>% 
              mutate(ProgType = "Part Time")) %>% 
  bind_rows(Trades %>% 
              mutate(ProgType = "Trades"))


write_csv(KeyanoTui, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Keyano_Tuition_Fees.csv")


#Lakeland
url <- "https://www.lakelandcollege.ca/admissions/tuition-and-fees/fee-calculator/"

webPage <- read_html(url)
webPage %>% 
  html_nodes("div.step__content") %>% 
  html_nodes("select") %>% 
  html_text()


#https://ropensci.github.io/RSelenium/articles/basics.html
#library(RSelenium)
#rD <- rsDriver(browser = "firefox", verbose = FALSE)
#rD
#
#remDr <- rD$client
#remDr$open(silent = T)
#Sys.sleep((5))
#remDr$navigate(url)
#webElem <- remDr$findElement(using = "id", "select--program")
##Returns random values...
#webElem$sendKeysToElement(list("Animal Health - Year 1"))
#
#remDr$close()
#rD$server$stop()

#workaround:
#This is the file that populates the calculator
url <- "http://lakelandcollege.ca/files/tuition-calculator/fees.csv"

LakelandTui <- read_csv(url)

LakelandTui <- LakelandTui %>% 
  gather(Category, feeValue, 3:ncol(.))

write_csv(LakelandTui, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Lakeland_Tuition_Fees.csv")


#Fleming

FlemingPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Fleming_1of2-(Crawl-Run)---2019-08-29T170404Z.csv")


url <- paste0(gsub("a-z", "", FlemingPrograms$url[2]), gsub(" ", "-", FlemingPrograms$Program1[2]), "#tuition-detail")


#Get cleaner urls!
library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
#rD
remDr <- rD$client
remDr$open(silent = T)

remove(Fees)
#weed out non responsive links...
#116, 121, 127
for(prog in 128:nrow(FlemingPrograms)){
  print(prog)
  print(FlemingPrograms$Program[prog])
  url <- paste0(FlemingPrograms$Program_link[prog], "#tuition-detail")
  Sys.sleep(sample(2:5, 1))
  remDr$open(silent = T)
  remDr$navigate(url)
  webElem <- remDr$findElement(using = "xpath", '//*[@id="tuition-breakdown-chart"]/div/h5/a')
  #Returns random values...
  webElem$sendKeysToElement(list(key = "enter"))
  
  webElems <- remDr$findElements(using = "css selector", "td")
  tuition <- unlist(lapply(webElems, function(x) {x$getElementText()}))
  if(prog == 56){
    tuition <- tuition[22:373]
  }
  if(prog == 61){
    tuition <- tuition[122:231]
  }
  if(prog == 77){
    tuition <- tuition[36:491]
  }
  tuition <- tibble(Index = rep(1:2, length(tuition)/2),
                    tuitionData = tuition,
                    id = rep(1:(length(tuition)/2), each = 2))
  tuition <- tuition %>% 
    spread(Index, tuitionData) %>% 
    arrange(id) %>% 
    mutate(Program = FlemingPrograms$Program[prog],
           ProgramUrl = FlemingPrograms$Program_link[prog])
  
  if(!exists("Fees")){
    Fees <- tuition
  } else {
    Fees <- rbind(Fees, tuition)
  }
  remDr$close()
  
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Fleming_Tuition_Fees.csv")

Fees <- read_csv("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Fleming_Tuition_Fees.csv")


#remDr$close()
rD$server$stop()



#Lethridge
Lethbridge <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Lethbridge_1of2-(Crawl-Run)---2019-09-12T123456Z.csv")
url <- "https://lethbridgecollege.ca/programs/agricultural-equipment-technician"


remove(Fees)
for(prog in 1:length(Lethbridge$Program)){
  print(prog)
  url <- Lethbridge$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    webPage %>% 
      html_nodes("div.views-element-container") %>% 
      html_nodes("table.views-table")
    
    if(length(webPage) > 0){
      print(Lethbridge$Program[prog])
      webPageData <- webPage %>% 
        html_table(fill = T) 
      
      webPageData <- webPageData[grepl("\\$", webPageData)]
      if(length(webPageData) > 0) {
        webPageData <- webPageData%>% 
          bind_rows() %>% 
          gather(Category, feeValue, 2:ncol(.)) %>% 
          mutate(Program = Lethbridge$Program[prog],
                 ProgramUrl = Lethbridge$Program_link[prog])
        colnames(webPageData)[1] <- "timePeriod"
        
        if(!exists("Fees")){
          Fees <- webPageData
        } else {
          Fees <- rbind(Fees, webPageData)
        }
      }
      
    }
    
  }
  
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Lethbridge_Tuition_Fees.csv")


#NorQuest

norquest <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_NorQuest_1of2-(Crawl-Run)---2019-09-12T133500Z.csv")
url <- paste0("https://www.norquest.ca/cal/2019-2020/program/veterinary-office-assistant/", "tuition.aspx")
remove(Fees)

for(prog in 1:length(norquest$Program)){
  print(prog)
  url <- paste0(norquest$Program_link[prog], "tuition.aspx")
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    
    webPageData <-  webPage %>% 
      html_nodes("div.norquest_tuition") %>% 
      html_text() %>% 
      gsub(": ", "\\1_", .) %>% 
      gsub("([[:digit:]])([A-Z])", "\\1_\\2", .) %>% 
      gsub("\r\n\r\n", ":", .) %>% 
      gsub("\r\n\t", "_", .) %>% 
      gsub("\r\n", "", .) %>% 
      strsplit("_") %>% 
      unlist()
    
    
    
    if(length(webPageData) > 0) {
      if(norquest$Program[prog] %in% c("Academic Upgrading", "Upgrading for ESL Youth", "ESL Intensive", "Apprenticeship Prep")) {
        webPageData <- webPageData[1:8]
      } 
      if (length(webPageData) == 9) {
        webPageData <- webPageData[1:8]
      }
      if (length(webPageData) > 9) {
        if(webPageData[9] == "Note") {
          webPageData <- webPageData[1:8]
        }
      }
      
      print(norquest$Program[prog])
      webPageData <- tibble(data = webPageData,
                            Index = rep(1:2, length(webPageData)/2), 
                            Id = rep(1:(length(webPageData)/4), each = 4))
      webPageData <- webPageData %>% 
        rowid_to_column() %>% 
        spread(Index, data) %>% 
        mutate(`1` = zoo::na.locf(`1`)) %>% 
        filter(!is.na(`2`)) %>% 
        separate(`1`, c("studentType", "tuitionType"), ":") %>% 
        mutate(tuitionType = ifelse(is.na(tuitionType), studentType, tuitionType),
               studentType = zoo::na.locf(ifelse(studentType == tuitionType, NA, studentType)),
               Program = norquest$Program[prog],
               ProgramUrl = norquest$Program_link[prog])
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
      
    }
    
  }
}
write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_NorQuest_Tuition_Fees.csv")


#Medicine hat

MHdom <- pdftools::pdf_text("https://www.mhc.ab.ca/-/media/Files/PDF/Services/FinancialAid/2019/2019-2020-FINAL-DOMESTIC-Program-Cost-Estimates_Apr-24.pdf?la=en&hash=986B1002997D4729FAD9CC58B07228F3EF6E9E18")
MHInt <- pdftools::pdf_text("https://www.mhc.ab.ca/-/media/Files/PDF/Services/FinancialAid/2019/2019-2020-FINAL-INTERNATIONAL-Program-Cost-Estimates.pdf?la=en&hash=E311F42040155635E9B63335DBB5F1C562C3E238")


MHdomSplit <- MHdom %>% 
  strsplit("(?<=[[:digit:]]|Total)(?=\r\n)", perl = T) %>% 
  unlist() %>% 
  #gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("\r\n[[:space:]]{2,}", " & ", .) %>% 
  gsub("^[[:space:]]{1,}&[[:space:]]|^\r\n", "", .) %>% 
  gsub("([[:space:]])(\\$)", "  \\2", .) %>% 
  #start here
  gsub("^[[:punct:]][[:space:]]{1,}", "", .) %>% 
  strsplit("[[:space:]]{2,}") 


MHdomSplit <- MHdomSplit[c(2:44, 47:(length(MHdomSplit) -1))]

#%>% 
#  strsplit("\r\n") %>% 


sapply(MHdomSplit, function(x) length(x))

MHdomSplit[grepl("&", MHdomSplit)]

MHdomSplit[[70]] <- MHdomSplit[[70]][2:8]

names(MHdomSplit) <- 1:length(MHdomSplit)
MHdomFees <- MHdomSplit %>% 
  bind_rows() %>% 
  gather(Category, Value) %>% 
  mutate(Index = rep(1:7, nrow(.)/7),
         Category = as.numeric(Category)) %>% 
  spread(Index, Value) %>% 
  gather(Category2, feeValue, 3:ncol(.)) %>% 
  arrange(Category, Category2) %>% 
  mutate(Category2 = rep(c("Tuition", "SA Fees", "NISF Fees", "Books & Supplies", "Health & Dental", "Total"), nrow(.)/6)) %>% 
  select(-Category) %>% 
  mutate(studentType = "domestic")

MHIntSplit <- MHInt %>% 
  strsplit("(?<=[[:digit:]]|Total)(?=\r\n)", perl = T) %>% 
  unlist() %>% 
  #gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("\r\n[[:space:]]{2,}", " & ", .) %>% 
  gsub("^[[:space:]]{1,}&[[:space:]]|^\r\n", "", .) %>% 
  gsub("([[:space:]])(\\$)", "  \\2", .) %>% 
  #start here
  gsub("^[[:punct:]][[:space:]]{1,}", "", .) %>% 
  strsplit("[[:space:]]{2,}") 


MHIntSplit <- MHIntSplit[c(2:44, 47:(length(MHIntSplit) -1))]
MHIntSplit[[70]] <- MHIntSplit[[70]][2:8]

names(MHIntSplit) <- 1:length(MHIntSplit)
MHIntFees <- MHIntSplit %>% 
  bind_rows() %>% 
  gather(Category, Value) %>% 
  mutate(Index = rep(1:7, nrow(.)/7),
         Category = as.numeric(Category)) %>% 
  spread(Index, Value) %>% 
  gather(Category2, feeValue, 3:ncol(.)) %>% 
  arrange(Category, Category2) %>% 
  mutate(Category2 = rep(c("Tuition", "SA Fees", "NISF Fees", "Books & Supplies", "Health & Dental", "Total"), nrow(.)/6)) %>% 
  select(-Category) %>% 
  mutate(studentType = "international")


Fees <- rbind(MHdomFees, MHIntFees)
colnames(Fees)[1] <- "Program"

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Medicine_Hat_Tuition_Fees.csv")






#NAIT

NAIT <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_NAIT_1of2-(Crawl-Run)---2019-09-12T162353Z.csv")


remove(Fees)

for(prog in 34:length(NAIT$Program)){
  print(prog)
  url <- paste0(NAIT$Program_link_link[prog], "?tiletabs=program-tuition")
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  #if(!NAIT$Program[prog] %in% c("Bachelor of Applied Information Systems Technology - Network Management", "Millwork and Carpentry")){
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    webPageData <- webPage %>% 
      html_nodes("div.program-details") %>% 
      html_nodes(xpath = '//*[@id= "program-tuition"]') %>% 
      html_text() %>% 
      strsplit("\r\n\r\n\r\n|\n\n\n\n\n|\n\n\n\n") %>% 
      unlist() 
    if(length(webPageData) > 0) {
      if(grepl("Cost Per Credit:", webPageData)) {
        print('yep')
        webPageData <- webPageData %>% 
          strsplit("\r\n\r\n|\n\n|\r\n            \r\n        \r\n        \r\n            |\n            \n        \n        \n           ") %>% 
          unlist() 
        
        webPageData <- webPageData[grepl("\\$", webPageData)] %>% 
          strsplit("\r\n\t|\n\t|\r\n            \r\n                |\n            \n                ") %>% 
          unlist() 
        
        webPageData <- webPageData[grepl("Cost Per Credit", webPageData, ignore.case = T)] %>% 
          gsub("([[:digit:]]|[a-z])([A-Z])", "\\1_\\2", .) %>% 
          gsub("\r\n            \r\n            ", "_", .) %>% 
          gsub("\r\n                |\n                ", " ", .) %>% 
          gsub("\n            \n            ", "_", .) %>% 
          strsplit("_") %>% 
          unlist()
        
        webPageHeaders  <- c("Tuition", webPageData[grepl("International Students", webPageData, ignore.case = T)])
        webPageData <- webPageData[!grepl("International students", webPageData, ignore.case = T)]
        webPageData <- tibble(data = webPageData[webPageData != "\r\n"]) %>% 
          separate(data, c("Category", "feeValue"), ":") %>% 
          mutate(timePeriod = NA,
                 Program = NAIT$Program[prog],
                 ProgramUrl = NAIT$Program_link_link[prog],
                 subProgram = NA,
                 studentType = rep(webPageHeaders, each = nrow(.)/length(webPageHeaders)))
        
        if(!exists("Fees")){
          Fees <- webPageData
        } else {
          Fees <- rbind(Fees, webPageData)
        }
      } else {
        
        
        webPageData <- webPageData[grepl("\\$", webPageData)]
        
        webPageData <- webPageData[!grepl("Additional information|Tuition cap", webPageData, ignore.case = T)] %>% 
          strsplit("\r\n\r\n|\n\n") %>% 
          unlist() %>% 
          strsplit("\r\n\t|\n\t")
        
        
        print(NAIT$Program[prog])
        webPageHeaders <- webPageData[grepl("Period", webPageData)] %>% 
          unlist()
        
        webPageData <- webPageData[!grepl("Period", webPageData)] %>% 
          unlist()
        smallHeaders <- webPageData[!grepl(":", webPageData)]
        
        if(NAIT$Program[prog] == "Roofer"){
          webPageData <- c(webPageData[1:5], "Books & Supplies:NA", webPageData[6:8], "Books & Supplies:NA", webPageData[9:10]) 
        }
        
        if(length(webPageData[grepl("Books", webPageData)]) == 1 & length(webPageHeaders) > 1){
          bookLabel = gsub("\\$.*", "NA", webPageData[grepl("Books", webPageData)])
          tuitionIndex <- grep("Tuition", webPageData, ignore.case = T)
          tuitionIndex <- tuitionIndex[2:length(tuitionIndex)]
          for(i in 1:length(tuitionIndex)){
            i = tuitionIndex[i] + ((i - 1) * 1)
            print(i)
            webPageData = c(webPageData[1:i], bookLabel, webPageData[(i + 1):length(webPageData)])
          }
        }
        
        webPageData <- webPageData[!grepl("\r\n.{1,}|^[[:space:]]$|^\n|Optional", webPageData, ignore.case = T)]
        webPageData <- webPageData[grepl(":", webPageData)]
        webPageData <- tibble(data = webPageData[webPageData != "\r\n"]) %>% 
          separate(data, c("Category", "feeValue"), ":") %>% 
          mutate(timePeriod = rep(webPageHeaders, each = nrow(.)/length(webPageHeaders)),
                 timePeriod = gsub("\r\n|\n", "", timePeriod),
                 Program = NAIT$Program[prog],
                 ProgramUrl = NAIT$Program_link_link[prog],
                 subProgram = gsub("\n", "", ifelse(length(smallHeaders) > 0, rep(smallHeaders, each = nrow(.)/length(smallHeaders)), Program)),
                 studentType = "Tuition")
        
        if(!exists("Fees")){
          Fees <- webPageData
        } else {
          Fees <- rbind(Fees, webPageData)
        }
      }
    }
    #}
  }
} 



write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_NAIT_Tuition_Fees.csv")







#Northern Lakes
#individual pdfs!
NLurls <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Northern_Lakes_1of2-(Crawl-Run)---2019-09-12T192457Z.csv")
#find pdf downloads
remove(Fees)
for(prog in 1:length(NLurls$Program)){
  print(prog)
  
  if(!(grepl("period", NLurls$Program[prog], ignore.case = T))){
    
    possibleError <- tryCatch(
      read_html(NLurls$Program_link[prog]),
      error = function(e) e
    )
    #if(!NAIT$Program[prog] %in% c("Bachelor of Applied Information Systems Technology - Network Management", "Millwork and Carpentry")){
    
    if(!inherits(possibleError, "error")){
      
      
      webPage <- read_html(NLurls$Program_link[prog])
      tuitionLinks <- webPage %>% 
        html_nodes("div.program-content") %>% 
        html_nodes("a") %>% 
        html_attr("href")
      
      tuitionLinks <- tuitionLinks[grepl("download/files", tuitionLinks)]
      
      
      #read and capture data.
      if(length(tuitionLinks) > 0){
        print(paste("looking at tuitionLinks for:", NLurls$Program[prog]))
        for(url in tuitionLinks){
          NL <- pdftools::pdf_text(url)
          NL <- NL %>% 
            strsplit("\r\n") %>% 
            unlist()
          
          TuitionRows <- NL[grepl("^.{,5}[A-Z]{4}[[:digit:]]{4}", NL)] %>% 
            gsub("^[[:space:]]{1,}", "", .) %>% 
            gsub("(\\$)([[:space:]]){1,}([[:digit:]])", "\\1\\3", .) %>% 
            gsub("([[:digit:]])([[:space:]]){1,}(\\$)", "\\1  \\3", .) %>%
            gsub("([[:digit:]])([[:space:]]){1,}([A-Z])", "\\1 \\3", .) %>%
            #Grabbing "Also offered" Term x variants
            gsub("(Term|term)([[:space:]]){,1}([[:digit:]])", "\\1\\3", .) %>% 
            gsub("([a-z])([[:space:]]{1,})([[:digit:]])", "\\1  \\3", .) %>% 
            gsub("(\\$)([[:digit:]]|[[:punct:]])*([[:space:]])([A-Z])", "\\1\\2  \\4", .) %>% 
            strsplit("[[:space:]]{2,}")
          
          TuitionRows <- TuitionRows[grepl("\\$", TuitionRows)]
          
          if(length(TuitionRows) > 0){
            
            print(paste("looking at tuitionnRows for:", NLurls$Program[prog]))
            
            #Checking for "core or opt" column and adjusting variables accordingly
            if(length(NL[grepl("Core", NL[1:7])]) > 0){
              for(datapoint in 1:length(TuitionRows)) {
                if(length(TuitionRows[[datapoint]]) == 10) {
                  TuitionRows[[datapoint]] <- c(TuitionRows[[datapoint]], NA)
                }
              }
              IndexNumber <- 11
              categoryNames <- c("Core/Opt", "Credits", "Tuition rate", "Tuition", "LRF rate", "LRF", "SA rate", "SA", "Total", "Also Offered")
              
            } else {
              for(datapoint in 1:length(TuitionRows)) {
                if(length(TuitionRows[[datapoint]]) == 9) {
                  TuitionRows[[datapoint]] <- c(TuitionRows[[datapoint]], NA)
                  if(datapoint == length(TuitionRows)){
                    IndexNumber <- 10
                    categoryNames <- c("Credits", "Tuition rate", "Tuition", "LRF rate", "LRF", "SA rate", "SA", "Total", "Also Offered")
                    
                  }
                }
                if(length(TuitionRows[[datapoint]]) == 7) {
                  TuitionRows[[datapoint]] <- c(TuitionRows[[datapoint]][1:4], "$500", TuitionRows[[datapoint]][5:7])
                  if(datapoint == length(TuitionRows)){
                    IndexNumber <- 8
                    categoryNames <- c("Credits", "Tuition rate", "Tuition", "LRF", "SA rate", "SA", "Total")
                  }
                }
              }
              
              
            }
            
            
            names(TuitionRows) <- 1:length(TuitionRows)
            
            TuitionData <- TuitionRows %>% 
              bind_rows() %>% 
              gather(Category, Value) %>% 
              mutate(Index = rep(1:IndexNumber, nrow(.)/IndexNumber),
                     Category = as.numeric(Category)) %>% 
              spread(Index, Value) %>% 
              gather(Category2, feeValue, 3:ncol(.)) %>% 
              mutate(Category2 = as.numeric(Category2)) %>% 
              arrange(Category, Category2) %>% 
              mutate(Category2 = rep(categoryNames, nrow(.)/(IndexNumber - 1))) %>% 
              select(-Category) %>% 
              separate(`1`, c("Course_code", "Course_name")) 
            
            TotalRows <- NL[grepl("Total", NL, ignore.case = T)] 
            TotalRows <- TotalRows[grepl("\\$", TotalRows)] %>% 
              gsub("^[[:space:]]{1,}", "", .) %>% 
              gsub("(\\$)([[:space:]]){1,}([[:digit:]])", "\\1\\3", .) %>% 
              gsub("([[:digit:]])([[:space:]]){1,}(\\$)", "\\1  \\3", .) %>% 
              strsplit("[[:space:]]{2,}") 
            
            names(TotalRows) <- 1:length(TotalRows)
            
            TotalRows <- TotalRows %>% 
              bind_rows() %>% 
              gather(Category, Value) %>% 
              mutate(Index = rep(1:6, nrow(.)/6),
                     Category = as.numeric(Category)) %>% 
              spread(Index, Value) %>% 
              gather(Category2, feeValue, 3:ncol(.)) %>% 
              mutate(Category2 = as.numeric(Category2)) %>% 
              arrange(Category, Category2) %>% 
              mutate(Category2 = rep(c("Credits","Tuition", "LRF", "SA", "Total"), nrow(.)/5)) %>% 
              rename(Course_name = `1`) %>% 
              mutate(Course_code = Course_name) %>% 
              select(Course_code, Course_name, Category2, feeValue)
            
            programFees <- bind_rows(TotalRows, TuitionData) %>% 
              mutate(Program = NLurls$Program[prog],
                     ProgramUrl = NLurls$Program_link[prog],
                     TuitionUrl = url)
            
            if(!exists("Fees")){
              Fees <- programFees
            } else {
              Fees <- rbind(Fees, programFees)
            }
            
            
          }
          
        }
      }
    }
  }
  
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Northern_Lakes_Tuition_Fees.csv")



#Olds 

Olds <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Olds_1of2-(Crawl-Run)---2019-09-13T152452Z.csv")

url <- "https://www.oldscollege.ca/programs/horticulture/basc-horticulture/index.html"


remove(Fees)
for(prog in 1:length(Olds$Program)){
  print(prog)
  url <- Olds$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(Olds$Program_link[prog]),
    error = function(e) e
  )
  #if(!NAIT$Program[prog] %in% c("Bachelor of Applied Information Systems Technology - Network Management", "Millwork and Carpentry")){
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    webPageData <- webPage %>% 
      html_nodes(xpath = '//*[@id="full-width-row-3-content"]/div') %>% 
      #html_nodes("div.rxbodyfield") %>% 
      html_nodes("ul") %>% 
      html_text()
    
    
    if(length(webPageData)){
      print(Olds$Program[prog])
      webPageData <- webPageData %>% 
        strsplit("(?<=[[:digit:]]|\\*)(?=[A-Z])", perl = T) %>% 
        unlist() 
      
      webPageData <- tibble(data = webPageData)
      
      webPageData <- webPageData %>% 
        separate(data, c("Category1", "Category2"), sep = "/") %>% 
        gather(Category, feeValue, c(Category1, Category2)) %>% 
        mutate(Program = Olds$Program[prog],
               ProgramUrl = Olds$Program_link[prog])
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Olds_Tuition_Fees.csv")

#Portage
Port <- pdftools::pdf_text("http://www.portagecollege.ca/Documents/student-services/dates-cost/Program%20Dates%20and%20Costs%2019%2020%20Aug_7.pdf")



tuitionData <- Port %>% 
  gsub("[[:space:]]{2,}", "  ", .) %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  gsub("(Year|Period)([[:space:]]){1,}([[:digit:]])", "\\1\\3", .) %>% 
  gsub("([[:space:]]{1,})([[:digit:]]{1,3})([[:space:]]){1,}([A-Z]|4th|3rd)", "_\\2\\3\\4", .) %>% 
  strsplit("_") %>% 
  unlist()

tuitionRows <- tuitionData[!grepl("NON-REFUNDABLE APPLICATION FEE", tuitionData, ignore.case = T)] %>% 
  gsub("([a-z])([[:space:]]{2,})([a-z])", "\\1 \\3", .) %>% 
  gsub("([A-Z]{2}|Delivery|Blended)([[:space:]]{1,})(Fall|Winter|Spring|Summer)", "\\1_\\3", .) %>% 
  gsub(" Blended", "_Blended", .) %>% 
  gsub("([[:digit:]])([[:space:]])(LB)", "\\1_\\3\\4", .) %>% 
  gsub("(LB|Delivery)([[:space:]]){1,}([[:digit:]])", "\\1_NA_\\3", .) %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  gsub("([[:digit:]])([[:space:]]){1,}(\\$)", "\\1_\\3", .) %>% 
  gsub("(\\)|Management|Accounting)([[:space:]])(Online|LB)", "\\1_\\3", .) %>% 
  strsplit("_") 

for(prog in 1:length(tuitionRows)){
  rowOfTui <- tuitionRows[[prog]]
  tuitionRows[[prog]] <- rowOfTui[!grepl("Portage|College|General|Store|Booklist|Textbooks|Courses", tuitionRows[[prog]])]
  index <- grep("^LB|^CL|^SP|^FR|^Blended|^Online|^BO", tuitionRows[[prog]])[2:length(grep("^LB|^CL|^SP|^FR|^Blended|^Online|^BO", tuitionRows[[prog]]))]
  if(!is.na(index)){
    for(i in rev(index)){
      print(i)
      tuitionRows[[length(tuitionRows) + 1]] <- c(tuitionRows[[prog]][1], tuitionRows[[prog]][i: length(tuitionRows[[prog]])])
      tuitionRows[[prog]] <- tuitionRows[[prog]][1:(i-1)]
    }
    
  }
  index <- grep("^Year", tuitionRows[[prog]])[2:length(grep("^Year", tuitionRows[[prog]]))]
  if(!is.na(index)){
    for(i in rev(index)){
      print(i)
      tuitionRows[[length(tuitionRows) + 1]] <- c(tuitionRows[[prog]][i], tuitionRows[[prog]][2], tuitionRows[[prog]][(i+1): length(tuitionRows[[prog]])])
      tuitionRows[[prog]] <- tuitionRows[[prog]][1:(i-1)]
    }
    
  }
  index <- grep("^Winter|^Fall|^Spring|^Summer", tuitionRows[[prog]])[2:length(grep("^Winter|^Fall|^Spring|^Summer", tuitionRows[[prog]]))]
  if(!is.na(index)){
    for(i in rev(index)){
      print(i)
      tuitionRows[[length(tuitionRows) + 1]] <- c(tuitionRows[[prog]][1:2], tuitionRows[[prog]][i: length(tuitionRows[[prog]])])
      tuitionRows[[prog]] <- tuitionRows[[prog]][1:(i-1)]
    }
    
  }
}

for(prog in 1:length(tuitionRows)){
  if(length(tuitionRows[[prog]] > 12)){
    tuitionRows[[prog]] <- tuitionRows[[prog]][1:12]
  }
}
names(tuitionRows) <- 1:length(tuitionRows)
tuitionTibble <- tuitionRows %>% 
  bind_rows() %>% 
  gather(category, catValue) %>% 
  mutate(index = rep(1:12, nrow(.)/12)) %>% 
  spread(index, catValue) %>% 
  mutate(category = as.numeric(category)) %>% 
  arrange(category)

tuitionTibble[24,] <- c(tuitionTibble[24,][1], "4 College Preparation / UCEP", tuitionTibble[24,][2:(ncol(tuitionTibble) - 1)])
tuitionTibble[62,] <- c(tuitionTibble[62,][1], "4 College Preparation / UCEP", tuitionTibble[62,][3:(ncol(tuitionTibble) - 1)])

tuitionTibble <- tuitionTibble %>% 
  mutate(`1` = gsub("(^[[:digit:]]{,3})", "\\1__", `1`)) %>% 
  separate(`1`, c("programCode", "programName"), sep = "__")
colnames(tuitionTibble)[4:ncol(tuitionTibble)] <- c("Delivery", "Term", "startDate", "endDate", "numOfWeeks", "Tuition", "StudentAssocFee", "compFee", "techFee", "recFee", "Other")

tuitionTibble <- tuitionTibble %>% 
  gather(category2, feeValue, 9:ncol(.)) %>% 
  mutate(category = as.numeric(category)) %>% 
  arrange(category)

write_csv(tuitionTibble, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Portage_Tuition_Fees.csv")

#Red_Deer

RDC <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Programs_Red_River_1of2-(Crawl-Run)---2019-09-13T190324Z.csv")


remove(Fees)
for(prog in 1:length(RDC$Program)) {
  print(prog)
  
  url <- RDC$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(RDC$Program_link[prog]),
    error = function(e) e
  )

  if(!inherits(possibleError, "error")) {
    webPage <- read_html(url)
    webPageData <- webPage %>% 
      html_nodes("div.field-item") %>% 
      html_text()
    
    if(length(webPageData[grepl("\\$", webPageData)]) > 0){
      webPageData <- tibble(data = webPageData[grepl("\\$", webPageData)] %>% 
                              strsplit("\\||\n") %>% 
                              unlist()) 
      
      if(length(webPageData) > 0) {
        print(RDC$Program[prog])
        webPageData <- webPageData %>% 
          separate(data, c("feeValue", "category"), "\\(") %>% 
          mutate(category = gsub("\\)|\\.|\n", "", category),
                 Program = RDC$Program[prog],
                 ProgramUrl = RDC$Program_link[prog])
        
        if(!exists("Fees")){
          Fees <- webPageData
        } else {
          Fees <- rbind(Fees, webPageData)
        }
    }
   
    }
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Red_Deer_Tuition_Fees.csv")


#SAIT

#includes courses
SAIT <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_SAIT_1of2-(Crawl-Run)---2019-09-13T195015Z.csv")
SAIT$extracol = NA

SAIT <- SAIT %>% 
  mutate(course = ifelse(grepl("/courses/", Program_link), 1, NA)) %>% 
  filter(is.na(course))

#write_csv(SAIT %>% select(url, Program, Program_link), "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/SAIT_links.csv")



library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)


xpathPossibleError <- function(errorIndex){
  tryCatch(
    remDr$findElement(using = "xpath", errorIndex),
    error = function(e) e
  )
}

xpathPossibleErrorIndex <- function(errorIndex){
  tryCatch(
    remDr$findElement(using = "xpath", paste0('//*[@id="costs"]/div/table[', errorIndex, ']')),
    error = function(e) e
  )
}


remove(Fees)
for(prog in 130:length(SAIT$Program)) {
  print(prog)
  url <- SAIT$Program_link[prog]
  
  possibleError <- tryCatch(
    read_html(SAIT$Program_link[prog]),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    
    remDr$navigate(url)
    Sys.sleep(sample(.25))
    
    moveON <- 0
    if(!inherits(xpathPossibleError('/html/body/div[1]/main/div/div[2]/div/div[3]/div/div/a[6]'), "error")){
      costElement <- remDr$findElement(using = "xpath", '/html/body/div[1]/main/div/div[2]/div/div[3]/div/div/a[6]')
      costElementCheck <- costElement$getElementText() %>% 
        unlist()
      
      if(grepl("cost", costElementCheck, ignore.case = T)) {
        print("moving on")
        costElement$clickElement()
        moveON = 1
      }
    }
    
    if(!inherits(xpathPossibleError('/html/body/div[1]/main/div/div[2]/div/div[4]/div/div/a[6]'), "error")){
      costElement <- remDr$findElement(using = "xpath", '/html/body/div[1]/main/div/div[2]/div/div[4]/div/div/a[6]')
      costElementCheck <- costElement$getElementText() %>% 
        unlist()
      
      if(grepl("cost", costElementCheck, ignore.case = T)) {
        print("moving on")
        costElement$clickElement()
        moveON = 1
      }
    }
    
    if(moveON > 0){    
        for(errorIndex in 1:4){
          errorCheck = suppressMessages(xpathPossibleErrorIndex(errorIndex))
          if(!inherits(errorCheck, "error")){
            #print("no error")
            tableIndex <- errorIndex
          }
        }
        print(paste("tableIndex:", tableIndex))
        remove(progInfoComp)
        
        if(!inherits(xpathPossibleError('//*[@id="costs"]/div/h3[1]'), "error")){
          print("got tuition fees")
          for(subTableIndex in 1:tableIndex){
            webElemName <- remDr$findElement(using = "xpath", paste0('//*[@id="costs"]/div/h3[', subTableIndex, ']'))
            webElemName <- webElemName$getElementText() %>% 
              unlist()
            
            webElem     <- remDr$findElement(using = "xpath", paste0('//*[@id="costs"]/div/table[', subTableIndex, ']'))
            webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
            progInfo    <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]] %>% 
              gather(Category, Value, -1) %>% 
              mutate(studentType = webElemName,
                     url = url,
                     Program = SAIT$Program[prog],
                     timeDetail = colnames(.[1])) %>% 
              set_names("Time", "Category", "Value", "studentType", "url", "Program", "timeDetail")
            if(!exists("progInfoComp")){
              progInfoComp <- progInfo
            } else {
              progInfoComp <- rbind(progInfoComp, progInfo)
            }
          }
          if(exists("progInfoComp")){
            if(!exists("Fees")){
              Fees <- progInfoComp
            } else {
              Fees <- rbind(Fees, progInfoComp)
            } 
          }
          
        }
        
        
      }
      
      

    }
  }

      
remDr$close()      
#
#      
#      
##Alternative      
#for(prog in 1:length(SAIT$Program)) {
#  print(prog)
#  url <- SAIT$Program_link[prog]
#  
#  possibleError <- tryCatch(
#    read_html(SAIT$Program_link[prog]),
#    error = function(e) e
#  )
#  
#  if(!inherits(possibleError, "error")) {
#    
#    
#    
#    
#    webPage <- read_html(url)
#    webPageData <- webPage %>% 
#      html_nodes("div.g-section") %>% 
#      html_nodes("table") %>% 
#      html_table()
#    
#    
#    webPageData <- webPageData[grepl("Tuition", webPageData)]
#    
#    if(length(webPageData) == 2){
#      print(SAIT$Program[prog])
#      webPageData <- webPageData[[1]] %>% 
#        mutate(studentType = "Domestic") %>% 
#        bind_rows(webPageData[[1]] %>% 
#                    mutate(studentType = "International")) %>% 
#        mutate(Program = SAIT$Program[prog],
#               ProgramUrl = SAIT$Program_link[prog]) %>% 
#        gather(Category, feeValue, 1:(ncol(.)-3))
#      
#      if(!exists("Fees")){
#        Fees <- webPageData
#      } else {
#        Fees <- rbind(Fees, webPageData)
#      }
#    } else if(length(webPageData) == 1 ) {
#      print(SAIT$Program[prog])
#      webPageData <- webPageData[[1]] %>% 
#        mutate(studentType = "Domestic",
#               Program = SAIT$Program[prog],
#               ProgramUrl = SAIT$Program_link[prog])%>% 
#        gather(Category, feeValue, 1:(ncol(.)-3))
#      
#      if(!exists("Fees")){
#        Fees <- webPageData
#      } else {
#        Fees <- rbind(Fees, webPageData)
#      }
#      
#    } else {
#      print(paste("not doing it, length = ", length(webPageData)))
#    }
#  }
#}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_SAIT_Tuition_Fees.csv")

#Marine institute

url <- "https://www.mi.mun.ca/departments/officeoftheregistrar/programsandfees/"
webPage <- read_html(url)
feeUrls <- webPage %>% 
  html_nodes("ul") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  paste0("https://www.mi.mun.ca/", .)


remove(Fees)
for(prog in 1:length(feeUrls)){
  print(url)
  possibleError <- tryCatch(
    read_html(feeUrls[prog]),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    url <- feeUrls[prog]
    #url <- feeUrls[1]
    
    webPage <- read_html(url)
    dataSets <- webPage %>% 
      html_nodes("table.fee_table") %>% 
      html_table()
    
    dataTitles <- webPage %>% 
      html_nodes("p.content_title") %>% 
      html_text()
    
    names(dataSets) <- dataTitles
    for(dS in 1:length(dataSets)){
      colnames(dataSets[[dS]])[1] <- "timePeriod"
    }
    
    webPageData <- dataSets %>% 
      purrr::map(~ filter(., timePeriod != "" )) %>% 
      purrr::map(~ mutate_all(., as.character)) %>% 
      bind_rows(.id = "name") %>% 
      gather(category, feeValue, 3:ncol(.))
    
    if(!exists("Fees")){
      Fees <- webPageData
    } else {
      Fees <- rbind(Fees, webPageData)
    }
    
  }
  
}



write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Marine_Inst_Tuition_Fees.csv")




#college l'ile
url <- "https://www.collegeacadieipe.ca/fr/futurs-etudiants/programmes-d-etudes"
webPage <- read_html(url)
programUrls <- webPage %>%
  html_nodes("a") %>%   
  html_attr("href")
programUrls <- paste0("https://www.collegeacadieipe.ca", programUrls[grepl("nos-formations/formation-collegiale", programUrls)]  )

remove(Fees)
for(prog in 1:length(programUrls)){
  print(prog)
  possibleError <- tryCatch(
    read_html(programUrls[prog]),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    url <- programUrls[prog]
    #url <- "https://www.collegeacadieipe.ca/fr/nos-formations/formation-collegiale/infirmier-auxiliaire"
    
    webPage <- read_html(url)
    webPageData <- webPage %>% 
      html_text() %>% 
      strsplit("\n\t") %>% 
      unlist()
    
    
    webPageData <- webPageData[grepl("DROITS DE SCOLARITÉ", webPageData)] %>% 
      strsplit("\r\n") %>% 
      unlist()
    
    if(length(webPageData) > 0) {
      print(programUrls[prog])
      
      index <- grep("DROITS DE SCOLARITÉ", webPageData)
      index2 <- grep("LISTE DE COURS", webPageData)
      
      webPageData <- webPageData[index:(index2 - 1)]
      webPageData <- webPageData[grepl("\\$", webPageData)]
      webPageData <- tibble(feeValue = gsub("([[:alpha:]]|:)([[:space:]]{1,})([[:digit:]])", "\\1_\\3", webPageData) )%>% 
        separate(feeValue, c("Category", "feeValue"), sep = "_") %>% 
        mutate(progrFeeUrl = programUrls[prog],
               Program = gsub("(.*)(/)(.*$)", "\\3", programUrls[prog]))
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
    
    
  } else {
    print(paste("DOESN'T WORK:", programUrls[prog]))
  }
}


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_College_deLile_Tuition_Fees.csv")


#Holland

Holland <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_HollandCollege_1of2-(Crawl-Run)---2019-07-08T160651Z.csv")
Holland$Programs_link <- gsub(" ", "%20", gsub("http:", "https:", Holland$Programs_link))

remove(Fees)
for(prog in 1:length(Holland$Programs_link)){
  print(prog)
  possibleError <- tryCatch(
    read_html(Holland$Programs_link[prog]),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    url <- Holland$Programs_link[prog]
    
    webPage <- read_html(url)
    webPageData <- webPage %>%  
      html_nodes("table.table") %>% 
      html_table(fill = T) 
    
    if(length(webPageData) > 0) {
      print(Holland$Programs[prog])
      
      webPageData <- webPageData %>% 
        bind_rows()
      
      colnames(webPageData)[1] <- "Category"
      if(colnames(webPageData)[2] == "") {
        colnames(webPageData)[2] <- "Fees"
      }
      
      if(is.na(colnames(webPageData)[3])) {
        webPageData <- webPageData[1:2]
      }
      
      if(is.na(colnames(webPageData)[4]) & length(colnames(webPageData)) > 2) {
        webPageData <- webPageData[1:3]
      }
      
      
      webPageData <- webPageData %>% 
        filter(!Category %in% c("", "Other Program Costs")) %>% 
        gather(timePeriod, feeValue, 2:ncol(.)) %>% 
        mutate(Program = Holland$Programs[prog],
               programUrl = Holland$Programs_link[prog])
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}
write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Holland_Tuition_Fees.csv")


#Cape Breton

url <- "https://www.cbu.ca/tuition-and-fee-calculator/"


#Get cleaner urls!
library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
#rD
remDr <- rD$client
remDr$open(silent = T)



remove(Fees)

Programs = c(Nursing = '//*[@id="choice_39_13_1"]', BEd = '//*[@id="choice_39_13_2"]', BEdPracticum = '//*[@id="choice_39_13_3"]', Education = '//*[@id="choice_39_13_4"]')
NScot = c(Yes = '//*[@id="choice_39_33_0"]', No = '//*[@id="choice_39_33_1"]')
Cdn = c(Intl = '//*[@id="choice_39_25_0"]', Cdn = '//*[@id="choice_39_25_1"]')

remove(Fees)
#Look at differences between international and canadian health/ dental plans

for(prog in 1:length(Programs)){
  print(prog)
  print(names(Programs)[prog])
  for(origin in 1:length(Cdn)){
    print(names(Cdn)[origin])
    
    remDr$open(silent = T)
    remDr$navigate(url)
    
    if(origin == 1) {
      #StudentType
      webElem <- remDr$findElement(using = "xpath", Cdn[origin])
      webElem$clickElement()
      print("click StudentType")
      
      #Nova scotia student
      #webElem <- remDr$findElement(using = "xpath", '//*[@id="choice_39_33_0"]')
      #webElem$clickElement()
      
      #Program
      webElem <- remDr$findElement(using = "xpath", Programs[prog])
      webElem$clickElement()
      print("click Program")
      
      
      #Course load
      #webElem <- remDr$findElement(using = "xpath", '//*[@id="input_39_56"]')
      #webElem$clickElement()
      #print("click Course load")
      
      webElem <- remDr$findElement(using = "xpath", '//*[@id="input_39_56"]/option[11]')
      webElem$clickElement()
      print("click Course load")
      
      #Health plan
      webElem <- remDr$findElement(using = "xpath", '//*[@id="label_39_21_0"]')
      webElem$clickElement()
      print("click Health Plan")
      
      #Dental
      
      webElem <- remDr$findElement(using = "xpath", '//*[@id="choice_39_26_0"]')
      webElem$clickElement()
      print("click Dental")
      
      #No to residence
      webElem <- remDr$findElement(using = "xpath", '//*[@id="choice_39_37_1"]')
      webElem$clickElement()
      print("click Residence")
      
      #Next
      webElem <- remDr$findElement(using = "xpath", '//*[@id="gform_next_button_39_65"]')
      webElem$clickElement()
      #webElem$sendKeysToElement(list(key = "enter"))
      print("click Next")
      
      webElems <- remDr$findElements(using = "class", "gfield_label")
      tuitionHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))
      
      webElems <- remDr$findElements(using = "css selector", 'input')
      tuition <- unlist(lapply(webElems, function(x) {x$getElementAttribute("value")}))
      
      #Need to fill in for hidden categories
      tuitionHeaders <- c("Total Cost of Tuition, Fees and Residence",
                          "Cost of Courses",                       
                          "Cost of Residence and Meal Plan",           
                          "Students' Union Fee",                       
                          "Students' Union Membership Fee",            
                          "IT Fee",
                          "B.Ed Practicum",
                          "International Differential Fees",
                          "Cost of chosen Health and Dental Insurance",
                          "Campus Renewal Fee",
                          "Campus Activity Fee",
                          "Total Nova Scotia Bursary")
      
      webPageData <- tibble(headers = tuitionHeaders,
                            feeValue = tuition[grepl("\\$", tuition)],
                            Program = names(Programs)[prog],
                            studentType = names(Cdn)[origin])
      
      
      if(!exists("Fees")){
        Fees <- tuition
      } else {
        Fees <- rbind(Fees, tuition)
      }
      
      Sys.sleep(sample(2:5, 1))
      
    } else {
      for(Novan in 1:length(NScot)){
        print(names(NScot)[Novan])
        
        #StudentType
        webElem <- remDr$findElement(using = "xpath", Cdn[origin])
        webElem$clickElement()
        print("click StudentType")
        
        #Nova scotia student
        webElem <- remDr$findElement(using = "xpath", NScot[Novan])
        webElem$clickElement()
        
        print("click Nova Scotia")
        
        #Program
        webElem <- remDr$findElement(using = "xpath", Programs[prog])
        webElem$clickElement()
        print("click Program")
        
        
        #Course load
        #webElem <- remDr$findElement(using = "xpath", '//*[@id="input_39_56"]')
        #webElem$clickElement()
        #print("click Course load")
        
        webElem <- remDr$findElement(using = "xpath", '//*[@id="input_39_56"]/option[11]')
        webElem$clickElement()
        print("click Course load")
        
        #Health plan
        webElem <- remDr$findElement(using = "xpath", '//*[@id="label_39_21_0"]')
        webElem$clickElement()
        print("click Health Plan")
        
        #Dental
        webElem <- remDr$findElement(using = "xpath", '//*[@id="choice_39_23_0"]')
        webElem$clickElement()
        print("click Dental")
        
        #No to residence
        webElem <- remDr$findElement(using = "xpath", '//*[@id="choice_39_37_1"]')
        webElem$clickElement()
        print("click Residence")
        
        #Next
        webElem <- remDr$findElement(using = "xpath", '//*[@id="gform_next_button_39_65"]')
        webElem$clickElement()
        #webElem$sendKeysToElement(list(key = "enter"))
        print("click Next")
        
        webElems <- remDr$findElements(using = "class", "gfield_label")
        tuitionHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))
        
        webElems <- remDr$findElements(using = "css selector", 'input')
        tuition <- unlist(lapply(webElems, function(x) {x$getElementAttribute("value")}))
        
        #Need to fill in for hidden categories
        tuitionHeaders <- c("Total Cost of Tuition, Fees and Residence",
                            "Cost of Courses",                       
                            "Cost of Residence and Meal Plan",           
                            "Students' Union Fee",                       
                            "Students' Union Membership Fee",            
                            "IT Fee",
                            "B.Ed Practicum",
                            "International Differential Fees",
                            "Cost of chosen Health and Dental Insurance",
                            "Campus Renewal Fee",
                            "Campus Activity Fee",
                            "Total Nova Scotia Bursary")
        
        webPageData <- tibble(headers = tuitionHeaders,
                              feeValue = tuition[grepl("\\$", tuition)],
                              Program = names(Programs)[prog],
                              studentType = names(Cdn)[origin])
        
        
        if(!exists("Fees")){
          Fees <- webPageData
        } else {
          Fees <- rbind(Fees, webPageData)
        }
        #remDr$close()
        Sys.sleep(sample(2:5, 1))
      }
      
    }
    
    remDr$close()
  }
  
  
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Cape_Breton_Tuition_Fees.csv")


#NSCC

url <- "https://www.nscc.ca/admissions/cost_and_financial_aid/programfees/default.aspx"

webPage <- read_html(url)
urls <- webPage %>% 
  html_nodes("div.inner-content") %>% 
  html_nodes("a") %>% 
  html_attr('href')

urls <- paste0("https://www.nscc.ca/admissions/cost_and_financial_aid/programfees/", urls[grepl("program", urls)])

remove(Fees)
for(prog in 1:length(urls)){
  print(prog)
  possibleError <- tryCatch(
    read_html(urls[prog]),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    url <- urls[prog]
    #url <- urls[22]
    
    webPage <- read_html(url)
    Program <- webPage %>%
      html_nodes("div.inner-content") %>% 
      html_nodes("h1") %>% 
      html_text()
    
    webPageData <- webPage %>%  
      html_nodes("table.tablesaw") %>% 
      html_table(fill = T)
    
    if(length(webPageData) > 0) {
      print(paste("Proceeding with", Program))
      for(df in 1:length(webPageData)){
        colnames(webPageData[[df]])[1] <- "Category"
      }
      
      #print(Holland$Programs[prog])
      webPageData <- webPageData %>% 
        bind_rows()
      
      webPageData <- webPageData[1:2] %>% 
        mutate(Program = Program,
               programFeeUrl = urls[prog])
      
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
    
    
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_NSCC_Tuition_Fees.csv")




#SteAnne

webPage <- read_html("https://www.usainteanne.ca/etudes-universitaires")
urls <- webPage %>% 
  html_nodes("section.article-content") %>% 
  html_nodes("a") %>% 
  html_attr("href")

urls <- paste0("https://www.usainteanne.ca", urls, "#questions-financieres")
remove(Fees)
for(prog in 1:length(urls)){
  print(prog)
  possibleError <- tryCatch(
    read_html(urls[prog]),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    url <- urls[prog]
    #url <- urls[4]
    
    webPage <- read_html(url)
    Program <- webPage %>%
      html_nodes("header.article-header") %>% 
      html_nodes("h1") %>% 
      html_text() %>% 
      gsub("\n|\t", "", .)
    
    webPageData <- webPage %>%  
      html_nodes("table.table")
    
    webPageData <- webPageData[grepl("\\$", webPageData)] %>% 
      html_table()
    
    if(length(webPageData) > 0) {
      print(paste("Proceeding with", Program))
      for(df in 1:length(webPageData)){
        if(ncol(webPageData[[df]]) >= 3){
          colnames(webPageData[[df]])[1:3] <- c("Category", "feeValue", "subCategory")
        } else {
          colnames(webPageData[[df]])[1:2] <- c("Category", "feeValue")
        }
      }
      
      #print(Holland$Programs[prog])
      webPageData <- webPageData %>% 
        bind_rows()
      
      webPageData <- webPageData[1:3] %>% 
        mutate(Program = Program,
               programFeeUrl = urls[prog])
      
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
    
    
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Ste_Anne_Tuition_Fees.csv")


## CCNB

#Manual grab from https://ccnb.ca/admission-et-bourses/droits-de-scolarite.aspx


#Maritime Forest Tech
webPage <- read_html("https://mcft.ca/en/explore/tuition-and-fees")
webPage %>% 
  html_nodes("h3") %>% 
  html_text()

webPageData <- webPage %>% 
  html_nodes("table.tuitionTable") %>% 
  html_table(fill = T)

names(webPageData)[1:4] <- paste(webPage %>%
                                   html_nodes("h5") %>% 
                                   html_text() %>% 
                                   gsub("\n\t", "", .) %>% 
                                   unlist(),
                                 rep(c("-Semester One", "-Semester Two"), each = 2))

FTD <- webPageData[1:4] %>% 
  bind_rows(.id = "name") %>% 
  gather(Category, feeValue, 2:ncol(.)) %>% 
  mutate(Category = ifelse(!grepl("[[:digit:]]", feeValue), feeValue, NA),
         Category = zoo::na.locf(ifelse(Category == "", NA, Category))) %>% 
  filter(Category != feeValue) %>% 
  mutate(Program = "Forest Technology Diploma",
         subCategory = NA) %>% 
  separate(name, c("Location", "timePeriod"), sep = "-")

names(webPageData)[5:6] <- c("Tuition Fees", "Professional Certifications/Course")
MCFTAdvDip <- webPageData[5:6] %>% 
  bind_rows(.id = "subCategory") %>% 
  filter(!grepl("[[:alpha:]]", X2)) %>% 
  mutate(Program = "Forest Technology Advanced Diploma",
         timePeriod = "Year two",
         Location = NA) %>% 
  rename(Category = X1,
         feeValue = X2) %>% 
  select(c(Location, timePeriod, Category, feeValue, Program, subCategory)) %>% 
  bind_rows(FTD %>% 
              filter(Category == "Totals") %>% 
              mutate(Category = "Total Fish & Wildlife",
                     Program = "Forest Technology Advanced Diploma",
                     timePeriod = paste(timePeriod, "- 1st Year FT Diploma"),
                     subCategory = "Tuition Fees"))

names(webPageData)[7:10] <- c("Fish & Wildlife Techniques I", "Fish & Wildlife Techniques II", "Fish & Wildlife Techniques III", "Fish & Wildlife Techniques IV")
FaWI <- webPageData[7:10] %>% 
  bind_rows(.id = "Program") %>% 
  rename(Category = X1,
         feeValue = X2) %>% 
  mutate(Location = NA,
         timePeriod = NA,
         subCategory = NA) %>% 
  select(c(Location, timePeriod, Category, feeValue, Program, subCategory)) 

Fees <- FTD %>% 
  bind_rows(MCFTAdvDip) %>% 
  bind_rows(FaWI)


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_MCFT_Tuition_Fees.csv")

#NBCCD

webPage <- read_html("https://nbccd.ca/admissions/tuition-fees/")
webPage %>% 
  html_nodes("h6")


webPageData <- webPage %>% 
  html_nodes("table") %>% 
  html_table()

webPageData[c(3,6)] <- webPageData[c(3,6)] %>% 
  purrr::map(~ gather(., Category, X2, 2:ncol(.) )) %>% 
  purrr::map(~ spread(., names(.)[1], value = "X2")) %>% 
  purrr::map(~ gather(., Category, X2, c(2:(ncol(.)-2)))) %>% 
  purrr::map(~ mutate(., subCategory = colnames(.)[2])) %>% 
  purrr::map(~ select(., -`Studio Fees`)) %>% 
  purrr::map(~ rename(., X1 = Category))
Domestic <- webPageData[1:3] %>% 
  purrr::map(~ mutate(., X2 = gsub("(\\$)([[:digit:]]{1,})([[:space:]])", "\\1\\2_", X2))) %>% 
  purrr::map(~ separate(., X2, c("feeValue", "Description"), sep = "_")) %>% 
  bind_rows() %>% 
  mutate(studentType = "Domestic")
Intl <- Domestic <- webPageData[4:6] %>% 
  purrr::map(~ mutate(., X2 = gsub("(\\$)([[:digit:]]{1,})([[:space:]]|\n)", "\\1\\2_", X2))) %>% 
  purrr::map(~ separate(., X2, c("feeValue", "Description"), sep = "_")) %>% 
  bind_rows()%>% 
  mutate(studentType = "International")

Fees <- Domestic %>% 
  bind_rows(Intl) %>% 
  filter(feeValue != "Amount")

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_NBCCD_Tuition_Fees.csv")



#New Brunswick Community College

webPage <- read_html("https://nbcc.ca/tuition-fees")

webPageData <- webPage %>% 
  html_nodes("table.table") %>% 
  html_table()


for(tbl in 1:length(webPageData)){
  colnames(webPageData[[tbl]]) <- c("subSubCategory", "Domestic", "International")
}


names(webPageData) <- webPage %>% 
  html_nodes("h2") %>% 
  html_text()

Fees <- webPageData %>% 
  bind_rows(.id = "Category") %>% 
  gather(studentType, feeValue, 3:4) %>% 
  mutate(subCategory = zoo::na.locf(ifelse(subSubCategory == feeValue, subSubCategory, NA), na.rm = F),
         subCategory = ifelse(is.na(subCategory), subSubCategory, subCategory)) %>% 
  select(Category, subCategory, subSubCategory, feeValue)

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_New_Brunswick_Community_Tuition_Fees.csv")



#BCIT
##Full time trades and tech

tuition <- pdftools::pdf_text("https://www.bcit.ca/files/pdf/admission/2019-20-trades-tuition.pdf")
tuitionClean <- tuition %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  gsub("([[:space:]]{2,})(\\$)", "_\\2", .) %>% 
  gsub("([[:space:]]{2,})([[:digit:]]|\\.)+", "", .) %>% 
  strsplit("_")

tuitionClean <- tuitionClean[10:139]
for(tui in 1:length(tuitionClean)){
  if(length(tuitionClean[[tui]]) == 7){
    tuitionClean[[tui]] <- c(tuitionClean[[tui]][1:5],NA, tuitionClean[[tui]][6:7])
  } else if(length(tuitionClean[[tui]]) == 6){
    tuitionClean[[tui]] <- c(tuitionClean[[tui]][1:2],NA, NA, tuitionClean[[tui]][3:6])
  }
}

tuitionClean <- tuitionClean[lengths(tuitionClean) != 1]

names(tuitionClean) <- 1:length(tuitionClean)

Fees <- tibble(colNames = c("Program", "Tuition", "Student_Association_Capital_Levy_Fee", "Student_Association_Health_Dental", "Student_Association_Activity_Fee", "Student_Association_Building_Centre_Fee", "Other Fees", "Total")) %>% 
  bind_cols(tuitionClean %>% 
              bind_cols()) %>% 
  gather(Program, feeValue, 2:ncol(.)) %>% 
  spread(colNames, feeValue) %>% 
  select(c("Program", "Tuition", "Student_Association_Capital_Levy_Fee", "Student_Association_Health_Dental", "Student_Association_Activity_Fee", "Student_Association_Building_Centre_Fee", "Other Fees", "Total")) %>% 
  gather(Category, feeValue, 2:ncol(.)) %>% 
  arrange(Program) %>% 
  filter(Program != "")


FTTFees <- Fees %>% 
  mutate(subject = "Full time trades and technical studies")

##Full time/Cohort technology

tuition <- pdftools::pdf_text("https://www.bcit.ca/files/pdf/admission/2019-20-technology-tuition.pdf")
tuitionClean <- tuition %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  gsub("([[:digit:]])([[:space:]]{20,})", "\\1_NA_", .) %>% 
  gsub("([[:space:]]{2,})(\\$|[[:digit:]])", "_\\2", .) %>% 
  gsub("([[:space:]]{2,})([[:digit:]]|\\.)+", "", .) %>% 
  strsplit("_")

tuitionClean <- tuitionClean[c(23:45, 47:63, 87:92, 94, 96:102, 104:131, 154:172)]
names(tuitionClean) <- 1:length(tuitionClean)

for(tui in 1:length(tuitionClean)){
  if(length(tuitionClean[[tui]]) != 13){
    tuitionClean[[tui]] <- c(tuitionClean[[tui]][1:length(tuitionClean[[tui]])], rep(NA, 13 - length(tuitionClean[[tui]])))
  }
}

tuitionClean <- tuitionClean[lengths(tuitionClean) != 1]



Fees <- tibble(colNames = c("Program", 
                            "Term_tuition", 
                            "Term_tuition_DUP", 
                            "Student_Association_Activity_Fee",
                            "Student_Association_Health_Dental", 
                            "Student_Association_Capital_Levy_Fee", 
                            "Student_Association_Building_Centre_Fee", 
                            "Other Fees", 
                            "Total_per_term", 
                            "1st_Year_tuition", 
                            "2nd_year_tuition", 
                            "Mandatory_practicum", 
                            "Total")) %>% 
  bind_cols(tuitionClean %>% 
              bind_cols()) %>% 
  gather(Program_id, feeValue, 2:ncol(.)) %>% 
  spread(colNames, feeValue) %>% 
  arrange(as.numeric(Program_id)) %>% 
  mutate(programHeader = ifelse(is.na(Student_Association_Activity_Fee), Program, NA),
         programHeader = zoo::na.locf(ifelse(grepl("^[A-Z]{2,}", Program), Program, programHeader))) %>% 
  select(c("programHeader", "Program", "Term_tuition", "Term_tuition_DUP", "Student_Association_Activity_Fee", "Student_Association_Health_Dental", "Student_Association_Capital_Levy_Fee", "Student_Association_Building_Centre_Fee", "Other Fees", "Total_per_term", "1st_Year_tuition", "2nd_year_tuition", "Mandatory_practicum", "Total")) %>% 
  filter(!is.na(Term_tuition))

TuiDup <- Fees %>% 
  filter(Term_tuition == "NA")

TuiDup <- TuiDup %>% 
  select(-Term_tuition) 

colnames(TuiDup) <- c("programHeader", "Program", "Term_tuition", "Student_Association_Activity_Fee", "Student_Association_Health_Dental", "Student_Association_Capital_Levy_Fee", "Student_Association_Building_Centre_Fee", "Other Fees", "Total_per_term", "1st_Year_tuition", "2nd_year_tuition", "Mandatory_practicum", "Total")

Fees <- Fees %>% 
  filter(Term_tuition != "NA") %>% 
  select(-Total)

colnames(Fees) <- c("programHeader", "Program", "Term_tuition", "Student_Association_Activity_Fee", "Student_Association_Health_Dental", "Student_Association_Capital_Levy_Fee", "Student_Association_Building_Centre_Fee", "Other Fees", "Total_per_term", "1st_Year_tuition", "2nd_year_tuition", "Mandatory_practicum", "Total")

Fees <- TuiDup %>% 
  bind_rows(Fees) %>% 
  arrange(Program)

TuiDup <- Fees %>% 
  filter(Student_Association_Activity_Fee == "NA")

TuiDup <- TuiDup %>% 
  select(-Student_Association_Activity_Fee) %>% 
  mutate(placeHolder = NA)

colnames(TuiDup) <- c("programHeader", "Program", "Term_tuition", "Student_Association_Activity_Fee", "Student_Association_Health_Dental", "Student_Association_Capital_Levy_Fee", "Student_Association_Building_Centre_Fee", "Other Fees", "Total_per_term", "1st_Year_tuition", "2nd_year_tuition", "Mandatory_practicum", "Total")
Fees <- TuiDup %>% 
  bind_rows(Fees %>% 
              filter(Student_Association_Activity_Fee != "NA")) %>% 
  arrange(Program)

Fees <- Fees %>% 
  mutate(Program = ifelse(Program == programHeader, "Total", ifelse(Program == "", "Total", Program)))

Fees <- Fees %>% 
  rename(timePeriod_Level = Program) %>% 
  gather(Category, feeValue, 3:ncol(.)) %>% 
  arrange(programHeader) %>% 
  filter(programHeader != "")

FTCTFees <- Fees %>% 
  mutate(subject = "Full time/Cohort technology")


##Technology professional and digital arts

tuition <- pdftools::pdf_text("https://www.bcit.ca/files/pdf/admission/2019-20-high-tech-tuition.pdf")
tuitionClean <- tuition %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  gsub("([[:digit:]])([[:space:]]{20,})", "\\1_NA_", .) %>% 
  gsub("([[:space:]]{2,})(\\$|[[:digit:]])", "_\\2", .) %>% 
  gsub("([[:space:]]{2,})([[:digit:]]|\\.)+", "", .) %>% 
  strsplit("_")

tuitionClean <- tuitionClean[c(13:16, 23:55)]
names(tuitionClean) <- 1:length(tuitionClean)

for(tui in 1:length(tuitionClean)){
  if(length(tuitionClean[[tui]]) != 12){
    tuitionClean[[tui]] <- c(tuitionClean[[tui]][1:length(tuitionClean[[tui]])], rep(NA, 12 - length(tuitionClean[[tui]])))
  }
}


Fees <- tibble(colNames = c("Program", 
                            "Tuition",
                            "Tuition_DUP",
                            "Book_fee",
                            "Book_fee_DUP",
                            "Student_Association_Activity_Fee",
                            "Student_Association_Health_Dental", 
                            "Student_Association_Capital_Levy_Fee", 
                            "Other Fees", 
                            "Non_refundable_commitment_fee",
                            "Balance",
                            "Total")) %>% 
  bind_cols(tuitionClean %>% 
              bind_cols()) %>% 
  gather(Program_id, feeValue, 2:ncol(.)) %>% 
  spread(colNames, feeValue) %>% 
  arrange(as.numeric(Program_id)) %>% 
  mutate(programHeader = ifelse(is.na(Student_Association_Activity_Fee), Program, NA),
         programHeader = zoo::na.locf(ifelse(!grepl("^Term", Program), Program, programHeader), na.rm = F),
         programHeader = ifelse(is.na(programHeader), Program, programHeader)) %>% 
  select(c("programHeader",
           "Program", 
           "Tuition",
           "Tuition_DUP",
           "Book_fee",
           "Book_fee_DUP",
           "Student_Association_Activity_Fee",
           "Student_Association_Health_Dental", 
           "Student_Association_Capital_Levy_Fee", 
           "Other Fees", 
           "Non_refundable_commitment_fee",
           "Balance",
           "Total")) %>% 
  filter(!is.na(Tuition))

TuiDup <- Fees %>% 
  filter(Tuition == "NA")

TuiDup <- TuiDup %>% 
  select(-c(Tuition, Book_fee) )

colnames(TuiDup) <- c("programHeader",
                      "Program", 
                      "Tuition",
                      "Book_fee",
                      "Student_Association_Activity_Fee",
                      "Student_Association_Health_Dental", 
                      "Student_Association_Capital_Levy_Fee", 
                      "Other Fees", 
                      "Non_refundable_commitment_fee",
                      "Balance",
                      "Total")

nonTotals <- Fees %>% 
  filter(is.na(Balance)) %>% 
  select(-c(Balance, Total))

colnames(nonTotals) <- c("programHeader",
                         "Program", 
                         "Tuition",
                         "Book_fee",
                         "Student_Association_Activity_Fee",
                         "Student_Association_Health_Dental", 
                         "Student_Association_Capital_Levy_Fee", 
                         "Other Fees", 
                         "Non_refundable_commitment_fee",
                         "Balance",
                         "Total")

Fees <- Fees %>% 
  filter(Tuition != "NA" & !is.na(Balance)) %>% 
  select(-c(Tuition_DUP, Total))

colnames(Fees) <- c("programHeader",
                    "Program", 
                    "Tuition",
                    "Book_fee",
                    "Student_Association_Activity_Fee",
                    "Student_Association_Health_Dental", 
                    "Student_Association_Capital_Levy_Fee", 
                    "Other Fees", 
                    "Non_refundable_commitment_fee",
                    "Balance",
                    "Total")

Fees <- TuiDup %>% 
  bind_rows(Fees) %>% 
  bind_rows(nonTotals) %>% 
  arrange(Program)

Fees <- Fees %>% 
  mutate(Program = ifelse(Program == programHeader, "Total", ifelse(Program == "", "Total", Program)))

Fees <- Fees %>% 
  rename(timePeriod_Level = Program) %>% 
  gather(Category, feeValue, 3:ncol(.)) %>% 
  arrange(programHeader) %>% 
  filter(programHeader != "")

TPDAFees <- Fees %>% 
  mutate(subject = "Technology professional and digital arts")


##



#### BCIT DEGREES
tuition <- pdftools::pdf_text("https://www.bcit.ca/files/pdf/admission/2019-20-degree-tuition.pdf")
tuitionClean <- tuition %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  gsub("([[:space:]]{80,})(\\$)", "_NA_\\2", .) %>% 
  gsub("([[:digit:]])([[:space:]]{20,})(\\$)", "\\1_NA_\\3", .) %>% 
  gsub("([[:space:]]{2,})(\\$|[[:digit:]])", "_\\2", .) %>% 
  gsub("([[:space:]]{2,})([[:digit:]]|\\.)+", "", .) %>% 
  gsub("(Bachelor|Master|Graduate)", "_\\1", .) %>% 
  strsplit("_")

tuitionClean <- tuitionClean[c(19:48, 68:93, 113:139, 159:188, 208:227, 233:237, 257:276)]
names(tuitionClean) <- 1:length(tuitionClean)

for(tui in 1:length(tuitionClean)){
  if(length(tuitionClean[[tui]]) != 10){
    tuitionClean[[tui]] <- c(tuitionClean[[tui]][1:length(tuitionClean[[tui]])], rep(NA, 10 - length(tuitionClean[[tui]])))
  }
}


Fees <- tibble(colNames = c("Program", 
                            "Credential",
                            "Tuition", 
                            "Student_Association_Capital_Levy_Fee",
                            "Student_Association_Health_Dental",
                            "Student_Association_Activity_Fee",
                            "Student_Association_Student_Centre_Fee",
                            "Other_Fees",
                            "Total",
                            "placeHolder")) %>% 
  bind_cols(tuitionClean %>% 
              bind_cols()) %>% 
  gather(Program_id, feeValue, 2:ncol(.)) %>% 
  spread(colNames, feeValue) %>% 
  arrange(as.numeric(Program_id)) %>% 
  #select(c(Program, Credential, Tuition))
  mutate(programHeader = zoo::na.locf(ifelse(is.na(Tuition) & !is.na(Credential), Program, NA), na.rm = F),
         programHeader = ifelse(is.na(Credential), NA, programHeader),
         programComment = zoo::na.locf(ifelse(is.na(programHeader), Program, NA), na.rm = F),
         #programHeader = zoo::na.locf(ifelse(!grepl("^Term|^Level|^Optional|^Mandatory|^Summer|^Winter", Program), Program, programHeader), na.rm = F),
         #programHeader = ifelse(is.na(programHeader), Program, programHeader)
  ) %>% 
  select(c("programHeader",
           "Program", 
           "programComment",
           "Credential",
           "Tuition", 
           "Student_Association_Capital_Levy_Fee",
           "Student_Association_Health_Dental",
           "Student_Association_Activity_Fee",
           "Student_Association_Student_Centre_Fee",
           "Other_Fees",
           "Total")) %>% 
  mutate(programComment = ifelse(programHeader == Program, NA, programComment),
         Credential = zoo::na.locf(ifelse(Credential == "NA", NA, Credential))) %>% 
  filter(!is.na(programHeader) & !is.na(programComment))


Fees <- Fees %>% 
  rename(timePeriod_Level = Program) %>% 
  gather(Category, feeValue, 5:ncol(.)) %>% 
  arrange(programHeader) %>% 
  filter(programHeader != "")

  FTCDGSFees <- Fees %>% 
  mutate(subject = "Full time/cohort degree and grad study")




##BCIT APPRENT
tuition <- pdftools::pdf_text("https://www.bcit.ca/files/pdf/admission/2019-20-apprentice-tuition.pdf")
tuitionClean <- tuition %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  #gsub("([[:space:]]{80,})(\\$)", "_NA_\\2", .) %>% 
  gsub("([[:digit:]]{2})([[:space:]]{40,})(\\$)", "\\1_NA_\\3", .) %>% 
  gsub("([[:space:]]{1,})(\\$)", "_\\2", .) %>% 
  gsub("([[:space:]]{2,})([[:digit:]]|\\.)+", "", .) %>% 
  gsub("([[:punct:]])([[:digit:]]){2}([[:space:]])(\\$)", "\\1\\2_\\4", .) %>% 
  strsplit("_")


tuitionClean <- tuitionClean[c(14:72, 75:121)]
names(tuitionClean) <- 1:length(tuitionClean)

for(tui in 1:length(tuitionClean)){
  if(length(tuitionClean[[tui]]) < 8){
    tuitionClean[[tui]] <- c(tuitionClean[[tui]][1:length(tuitionClean[[tui]])], rep(NA, 8 - length(tuitionClean[[tui]])))
  }
}


Fees <- tibble(colNames = c("Program", 
                            "Tuition", 
                            "Student_Association_Capital_Levy_Fee",
                            "Student_Association_Activity_Fee",
                            "Student_Association_Student_Centre_Fee",
                            "Other_Fees",
                            "Total",
                            "placeHolder")) %>% 
  bind_cols(tuitionClean %>% 
              bind_cols()) %>% 
  gather(Program_id, feeValue, 2:ncol(.)) %>% 
  spread(colNames, feeValue) %>% 
  arrange(as.numeric(Program_id)) %>% 
  #select(c(Program, Credential, Tuition))
  mutate(Program = ifelse(Program == "", lead(Program), Program),
         programHeader = zoo::na.locf(ifelse(substr(Program,1,1) != " ", Program, NA), na.rm = F),
         #programHeader = ifelse(is.na(Credential), NA, programHeader),
         programComment = zoo::na.locf(ifelse(is.na(programHeader), Program, NA), na.rm = F),
         #programHeader = zoo::na.locf(ifelse(!grepl("^Term|^Level|^Optional|^Mandatory|^Summer|^Winter", Program), Program, programHeader), na.rm = F),
         #programHeader = ifelse(is.na(programHeader), Program, programHeader)
  ) %>% 
  select(c("programHeader",
           "Program", 
           "Tuition", 
           "Student_Association_Capital_Levy_Fee",
           "Student_Association_Activity_Fee",
           "Student_Association_Student_Centre_Fee",
           "Other_Fees",
           "Total",
           "placeHolder")) %>% 
  filter(!is.na(Tuition))


shiftDf <- Fees %>% 
  filter(!is.na(placeHolder)) %>% 
  unite(Program, Tuition)

colnames(shiftDf) <- c("programHeader",
                       "Program", 
                       "Tuition", 
                       "Student_Association_Capital_Levy_Fee",
                       "Student_Association_Activity_Fee",
                       "Student_Association_Student_Centre_Fee",
                       "Other_Fees",
                       "Total")

Fees <- Fees %>% 
  select(-placeHolder)

colnames(Fees) <- c("programHeader",
                       "Program", 
                       "Tuition", 
                       "Student_Association_Capital_Levy_Fee",
                       "Student_Association_Activity_Fee",
                       "Student_Association_Student_Centre_Fee",
                       "Other_Fees",
                       "Total")

Fees <- Fees %>% 
  bind_rows(shiftDf) %>% 
  rename(timePeriod_Level = Program) %>% 
  gather(Category, feeValue, 3:ncol(.)) %>% 
  arrange(programHeader) %>% 
  filter(programHeader != "")

ApprFees <- Fees %>% 
  mutate(subject = "Apprentice")


Fees <- ApprFees %>% 
  mutate(programComment = NA) %>% 
  select(c(programHeader, timePeriod_Level, Category, feeValue, subject, programComment)) %>% 
  bind_rows(FTTFees %>% 
    mutate(timePeriod_Level = NA,
           programComment = NA) %>% 
    rename(programHeader = Program) %>% 
    select(c(programHeader, timePeriod_Level, Category, feeValue, subject, programComment))) %>% 
  bind_rows(FTCTFees %>% 
    mutate(programComment = NA) %>% 
    select(c(programHeader, timePeriod_Level, Category, feeValue, subject, programComment))) %>% 
  bind_rows(TPDAFees %>% 
    mutate(programComment = NA) %>% 
    select(c(programHeader, timePeriod_Level, Category, feeValue, subject, programComment))) %>% 
  bind_rows(FTCDGSFees)

write_csv(Fees , "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_BCIT_Tuition_Fees.csv")


#Camosun

webPage <- read_html("http://camosun.ca/learn/az/programs.html")
programs <- webPage %>% 
  html_nodes('table.table') %>% 
  html_table() %>% 
  bind_rows()

urls <- webPage %>% 
  html_nodes('table.table') %>% 
  html_nodes('a') %>% 
  html_attr("href")
urls <- paste0("http://camosun.ca", urls, "what-it-costs.html")

remove(Fees)
for(prog in 1:150){
  
  print(prog)
  url <- urls[prog]
  #url <- urls[45]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url) 
    
    webPageData <- webPage%>% 
      html_nodes("table.table") %>% 
      html_table() 
    
    if(length(webPageData) > 0){
      print(programs$Program[prog])
      webPageData <- webPageData %>% 
        bind_rows() %>% 
        gather(Category, feeValue, 2:ncol(.)) 
      
      if(colnames(webPageData)[1] == "Course"){
        webPageData <- webPageData %>% 
          mutate(Term = NA)
      } else {
        webPageData <- webPageData %>% 
          mutate(Course = NA) 
        
        colnames(webPageData)[1] <- "Term"
      }  
      
      webPageData <- webPageData %>%   
        mutate(Program = programs$Program[prog],
               Credential = programs$Credential[prog],
               Length = programs$Length[prog],
               School = programs$School[prog],
               programFeeUrl = url) %>% 
        select(Term, Course, Category, feeValue, Program, Credential, Length, School, programFeeUrl)
      
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}


write_csv(Fees , "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Camosun_Fees.csv")


#Capilano

webPage <- read_html("https://www.capilanou.ca/programs--courses/search--select/find-a-program-or-course/")

urls <- webPage %>% 
  html_nodes("table") %>% 
  html_nodes("a") %>% 
  html_attr("href")

urls <- paste0("https://www.capilanou.ca", urls,"?&tab=tab-program-details" )


remove(Fees)
for(prog in 1:length(urls)){
  
  print(prog)
  url <- urls[prog]
  #url <- "https://www.capilanou.ca/programs--courses/program-profiles/2d-animation--visual-development-diploma/?&tab=tab-program-details"
  
  
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url) 
    
    webPageData <- webPage %>% 
      #html_nodes("div.domestic-tuition") %>% 
      html_nodes("table") %>% 
      html_table(fill = T) 
    
    webPageData <- webPageData[grepl("\\$", webPageData)]
    
    programHeader <- webPage %>% 
      html_nodes("h1") %>% 
      html_text()
    
    programHeader <- programHeader[length(programHeader)]
    
    ##if(length(webPageData) > 0){
    ##  
    ##  domwebPageData <- webPageData %>% 
    ##    bind_cols() %>% 
    ##    mutate(studType = colnames(.)[1])
    ##  
    ##}
    ##  
    ##webPageData <- webPage %>% 
    ##  html_nodes("div.intl-tuition") %>% 
    ##  html_nodes("table") %>% 
    ##  html_table(fill = T)
    ##  
    ##if(length(webPageData) > 0){
    ##  
    ##  intlwebPageData <- webPageData %>% 
    ##    bind_cols() %>% 
    ##    mutate(studType = colnames(.)[1])
    ##  
    ##}  
    
    if(length(webPageData) > 0){
      
      for(df in 1:length(webPageData)){
        colnames(webPageData[[df]]) <- paste0(colnames(webPageData[[df]]), 1:length(colnames(webPageData[[df]])))
      }
      
      webPageData <- webPageData %>% 
        purrr::map(~ mutate(., studType = colnames(.)[1])) %>%
        purrr::map(~ magrittr::set_colnames(., c(.[1,1:(ncol(.) - 1)], "studType"))) %>%
        purrr::map(~ janitor::remove_empty(., "cols")) %>% 
        purrr::map(~ slice(., -1L)) %>% 
        bind_rows() 
      
      
      if("Attend a mandatory information session" %in% colnames(webPageData)){
        webPageData <- webPageData %>% 
          select(-c(`Attend a mandatory information session`, November))
      }
      
      webPageData <- webPageData %>% 
        gather(Category, feeValue, 2:(ncol(.) - 1))
      
      #print(programs$Program[prog])
      
      webPageData <- webPageData %>%   
        mutate(programFeeUrl = url,
               programHeader = programHeader)
      
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}

write_csv(Fees , "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Capilano_Fees.csv")

#Coast mountain
webPage <- read_html("https://www.coastmountaincollege.ca/programs/programs")


urls <- webPage %>% 
  html_nodes("a") %>% 
  html_attr("href")

urls <- urls[grepl("https://catalogue.coastmountaincollege.ca/programs", urls)] %>% 
  unique()

urls <- paste(urls, "#programfeestext", sep = "/")


remove(Fees)
for(prog in 1:length(urls)){
  
  print(prog)
  url <- urls[prog]
  #url <- urls[2]
  
  
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url) 
    
    webPageData <- webPage %>% 
      #html_nodes("div.domestic-tuition") %>% 
      html_nodes("table.sc_sctable") %>% 
      html_table(fill = T) 
    
    webPageData <- webPageData[grepl("\\$", webPageData)]
    
    programHeader <- webPage %>% 
      html_nodes("h1") %>% 
      html_text()
    
    programHeader <- programHeader[length(programHeader)]
    
    
    if(length(webPageData) == 1){
      
      
      webPageData <- webPageData %>% 
        bind_rows() %>% 
        filter(Fees != "Additional Costs:") %>% 
        gather(studType, feeValue, 2:ncol(.))%>% 
        rename(Category = Fees) %>% 
        mutate(programFeeUrl = url,
               programHeader = programHeader)
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}

write_csv(Fees , "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Coast_Mountain_Fees.csv")



#CNC
webPage <- "https://cnc.bc.ca/programs-courses/programs"


library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
#rD
remDr <- rD$client
Sys.sleep(sample(2:5, 1))
remDr$open(silent = T)
remDr$navigate(webPage)
webElem <- remDr$findElement(using = "xpath", '//*[@id="listResult"]')
#Returns random values...
webElem$clickElement()
webElems <- remDr$findElements(using = "class", "title-link")


Programs <- unlist(lapply(webElems, function(x) {x$getElementText()}))
navLinks <- sapply(webElems, function(x) {x$findChildElement("css", "a")})
urls <- unlist(lapply(navLinks, function(x) {x$getElementAttribute("href")}))  
remDr$close()

urlDf <- tibble(Programs = Programs,
                urls = urls)  


remove(Fees)
for(prog in 1:length(urls)){
  
  print(prog)
  url <- urls[prog]
  #url <- urls[2]
  
  
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url) 
    
    webPageData <- webPage %>% 
      #html_nodes("div.domestic-tuition") %>% 
      html_nodes("table.pro-table") %>% 
      html_table()
    
    
    
    
    if(length(webPageData) == 1){
      
      
      webPageData <- webPageData %>% 
        bind_rows() %>% 
        mutate(url = url,
               program = urlDf$Programs[prog])
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}

write_csv(Fees , "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_CNC_Fees.csv")


#College of the rockies
urls <- read_html("http://www.cotr.bc.ca/tuition/")

groups <- urls %>% 
  html_nodes("optgroup")

names(groups) <- urls %>% 
  html_nodes("optgroup") %>% 
  html_attr("label")
group_values <- list()
for(group in 1:length(groups)){
  group_values[[group]] <- groups[[group]] %>% 
    html_text()%>% 
    strsplit("(?<=[a-z])(?=[A-Z])", perl = T) %>% 
    unlist()
  names(group_values)[group] <- names(groups)[group]
}
for(group in 1:(length(group_values) -1)){
  group_values[[group]] <- group_values[[group]][!group_values[[group]] %in% group_values[[group + 1]]]
}


library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
#rD


remove(Fees)
remove(group)
remove(prog)

grep("Emergency Medical Responder - Certificate of Achievement", group_values[[5]])

for(group in 5:length(group_values)){
  print(names(group_values)[group])
  url <- "http://www.cotr.bc.ca/tuition/"
  remDr$open(silent = T)
  remDr$navigate(url)
  
  for(prog in 1:length(group_values[[group]])){
    Sys.sleep(sample(2:5, 1))
    print(group_values[[group]][prog])
    xpathValue = paste0('/html/body/div[2]/div[5]/div[2]/div/div[2]/form/select/optgroup[',group, ']/option[', prog,']' )
    
    remDr <- rD$client
    
    webElem <- remDr$findElement(using = "xpath", xpathValue)
    webElem$clickElement()
    
    webElem <- remDr$findElement(using = "xpath", '//*[@id="content"]/div/div[2]/form/input[2]')
    webElem$clickElement()
    
    #summary //*[@id="content"]/div/div[2]/div/div/table[1]
    
    webElems <- remDr$findElements(using = "xpath", '//*[@id="content"]/div/div[2]/div/div/table[1]')
    summary <- unlist(lapply(webElems, function(x) {x$getElementText()}))
    
    #other //*[@id="content"]/div/div[2]/div/div/table[2]
    webElems <- remDr$findElements(using = "xpath", '//*[@id="content"]/div/div[2]/div/div/table[2]')
    other <- unlist(lapply(webElems, function(x) {x$getElementText()}))
    
    summary <- summary %>% gsub("Tuition:\n", "Tuition:", .) %>% gsub("and Year", "\n and Year", .) %>% strsplit("\n") %>% unlist() 
    summary <- summary[1:(length(summary) - 1)]
    names(summary) <- 1:length(summary)
    summary <- summary %>% 
      bind_rows() %>% 
      gather(Category, feeValue) %>% 
      separate(feeValue, c("Category", "feeValue"), sep = "\\$")
    
    other <- other %>% gsub("Additional Fees\n", "", .) %>% strsplit("(?<=[[:digit:]])(\n)", perl = T) %>% unlist()
    
    if(length(other) > 0){
      names(other) <- 1:length(other)
      other <- other %>% 
        bind_rows() %>% 
        gather(Category, feeValue) %>% 
        separate(feeValue, c("Category", "feeValue"), sep = "\\$|\n")
      
      tuition <- summary %>% 
        bind_rows(other) %>% 
        mutate(Program = group_values[[group]][prog],
               programGroup = names(group_values)[group])
    } else {
      tuition <- summary %>% 
        mutate(Program = group_values[[group]][prog],
               programGroup = names(group_values)[group])
    }
    
    
    if(!exists("Fees")){
      Fees <- tuition
    } else {
      Fees <- rbind(Fees, tuition)
    }
    
  }
}
remDr$close()


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_College_ot_Rockies_Tuition_Fees.csv")

#Douglas college
url <-"https://banserv2.douglas.bc.ca/prod/bwyfaapx.p_select_year"
webPage <- read_html(url)


groups <- urls %>% 
  html_nodes("option")

names(groups) <- urls %>% 
  html_nodes("optgroup") %>% 
  html_attr("label")
group_values <- list()
for(group in 1:length(groups)){
  group_values[[group]] <- groups[[group]] %>% 
    html_text()%>% 
    strsplit("(?<=[a-z])(?=[A-Z])", perl = T) %>% 
    unlist()
  names(group_values)[group] <- names(groups)[group]
}
for(group in 1:(length(group_values) -1)){
  group_values[[group]] <- group_values[[group]][!group_values[[group]] %in% group_values[[group + 1]]]
}


library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
#rD


remove(Fees)
remove(group)
remove(prog)

grep("Not StudentAidBC Eligible - Non-Degree (DDD1)", group_values[[1]])

#for(group in 5:length(group_values)){
#  print(names(group_values)[group])
url <- "https://banserv2.douglas.bc.ca/prod/bwyfaapx.p_select_year"
remDr$open(silent = T)
remDr$navigate(url)

webElem <- remDr$findElement(using = "xpath", '/html/body/div[3]/form/input[4]')
webElem$clickElement()
Sys.sleep(5)

webElem <- remDr$findElement(using = "xpath", '//*[@id="pgm_id"]')
progOptions <- webElem$selectTag()

remove(Fees)
for(index in 1:81){
  print(progOptions$text[index])
  xpathValue <- paste0('//*[@id="pgm_id"]/option[', index, ']')
  
  webElem <- remDr$findElement(using = "xpath", xpathValue)
  webElem$clickElement()
  
  webElem <- remDr$findElement(using = "xpath", '/html/body/div[3]/form/input[5]')
  webElem$clickElement()
  Sys.sleep(5)
  
  #only using first link
  webElem <- remDr$findElement(using = "xpath", '/html/body/div[3]/form/table[2]/tbody/tr[2]/td[1]/span/a')
  webElem$clickElement()
  
  Sys.sleep(5)
  webElem <- remDr$findElement(using = "xpath", '/html/body/div[3]/form/table[2]')
  webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
  tuition <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
  tuition <- tuition%>% 
    mutate(Program = progOptions$text[index])
  
  if(!exists("Fees")){
    Fees <- tuition
  } else {
    Fees <- rbind(Fees, tuition)
  }
  
  webElem <- remDr$findElement(using = "xpath", '/html/body/div[3]/form/input[5]')
  webElem$clickElement()
  Sys.sleep(5)
  
  webElem <- remDr$findElement(using = "xpath", '/html/body/div[3]/form/input[4]')
  webElem$clickElement()
  Sys.sleep(5)
}




remDr$close()


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Douglas_Tuition_Fees.csv")

#Langara
url <-"https://langara.ca/admissions/cost-of-study-tool/index.html"

library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr$open(silent = T)


possibleError <- function(errorIndex){
  tryCatch(
    remDr$findElement(using = "xpath", paste0('/html/body/table[', errorIndex, ']')),
    error = function(e) e
  )
}

remove(Fees)
for(index in 71:103){
  print(paste("index:",index))
  remDr$navigate(url)
  Sys.sleep(sample(4:5, 1))
  
  xpathValue <- paste0('/html/body/form/table/tbody/tr[2]/td[2]/select/option[', index, ']')
  
  frames <- remDr$findElements("css", "iframe")
  
  remDr$switchToFrame(frames[[1]])
  
  webElem <- remDr$findElement(using = "xpath", xpathValue)
  Program <- webElem$getElementAttribute("outerHTML")[[1]] %>% gsub("(.*>)([A-Z]{1,})(.*)(</option>)", "\\2\\3", .)
  print(Program)
  webElem$clickElement()
  Sys.sleep(sample(1:2, 1))
  
  webElem <- remDr$findElement(using = "xpath", '/html/body/form/pre/input[1]')
  webElem$clickElement()
  Sys.sleep(8)
  
  
  for(errorIndex in 1:31){
    errorCheck = suppressMessages(possibleError(errorIndex))
    if(!inherits(errorCheck, "error")){
      #print("no error")
      tableIndex <- errorIndex
    }
  }
  print(paste("tableIndex:", tableIndex))
  
  
  
  for(subTableIndex in c(4:tableIndex)[c(even(4:tableIndex))]){
    webElem     <- remDr$findElement(using = "xpath", paste0('/html/body/table[', subTableIndex, ']'))
    webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
    progInfo    <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]] %>% 
      spread(V1, V2)
    if(!grepl("Comment:", colnames(progInfo)[1], ignore.case = T)){
      progInfo <- tibble(`Comment:` = NA) %>% 
        bind_cols(progInfo)
    }
    if(!exists("progInfoComp")){
      progInfoComp <- progInfo
    } else {
      progInfoComp <- rbind(progInfoComp, progInfo)
    }
  }
  
  
  for(subTableIndex in c(4:tableIndex)[c(odd(4:tableIndex))]){
    webElem     <- remDr$findElement(using = "xpath", paste0('/html/body/table[', subTableIndex, ']'))
    webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
    tuiInfo     <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
    if(!exists("tuiInfoComp")){
      tuiInfoComp <- tuiInfo
    } else {
      tuiInfoComp <- rbind(tuiInfoComp, tuiInfo)
    }
    if(!exists("tuiInfoLength")){
      tuiInfoLength <- nrow(tuiInfo)
    } else {
      tuiInfoLength <- c(tuiInfoLength, nrow(tuiInfo))
    }
  }
  
  
  
  tuition <- progInfoComp %>% 
    bind_cols(tibble(count = tuiInfoLength)) %>% 
    uncount(count) %>% 
    bind_cols(tuiInfoComp)%>% 
    mutate(Program = Program)
  
  if(grepl("Comment", colnames(tuition)[1], ignore.case = T)){
    tuition <- tuition %>% 
      mutate(Comment = paste(Comment, `Comment:`)  )
    tuition <- tuition[2:ncol(tuition)]  
  }
  
  if(!grepl("Credits per Term", colnames(tuition)[1], ignore.case = T)){
    tuition <- tuition %>% 
      mutate(`Credits per Term:` = NA) %>% 
      select(`Credits per Term:`, `Number of Weeks:`:Program)
  }
  
  if(!exists("Fees")){
    Fees <- tuition
  } else {
    Fees <- rbind(Fees, tuition)
  }
  
  remove(tuiInfoLength)
  remove(tuiInfoComp)
  remove(progInfoComp)
  
}




remDr$close()
rD$server$stop()

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Langara_Tuition_Fees.csv")

#native education college

url <- "http://www.necvancouver.org/programs"
webPage <- read_html(url)

urls <- webPage %>% 
  html_nodes("div.container") %>% 
  html_nodes("a.program-course") %>% 
  html_attr("href")

urls <- paste0("http://www.necvancouver.org", urls)
urls[[1]]


remove(Fees)
for(prog in 1:length(urls)){
  
  print(prog)
  url <- urls[prog]
  #url <- urls[2]
  
  
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url) 
    
    webPageData <- webPage %>% 
      #html_nodes("div.domestic-tuition") %>% 
      html_nodes("table") %>% 
      html_table(fill = T) 
    
    webPageData <- webPageData[grepl("Tuition", webPageData, ignore.case = T)]
    
    if(length(webPageData) == 1){
      print("Processing program")
      
      
      webPageData <- webPageData %>% 
        bind_rows() 
      
      if(ncol(webPageData) < 2){
        webPageData <- webPageData %>% 
          separate(X1, c("X1", "X2"), ":")
      }
      
      webPageData <- webPageData %>%
        mutate(programUrl = url)
      
      
      if(!exists("Fees")){
        Fees <- webPageData
      } else {
        Fees <- rbind(Fees, webPageData)
      }
    }
  }
}

write_csv(Fees , "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Native_Education_Fees.csv")

#Okanagan
url <- "https://webapps-5.okanagan.bc.ca/ok/TuitionFees/"
webPage <- read_html(url)

tuition <- webPage %>% 
  html_nodes("table") %>% 
  html_table(fill = T)



tuition <- tuition %>%
  bind_rows() %>% 
  as_tibble()


tuition <- tuition[9:nrow(tuition),]
tuition_long <- tuition %>% 
  mutate(groupVar = ifelse(X2 %in% c(""), lead(X2), NA),
         groupVar = zoo::na.locf(ifelse(X2 == "Credits", "Credits", groupVar))) %>% 
  filter(X2 != "") %>% 
  group_by(groupVar) %>% 
  group_split() %>% 
  purrr::map(~ set_names(., nm = .[1,])) %>% 
  purrr::map(~ .[-1,]) %>% 
  purrr::map(~ janitor::clean_names(.))%>% 
  purrr::map(~ tibble::rowid_to_column(.,"sortOrder")) %>% 
  purrr::map(~ gather(., Category, feeValue, 4:ncol(.))) %>% 
  purrr::map(~ mutate(., Category = ifelse(Category == "tuition", "total_tuition", Category),
                      Category = gsub("_[[:digit:]]$", "", Category),
                      Type = colnames(.)[2])) %>% 
  purrr::map(~ distinct(.)) %>%
  purrr::map(~ set_names(., nm = c("sortOrder", "Program_or_course", "Credits_Length_level_totalhrs", "Category", "feeValue", "Type"))) %>% 
  purrr::map(~ arrange(., Program_or_course)) %>% bind_rows(., .id = "groupOrder") %>% 
  arrange(groupOrder, sortOrder) %>% 
  mutate(altType = ifelse(!grepl("[a-z]", Program_or_course), Program_or_course, NA),
         altType = zoo::na.locf(ifelse(lag(Type) != Type, Type, altType), na.rm = F),
         altType = ifelse(is.na(altType), Type, altType)) %>% 
  select(-Type) %>% 
  rename(Type = altType)

write_csv(tuition_long, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Okanagan_Fees.csv")


#Selkirk
#url <- "http://selkirk.ca/programs"
#for(index in 1:9){
#  urlPage = paste0(url, "?page=", index)
#  webPage <- read_html(url) %>% 
#    html_nodes("div.view-content") %>% 
#    html_nodes("a") %>% 
#    html_attr("href")
#  
#  if(!exists("urls")){
#    urls <- webPage
#  } else {
#    urls <- c(urls, webPage)
#  }
#}
#
#
#urls <- urls[!grepl("interest=\\&keyword=", urls, ignore.case = T)]
#
#for(prog in 1:length(urls)){
#  url <- paste0("http://selkirk.ca", urls[prog])
#  print(prog)
#  
#  possibleError <- tryCatch(
#    read_html(url),
#    error = function(e) e
#  )
#  
#  if(!inherits(possibleError, "error")){
#  
#    webPage <- read_html(url)
#    webPageUrls <- webPage %>% 
#      html_nodes("li.expanded") %>% 
#      html_nodes("ul.menu") %>% 
#      html_nodes("a") %>% 
#      html_attr("href")
#    
#    webPageUrls <- webPageUrls[grepl("fees", webPageUrls, ignore.case = T)]
#    
#    if(!exists("feeUrls")){
#      feeUrls <- webPageUrls
#    } else {
#      feeUrls <- c(feeUrls, webPageUrls)
#    }
#    }
#}
#

url <- "http://selkirk.ca/financial-information/tuition-fees/tuition-fees"
webPage <- read_html(url)

tuition <- webPage %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  bind_rows()

colnames(tuition) <- tuition[1,]
tuition <- tuition %>% 
  mutate(type = zoo::na.locf(ifelse(!grepl("[a-z]", PROGRAM), PROGRAM, NA ), na.rm = F)) %>% 
  janitor::clean_names() %>% 
  filter(!domestic_tuition_fees %in% c("", "Domestic\n\t\t\t\tTuition & Fees")) %>% 
  gather(Category, feeValue, 2:3)


write_csv(tuition, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Selkirk_Fees.csv")

#Fraser Valley

url <- "https://www.ufv.ca/programs/"
webPage <- read_html(url)
urls <- webPage %>% 
  html_nodes("table.display") %>% 
  html_nodes("a") %>% 
  html_attr("href")

remove(Fees)
for(prog in 1:length(urls)){
  url <- paste0("https://www.ufv.ca", urls[prog])
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    
    nextUrl <- webPage %>% 
      html_nodes("div.call-to-action") %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    nextUrl <- paste0("https://www.ufv.ca", nextUrl)
    
    
    possibleError2 <- tryCatch(
      read_html(nextUrl),
      error = function(e) e
    )
    
    if(!inherits(possibleError2, "error")){
      print("proceeding to next")
      
      webPage <- read_html(nextUrl)
      
      nextNextUrl <- webPage %>% 
        html_nodes("div.quick-facts-right") %>% 
        html_nodes("a") %>% 
        html_attr("href")
      
      nextNextUrl <- paste0("https://www.ufv.ca", nextNextUrl)
      
      Program <- webPage %>% 
        html_nodes("h1") %>% 
        html_text()
      
      possibleError3 <- tryCatch(
        read_html(nextUrl),
        error = function(e) e
      )
      
      if(!inherits(possibleError3, "error")){
        webPage <- read_html(nextNextUrl)
        print(paste("working on ", Program))
        
        tuition <- webPage %>% 
          html_nodes("table.table-hover") %>% 
          html_table() 
        
        if(length(tuition) > 0){
          colnames(tuition[[1]])[1] <- "Category"
          tuition <- tuition %>% 
            bind_rows() %>% 
            gather(subCategory, feeValue, 2:ncol(.)) %>% 
            mutate(Program = Program,
                   ProgramUrl = url,
                   ProgramSubUrl = nextUrl)
          
          if(!exists("Fees")){
            Fees = tuition
          } else {
            Fees = bind_rows(Fees, tuition)
          }
        }
        
      }
    }
  }
  
  
}


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Fraser_Valley_Fees.csv")

#Vancouver community college
url <- "https://www.vcc.ca/programscourses/programs-a-z/"
webPage <- read_html(url)
urls <- webPage %>% 
  html_nodes("table") %>% 
  html_nodes("a") %>% 
  html_attr('href')

urls <- urls[grepl("programscourses", urls)]


remove(Fees)
for(prog in 1:length(urls)){
  
  url <- paste0("https://www.vcc.ca", urls[prog], "#fees")
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    webPage <- read_html(url)
    tuition <- webPage %>% 
      html_nodes(xpath = '//*[@id="table-one-column-emphasis"]') %>% 
      html_table(fill = T)
    
    Program <- webPage %>% 
      html_nodes("h1") %>% 
      html_text()
    
    tuition <- tuition[grepl("\\$", tuition)]
    if(length(tuition) > 0){
      print(paste("proceeding with:", Program))
      tuition <- tuition %>% 
        bind_rows() %>% 
        mutate(Program = Program,
               programUrl = paste0("https://www.vcc.ca", urls[prog]))
      
      if(!exists("Fees")){
        Fees = tuition
      } else {
        Fees = bind_rows(Fees, tuition)
      }
      
    }
    
  }
}


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Vancouver_Community_College_Fees.csv")

#Vancouver Island

url <- "https://www.viu.ca/programs/areas-study"

webPage <- read_html(url)
urls <- webPage %>% 
  html_nodes("a.progAreaGrid__link") %>% 
  html_attr("href")


remove(Fees)
for(prog in 1:length(urls)){
  
  url <- urls[prog]
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    nextUrls <- webPage %>% 
      html_nodes("ul.sub-grouping") %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    if(length(nextUrls) > 0){
      if(!exists("programUrls")){
        programUrls <- nextUrls
      } else {
        programUrls <- c(programUrls, nextUrls)
      }
      
    }
    
  }
  
}

urls <- unique(programUrls)

for(prog in 1:length(urls)){
  
  url <- urls[prog]
  print(prog)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    tuition <- webPage %>% 
      html_nodes("table.table") %>% 
      html_table(fill = T)
    
    tuition <- tuition[grepl("Amount", tuition)]
    headers <- webPage %>% 
      html_nodes("h2") %>% 
      html_text()
    headers <- headers[grepl("international fees|domestic fees", headers, ignore.case = T)]
    Program <- webPage %>% 
      html_nodes("h1") %>% 
      html_text()
    
    if(length(tuition) > 0){
      print(paste("proceeding with:", Program))
      if(length(headers) > 1){
        names(tuition) <- paste(rep(headers, each = length(tuition)/2), rep(1:(length(tuition)/2), 2), sep = "_")
      } else {
        names(tuition) <- paste(headers, 1:length(tuition), sep = "_")
      }
      
      tuition <- tuition %>% 
        purrr::map(~ mutate(., Amount = as.character(Amount),
                            feeType = colnames(.)[1])) %>% 
        purrr::map(~ setNames(., c("Category", "feeValue", "feeType"))) %>% 
        bind_rows(.id = "studentType") %>% 
        mutate(Program = Program,
               programUrls = url)
      
      if(!exists("Fees")){
        Fees = tuition
      } else {
        Fees = bind_rows(Fees, tuition)
      }
      
    }
  }
}

write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Vancouver_Island_College_Fees.csv")

#Kengjewinteg

url <- "https://www.kenjgewinteg.ca/post-secondary-programs---current.html"
webPage <- read_html(url)

webPageData <- webPage %>% 
  html_nodes("div.paragraph") %>%
  html_text()  ##Very unstructured

webPageData <- webPageData[grepl("Tuition", webPageData)] %>% 
  strsplit("(?<=[a-z]|\\.|\\))(?=[A-Z])|(?<=[[:digit:]])(?=\\*)|(?<=.)(?=Fees/)|(?<=.)(?=Partner:)|(?<=.)(?=Fall 2019)", perl = T)

sapply(webPageData, function(x) length(x))
webPageData[[4]][4] <- paste(webPageData[[4]][4], webPageData[[4]][5])
webPageData[[4]][6] <- paste(webPageData[[4]][6], webPageData[[4]][7])
webPageData[[4]] <- webPageData[[4]][c(1:4,6,8:9)]


Programs <- webPage %>% 
  html_nodes("h2") %>%
  html_text() 


names(webPageData) <- Programs[4:7]

tuition <- webPageData %>% 
  bind_rows() %>% 
  gather(Program, Category) %>% 
  separate(Category, c("Category", "feeValue"), sep = ":")

write_csv(tuition, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Kenjgewin_Teg_Fees.csv")

#Conestoga

ConestogaPrograms <- read_csv("C:/Users/psoeraas/OneDrive - Colleges and Institutes Canada/Peder/2019/temp/2019_Prgs_Conestoga_1of2-(Crawl-Run)---2019-08-28T200541Z.csv")

url <- paste0(ConestogaPrograms$url[1], "#domestic-fees-link-noncoop")

library(RSelenium)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)

possibleError <- function(xpathValue){
  tryCatch(
    remDr$findElement(using = "xpath", xpathValue),
    error = function(e) e
  )
}



remDr$navigate(url)
remove(ConestogaProgramsTable)
for(tableIndex in 1:19){
  xpathValue = paste0('//*[@id="maincontent"]/table[', tableIndex, ']')
  webElem <- remDr$findElement(using = "xpath", xpathValue)
  webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
  progInfo    <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
  
  urls <- webElem$findChildElements(using = "css", 'a')
  urls <- unlist(sapply(urls, function(x){x$getElementAttribute("href")}))
  
  progInfo <- progInfo %>% 
    bind_cols(tibble(urls = urls))
  
  if(!exists("ConestogaProgramsTable")){
    ConestogaProgramsTable <- progInfo
  } else {
    ConestogaProgramsTable <- bind_rows(ConestogaProgramsTable,progInfo)
  }
}

remove(Fees)
#start at 123
for(prog in 123:nrow(ConestogaProgramsTable)){
  print(prog)
  
  
  
  url = ConestogaProgramsTable$urls[prog]
  
  remDr$navigate(url)
  Sys.sleep(sample(3:5, 1))
  
  
  firstElementErrorCheck = suppressMessages(possibleError('//*[@id="maincontent"]/h1'))
  if(!inherits(firstElementErrorCheck, "error")) {
    
    
    
    Program = remDr$findElement(using = "xpath", '//*[@id="maincontent"]/h1')
    Program = Program$getElementText()
    
    #Domestic
    remove(domTuition)
    errorCheck = suppressMessages(possibleError('//*[@id="domestic-fees-link-noncoop"]'))
    if(!inherits(errorCheck, "error")){
      webElem <- remDr$findElement(using = "xpath", '//*[@id="domestic-fees-link-noncoop"]')
      webElem$clickElement()
      Sys.sleep(2)
      
      for(tableIndex in 1:4){
        xpathValue = paste0('//*[@id="domestic-fees-noncoop"]/table[', tableIndex, ']')
        
        
        
        subErrorCheck = suppressMessages(possibleError(xpathValue))
        if(!inherits(subErrorCheck, "error")){
          webElem <- remDr$findElement(using = "xpath", xpathValue)
          webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
          webElem <- remDr$findElement(using = "xpath", paste0('//*[@id="domestic-fees-noncoop"]/table[', tableIndex, ']/caption'))
          tableCaption <- webElem$getElementText()
          progInfo    <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
          progInfo <- progInfo %>% 
            mutate(Caption = tableCaption,
                   studentType = "Domestic",
                   Program = Program,
                   programUrl = url)
          
          if(!exists("domTuition")){
            domTuition <- progInfo
          } else {
            domTuition <- bind_rows(domTuition, progInfo)
          }
        }
      }
    }
    
    
    #International
    remove(intlTuition)
    errorCheck = suppressMessages(possibleError('//*[@id="international-fees-link-noncoop"]'))
    if(!inherits(errorCheck, "error")){
      webElem <- remDr$findElement(using = "xpath", '//*[@id="international-fees-link-noncoop"]')
      webElem$clickElement()
      Sys.sleep(2)
      
      for(tableIndex in 1:4){
        
        xpathValue = paste0('//*[@id="international-fees-noncoop"]/table[', tableIndex, ']')
        
        subErrorCheck = suppressMessages(possibleError(xpathValue))
        if(!inherits(subErrorCheck, "error")){
          webElem <- remDr$findElement(using = "xpath", xpathValue)
          webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
          webElem <- remDr$findElement(using = "xpath", paste0('//*[@id="international-fees-noncoop"]/table[', tableIndex, ']/caption'))
          tableCaption <- webElem$getElementText()
          progInfo    <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
          progInfo <- progInfo %>% 
            mutate(Caption = tableCaption,
                   studentType = "International",
                   Program = Program,
                   programUrl = url)
          
          if(!exists("intlTuition")){
            intlTuition <- progInfo
          } else {
            intlTuition <- bind_rows(intlTuition, progInfo)
          }
        }
      }
    }  
    
    
    #Domestic Coop
    remove(domCoopTuition)
    errorCheck = suppressMessages(possibleError('//*[@id="domestic-fees-link-coop"]'))
    if(!inherits(errorCheck, "error")){
      webElem <- remDr$findElement(using = "xpath", '//*[@id="domestic-fees-link-coop"]')
      webElem$clickElement()
      Sys.sleep(2)
      
      for(tableIndex in 1:4){
        
        xpathValue = paste0('//*[@id="domestic-fees-link-coop"]/table[', tableIndex, ']')
        
        subErrorCheck = suppressMessages(possibleError(xpathValue))
        if(!inherits(subErrorCheck, "error")){
          webElem <- remDr$findElement(using = "xpath", xpathValue)
          webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
          webElem <- remDr$findElement(using = "xpath", paste0('//*[@id="domestic-fees-link-coop"]/table[', tableIndex, ']/caption'))
          tableCaption <- webElem$getElementText()
          progInfo    <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
          progInfo <- progInfo %>% 
            mutate(Caption = tableCaption,
                   studentType = "Domestic Coop",
                   Program = Program,
                   programUrl = url)
          
          if(!exists("domCoopTuition")){
            domCoopTuition <- progInfo
          } else {
            domCoopTuition <- bind_rows(domCoopTuition, progInfo)
          }
        }
      }
    }
    
    
    #Intl Coop
    remove(intlCoopTuition)
    errorCheck = suppressMessages(possibleError('//*[@id="international-fees-coop"]'))
    if(!inherits(errorCheck, "error")){
      webElem <- remDr$findElement(using = "xpath", '//*[@id="international-fees-coop"]')
      webElem$clickElement()
      Sys.sleep(2)
      
      for(tableIndex in 1:4){
        
        xpathValue = paste0('//*[@id="international-fees-coop"]/table[', tableIndex, ']')
        
        subErrorCheck = suppressMessages(possibleError(xpathValue))
        if(!inherits(subErrorCheck, "error")){
          webElem <- remDr$findElement(using = "xpath", xpathValue)
          webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
          webElem <- remDr$findElement(using = "xpath", paste0('//*[@id="international-fees-coop"]/table[', tableIndex, ']/caption'))
          tableCaption <- webElem$getElementText()
          progInfo    <- XML::readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
          progInfo <- progInfo %>% 
            mutate(Caption = tableCaption,
                   studentType = "Domestic Coop",
                   Program = Program,
                   programUrl = url)
          
          if(!exists("domCoopTuition")){
            intlCoopTuition <- progInfo
          } else {
            intlCoopTuition <- bind_rows(intlCoopTuition, progInfo)
          }
        }
      }
    }
    
    if(!exists("Fees")){
      if(exists("domTuition")){
        Fees <- domTuition
      }
      if(!exists("Fees") & exists("intlTuition")){
        Fees <- intlTuition
      } else if(exists("Fees") & exists("intlTuition")){
        Fees <- bind_rows(Fees, intlTuition)
      }
      if(!exists("Fees") & exists("domCoopTuition")){
        Fees <- domCoopTuition
      } else if(exists("Fees") & exists("domCoopTuition")){
        Fees <- bind_rows(Fees, domCoopTuition)
      }
      if(!exists("Fees") & exists("intlCoopTuition")){
        Fees <- intlCoopTuition
      } else if(exists("Fees") & exists("intlCoopTuition")){
        Fees <- bind_rows(Fees, intlCoopTuition)
      }
    } else {
      if(exists("domTuition")){
        Fees <- bind_rows(Fees, domTuition)
      }
      if(exists("intlTuition")){
        Fees <- bind_rows(Fees, intlTuition)
      }
      if(exists("domCoopTuition")){
        Fees <- bind_rows(Fees, domCoopTuition)
      }
      if(exists("intlCoopTuition")){
        Fees <- bind_rows(Fees, intlCoopTuition)
      }
    }
  }
  
}

Fees$Caption <- unlist(Fees$Caption)  
Fees$Program <- unlist(Fees$Program)  


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Conestoga_Fees.csv")

remDr$close()



## Assiniboine

urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl("assiniboine", urlFiles, ignore.case = T)])) %>% 
  select(url, Program)


for(prog in 1:nrow(urls)){
  print(prog)
  url <- gsub("program-details", "courses", urls$url[prog])
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url) %>% 
      html_nodes("p.costs") %>% 
      html_text()
    
    if(length(webPage) > 0){
      print("got tuition")
      webPage <- webPage %>% 
        gsub("([[:digit:]]{2}|\\$)([A-Z])", "\\1_\\2", .) %>% 
        strsplit("_") %>% 
        unlist() %>% 
        strsplit(":") %>% 
        unlist()
      tuition_df <- strToDf(webPage) %>% 
        mutate(Program = urls$Program[prog],
               url = urls$url[prog])
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }
      
    }
  }
}


write_csv(Fees, "C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_Assiniboine_Fees.csv")




## Cambrian
Institution <- "Cambrian"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)

url <- "https://cambriancollege.ca/apply/how-to-apply/tuition-and-fees/domestic-students-tuition-2019-20/"

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)

XpathPossibleError <- function(xpathValue){
  tryCatch(
    remDr$findElement(using = "xpath", xpathValue),
    error = function(e) e
  )
}

remDr$navigate(url)
Sys.sleep(2)
webElem <- remDr$findElement(using = "xpath", '//*[@id="tuition-fees-table_info"]')
entryNumber <- webElem$getElementText() %>% 
  unlist() %>% 
  gsub("(Showing.*of )([[:digit:]]{3,4})( entries)", "\\2", .) %>% 
  as.numeric()

webElem <- remDr$findElement(using = "xpath", '/html/body/main/div/div/div[2]/div[1]/div/div[1]/label/select/option[4]')
webElem$clickElement()

for(i in 1:ceiling(entryNumber/100)){
  print(paste("Overall Page:", i))
  Sys.sleep(2)
  webElem <- remDr$findElement(using = 'xpath', '/html/body/main/div/div/div[2]/div[1]/div/div[3]')
  #webElem5txt <- webElem$getElementAttribute("outerHTML")[[1]]
  webElemtable <- webElem$getElementText() %>% 
    unlist() %>% 
    strsplit("\n") %>% 
    unlist() %>% 
    gsub("([A-Z]{4}|20[[:digit:]]{2}|\\$[[:digit:]]*,*[[:digit:]]{3}\\.[[:digit:]]{2})", "_\\1_", .) %>% 
    strsplit("_")
  
  #tuitionColnames <- webElemtable[[1]]
  webElemtable <- webElemtable[2:length(webElemtable)]
  
  names(webElemtable) <- 1:length(webElemtable)
  tuition_df <- webElemtable %>% 
    bind_cols() %>% 
    gather(Category, Value) %>% 
    mutate(Index = rep(1:7, nrow(.)/7)) %>% 
    spread(Index, Value) %>% 
    mutate(Category = as.numeric(Category)) %>% 
    arrange(Category) %>% 
    select(-Category) %>% 
    set_names(c("Program_Name", "Program_Code", "Term", "Year", "Semester", "Total_Fees", "Fees_Breakdown"))
  

  #remove(Fees)
  for(i in 42:100){
    print(i)
    xpath = paste0("/html/body/main/div/div/div[2]/div[1]/div/div[3]/div[2]/table/tbody/tr[", i, "]/td[6]/a")
    webElem <- remDr$findElement(using = "xpath", xpath)
    webElem$clickElement()
    Sys.sleep(1)
    
    webElem <- remDr$findElement(using = 'xpath', '/html/body/main/div/div/div[2]/div[2]/div')
    webElemtable <- webElem$getElementText() %>% 
      unlist() %>% 
      strsplit("\n") %>% 
      unlist()
    
    newCols <- strToDf(webElemtable) %>% 
      spread(Category, Value) %>% 
      janitor::clean_names() %>% 
      select(c("tuition_coop_fee", 
               "compulsory_program_and_institutional_ancillary_fees",
               "total_fees",
               "optional_ancillary_fees",  
               "total_fees_w_optional_fees"))
    
    
    if(!exists("allNewCols")){
      allNewCols <- newCols
    } else {
      allNewCols <- allNewCols %>% 
        bind_rows(newCols)
    }
    
    webElem <- remDr$findElement(using = "xpath", '/html/body/main/div/div/div[2]/div[2]/a')
    webElem$clickElement()
    Sys.sleep(2)
  }
  
  tuition_df <- tuition_df %>% 
    bind_cols(allNewCols)
  remove(allNewCols)
  
  if(!exists("Fees")){
    Fees <- tuition_df
  } else {
    Fees <- Fees %>% 
      bind_rows(tuition_df)
  }
  
  webElem <- remDr$findElement(using = "xpath", '//*[@id="tuition-fees-table_next"]')
  webElem$clickElement()
  
}
  
  
  
remDr$close()


write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))





## Canadore
Institution <- "Canadore"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)


for(prog in 1:nrow(urls)){
  print(prog)
  url <- urls$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    
    tableError <- tryCatch(
      read_html(url) %>% 
        html_nodes("table.table") %>% 
        html_table(fill = T),
      error = function(e) e
    )
    
    if(!inherits(tableError, "error")){
      tuition_df <- read_html(url) %>% 
        html_nodes("table.table") %>% 
        html_table(fill = T)
      if(length(tuition_df) > 0){
        tuition_df <- tuition_df[grepl("\\$", tuition_df)]
        if(length(tuition_df) > 0){
          print('found tuition')
          tuition_df <- tuition_df %>% 
            purrr::map(~ mutate_all(., as.character()))
          tuition_df <- tuition_df %>% 
            bind_rows() %>% 
            gather(Category, Value, Tuition:Total) %>% 
            mutate(url = url,
                   Program = urls$Program[prog])
          
          if(!exists("Fees")){
            Fees <- tuition_df
          } else {
            Fees <- Fees %>% 
              bind_rows(tuition_df)
          }
        }
      }
    }
  }
}




write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))




## Confederation
Institution <- "Confederation"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)

for(prog in 1:nrow(urls)){
  print(prog)
  url <- urls$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    tuition_labs <- webPage %>% 
      html_nodes("div.program-fees-block") %>%
      html_nodes("li") %>% 
      html_nodes("div.label") %>% 
      html_text()
    
    tuition_dets <- webPage %>% 
      html_nodes("div.program-fees-block") %>%
      html_nodes("li") %>% 
      html_nodes("span") %>% 
      html_text()
    
    if(length(tuition_labs) > 0){
      print("got tuition")
      tuition_df <- tibble(Category = tuition_labs,
                           Value = tuition_dets,
                           Program = urls$Program[prog],
                           url = url)
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }
    }
  }
}

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))



## Durham
Institution <- "Durham"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)

for(prog in 1:nrow(urls)){
  print(prog)
  url <- urls$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    tuition <- webPage %>% 
      html_nodes("table.program-costs-table") %>%
      html_table(fill = T)
    
    if(length(tuition) > 0){
      tuition <- tuition[grepl("\\$", tuition)]
      if(length(tuition) > 0){
        print("got tuition")
        tuition_df <- tuition %>% 
          bind_rows() %>% 
          gather(Category, Value) %>% 
          mutate(Program = urls$Program[prog],
                 url = url)
        
        
        if(!exists("Fees")){
          Fees <- tuition_df
        } else {
          Fees <- Fees %>% 
            bind_rows(tuition_df)
        }
      }
    }
  }
}

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))



## Georgian
Institution <- "Georgian"

tuition <- pdftools::pdf_text("https://www.georgiancollege.ca/wp-content/uploads/2019-20-Web-Fees-13.pdf")

tuition <- tuition %>% 
  strsplit("\r\n") %>% 
  unlist()

tuition_cleaner <- tuition[grepl("^ [A-Z]{3,4}", tuition)] %>% 
  gsub("[[:space:]]{2,}", "_", .) %>% 
  strsplit("_") %>% 
  unlist()

tuition_clean <- strToDf(tuition_cleaner, 11, c("Program", "Description", "Campus", "Term", "Semester", "Domestic_tuition", "Domestic_compuls_Anc_Fees", "Domestic_Total_Fees", "Intl_tuition", "Intl_compuls_Anc_Fees", "Intl_Total_Fees"))

tuition_df <- tuition_clean %>% 
  gather(Category, Value, -c(1:5))


write_csv(tuition_df, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))




## Fanshawe
Institution <- "Fanshawe"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)

for(prog in 1:nrow(urls)){
  print(prog)
  url <- paste0(urls$url[prog], "/domestic/fees/next")
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    tuition <- webPage %>% 
      html_nodes("table.fees-tbl") %>%
      html_table(fill = T)
    
    if(length(tuition) > 0){
      tuition <- tuition[grepl("\\$", tuition)]
      if(length(tuition) > 0){
        print("got tuition")
        
        tuition_df <- tuition %>% 
          bind_rows() 
        colnames(tuition_df) <- tuition_df[1,]
        tuition_df <- tuition_df[-1,]
        tuition_df <- tuition_df %>% 
          gather(Category, Value, -1) %>% 
          mutate(Program = urls$Program[prog],
                 url = url)
        
        
        if(!exists("Fees")){
          Fees <- tuition_df
        } else {
          Fees <- Fees %>% 
            bind_rows(tuition_df)
        }
      }
    }
  }
}

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))



## Centennial
Institution <- "Centennial"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)

rD <- rsDriver(browser = "firefox", verbose = FALSE)
remDr <- rD$client
remDr$open(silent = T)
XpathPossibleError <- function(xpathValue){
  tryCatch(
    remDr$findElement(using = "xpath", xpathValue),
    error = function(e) e
  )
}

for(prog in 1:nrow(urls)){
  print(prog)
  url <- urls$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    
    remDr$navigate(url)
    Sys.sleep(2)
    
    tuiButtonCheck <- suppressMessages(XpathPossibleError('/html/body/div[2]/main/section[3]/div/aside/ul/li[6]/a'))
    
    if(!inherits(tuiButtonCheck, "error")){
      webElem <- remDr$findElement(using = 'xpath', '/html/body/div[2]/main/section[3]/div/aside/ul/li[6]/a')
      webElem$clickElement()
      
      webElem <- remDr$findElement(using = 'xpath', '/html/body/div[2]/main/section[3]/div/section/div[6]/table')
      webElemtable <- webElem$getElementText() %>% 
        unlist() %>% 
        strsplit("\n") %>% 
        unlist() %>% 
        gsub("\\$", "_\\1", .) %>% 
        strsplit("_") %>% 
        unlist()
      
      webElemtable <- webElemtable[-1]
      
      tuition_df <- strToDf(webElemtable, 4, c("Student", "Tuition_total", "Ancillary", "Total")) %>% 
        gather(Category, Value) %>% 
        mutate(Program = urls$Program[prog],
               url = url)
      
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }
    }
    
  }
}

Fees <- Fees %>% 
  filter(!is.na(Category)) %>% 
  select(Category:url)

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))




## JIBC
Institution <- "JIBC"
remove(overallPrograms)
for(i in 0:3){
  print(i)
  if(i == 0){
    url <- "http://www.jibc.ca/programs"
  } else {
    url = paste0("http://www.jibc.ca/programs?page=", i)
  }
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    Programs <- webPage %>% 
      html_nodes("table.views-table") %>% 
      html_table(fill = T)
    
    Program_urls <- webPage %>% 
      html_nodes("table.views-table") %>% 
      html_nodes("td.views-field-title") %>% 
      html_nodes("a") %>% 
      html_attr("href") %>% 
      paste0("http://www.jibc.ca", .)
    
    if(length(Programs) > 0){
      print("got programs")
      Programs <- Programs %>% 
        bind_rows() %>% 
        set_names(c("Program", "Campus", "Credential", "Description")) %>% 
        mutate(url = Program_urls,
               Duration = NA,
               WIL = NA) %>% 
        select(url, Program, Credential, Campus, Duration, Description, WIL)
      
      if(!exists("overallPrograms")){
        overallPrograms <- Programs
      } else {
        overallPrograms <- overallPrograms %>% 
          bind_rows(Programs)
      }
    }
    
  }
}
remove(Fees)  
for(prog in 1:nrow(overallPrograms)){
  print(prog)
  url <- overallPrograms$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    index <- webPage %>% 
      html_nodes('h3.accordion-heading') %>% 
      #html_nodes("h3") %>% 
      html_text()
    
    index <- grep('cost', index, ignore.case = T)
    
    if(length(index) > 0){
      #xpathValue <- paste0('//*[@id="middle"]/div[2]/div[', index, ']')
      tuition <- webPage  %>% 
        html_nodes('div.accordion-section')
      #prog = 1
      #http://www.jibc.ca/programs-courses/schools-departments/school-health-community-social-justice/centre-counselling-community-safety/programs/critical-incident-stress-management
      ##start here  
      tuition <-tuition[as.numeric(index[1])] %>% 
        html_nodes("p") %>% 
        html_text() 
      
      if(length(tuition) > 0){
        
        tuition <- tuition[grepl("\\$", tuition)]
        if(length(tuition) > 0){
          tuition <- tuition %>% 
            gsub("([[:digit:]])([A-Z])", "\\1_\\2", .) %>% 
            strsplit("_") %>% 
            unlist() %>% 
            gsub("(\\$)", "_\\1", .) %>% 
            strsplit("_") %>% 
            unlist() 
          
          if(length(tuition) %% 2 == 0){
            print("got fees")
            tuition_df <- strToDf(tuition, 2, c("Category", "Value")) %>% 
              mutate(Program = overallPrograms$Program[prog],
                     url = url)
            if(!exists("Fees")){
              Fees <- tuition_df
            } else {
              Fees <- Fees %>% 
                bind_rows(tuition_df)
            }
          }
          
        }
      }  
    }
  }
}


write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))




## Kwantlen
Institution <- "Kwantlen"


# Kwantlen
## dac000bb-3eed-43d0-bb94-00d4866a1f7f

Institution = "Kwantlen"


url <- "https://www.kpu.ca/calendar/2018-19/program_indices.html"

webPage <- read_html(url)
urls_info <- webPage %>% 
  html_nodes(xpath = '/html/body/div[2]/div/div/div[1]/div[4]/div/article/div/table/tbody/tr[2]/td[3]')

d <- tibble(url = urls_info %>% html_nodes('a') %>% html_attr("href"),
            Program = urls_info %>% html_nodes('p') %>% html_text() %>% gsub("[[:space:]]{2,}", "", .))


tuition_df <- d %>% 
  mutate(Duration = NA,
         url = paste0("https://www.kpu.ca/", url)) %>% 
  select(url, Program, Duration)



for(prog in 1:nrow(tuition_df)){
  print(prog)
  
  
  url <- paste0(tuition_df$url[prog])
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    print(paste("moving on with:", prog))
    webPage <- read_html(url)
    
    prog_dets <- webPage %>% 
      html_nodes(xpath = '/html/body/div[2]/div/div/div[1]/div[4]/div/article/div[2]') %>% 
      html_text() %>% 
      strsplit("\n")  %>% 
      unlist() %>% 
      gsub("\n|\t\r|[[:space:]]{2,}", "", .)
    
    if(length(prog_dets) > 0){
      prog_dets <- prog_dets[!grepl("^$", prog_dets)]
      prog_dets_df <- tibble(Value = prog_dets) %>% 
        mutate(Category = ifelse(Value %in% c("At a Glance", 
                                              "Program Type:", 
                                              "Credential Granted:", 
                                              "Offered At:", 
                                              "Start Date(s):", 
                                              "Intake Type:", 
                                              "Format:", 
                                              "Minimum Credits Required:", 
                                              "Instructional Cycle:", 
                                              "Curriculum Effective Date:",
                                              "Applying to KPU:",
                                              "Apply Now"), Value, NA),
               Category = zoo::na.locf(Category)) %>% 
        filter(Category != Value)

      Duration <- paste("Minimum Credits Required:", prog_dets_df %>% filter(Category == "Minimum Credits Required:") %>% select(Value) %>% unlist() %>% paste(collapse = " AND "))
      if(length(Duration) > 0){
        print(Duration)
        tuition_df$Duration[prog] <- Duration
      }
    }
 
  }
}

url <- "https://www.kpu.ca/calendar/2018-19/ar/tuitionfees.html"

webPage <- read_html(url)

tuition_table <- webPage %>% 
  html_nodes("table.table-box") %>% 
  html_table(fill = T)
tuition_table <- tuition_table[grepl("\\$", tuition_table)]
tuition_table_long <- tuition_table[1:3] %>% 
  bind_rows() %>% 
  gather(Category, Value, -(1:2)) %>% 
  separate(Category, c("Category", "Term"), sep = "\\(") %>% 
  filter(!is.na(Value)) %>% 
  mutate(Term = gsub("\\)", "", Term),
         `Tuition Category` = zoo::na.locf(ifelse(grepl("^[[:digit:]]", `Tuition Category`), NA, `Tuition Category`))) %>% 
  janitor::clean_names() %>% 
  filter(applicable_courses_program_s != tuition_category) %>% 
  rename(Program = applicable_courses_program_s) %>% 
  separate_rows(Program, sep = "[[:space:]]{4,}")

tuition_df_joined <- tuition_df %>% 
  mutate(Program = gsub(":.*$", "", Program)) %>% 
  left_join(tuition_table_long %>% 
              mutate(Program = gsub("Post-Baccalaureate Diploma in |Diploma in |Graduate Diploma in ", "", Program),
                     Program = gsub("&", "and", Program)), by = "Program") %>% 
  filter(!is.na(tuition_category)) %>% 
  mutate(Credits_req = as.numeric(gsub("Minimum Credits Required: ", "", Duration)),
         calculated_tuition = ifelse(grepl("tuition", category, ignore.case = T) & grepl("per credit", term, ignore.case = T), as.numeric(gsub("\\$| |,", "", value)) * Credits_req, NA))
 


write_csv(tuition_table_long, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_includes_general_mentions_Fees.csv")) 



write_csv(tuition_df_joined, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))





## MITT
Institution <- "MITT"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)

remove(Fees)
for(prog in 1:nrow(urls)){
  print(prog)
  url <- urls$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    prog_dets <- webPage %>% 
      html_nodes("ul.atAGlanceContainer") 
    
    if(length(prog_dets) == 2) {
      print("got fees")
      Canadian <- prog_dets[1] %>% 
        html_nodes('li') %>% 
        html_nodes("div") %>% 
        html_text() %>% 
        strToDf() %>% 
        mutate(StudentType = "Canadian")
      
      
      International <- prog_dets[2] %>% 
        html_nodes('li') %>% 
        html_nodes("div") %>% 
        html_text()%>% 
        strToDf() %>% 
        mutate(StudentType = "International")
      
      tuition_df <- Canadian %>% 
        bind_rows(International) %>% 
        mutate(Program = urls$Program[prog],
               url = url)
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }
      
    } else if(length(prog_dets) == 1){
      print("got fees")
      tuition_df <- prog_dets[1] %>% 
        html_nodes('li') %>% 
        html_nodes("div") %>% 
        html_text()%>% 
        strToDf() %>% 
        mutate(StudentType = "Canadian") %>% 
        mutate(Program = urls$Program[prog],
               url = url)
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }
    }
  }
}

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))




## Nicola_Valley
Institution <- "Nicola_Valley"



url <- "https://www.nvit.ca/tuitionandfees.htm"
webPage <- read_html(url)

tuition_df <- webPage %>% 
  html_nodes("table.respond") %>% 
  html_table(fill = T) %>% 
  bind_rows() %>% 
  set_names(c("Category", "Value", letters[1:3])) %>% 
  mutate(Category = ifelse(!is.na(a), a, Category),
         Value = ifelse(!is.na(b), b, Value)) %>% 
  filter(!is.na(Category)) %>% 
  select(Category, Value)





write_csv(tuition_df, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))




## North_Island
Institution <- "North_Island"

url <- "https://www.nic.bc.ca/programs-courses/search-programs/programs-a-z/"  

webPage <- read_html(url)  
urls <- webPage %>% 
  html_nodes(xpath = '//*[@id="ctl00_ctl00_content_presentation_content_browse_section"]') %>% 
  html_nodes("a") %>% 
  html_attr("href")

names <- webPage %>% 
  html_nodes(xpath = '//*[@id="ctl00_ctl00_content_presentation_content_browse_section"]') %>% 
  html_nodes("div") %>% 
  html_text()

urls <- urls[!is.na(urls)]  
names <- names[-1]
urls <- urls[!grepl("^#[A-Z]$", urls)]
names <- names[!grepl("^[A-Z]$", names)]
length(urls)
length(names)

d <- tibble(Program_url = paste0("https://www.nic.bc.ca/", urls),
            Program = names,
            Credential = NA,
            Campus = NA,
            Duration = NA,
            Description = NA,
            WIL = NA) %>% 
  filter(Program_url != "#top")

d_expand <- d %>% 
  filter(grepl("[[:digit:]] programs", Program, ignore.case = T))


d <- d %>% 
  filter(!grepl("[[:digit:]] programs", Program, ignore.case = T))



for(prog in 1:nrow(d_expand)){
  print(prog)
  url <- d_expand$Program_url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    Program_urls <- webPage %>% 
      html_node(xpath = '//*[@id="ctl00_ctl00_ctl00_content_presentation_content_buttonsection"]') %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    Program_names <- webPage %>% 
      html_node(xpath = '//*[@id="ctl00_ctl00_ctl00_content_presentation_content_buttonsection"]') %>% 
      html_nodes("a") %>% 
      html_text()
    
    if(length(Program_urls) >0){
      d <- d %>% 
        bind_rows(tibble(Program_url = paste0("https://www.nic.bc.ca/", Program_urls),
                         Program = Program_names,
                         Credential = NA,
                         Campus = NA,
                         Duration = NA,
                         Description = NA,
                         WIL = NA))
    }
  }
}

remove(Fees)

for(prog in 1:nrow(d)){
  print(prog)
  url <- d$Program_url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    tuitionTab <- webPage %>% 
      html_node(xpath = '//*[@id="tuition"]') %>% 
      html_nodes("table") %>% 
      html_table(fill = T)
    
    
    if(length(tuitionTab) == 2) {
      print("got fees")
      Canadian <- tuitionTab[1] %>% 
        bind_rows() %>% 
        gather(Category, Value, -1) %>% 
        set_names(c("Category","Column", "Value")) %>% 
        mutate(StudentType = "Canadian")
      
      
      International <- tuitionTab[2]  %>% 
        bind_rows() %>% 
        gather(Category, Value, -1) %>% 
        set_names(c("Category","Column", "Value")) %>% 
        mutate(StudentType = "International")
      
      tuition_df <- Canadian %>% 
        bind_rows(International) %>% 
        mutate(Program = d$Program[prog],
               url = url)
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }
      
    } else if(length(tuitionTab) == 1) {
      print("got fees")
      tuition_df <-  tuitionTab[1] %>% 
        bind_rows() %>%
        gather(Category, Value, -1) %>% 
        set_names(c("Category","Column", "Value")) %>% 
        mutate(StudentType = "Canadian") %>% 
        mutate(Program = d$Program[prog],
               url = url)
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }
    }
  }
}

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))



## Northern_Lights
remove(Fees)
Institution <- "Northern_Lights"

url <- "https://www.nlc.bc.ca/Programs/All-Programs-Alphabetical"
webPage <- read_html(url)
urls <- tibble(Name = webPage %>% 
                 html_nodes("table.telerik-reTable-4") %>% 
                 html_nodes("a") %>% 
                 html_text(),
               url = webPage %>% 
                 html_nodes("table.telerik-reTable-4") %>% 
                 html_nodes("a") %>% 
                 html_attr("href") %>% 
                 paste0("https://www.nlc.bc.ca",.) %>% 
                 gsub("https://www.nlc.bc.cahttps://", "https://", .) %>% 
                 gsub("\n", "", .) %>% 
                 gsub("AllPrograms–", "AllPrograms%E2%80%93", .))

url <- "https://www.nlc.bc.ca/tuition"

tuition_urls <- read_html(url) %>% 
  html_nodes("table.dnnFormItem") %>% 
  html_nodes("tr.dnnGridItem") %>% 
  html_nodes("a")

tuition_urls_df <- tibble(tuition_url = tuition_urls %>% html_attr("href"),
                          Name = tuition_urls %>% html_text()) %>% 
  mutate(remove = rep(1:2, (nrow(.)/2))) %>% 
  filter(remove == 2) %>% 
  mutate(tuition_url = paste0("https://www.nlc.bc.ca/", tuition_url)) %>% 
  select(-remove) %>% 
  left_join(urls, by = "Name")


for(prog in 1:nrow(tuition_urls_df)){
  print(prog)
  url <- tuition_urls_df$tuition_url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    tuition <- webPage %>% 
      html_nodes("table.Normal") %>% 
      html_table(fill = T)
    
    if(length(tuition) > 0){
      tuition_df <- tuition %>% 
        bind_rows() %>% 
        set_names(c("Category", "Value")) %>% 
        filter(Category != "") %>% 
        mutate(Program = tuition_urls_df$Name[prog],
               url = url)
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      } 
        
    }
  }
}
    


write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))



### Red_River
Institution = "Red_River"

urls <- c("https://catalogue.rrc.ca/Programs/WPG/FullTime", 
          "https://catalogue.rrc.ca/Programs/WPG/PartTime", 
          "https://catalogue.rrc.ca/Programs/PFR",
          "https://catalogue.rrc.ca/Programs/PC",
          "https://catalogue.rrc.ca/Programs/SC",
          "https://catalogue.rrc.ca/Programs/WC")

for(link in 1:length(urls)){
  webPage <- read_html(urls[link])
  
  Programs <- webPage %>% 
    html_nodes(xpath = '/html/body/div/div[2]') %>% 
    html_nodes("a")
  
  Programs <- tibble(Program = Programs %>% html_text(),
                     Program_url = Programs %>% html_attr("href"))
  
  if(!exists("Programs_df")){
    Programs_df <- Programs
  } else {
    Programs_df <- Programs_df %>% 
      bind_rows(Programs)
  }
}

Programs_df <- Programs_df %>% 
  distinct() %>% 
  mutate(Program_url = paste0("https://catalogue.rrc.ca/", Program_url)) %>% 
  filter(Program != "back to top")

d <- Programs_df %>% 
  mutate(#Program_url = gsub("	https://catalogue\\.rrc.ca/https://catalogue", "https://catalogue", Program_url),
    Credential = NA,
    Campus = NA,
    Duration = NA,
    Description = NA,
    WIL = NA)
remove(Fees)
for(prog in 1:nrow(d)){
  print(prog)
  
  
  feeUrl <- paste0(d$Program_url[prog], "/LocationsDatesandFees")
  
  
  feeError <- tryCatch(
    read_html(feeUrl),
    error = function(e) e
    
    
  )
  
  if(!inherits(feeError, "error")){
    webPage <- read_html(feeUrl)
    webFees <- webPage %>% 
      html_nodes('div.catalogue-program-table')
    
    if(length(webFees) > 0){
      print("found fees")

      time = webFees %>% 
        #html_nodes("div.justify-content-between") %>% 
        html_text() %>% 
        gsub("^\r\n[[:space:]]{1,}|\r\n[[:space:]]{1,}$", "", .) %>% 
        strsplit("\r\n[[:space:]]{1,}")  %>% 
        unlist()
      
      tuition_df <- tibble(Category = time) %>% 
        mutate(Index = ifelse(grepl("^year|^month|^week", Category, ignore.case = T), 1,
                              ifelse(grepl("^\\$", Category), 2, NA))) %>% 
        mutate(Index = ifelse(is.na(Index) & !is.na(lead(Index)), 3, Index)) %>% 
        filter(!is.na(Index)) %>% 
        mutate(Time = zoo::na.locf(ifelse(Index == 1, Category, NA), na.rm = F)) %>% 
        mutate(Type = zoo::na.locf(ifelse(Index == 3, Category, NA))) %>%
        filter(Category != Time) %>% 
        filter(Type != Category) %>% 
        select(-Index) %>% 
        mutate(Program = d$Program[prog],
               url = d$Program_url[prog])
      
      
      if(!exists("Fees")){
        Fees <- tuition_df
      } else {
        Fees <- Fees %>% 
          bind_rows(tuition_df)
      }

    }
    
    
  }
}

write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))












## SIIT
Institution <- "SIIT"
urls <- read_csv(paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/2019_Programs/temp/",urlFiles[grepl(Institution, urlFiles, ignore.case = T)])) %>% 
  select(url, Program)

remove(Fees)
for(prog in 1:nrow(urls)){
  print(prog)
  url <- urls$url[prog]
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    webPage <- read_html(url)
    
    tuition <- webPage %>% 
      #html_nodes(xpath = '//*[@id="et-boc"]/div/div[2]/div/div[1]/div[9]') %>% 
      html_nodes("div.et_pb_text_inner") %>% 
      html_nodes("table")
    if(length(tuition) > 0){
      tuition <- tuition %>% 
        html_table(fill = T)
      tuition <- tuition[grepl("tuition", tuition, ignore.case = T)]
      
      if(length(tuition) > 0){
        tuition_df <- tuition %>% 
          bind_rows() %>% 
          gather(Category, Value) %>% 
          filter(!grepl("Please note", Value, ignore.case = T)) %>% 
          mutate(Program = urls$Program[prog],
                 url = urls$url[prog],
                 Institution = Institution)
        
        if(!exists("Fees")){
          Fees <- tuition_df
        } else {
          Fees <- Fees %>% 
            bind_rows(tuition_df)
        }
        
      }
    }
    
    
  }
}
    
write_csv(Fees, paste0("C:/Users/psoeraas/Colleges and Institutes Canada/Planning and Statistics - Research/Sandbox/Data_Requests/Tuition_fees/temp/2019_", Institution, "_Fees.csv"))




####################
###Needs further work
####################

#Aurora

#Yukon





#pdf
#CNA
url <- "https://dls.cna.nl.ca/future/program_fees.shtml"

webPage <- read_html(url)
pdfUrls <- webPage %>% 
  html_nodes("ul") %>% 
  html_nodes("a") %>% 
  html_attr("href")

pdfUrls <- pdfUrls[grepl("pdf", pdfUrls)]

pdfUrls <- tibble(urls = paste0("https://dls.cna.nl.ca/", gsub("\\.\\./", "", pdfUrls)))

for(url in 1:length(pdfUrls$urls)){
  print(url)
  

  webPage <- pdftools::pdf_text(url)
  webPage <- pdftools::pdf_text("https://dls.cna.nl.ca/pdf/Art_Design_Essentials_Program_Fees_NL.pdf")

  infoFrame <- webPage %>% 
    strsplit("\r\n") %>% 
    unlist()
  
  programTitle <- gsub("[[:space:]]{2,}", "", infoFrame[3])
  specifics <- infoFrame[grepl(":", infoFrame)]
  specifics <- specifics[!grepl("Updated|note", specifics, ignore.case = T)] %>% 
    gsub("[[:space:]]{2,}", "", .) %>% 
    as_tibble() %>% 
    separate(value, c("Category", "feeValue"), sep = ":") %>% 
    separate(feeValue, c("feeValue", "description"), sep = " - ")
  
  index <- grep("FALL|WINTER", infoFrame)[1]
  dataFrame <- infoFrame[index:length(infoFrame)] %>% 
    gsub("[[:space:]]{2,}", "_", .) %>% 
    strsplit("_") %>% 
    unlist()
  index <- grep("NOTE:", dataFrame)
  if(length(index) > 0){
    dataFrame <- dataFrame[1:(index - 1)]
  }
  index <- max(grep("Textbook Cost", dataFrame, ignore.case = T))
  headers <- dataFrame[1:index]
  dataFrame <- dataFrame[(index + 1):length(dataFrame)]
  
  #Giving up here - do manual grab?
}


#pdfs







#sits behind a button

#https://thescreenshotguy.wordpress.com/2016/02/13/navigating-scraping-job-sites-rvest-rselenium/
#https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused

##Everybody seems to have issues with rsDriver and Docker is the recommended option so we'll go this route:
##
##install docker
##run it, restart computer as requested
##pull image by running in command line: docker pull selenium/standalone-firefox(or chrome instead of firefox) or in R shell('docker pull selenium/standalone-firefox')
##start server by running in command line: docker run -d -p 4445:4444 selenium/standalone-firefox or in R shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
##Then run remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox'") . The doc suggests something different with a virtual machine but i couldn't get it to work.
##With this I was set, here is my code:
##
##shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
##remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
##remDr$open()
##remDr$navigate("http://www.google.com/ncr")
##remDr$getTitle()
### [[1]]
### [1] "Google" 


##Topline tuiton:
websiteData <- website %>% 
  html_nodes("div.program-tuition.alt-row.row.no-gutters")%>% 
  html_text()



  
  



### behind button



#Centennial
"https://www.centennialcollege.ca/admissions/tuition-and-fees/tuition-information/"
"https://www.centennialcollege.ca/admissions/international-education/fees/"


#Durham
"https://durhamcollege.ca/financial-info/tuition-and-fees/other-fees-to-consider?utm_source=dcwebsite&utm_medium=megamenu&utm_campaign=dcmegamenu&utm_content=Other%20Fees%20to%20Consider"
#example:
  "https://durhamcollege.ca/programs/9-1-1-emergency-and-call-centre-communications#tabCosts"
  "https://durhamcollege.ca/programs/biomedical-engineering-technology#tabCosts"

#Also given as year one cost on program website
"https://durhamcollege.ca/programs/entrepreneurship-and-small-business-business-transfer-to-uoit-bachelor-of-commerce-hons"

#Durham transfer/pathways
"https://durhamcollege.ca/dc-to-uoit-diploma-to-degree-programs"
#coop
"https://durhamcollege.ca/programs-and-courses/co-operative-education-programs?utm_source=dcwebsite&utm_medium=megamenu&utm_campaign=dcmegamenu&utm_content=Co-operative%20Education%20programs"



#Manual grabs:
"https://fnti.net/college-programs.php"  #on each separate page
"https://www.georgiancollege.ca/finance-and-fees/tuition-and-ancillary-fees/"





  
  
  #https://stat4701.github.io/edav/2015/04/02/rvest_tutorial/
  # The rvest submit_form function is still under construction and does not work for web sites which build URLs (i.e. GET requests. It does seem to work for POST requests). 
  #url <- submit_form(session, indeed)
  
  # Version 1 of our submit_form function
  submit_form2 <- function(session, form){
    library(XML)
    url <- XML::getRelativeURL(form$url, session$url)
    url <- paste(url,'?',sep='')
    values <- as.vector(rvest:::submit_request(form)$values)
    att <- names(values)
    if (tail(att, n=1) == "NULL"){
      values <- values[1:length(values)-1]
      att <- att[1:length(att)-1]
    }
    q <- paste(att,values,sep='=')
    q <- paste(q, collapse = '&')
    q <- gsub(" ", "+", q)
    url <- paste(url, q, sep = '')
    html_session(url)
  }
  
  
  
  WIL:
    class: "proStat clinicalPlacement"
  href="#clinicalPlacement"
  
  #grab apprenticeships
  

  
  
  
  
  
  
  
  
### Trying to crack javascript block  
    
  library(rvest)
  library(V8)
  library(jsonlite)
  library(stringi)
url <- "https://www.algonquincollege.com/future-students/programs/areas-of-interest/"
#Read the html page content and extract all javascript codes that are inside a list
webpage <- read_html(url) %>% html_nodes('script') %>% html_text() %>% stri_split_lines() 

webpage[[4]]

idx <- which(stri_detect_fixed(webpage, '$(document).ready(function() {')) - 1

cprof <- getChromeProfile(
  "C:/Users/Dell/psoeraas/Local/Google/Chrome Beta/User Data",
  "Default"
)

appURL <- "https://www.algonquincollege.com/future-students/programs/areas-of-interest/"
remDr$open()
remDr$navigate(appURL)


webpage <- read_html(url)
read_html(url) %>% 
  html_nodes(xpath = "//div[@class='a']")

webpage %>% 
  html_nodes("*")  %>% 
  html_attr("class") %>% 
  unique()

program_url <- html_nodes(webpage, css = "td.sorting_2")
program_url_text <- html_text(program_url)



################# PDFs #################

##https://www.r-bloggers.com/how-to-extract-data-from-a-pdf-file-with-r/



GraPraTui %>% 
  gsub("([[:digit:]])([[:space:]])([[:digit:]])", "\\1_\\3", .)

# St Lawrence

"https://www.stlawrencecollege.ca/-/media/files/documents/pay-for-college/domestic-tuition-fees-all-programs.pdf?la=en"
"https://www.stlawrencecollege.ca/-/media/files/documents/pay-for-college/international-tuition-fees.pdf?la=en"
"https://www.stlawrencecollege.ca/-/media/files/documents/pay-for-college/201920-administrative-fees.pdf?la=en"

#Boreal

"http://www.collegeboreal.ca/wp-content/uploads/2019/06/Frais-de-scolarit%C3%A9-%C3%89tudiants-domestiques-2019-2020-2019-06-19.pdf"
"http://www.collegeboreal.ca/wp-content/uploads/2019/05/Frais-de-scolarit%C3%A9-temps-partiel-%C3%89tudiants-domestiques-2019-2020-2019-05-03.pdf"
"http://www.collegeboreal.ca/wp-content/uploads/2019/06/Frais-de-scolarit%C3%A9-%C3%89tudiants-internationaux-2019-2020-2019-06-19.pdf"
"http://www.collegeboreal.ca/wp-content/uploads/2019/05/Frais-de-scolarit%C3%A9-temps-partiel-%C3%89tudiants-internationaux-2019-2020-2019-05-03.pdf"

"http://www.collegeboreal.ca/wp-content/uploads/2019/08/Frais-accessoires-et-autres-frais-%C3%89tudiants-domestiques-2019-2020.pdf"
"http://www.collegeboreal.ca/wp-content/uploads/2019/08/Frais-accessoires-et-autres-frais-%C3%89tudiants-internationaux-2019-2020.pdf"




#Confederation
"https://www.confederationcollege.ca/sites/default/files/uploads/department/fall_start_-_2019-2020_-_tuition_chart_1.pdf"
"https://www.confederationcollege.ca/registration-services/tuition-fees/ancillary-fees"
"https://www.confederationcollege.ca/sites/default/files/uploads/basic-page/confederation-college-international_program_chart_2019-2020_1.pdf"
"https://www.confederationcollege.ca/international-students/ancillary-fees"



#Fanshawe
"https://www.fanshawec.ca/sites/default/files/uploads/fees/canadian_fees_2019.pdf"
"https://www.fanshawec.ca/sites/default/files/uploads/fees/international_fees_2019.pdf"
"https://www.fanshawec.ca/sites/default/files/uploads/fees/admin_charges.pdf"


## Georgian
"https://www.georgiancollege.ca/wp-content/uploads/2019-20-Lab-Fees.pdf"










####
#field of study
"https://catalog.cambriancollege.ca/"
"https://www.algonquincollege.com/future-students/programs/areas-of-interest/"
  #class: "min-tablet sorting_1

#Online indicator
"https://saskpolytech.ca/programs-and-courses/browse-programs/a-z-listing.aspx"
