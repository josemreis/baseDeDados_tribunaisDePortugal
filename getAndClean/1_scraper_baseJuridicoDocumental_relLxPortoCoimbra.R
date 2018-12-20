############################################################################

## file: 1_scraper_baseJuridicoDocumental_relLxPortoCoimbra.R

### Author: J. M. Reis

### Date: 29/11/2018

### Purpose: criar base the dados com todos os casos das relaçoes de lisboa (http://www.dgsi.pt/jtrl.nsf?OpenDatabase), porto (http://www.dgsi.pt/jtrp.nsf?OpenDatabase), e coimbra (http://www.dgsi.pt/jtrc.nsf?OpenDatabase)

############################################################################

#### Setting things up-------------------------------------------------------------------------------

### ICJ sub-directory as the main current directory
main_dir <- getwd()
setwd(paste0(main_dir, "/getAndClean"))

### loading the relevant packages
library(tidyverse)
library(rvest)
library(openxlsx)
library(lubridate)
require(RSelenium)

### Create a data directory
if(!dir.exists("data")){
  
  dir.create("data")
  
}

## intermediary
if(!dir.exists("interm_data")){
  
  dir.create("interm_data")
  
}

### Generate a small table with the id's for the courts, url for their database, as well as the number of cases in it at the moment of the scraping. It will be useful later.
index_table <- "http://www.dgsi.pt/" %>%
  read_html() %>%
  html_node(xpath = "//table[2]") %>%
  html_table(fill = TRUE) %>%
  as_tibble() %>%
  select(2)  %>%
  filter(nchar(X2) > 5, str_detect(X2, "[a-z]")) %>%
  mutate(repo_url =  "http://www.dgsi.pt/" %>%
           read_html() %>%
           html_nodes(xpath = "//table[2]//a") %>%
           html_attr("href") %>%
           paste0("http://www.dgsi.pt", .)) %>%
  filter(str_detect(X2, "\\s+doc\\.")) %>%
  separate(., X2, c("docs", "n"),"\\s+\\((?=[0-9])") %>%
  mutate(n = str_extract(n, "[0-9]+") %>%
           as.numeric())
  

#### Scraping the case pages------------------------------------------------------------------

### The helper function below will scrape the case pages for the relevant institutions. It takes as arguments (i) the index table above, (ii) a regex for describing the institution we are interested in scraping

scraper_casePages_bjd <- function(index_data = index_table,
                                  inst_regex = "Lisboa"){
  
  
  ### define the relevant base url depending on the selected institution regex
  base_url <- paste0(index_data$repo_url[str_detect(index_data$docs, regex(inst_regex, ignore_case = TRUE))], "&Start=")
  
  ### each sub-page of the repo, by default, contains 29 cases as well as the remaining. Generate a sequence from 1 to the last case in groups of 29, and past it with the base url
  sub_pages <- map2_chr(base_url, seq(1, index_data$n[str_detect(index_data$docs,regex(inst_regex, ignore_case = TRUE))], 29), paste0)
  
  ### Now we loop across each sub-page and extract tables with the case page as well as metadata.
  output <- map2_df(sub_pages, 1:length(sub_pages), function(sub_page, n){
    
    cat(paste0("scraping page ", n, "\n\n"))
    
    ## check the internet conection, and wait if it is weak. If not, just parse the HTML page
    con_test <- try(parsed_sub_page <- sub_page %>%
                      read_html(), silent = TRUE)
    
    # if true, weak connection...wait 1 minute, and then reconnect and parse the HTML page
    if(class(con_test) == "try-error") {
      
      print("reconecting in 1 minute!")
      Sys.sleep(60)
      parsed_sub_page <- sub_page %>%
        read_html()
    
    }
    
    ## scrape the metadata of the cases in the sub-page
    metadata <- parsed_sub_page %>%
      html_node(xpath = "//td//table") %>%
      html_table() %>%
      as_tibble() %>%
      mutate(case_page = parsed_sub_page %>%
               html_nodes("td td a") %>%
               html_attr("href") %>%
               paste0("http://www.dgsi.pt", .)) %>%
      set_names(str_to_lower(names(.))) %>%
      mutate(dateOfAccess = Sys.Date(),
             institution = index_data$docs[str_detect(index_data$docs, regex(inst_regex, ignore_case = TRUE))],
             `sessão` = mdy(`sessão`),
             processo = as.character(processo),
             descritor = str_replace_all(descritor, "[[:cntrl:]]", "; ")) %>%
      select(inst = institution, data = "sessão",  proc = processo, relator, palavras_chave = descritor, everything())
    
    print(sample_n(metadata, 3))
    
    ## rest time for the server
    Sys.sleep(runif(5,3,7))
    
    return(metadata)
    
  })
  
}


### scrape the metadata of the cases ---> Relacao de Lx
metadata_relLx <- scraper_casePages_bjd(index_data = index_table,
                                        inst_regex = "Rela.*Lisboa")

beepr::beep(8)

## export it
save(metadata_relLx,
     file = "interm_data/metadata_casos_relLx.Rdata")
write.csv(metadata_relLx,
     file = "interm_data/metadata_casos_relLx.csv")


#### Scraping the case data------------------------------------------------------------------

relLx_data_raw <- map2(metadata_relLx$case_page, 1:length(metadata_relLx$case_page), function(case_page, n){
  
  cat(paste0("\n\n", "scraping case ", n, "\n\n"))
  
  ## check the internet conection, and wait if it is weak. If not, just parse the HTML page
  con_test <- try(parsed_case_page <- case_page %>%
                    read_html(), silent = TRUE)
  
  # if true, weak connection...wait 1 minute, and then reconnect and parse the HTML page
  if(class(con_test) == "try-error") {
    
    print("reconecting in 1 minute!")
    Sys.sleep(60)
    parsed_case_page <- case_page %>%
      read_html()
    
  }
  
  ### scrape the case details table
  md_table <- parsed_case_page %>%
    html_node(xpath = "//table") %>%
    html_table(fill = TRUE) %>%
    as_tibble() %>%
    select(1:2) %>%
    filter(str_detect(X1, "\\:$")) %>%
    t() %>%
    as_tibble() %>%
    set_names(.,
              nm = .[1,] %>%
                str_replace_all(., "[[:punct:]]|º", "") %>%
                str_to_lower() %>%
                str_replace_all(., "\\s+", "_")) %>%
    slice(2) %>%
    mutate_all(funs(str_replace_all(., "[[:cntrl:]]", "; ")))
  
  ### Next, we check if the "integral text" row exists. If yes, we scrape it and add it to the table above.
  dec_table <- parsed_case_page %>%
      html_node(xpath = "//table[2]") %>%
      html_table(fill = TRUE) %>%
      as_tibble() %>%
      select(1:2) %>%
      filter(str_detect(X1, "\\:")) %>%
      t() %>%
      as_tibble() %>%
      set_names(.,
                nm = .[1,] %>%
                  str_replace_all(., "[[:punct:]]|º", "") %>%
                  str_to_lower() %>%
                  str_replace_all(., "\\s+", "_")) %>%
      slice(2)
  
  if(ncol(dec_table) == 1){
    
    
    md_table$decisao_texto <- pull(dec_table[1])
    
  } else {
    
    md_table$decisao_texto <- NA_character_
    
    
  }
  
  case_table <- md_table %>%
    mutate(case_page = case_page,
           dateOfAccess = Sys.Date()) %>%
    select(data_do_acordao = "data_do_acordão", relator, decisao = "decisão", votacao = "votação", palavras_chave = descritores, meio_processual, sumario = "sumário", decisao_texto, one_of("decisão_texto_parcial", "decisão_texto_integral"), dateOfAccess, case_page)
  
  print(case_table[,sample(1:ncol(case_table), 4)])
  
  return(case_table)
  
  Sys.sleep(runif(2,1,3))
  
})
  
beepr::beep(8)

## export it
save(relLx_data_raw,
     file = "interm_data/raw_relLx_data.Rdata")
write.csv(relLx_data_raw,
          file = "interm_data/raw_relLx_data.csv")

  
  
  
  
  
  
  