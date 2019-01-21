############################################################################

## file: 1_scraper_baseJuridicoDocumental_2ainstancia.R

### Author: J. M. Reis

### Date: 29/11/2018

### Purpose: Criar base the dados com todos os casos das relaçoes de lisboa (http://www.dgsi.pt/jtrl.nsf?OpenDatabase), Porto (http://www.dgsi.pt/jtrp.nsf?OpenDatabase), e Coimbra (http://www.dgsi.pt/jtrc.nsf?OpenDatabase)

############################################################################

#### Setting things up-------------------------------------------------------------------------------

### sub-directory as the main current directory
main_dir <- getwd()
setwd(paste0(main_dir, "/getAndClean"))

### loading the relevant packages
library(tidyverse)
library(rvest)
library(openxlsx)
library(lubridate)
library(RSelenium)
library(data.table)

### Create a data directory
if(!dir.exists("data")){
  
  dir.create("data")
  
}

## intermediary
if(!dir.exists("interm_data")){
  
  dir.create("interm_data")
  
}

## decision_corpus
if(!dir.exists("dec_corpus")){
  
  dir.create("dec_corpus")
  
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

scraper_casePages_bjd <- function(index_table = index_table,
                                  inst_regex = "Lisboa"){
  
  
  ### define the relevant base url depending on the selected institution regex
  base_url <- paste0(index_table$repo_url[str_detect(index_table$docs, regex(inst_regex, ignore_case = TRUE))], "&Start=")
  
  ### each sub-page of the repo, by default, contains 29 cases as well as the remaining. Generate a sequence from 1 to the last case in groups of 29, and past it with the base url
  sub_pages <- map2_chr(base_url, seq(1, index_table$n[str_detect(index_table$docs,regex(inst_regex, ignore_case = TRUE))], 29), paste0)
  
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
      subset(., nchar(PROCESSO > 2) & RELATOR != "N") %>%
      mutate(case_page = parsed_sub_page %>%
               html_nodes("td td font a") %>%
               html_attr("href") %>%
               paste0("http://www.dgsi.pt", .)) %>%
      set_names(str_to_lower(names(.))) %>%
      mutate(dateOfAccess = Sys.Date(),
             institution = index_table$docs[str_detect(index_table$docs, regex(inst_regex, ignore_case = TRUE))],
             `sessão` = mdy(`sessão`),
             processo = as.character(processo),
             descritor = str_replace_all(descritor, "[[:cntrl:]]", "; ")) %>%
      select(inst = institution, data = "sessão",  proc = processo, relator, palavras_chave = descritor, everything())
    
    print(sample_n(metadata, nrow(metadata)/2))
    
    ## rest time for the server
    Sys.sleep(runif(5,3,7))
    
    return(metadata)
    
  })
  
}


### scrape the metadata of the cases ---> Relacao de Lx
metadata_relLx <- scraper_casePages_bjd(index_table = index_table,
                                        inst_regex = "Rela.*Lisboa")

beepr::beep(8)

## export it
save(metadata_relLx,
     file = "interm_data/metadata_casos_relLx.Rdata")
write.csv(metadata_relLx,
     file = "interm_data/metadata_casos_relLx.csv")


### scrape the metadata of the cases ---> Relacao do Porto
metadata_relPorto <- scraper_casePages_bjd(index_table = index_table,
                                          inst_regex = "Rela.*Porto")

beepr::beep(8)

## export it
save(metadata_relPorto,
     file = "interm_data/metadata_casos_relPorto.Rdata")
write.csv(metadata_relPorto,
          file = "interm_data/metadata_casos_relPorto.csv")

### scrape the metadata of the cases ---> Relacao doe Coimbra
metadata_relCoimbra <- scraper_casePages_bjd(index_table = index_table,
                                        inst_regex = "Rela.*Coimbra")

beepr::beep(8)

## export it
save(metadata_relCoimbra,
     file = "interm_data/metadata_casos_relCoimbra.Rdata")
write.csv(metadata_relCoimbra,
          file = "interm_data/metadata_casos_relCoimbra.csv")


### scrape the metadata of the cases ---> Relacao de Evora
metadata_relEvora <- scraper_casePages_bjd(index_table = index_table,
                                             inst_regex = "Rela.*vora")

beepr::beep(8)

## export it
save(metadata_relEvora,
     file = "interm_data/metadata_casos_relEvora.Rdata")
write.csv(metadata_relEvora,
          file = "interm_data/metadata_casos_relEvora.csv")

### scrape the metadata of the cases ---> Relacao de Evora
metadata_relGuimar <- scraper_casePages_bjd(index_table = index_table,
                                           inst_regex = "Rela.*guimar")

beepr::beep(8)

## export it
save(metadata_relGuimar,
     file = "interm_data/metadata_casos_relGuimar.Rdata")
write.csv(metadata_relGuimar,
          file = "interm_data/metadata_casos_relGuimar.csv")


#### Join them and export

## rbinding all the dfs. Keep only the unique cases
metadata_rel <- do.call("rbind", list(metadata_relLx, metadata_relPorto, metadata_relCoimbra, metadata_relEvora, metadata_relGuimar)) %>%
  distinct(proc, .keep_all = TRUE) %>%
  arrange(desc(data)) %>%
  rename(relacao = inst) 


## clean up the relacao variable, keep only city name. Using data.table for speedness sake.
##  turn to dt
metadata_rel <- as.data.table(metadata_rel)

## clean the variable up
metadata_rel[, relacao := str_trim(str_extract(relacao, regex("(?<=ção\\s{1,6}de\\s{1,6}|ção\\s{1,6}do\\s{1,6}).*", ignore_case = TRUE)))]

## coerce it back into tibble 
metadata_rel <- as_tibble(metadata_rel)

## export it
save(metadata_rel,
     file = "data/1_metadata_casos_relTodas.Rdata")
write.csv(metadata_rel,
          file = "data/1_metadata_casos_relTodas.csv")

#### Scraping the case data and decisions------------------------------------------------------------------

decision_data_raw <- map2(metadata_rel$case_page, metadata_rel$proc, function(case_page, proc){
  
  cat(paste0("\n\n\n", "scraping case ", proc, "\n\n\n"))
  
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
  md_table <- try(parsed_case_page %>%
                    html_nodes(xpath = "//table") %>%
                    html_table(fill = TRUE) %>%
                    `[[`(1) %>%
                    as_tibble() %>%
                    filter(str_detect(X1, "\\:$")) %>%
                    t() %>%
                    as_tibble() %>%
                    set_names(.,
                              nm = .[1,] %>%
                                str_replace_all(., "[[:punct:]]|º", "") %>%
                                str_to_lower() %>%
                                str_replace_all(., "\\s+", "_")) %>%
                    slice(2) %>%
                    mutate_all(funs(str_replace_all(., "[[:cntrl:]]", "; "))) %>%
                    mutate(proc = proc) %>%
                    select(., one_of(c("processo", 
                                       "relator",
                                       "data_do_acordão",
                                       "data_do_acordao",
                                       "tribunal_recorrido",
                                       "tribunal_recurso",
                                       "data_dec_recorrida",
                                       "área_tematica",
                                       "área_temática",
                                       "area_tematica",
                                       "descritores",
                                       "meio_processual",
                                       "legislação_nacional",
                                       "legislaçao_nacional",
                                       "jurisprudência_nacional",
                                       "jurisprudencia_nacional",
                                       "decisão",
                                       "decisao",
                                       "votação",
                                       "votaçao",
                                       "votacão",
                                       "votacao",
                                       "n_convencional",
                                       "sumário",
                                       "sumario",
                                       "privacidade",
                                       "n_do_documento",
                                       "reclamações",
                                       "reclamacões",
                                       "reclamaçoes",
                                       "reclamacoes",
                                       "indicações_eventuais",
                                       "indicaçoes_eventuais",
                                       "indicacoes_eventuais",
                                       "indicacões_eventuais",
                                       "processo_no_tribunal_recorrido"))),
                  silent = TRUE)
  
  ## if md_table is empty, assign NA
  if(class(md_table) == "try-error" ||is_empty(md_table) || ncol(md_table) < 2){
    
    md_table <- NA_character_
    
  }
  
  ### Next, we get the integral or partial decisions, when existing. If not, assign NA
  ### partial
  decisao_texto_parcial <- try(parsed_case_page %>%
                                 html_nodes(xpath = "//td[preceding-sibling::td/b/font[contains(text(), 'o Texto Parcial')]]") %>%
                                 html_text(),
                               silent = TRUE) 
  
  decisao_texto_parcial <- ifelse(is_empty(decisao_texto_parcial) || nchar(decisao_texto_parcial) < 5,
                                   NA_character_,
                                   decisao_texto_parcial)
  
  ### integral
  decisao_texto_integral <- try(parsed_case_page %>%
                                  html_nodes(xpath = "//td[preceding-sibling::td/b/font[contains(text(), 'o Texto Integral')]]") %>%
                                  html_text(),
                                silent = TRUE) 
  
  decisao_texto_integral <- ifelse(is_empty(decisao_texto_integral) || nchar(decisao_texto_integral) < 5,
                                    NA_character_,
                                    decisao_texto_integral)
  
  #### categorical variable for the decision
  md_table$decisao_disponivel <- if_else(!is.na(decisao_texto_integral),
                                         "texto integral",
                                         if_else(!is.na(decisao_texto_parcial),
                                                 "texto parcial",
                                                 "texto nao disponivel"))
  
  #### Generate a corpus id
  corpus_id <- paste0("dec_corpus/",
    str_extract(case_page, "(?<=pt\\/).*?(?=\\.nsf)"),
    "_",
    str_replace_all(proc, "[[:punct:]]", "_"),
    ".txt")
  
  ### assign corpus id to the dataset
  md_table$corpus_id <- corpus_id
  
  ### store the decision text in the corpus
  if(md_table$decisao_disponivel == "texto integral"){
    
    cat(decisao_texto_integral,
        file = corpus_id)
    
  } else if(md_table$decisao_disponivel == "texto parcial"){
    
    cat(decisao_texto_integral,
        file = corpus_id)
    
  } else {
    
    cat("Decisao nao publicada",
        file = corpus_id)
    
  }
  
  
  
  print(md_table[,sample(1:ncol(md_table), 4)])

  # rest time for the server
  Sys.sleep(sample(1:12, 1))

  return(md_table)
  
})
  

  
  
  
  
  
  
  