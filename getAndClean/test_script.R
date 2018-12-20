base_lx <- "http://www.dgsi.pt/jtrl.nsf?OpenDatabase&Start=1"

remDr$open()

remDr$navigate(base_lx)

remDr$screenshot(file = "interm_data/test.PNG")

## once there, extract the source page
source_page <- remDr$getPageSource()[[1]]

## move to the next page
next_page <- remDr$findElement("css selector",
                               "body > table > tbody > tr:nth-child(7) > td > div > a:nth-child(3)")

Sys.sleep(3)

next_page$clickElement()

Sys.sleep(3)

remDr$screenshot(file = "interm_data/test.PNG")


remDr$open()

remDr$navigate(base_lx)

remDr$screenshot(file = "interm_data/test.PNG")

### extract all the URLs of the "next page"
current_page <- base_lx
sub_pages <- list()

## add the first page
sub_pages[[1]] <- current_page

### While-Loop. It clicks on the "next button" as long as it exists to find all existing sub-pages. ## buggy, last page http://www.dgsi.pt/jtrl.nsf?OpenDatabase&Start=46963 still has a "next button"...

while(class(try(remDr$navigate(current_page), silent = TRUE)) != "try-error"){
  
  ### get the page source
  Sys.sleep(runif(2,1,3))
  
  remDr$screenshot(file = "interm_data/test2.PNG")
  
  parsed_page <- remDr$getPageSource()[[1]]
  
  ## extract the url of the next page
  next_page <- parsed_page %>%
    read_html() %>%
    html_node("body > table > tbody > tr:nth-child(7) > td > div > a:nth-child(3)") %>%
    html_attr("href") %>%
    str_trim() %>%
    paste0("http://www.dgsi.pt", .)
  
  ## take a pic!
  remDr$screenshot(file = "interm_data/test2.PNG")
  
  sub_pages[[i]] <- next_page
  
  ## next page becomes the current page for the next iteration
  current_page <- next_page
  
  print(current_page)
}

### export
subPages_relLx <- sub_pages

save(subPages_relLx,
     file = "interm_data/subPages_relLx.Rdata")