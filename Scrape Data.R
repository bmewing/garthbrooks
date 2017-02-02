library(RSelenium)
library(rvest)
library(magrittr)
library(dplyr)

#helper functions for zip codes
source('Zip Codes/get all zip codes needed.R')

#It seems that brickseek uses a radius of 100 miles based on observation
#This function will generate a (conservative) grid of zip codes that provide complete coverage of the US for a given radius
ziplist = getZipList(100) 

#This function takes a row of the table returned by brickseek and extracts the Store and Saleable quantity information
getDetails = function(row){
  res = row %>% 
    html_nodes(css = "td") %>% 
    html_text() %>% 
    gsub("\\(.*?\\)","",.)
  return(dplyr::data_frame(Store = res[2],Qty = res[4]))
}

#This function manages the Selenium browser to reload the Garth Brooks page, insert the zip code, process results and return them in a data frame
getCounts = function(zip,remDr){
  remDr$navigate("http://brickseek.com/target-inventory-checker/?sku=012-05-0331")
  elem = remDr$findElement(using = "css",value = "input[name='zip']")
  elem$sendKeysToElement(list(zip))
  elem = remDr$findElement(using = "xpath",value = "(//input[@type='submit'])[2]")
  elem$submitElement()
  Sys.sleep(10)
  source = remDr$getPageSource()
  page = read_html(source[[1]])
  results = page %>% 
    html_nodes(css = "tr[class='store_row']")
  return(lapply(results,getDetails) %>% 
    do.call(dplyr::bind_rows,.))
}

remDr = remoteDriver()
remDr$open()
out = list()

#For loops are bad
#However, at some point brickseek will check if you're a robot and it will break the results
#This is setup to allow for easy recovery from a break by updating what zip codes are in the loop
#There may be sexier ways of doing this which I'd love to see
ziplist2 = ziplist[!(ziplist %in% names(out))]
for(i in ziplist3){
  out[[i]] = getCounts(i,remDr)
}
remDr$close()

#Clean up the results, make it into one big data frame with no duplicates
finalOut = out %>% 
  do.call(bind_rows,.) %>%
  distinct() %>% 
  mutate(State = gsub(".*?, ([A-Z]+) [0-9\\-]+","\\1",Store))

#Summarize the output
summarisedCounts = finalOut %>% 
  group_by(State) %>% 
  dplyr::summarise(nTargets = n(), Avg = mean(as.numeric(Qty)),Total = sum(as.numeric(Qty)))

#This is the worst possible way to fix this but it's fast enough
for(i in 1:nrow(summarisedCounts)){
  summarisedCounts$FullState[i] = tolower(state.name[grep(summarisedCounts$State[i],state.abb)])
}

#Make the choropleth map
library(choroplethr)
summarisedCounts %>% 
  select(region = FullState, value = Total) %>% 
  state_choropleth(title = 'Average Limited Edition Garth Brooks CD Sets in a Target by State')
