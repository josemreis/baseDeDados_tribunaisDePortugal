#############################################################################

# file: helperFunction_randomUserAgent.R

# date: 28/01/2019

# author: J. M. Reis

# Purpose: Helper function which randomizes the user agent given a dataset of frequently used user agents. From https://github.com/yusuzech/r-web-scraping-template/blob/master/README.md 

#############################################################################

# get the dataset
agent_table <- read.csv("https://raw.githubusercontent.com/yusuzech/top-50-user-agents/master/user_agent.csv",stringsAsFactors = F)
agent_list <- agent_table[["User.agent"]]

# sample a user agent
random_agent <- function(){
  
  httr::user_agent(sample(agent_list,1))
  
}
