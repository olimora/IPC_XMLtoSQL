library("xml2")
library("XML")

source("./func__branching_utils.R")
source("./func__process_objects.R")
source("./func__write_sql.R")

## global variables ##########
counters_env <- new.env()
counters_env$select_no <- 0
counters_env$source_no <- 0
xml_query <- newXMLDoc()

## read xml
xml_input <- read_xml("example.xml")

## get connectors
connectors <- get_connectors_dfs(xml_input)

## create branches
beginnings <- get_branches_beginnings(connectors)
endings <- get_branches_endings(connectors)
matching_ends <- sapply(beginnings, function(x) get_end_of_branch(connectors, x, endings))
branches <- data.frame(beg = beginnings, end = matching_ends, done = rep(F, length(beginnings)),
                       stringsAsFactors = F)

get_queue(branches)
get_queue_size(branches)

## follow branches
# loop while queue is not empty
while (get_queue_size(branches) > 0) {
  # get any branch with done preddispositions
  br <- get_branch_to_follow(branches, connectors)
  print(br)
  
  ## follow branch
  xml_query <- follow_branch(br, xml_query, xml_input, connectors)
  branches[br$beg,]$done = T
  message(paste("branche processed", br$beg, br$end))
}

## SQL query
sql_query <- xml_to_sql(xml_query)

## output at end
writeLines("")
writeLines("output:")
print(xml_query)
writeLines(sql_query)
