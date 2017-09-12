library("xml2")
library("XML")

source("./func__branching_utils.R")

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
  branches[br$beg,]$done = T
  print(paste("branche processed", br$beg, br$end))
}
