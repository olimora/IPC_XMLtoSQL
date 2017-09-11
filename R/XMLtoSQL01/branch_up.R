library("xml2")
library("XML")

source("./func__branching_utils.R")

## read xml
xml_input <- read_xml("example.xml")

## get connectors
connectors <- get_connectors_dfs(xml_input)
## beginnings, endings
beginnings <- get_branches_beginnings(connectors)
endings <- get_branches_endings(connectors)

match_ends <- vector(mode = "character", length = length(beginnings))
## create branches
for (i in 1:length(beginnings)) {
  end <- get_end_of_branch(connectors, beginnings[i], endings)
  match_ends[i] <- end
}

branches <- data.frame(beg = beginnings, end = match_ends, preddisp = rep("", length(beginnings)),
                       stringsAsFactors = F) #predispositions - tam bude proste string s nazvami objektov oddelenymi bodkociarkou



## find predispositions for branches

