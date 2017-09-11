library("xml2")
library("XML")

source("./func__branching_utils.R")

## read xml
xml_input <- read_xml("example.xml")

## get connectors
connectors <- get_connectors_dfs(xml_input)

## find begining of branches
{
  #to which no connectors go
  beg1 <- subset(connectors$unique, !(from_instance %in% connectors$unique$to_instance))$from_instance 
  #to which more than one connectors go
  beg2 <- subset(as.data.frame(table(connectors$unique$to_instance), stringsAsFactors = F), Freq > 1)$Var1
  #obj do ktoreho idu konektory z objektu, ktory ma konektory do viacerych
  #to which connectors go from object with connectors to more than one object / targets of those connectors
  beg3 <- subset(connectors$unique, from_instance %in% 
                   subset(as.data.frame(table(connectors$unique$from_instance), stringsAsFactors = F), Freq > 1)$Var1)$to_instance
  beginings <- unique(c(beg1, beg2, beg3))
}


## find ends of branches
{
  #from which no connectors go
  end1 <- subset(connectors$unique, !(to_instance %in% connectors$unique$from_instance))$to_instance 
  #from which more than one connectors go
  end2 <- subset(as.data.frame(table(connectors$unique$from_instance), stringsAsFactors = F), Freq > 1)$Var1
  #from which connectors go to object with connectors from more than one object / sources of those connectors
  end3 <- subset(connectors$unique, to_instance %in% 
                   subset(as.data.frame(table(connectors$unique$to_instance), stringsAsFactors = F), Freq > 1)$Var1)$from_instance
  endings <- unique(c(end1, end2, end3))
}

## create branches

## find predispositions for branches

