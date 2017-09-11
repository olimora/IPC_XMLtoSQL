###############################################################
#
# functions used while managing branching
# params: 
#   xml_input - original XML doc with mapping.
#   connectors - list with data about connectors, $uniqie, $all, $objects
#
##################

## returns dataframes with connectors between objects ########
get_connectors_dfs <- function(xml_input) {
  connectors_all <- xml_find_all(xml_input, ".//CONNECTOR")
  all_objects <- c()
  from_instance <- c()
  to_instance <- c()
  from_field <- c()
  to_field <- c()
  for (con_i in 1:length(connectors_all)) {
    from_instance <- append(from_instance, xml_attr(connectors_all[con_i], "FROMINSTANCE"))
    to_instance <- append(to_instance, xml_attr(connectors_all[con_i], "TOINSTANCE"))
    from_field <- append(from_field, xml_attr(connectors_all[con_i], "FROMFIELD"))
    to_field <- append(to_field, xml_attr(connectors_all[con_i], "TOFIELD"))
  } 
  connectors <- list()
  connectors$all <- data.frame(from_instance, to_instance, from_field, to_field, stringsAsFactors = F)
  connectors$unique <- unique(data.frame(from_instance, to_instance, stringsAsFactors = F))
  connectors$objects <- unique(append(from_instance,to_instance))
  #rm("from_instance", "to_instance", "from_field", "to_field", "con_i")
  return(connectors)
}

## returns begining of branches
get_branches_beginnings <- function(connectors) {
  #to which no connectors go
  beg1 <- subset(connectors$unique, !(from_instance %in% connectors$unique$to_instance))$from_instance 
  #to which more than one connectors go
  beg2 <- subset(as.data.frame(table(connectors$unique$to_instance), stringsAsFactors = F), Freq > 1)$Var1
  #obj do ktoreho idu konektory z objektu, ktory ma konektory do viacerych
  #to which connectors go from object with connectors to more than one object / targets of those connectors
  beg3 <- subset(connectors$unique, from_instance %in% 
                   subset(as.data.frame(table(connectors$unique$from_instance), stringsAsFactors = F), Freq > 1)$Var1)$to_instance
  beginnings <- unique(c(beg1, beg2, beg3))
  return(beginnings)
}

## returns ends of branches
get_branches_endings <- function(connectors) {
  #from which no connectors go
  end1 <- subset(connectors$unique, !(to_instance %in% connectors$unique$from_instance))$to_instance 
  #from which more than one connectors go
  end2 <- subset(as.data.frame(table(connectors$unique$from_instance), stringsAsFactors = F), Freq > 1)$Var1
  #from which connectors go to object with connectors from more than one object / sources of those connectors
  end3 <- subset(connectors$unique, to_instance %in% 
                   subset(as.data.frame(table(connectors$unique$to_instance), stringsAsFactors = F), Freq > 1)$Var1)$from_instance
  endings <- unique(c(end1, end2, end3))
  return(endings)
}

## returns end of branch
get_end_of_branch <- function(connectors, beg, endings) {
  if (beg %in% endings) {
    return(beg)
  } else {
    next_obj <- subset(connectors$unique, from_instance == beg)$to_instance
    get_end_of_branch(connectors, next_obj, endings)
  }
}
