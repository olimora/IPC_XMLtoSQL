###############################################################
#
# functions used while managing branching
# params: 
#   xml_input - original XML doc with mapping.
#   connectors - list with data about connectors, $uniqie, $all, $objects
#   branches - df with all branches, $beg, $end, $done
#   xml_query - XML doc that we are building
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

## returns queue = branches not done yet
get_queue <- function(branches) {
  return(subset(branches, done == F))
}

## returns size of queue
get_queue_size <- function(branches) {
  return(nrow(get_queue(branches)))
}

## get any branch with done preddispositions
get_branch_to_follow <- function(branches, connectors) {
  not_done <- get_queue(branches) # get all branches not done yet == queue
  if (nrow(not_done) == 0) { # if there is no branch not done yet = all branches are done. good
    message("Following branches finished")
    return(-1) 
  }
  for (i in 1:nrow(not_done)) { # for branches not done yet
    # get all preddispositions = sources of connectors to beginning of the branch
    preddisp <- subset(connectors$unique, to_instance == not_done$beg[i])$from_instance 
    value <- T # changes to FALSE if some preddisposition is not done
    if (length(preddisp) == 0) { # has no preddispositions, can go
      return(not_done[i,])
    } else { # for every preddispositon. Use AND on value and status of branch that ands with the preddisposition
      for (j in 1:length(preddisp)) {
        value <- value & subset(branches, end == preddisp[j])$done
      }
    }
    if (value == T) { #has all preddispositions met, else it continues to next iteration for i loop
      return(not_done[i,])
    } 
  }
  message("Branch to follow not availible.")
  return(-2)
}

follow_branch <- function(branch, xml_query, xml_input, connectors) {
  # get original object node
  xpath <- paste0("//*[(name()='SOURCE' or name()='TRANSFORMATION') and @NAME='", branch$beg[1], "']")
  beg_obj <- xml_find_all(xml_input, xpath)
  xml_query <- process_general_object(xml_query, beg_obj, xml_input, NULL)
  
  return(xml_query)
}
