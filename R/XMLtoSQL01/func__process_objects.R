#################################################################################
#
# functions for processing objects from XML mapping
# they produce new XML representation of SQL query
# each adds COLUMNS, CONDITIONS ... to existing XML doc - xml_query
# params: 
#   xml_query - XML doc that we are building
#   source - object from original XML to be processed
#   from_name - name of object processed previously
#
#################################################################################


process_general_object <- function(xml_query, xml_node, xml_input, from_name) {
  # get type of node
  node_tag <- xml_name(xml_node)
  node_name <- xml_attr(xml_node, "NAME")
  node_type <- xml_attr(xml_node, "TYPE")
  # print(node_tag)
  # print(node_name)
  # print(node_type)
  if (node_tag == "SOURCE") {                       # source
    xml_query <- process_source_table(xml_query, xml_node)
  } else if (node_type == "Source Qualifier") {     # SQ
    xml_query <- process_source_qualifier(xml_query, xml_node, from_name)
  } else if (node_type == "Filter") {               # Filter
    xml_query <- process_filter(xml_query, xml_node, from_name)  
  } else if (node_type == "Expression") {           # Expression
    xml_query <- process_expression(xml_query, xml_node, from_name)  
  }
  
  ## sort tags to make XML more readable
  slct <- getNodeSet(xml_query, "//SELECT")[[1]]
  xmlChildren(slct) <- c(xmlChildren(getNodeSet(xml_query, "//SELECT")[[1]]))[c(order(factor(names(getNodeSet(xml_query, "//SELECT")[[1]]), levels = c("COLUMN","TABLE","CONDITION","INCLUDED_OBJECT"))))] 
  # nahradit select v xml_query tymto usporadanym v slct - inspiracia z riadku hore? 
  xml_query <- newXMLDoc(node = slct)
  
  # check outgoing connectors - where to go
  # if only one, can go
  # if the target object has only this unique connector, can go
  outgoing_connectors <- subset(connectors_unique_df, from_instance == node_name)
  target_inc_connectors <- subset(connectors_unique_df, to_instance == outgoing_connectors$to_instance)
  if (nrow(outgoing_connectors) == 1 && nrow(target_inc_connectors) == 1) { #straight line of succesion
    target_xpath <- paste0("//*[not(name()='INSTANCE') and @NAME='", outgoing_connectors$to_instance[1], "']")
    target_node <- xml_find_all(xml_input, target_xpath)
    if (length(target_node) != 1) {
      print("error2001")
    }
    xml_query <- process_general_object(xml_query, target_node, xml_input, node_name)#z xml povodneho node s menom targetu)
  } else {
    
  }
  
  return(xml_query)
}


process_source_table <- function(xml_query, source) {
  # create main SELECT node 
  no_select <- no_select+1
  curr_select <- paste0("sub", no_select)
  select_node <- newXMLNode("SELECT", attrs = c(alias = curr_select), doc = xml_query)
  
  # add TABLE node inside
  no_source <- no_source+1
  curr_source <- paste0("src", no_source)
  newXMLNode("TABLE", attrs = c(name = xml_attr(source, "NAME"), 
                                alias = curr_source),
             parent = select_node)
  
  # add COLUMN nodes inside
  columns <- xml_find_all(source, ".//SOURCEFIELD") 
  for (col in columns) {
    newXMLNode("COLUMN", attrs = c(name = xml_attr(col, "NAME"), 
                                   alias = xml_attr(col, "NAME"),
                                   source = curr_source),
               parent = select_node)
  }
  
  # add INCLUDED_OBJECT node inside
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source, "NAME"), alias = curr_source),
             parent = getNodeSet(xml_query, "//SELECT")[[1]])
  
  # return whole SELECT node back
  return(xml_query)
}

process_source_qualifier <- function(xml_query, source, from_name) {
  xml_query <- update_column_aliases(xml_query, from_name, xml_attr(source, "NAME"))
  
  # condition - only if exists 
  condition_node <- xml_find_all(source, ".//TABLEATTRIBUTE[@NAME='Source Filter']") 
  if (length(condition_node) == 1) {
    cond_value <- xml_attr(condition_node, "VALUE")
    newXMLNode("CONDITION", attrs = c(value = cond_value, object = xml_attr(source, "NAME")),
               parent = getNodeSet(xml_query, "//SELECT")[1])
    # TODO: doplnit aliasy stlpcov v podmienke (where cast);
  } else {
    print("error2002")
  }
  
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source, "NAME"), alias = xml_attr(source, "NAME")), #TODO: alias - prenasat alias zo sourcu, ak teda je to jednoducha vetva. nemusi sa potom tracovat source dozadu az, ?
             parent = getNodeSet(xml_query, "//SELECT")[[1]])
  
  # return whole xml_query back
  return(xml_query)
}

process_filter <- function(xml_query, source, from_name) {
  xml_query <- update_column_aliases(xml_query, from_name, xml_attr(source, "NAME"))
  
  # condition - only if exists 
  condition_node <- xml_find_all(source, ".//TABLEATTRIBUTE[@NAME='Filter Condition']") 
  if (length(condition_node) == 1) {
    cond_value <- xml_attr(condition_node, "VALUE")
    newXMLNode("CONDITION", attrs = c(value = cond_value, object = xml_attr(source, "NAME")),
               parent = getNodeSet(xml_query, "//SELECT")[1])
    # TODO: doplnit aliasy stlpcov v podmienke (where cast);
  } else {
    print("error2003")
  }
  
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source, "NAME"), alias = xml_attr(source, "NAME")), #TODO: alias - to iste
             parent = getNodeSet(xml_query, "//SELECT")[[1]])
  
  # return whole xml_query back
  return(xml_query)
}

process_expression <- function(xml_query, source_in, from_name) {
  xml_query <- update_column_aliases(xml_query, from_name, xml_attr(source_in, "NAME"))
  
  # check for adding columns
  # get all TRANSFORMFIELD tags, 
  transformfields <- xml_find_all(source_in, ".//TRANSFORMFIELD")
  # check NAME attr
  for (trans in transformfields) {
    name <- xml_attr(trans, "NAME")
    # ak taky nie je v COLUMN -- v mojom xml,
    if (length(getNodeSet(xml_query, paste0("//COLUMN[@alias='", name, "']"))) == 0) {
      # tak tam taky column pridam s expression z EXPRESSION attr
      # unique connectors back to the source 
      ##TODO: neskor - nech sa zastavi na nejakom rozdeleni/spoji ako join/union, a nech ten je ten zdroj, alebo sa pozret z ktorej z joinovanych tabuliek ide
      source_alias <- trace_source(xml_attr(source_in, "NAME"))
      newXMLNode("COLUMN", attrs = c(name = name, alias = name,
                                     source = source_alias), 
                 parent = getNodeSet(xml_query, "//SELECT")[1],
                 .children = list(newXMLNode("EXPRESSION", attrs = c(value = xml_attr(trans, "EXPRESSION"), 
                                                                     level = 1, object = xml_attr(source_in, "NAME")))))
      # prirobit vlozene tagy - expression - tu nad tymto riadko je to uz
      
    }
  }
  
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source_in, "NAME"), alias = xml_attr(source_in, "NAME")), #TODO: alias -- to iste
             parent = getNodeSet(xml_query, "//SELECT")[[1]])
  
  return(xml_query)
}

## utility function to update aliases in XML query called at beginning of every process function
update_column_aliases <- function(xml_query, from_name, to_name) {
  #get connectors from from_name object - na zmenu aliasu column
  curr_connectors <- subset(connectors_all_df, from_instance == from_name & to_instance == to_name)
  for (i_conn in 1:nrow(curr_connectors)) {
    from_field <- curr_connectors$from_field[i_conn]
    to_field <- curr_connectors$to_field[i_conn]
    if (from_field != to_field) { # need to update alias
      # find in xml_query column s aliasom $from_field, zmenit ho na $to_field
      xpathApply(xml_query, paste0("//COLUMN[@alias='", from_field, "']"), 
                 addAttributes, alias = to_field)
    } # else # no need to update alias
  }
  return(xml_query)
}

## utility func to trace back alias of source table for column names that are specified in later objects like expressions
trace_source <- function(object_name) {
  ret <- NULL
  from_object <- subset(connectors_unique_df, to_instance == object_name)$from_instance
  if (length(from_object) > 1) { #problem - vetvenie, dva do jednoho ako pri union/join, STOP! alebo trace len do jednej vetvy
    print("error8001")
  } else if (length(from_object) == 0) { #nie je uz ziadny predchadzajuci objekt, return alias
    ret <- unname(xmlAttrs(getNodeSet(xml_query, paste0("//INCLUDED_OBJECT[@name='", object_name, "']"))[[1]])['alias']) #vrati vektor attributov, druhy je alias
  } else if (length(from_object) == 1) { #jeden predchadzajuci, call itself
    ret <- trace_source(from_object)
  } else { #necakane
    print("error8002")
  }
  #print(ret)
  return(ret)
}