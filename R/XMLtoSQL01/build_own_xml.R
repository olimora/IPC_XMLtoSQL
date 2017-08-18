library("xml2")
library("XML")

to_oracle_syntax <- function(input) {
  input <- gsub("\\$\\$", "$", input)
  input <- gsub("&lt;", "<", input) 
  input <- gsub("&gt;", ">", input)
  return(input)
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
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source, "NAME")),
             parent = select_node)
  
  # return whole SELECT node back
  return(xml_query)
}

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

process_source_qualifier <- function(xml_query, source, from_name) {
  xml_query <- update_column_aliases(xml_query, from_name, xml_attr(source, "NAME"))
  
  # condition - only if exists 
  condition_node <- xml_find_all(source, ".//TABLEATTRIBUTE[@NAME='Source Filter']") 
  if (length(condition_node) == 1) {
    cond_value <- xml_attr(condition_node, "VALUE")
    newXMLNode("CONDITION", attrs = c(value = cond_value),
               parent = getNodeSet(xml_query, "//SELECT")[1])
    # aliasy stlpcov v podmienke!!! TODO
  } else {
    print("error2002")
  }
  
  # return whole xml_query back
  return(xml_query)
}

process_filter <- function(xml_query, source, from_name) {
  xml_query <- update_column_aliases(xml_query, from_name, xml_attr(source, "NAME"))
  
  # condition - only if exists 
  condition_node <- xml_find_all(source, ".//TABLEATTRIBUTE[@NAME='Filter Condition']") 
  if (length(condition_node) == 1) {
    cond_value <- xml_attr(condition_node, "VALUE")
    newXMLNode("CONDITION", attrs = c(value = cond_value),
               parent = getNodeSet(xml_query, "//SELECT")[1])
    # aliasy stlpcov v podmienke!!! TODO
  } else {
    print("error2003")
  }
  
  # return whole xml_query back
  return(xml_query)
}

process_expression <- function(xml_query, source, from_name) {
  xml_query <- update_column_aliases(xml_query, from_name, xml_attr(source, "NAME"))
  
  # check for adding columns
  # get all TRANSFORMFIELD tags, 
  transformfields <- xml_find_all(source, ".//TRANSFORMFIELD")
  # check NAME attr
  for (trans in transformfields) {
    name <- xml_attr(trans, "NAME")
    # ak taky nie je v COLUMN -- v mojom xml,
    if (length(getNodeSet(xml_query, paste0("//COLUMN[@alias='", name, "']"))) == 0) {
      # tak tam taky column pridam s expression z EXPRESSION attr
      newXMLNode("COLUMN", attrs = c(name = name,
                                     alias = name,
                                     source = xml_attr(source, "NAME")), # pozret podla connectors source toho stlpca, odkial ide //alebo v mojom xml, v COLUMN podla mena
                 parent = getNodeSet(xml_query, "//SELECT")[1],
                 .children = list(newXMLNode("EXPRESSION", attrs = c(value = xml_attr(trans, "EXPRESSION"), level = 1))))
      # prirobit vlozene tagy - expression
      
    }
  }

  return(xml_query)
}

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
  
  # check outgoing connectors - where to go
  # if only one, can go
  # if the target object has only this unique connector, can go
  outgoing_connectors <- subset(connectors_unique_df, from_instance == node_name)
  target_inc_connectors <- subset(connectors_unique_df, to_instance == outgoing_connectors$to_instance)
  if (nrow(outgoing_connectors) == 1 && nrow(target_inc_connectors) == 1) { #straight line of succesion
    target_xpath <- paste0("//*[not(name()='INSTANCE') and @NAME='", outgoing_connectors$to_instance[1], "']")
    #print(target_xpath)
    target_node <- xml_find_all(xml_input, target_xpath)
    #print(target_node)
    if (length(target_node) != 1) {
      print("error2001")
    }
    xml_query <- process_general_object(xml_query, target_node, xml_input, node_name)#z xml povodneho node s menom targetu)
  } else {
    
  }
  
  return(xml_query)
}

xml_to_sql <- function(xml_query) {
  select_part <- "SELECT"
  from_part <- "FROM"
  where_part <- "WHERE"
  
  # columns to select
  #TODO: expressions z columnov
  #TODO: src. - pri tych co su z exp_src_cd - vyriesit tak asi ze tam nebude ziadny source
  columns <- getNodeSet(xml_query, "//COLUMN")
  for (i in 1:length(columns)) {
    row <- xmlAttrs(columns[i][[1]])
    #print(row)
    if (i == 1) { #prvy, bez ciarky na zaciatku (koniec predchadzajuceho)
      select_part <- cat(select_part, paste0(row[3], ".", row[1], " AS ", row[2]), sep = "\n")
    } else { #ostatne
      select_part <- cat(select_part, paste0(", ", row[3], ".", row[1], " AS ", row[2]), sep = "\n")
    }
  }
  
  print(select_part)

}

## global variables ##########
no_select <- 0
no_source <- 0

#read xml
xml_input <- read_xml("example.xml")

## connectory medzi objektmi ########
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
connectors_all_df <- data.frame(from_instance, to_instance, from_field, to_field, stringsAsFactors = F)
connectors_unique_df <- unique(data.frame(from_instance, to_instance, stringsAsFactors = F))
all_objects <- unique(append(from_instance,to_instance))
rm("from_instance", "to_instance", "from_field", "to_field")

## find all sources ################
sources <- xml_find_all(xml_input, ".//SOURCE")

xml_query <- newXMLDoc()
for (src in sources) {
  xml_query <- process_general_object(xml_query, src, xml_input, NULL)
}

sql_query <- xml_to_sql(xml_query)

