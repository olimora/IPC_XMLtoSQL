library("xml2")
library("XML")

process_source_table <- function(source) {
  # create main SELECT node 
  no_select <- no_select+1
  curr_select <- paste0("sub", no_select)
  select_node <- newXMLNode("SELECT", attrs = c(alias = curr_select))
  
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
  return(select_node)
}

process_source_qualifier <- function(xml_query, source, from_name) {
  
  #get connectors from from_name object - na zmenu aliasu column
  curr_connectors <- subset(connectors_all_df, from_instance == from_name & to_instance == xml_attr(source, "NAME"))
  for (i_conn in 1:nrow(curr_connectors)) {
    if (curr_connectors$from_field[i_conn] != curr_connectors$to_field[i_conn]) { # need to update alias
      # find in xml_query column s aliasom $from_field, zmenit ho na $to_field
      #col <- xml_find_all(xml_query, paste0(".//COLUMN[@alias='", curr_connectors$from_field[i_conn] ,"']"))
      #addAttributes() / removeAttributes()
      col <- xmlChildren(xml_query, F) #je ich vela. prejst v cykle, kde attribut... alebo ina library piceee
    } # else # no need to update alias
  }
  
  # condition - only if exists 
  condition_node <- xml_find_all(source, ".//TABLEATTRIBUTE[@NAME='Source Filter']") 
  if (length(condition_node) == 1) {
    newXMLNode("CONDITION", attrs = c(value = xml_attr(condition_node, "VALUE")),
               parent = xml_query)
    # aliasy stlpcov v podmienke!!! TODO
  } else {
    print("error2002")
  }
  
  # return whole xml_query back
  return(xml_query)
}

process_general_object <- function(xml_query, xml_node, xml_input, from_name) {
  # get type of node
  node_tag <- xml_name(xml_node)
  node_name <- xml_attr(xml_node, "NAME")
  node_type <- xml_attr(xml_node, "TYPE")
  print(node_tag)
  print(node_name)
  print(node_type)
  if (is.null(xml_query) && node_tag == "SOURCE") {                       # source
    xml_query <- process_source_table(xml_node)
  } else if (node_type == "Source Qualifier") {     # SQ
    xml_query <- process_source_qualifier(xml_query, xml_node, from_name)
  }
  
  # check outgoing connectors - where to go
  # if only one, can go
  # if the target object has only this unique connector, can go
  outgoing_connectors <- subset(connectors_unique_df, from_instance == node_name)
  target_inc_connectors <- subset(connectors_unique_df, to_instance == outgoing_connectors$to_instance)
  if (nrow(outgoing_connectors) == 1 && nrow(target_inc_connectors) == 1) { #straight line of succesion
    target_xpath <- paste0("//*[not(name()='INSTANCE') and @NAME='", outgoing_connectors$to_instance[1], "']")
    print(target_xpath)
    target_node <- xml_find_all(xml_input, target_xpath)
    print(target_node)
    if (length(target_node) != 1) {
      print("error2001")
    }
    xml_query <- process_general_object(xml_query, target_node, xml_input, node_name)#z xml povodneho node s menom targetu)
  } else {
    
  }
  
  return(xml_query)
  
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

for (src in sources) {
  xml_query <- process_general_object(NULL, src, xml_input, NULL)
}



## pozret na unique connectory, ze kam sa ide od tialto, prejst na ten novy objekt
## tu ak je to SQ, tak vykonaj ---
## vsetky connectory od predchadzajuceho sem, pozret ci sa nezmeni nazov stlpcov -- teda alias ci sa nemeni
## pole source filter do WHERE 
## <TABLEATTRIBUTE NAME ="Source Filter" VALUE ="edw_business_date = to_date(&apos;$$BUSINESS_DATE&apos;,&apos;YYYYMMDDHH24MISS&apos;)"/>


