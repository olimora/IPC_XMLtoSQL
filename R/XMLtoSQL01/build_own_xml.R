library("xml2")
library("XML")

to_oracle_syntax <- function(input) {
  input <- gsub("\\$\\$", "$", input)
  input <- gsub("&lt;", "<", input) 
  input <- gsub("&gt;", ">", input)
  return(input)
}

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
    newXMLNode("CONDITION", attrs = c(value = cond_value, object = xml_attr(source, "NAME")),
               parent = getNodeSet(xml_query, "//SELECT")[1])
    # aliasy stlpcov v podmienke!!! TODO
  } else {
    print("error2002")
  }
  
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source, "NAME"), alias = xml_attr(source, "NAME")), #TODO: alias 
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
    # aliasy stlpcov v podmienke!!! TODO
  } else {
    print("error2003")
  }
  
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source, "NAME"), alias = xml_attr(source, "NAME")), #TODO: alias
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
                                     source = source_alias), # TODO: pozret podla connectors source toho stlpca, odkial ide - v kazdom INCLUDED_OBJECT moze byt alias, ktory sa tuto bude tahat uz od zdroja. mozem pozret ze odkial som prisiel (from_name), a aky tam je alias //alebo v mojom xml, v COLUMN podla mena
                 parent = getNodeSet(xml_query, "//SELECT")[1],
                 .children = list(newXMLNode("EXPRESSION", attrs = c(value = xml_attr(trans, "EXPRESSION"), 
                                                                     level = 1, object = xml_attr(source_in, "NAME")))))
      # prirobit vlozene tagy - expression - tu nad tymto riadko je to uz
      
    }
  }
  
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(source_in, "NAME"), alias = xml_attr(source_in, "NAME")), #TODO: alias
             parent = getNodeSet(xml_query, "//SELECT")[[1]])
  
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
  
  
  ## usporiadat tagy - opravit. nejde zmenit deti takto. prava strana je dobra
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
  where_part <- "WHERE 1=1"
  
  # columns to select
  #TODO: expressions z columnov
  #TODO: src. - pri tych co su z exp_src_cd - vyriesit tak asi ze tam nebude ziadny source
  columns <- getNodeSet(xml_query, "//COLUMN")
  for (i in 1:length(columns)) {
    attrs <- xmlAttrs(columns[i][[1]])
    added_row <- paste0(attrs['source'], ".", attrs['name'], " AS ", attrs['alias'])
    # ak ma EXPRESSION, tak pridat do komentu 
    exprs <- getNodeSet(xml_query, paste0("//COLUMN[@name='", attrs['name'], "' and @alias='", attrs['alias'], "' and @source='", attrs['source'], "']",
                                          "/EXPRESSION"))
    if (!is.null(exprs[1][[1]])) {
      for (j in 1:length(exprs)) {
        added_row <- paste0(added_row, " --", unname(xmlAttrs(exprs[j][[1]])['value']))
      }
    }

    if (i == 1) { #prvy, bez ciarky na zaciatku (koniec predchadzajuceho)
      select_part <- paste(select_part, "\n", paste0(added_row))
    } else { #ostatne
      select_part <- paste(select_part, "\n", paste0(", ", added_row))
    }
    
  }
  
  #print(select_part)
  
  # tables and joins
  # schemu pred tabulku . ako string pred prvym "_" ?
  tables <- getNodeSet(xml_query, "//TABLE")
  for (i in 1:length(tables)) {
    row <- xmlAttrs(tables[i][[1]])
    if (i == 1) { # prva tabulka, bez joinu
      from_part <- paste(from_part, paste0(row['name'], " AS ", row['alias']))
    } else { # join
      from_part <- paste(from_part, paste0("JOIN ", row['name'], " AS ", row['alias']), "\n") #TODO: on .... 
    }
  }
  
  #print(from_part)
  
  # conditions
  # TODO: aliasy v podmienkach. da sa? -> podla koenktorov, ze odkial idu?, aliasy nazvu objektu v mappingu?
  conditions <- getNodeSet(xml_query, "//CONDITION")
  for (i in 1:length(conditions)) {
    row <- xmlAttrs(conditions[i][[1]])
    where_part <- paste(where_part, "\n", paste0(" AND ", row[1]))
  }
  
  whole_query <- paste(select_part, from_part, where_part, sep = "\n")
  writeLines(whole_query)
  #TODO: cat() dava "NULL" na konci

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


