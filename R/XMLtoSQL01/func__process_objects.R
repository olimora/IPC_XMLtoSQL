#################################################################################
#
# functions for processing objects from XML mapping
# they produce new XML representation of SQL query
# each adds COLUMNS, CONDITIONS ... to existing XML doc - xml_query
# params: 
#   xml_query - XML doc that we are building
#   xml_node - object from original XML to be processed
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
  
  # update aliases on columns
  if (node_tag != "SOURCE") { #another condition needed?
    xml_query <- update_column_aliases(xml_query, from_name, xml_attr(xml_node, "NAME"))
  }
  
  # process xml mapping object
  if (node_tag == "SOURCE") {                       # source
    xml_query <- process_source_table(xml_query, xml_node)
  } else if (node_type == "Source Qualifier") {     # SQ
    xml_query <- process_source_qualifier(xml_query, xml_node, from_name)
  } else if (node_type == "Filter") {               # Filter
    xml_query <- process_filter(xml_query, xml_node, from_name)  
  } else if (node_type == "Expression") {           # Expression
    xml_query <- process_expression(xml_query, xml_node, from_name)  
  }
  
  # add INCLUDED_OBJECT node inside
  curr_source <- "NA"
  if (is.null(from_name)) {     #make new
    if (node_tag != "SOURCE") { counters_env$source_no <- counters_env$source_no+1  }
    curr_source <- paste0("src", counters_env$source_no)
  } else {        #find source alias from xml_query - INCLUDED with name == from_name
    curr_source <- unname(xmlAttrs(getNodeSet(xml_query, paste0("//INCLUDED_OBJECT[@name='", from_name, "']"))[[1]])['alias'])
  }
  newXMLNode("INCLUDED_OBJECT", attrs = c(name = xml_attr(xml_node, "NAME"), alias = curr_source),
             parent = getNodeSet(xml_query, "//SELECT")[[1]])
  
  ## sort tags to make XML more readable
  slct <- getNodeSet(xml_query, "//SELECT")[[1]]
  xmlChildren(slct) <- c(xmlChildren(getNodeSet(xml_query, "//SELECT")[[1]]))[c(order(factor(names(getNodeSet(xml_query, "//SELECT")[[1]]), levels = c("COLUMN","TABLE","CONDITION","INCLUDED_OBJECT"))))] 
  xml_query <- newXMLDoc(node = slct) 
  
  #TODO: vymazat COLUMNs ktore nejdu nikam do ziadneho objektu? alebo raz na konci pred target? budem ich niekedy potrebovat?
  
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
  } else { #TODO: - with branching problem
    
  }
  
  return(xml_query)
}

process_source_table <- function(xml_query, xml_node) {
  # global variables 
  counters_env$select_no <- counters_env$select_no+1
  curr_select <- paste0("sub", counters_env$select_no)
  counters_env$source_no <- counters_env$source_no+1
  curr_source <- paste0("src", counters_env$source_no)
  
  # create main SELECT node
  select_node <- newXMLNode("SELECT", attrs = c(alias = curr_select), doc = xml_query)
  
  # add TABLE node inside
  newXMLNode("TABLE", attrs = c(name = xml_attr(xml_node, "NAME"), 
                                alias = curr_source),
             parent = select_node)
  
  # add COLUMN nodes inside
  columns <- xml_find_all(xml_node, ".//SOURCEFIELD") 
  for (col in columns) {
    at_name <- xml_attr(col, "NAME")
    newXMLNode("COLUMN", attrs = c(name = at_name, alias = at_name, source = curr_source,
                                   value = paste0(curr_source, ".", at_name)),
               parent = select_node)
  }

  # return whole SELECT node back
  return(xml_query)
}

process_source_qualifier <- function(xml_query, xml_node, from_name) {
  # condition - only if exists 
  condition_node <- xml_find_all(xml_node, ".//TABLEATTRIBUTE[@NAME='Source Filter']") 
  if (length(condition_node) == 1) {
    cond_value <- xml_attr(condition_node, "VALUE")
    # add scheme to column names (replace with scheme before the column name)
    transformfields <- xml_find_all(xml_node, ".//TRANSFORMFIELD")
    for (tf in transformfields) {
      find <- xml_attr(tf, "NAME")
      src_alias <- unname(xmlAttrs(getNodeSet(xml_query, paste0("//SELECT/COLUMN[@alias='", find, "']"))[[1]])['source'])
      cond_value <- gsub(paste0("([^a-zA-Z0-9]*)(",find,")([^a-zA-Z0-9]*)"), #pattern
                         paste0("\\1",src_alias,".",find,"\\3"), #replacement 
                         cond_value, ignore.case = T)
    }
    newXMLNode("CONDITION", attrs = c(value = cond_value, object = xml_attr(xml_node, "NAME")),
               parent = getNodeSet(xml_query, "//SELECT")[1])
  } else {
    print("error2002")
  }
  
  return(xml_query)
}

process_filter <- function(xml_query, xml_node, from_name) {
  # condition - only if exists 
  condition_node <- xml_find_all(xml_node, ".//TABLEATTRIBUTE[@NAME='Filter Condition']") 
  if (length(condition_node) == 1) {
    cond_value <- xml_attr(condition_node, "VALUE")
    # add scheme to column names (replace with scheme before the column name)
    transformfields <- xml_find_all(xml_node, ".//TRANSFORMFIELD")
    for (tf in transformfields) {
      find <- xml_attr(tf, "NAME")
      src_alias <- unname(xmlAttrs(getNodeSet(xml_query, paste0("//SELECT/COLUMN[@alias='", find, "']"))[[1]])['source'])
      cond_value <- gsub(paste0("([^a-zA-Z0-9]*)(",find,")([^a-zA-Z0-9]*)"), #pattern
                         paste0("\\1",src_alias,".",find,"\\3"), #replacement 
                         cond_value, ignore.case = T)
    }
    newXMLNode("CONDITION", attrs = c(value = cond_value, object = xml_attr(xml_node, "NAME")),
               parent = getNodeSet(xml_query, "//SELECT")[1])
  } else {
    print("error2003")
  }
  
  # return whole xml_query back
  return(xml_query)
}

process_expression <- function(xml_query, xml_node, from_name) {
  ## get all TRANSFORMFIELD tags, 
  transformfields <- xml_find_all(xml_node, ".//TRANSFORMFIELD")
  ## find pairs for - replace column name in expression value for column name from source
  finds <- c()   #ones I am looking for to replace
  repls <- c()   #replacements
  for (tf in transformfields) {
    curr_f <- xml_attr(tf, "NAME")
    curr_r <- NULL
    tryCatch({
      curr_r <- unname(xmlAttrs(getNodeSet(xml_query, paste0("//SELECT/COLUMN[@alias='", curr_f, "']"))[[1]])['name'])
      finds <- append(finds, curr_f)
      repls <- append(repls, curr_r)
    }, error = function(err) {
      # error handler picks up where error was generated
      print(paste0("EXPECTED ERROR 7001 (process_expression, column name in expression replacement for '",curr_f,"'):  ",err))
    })
  }
  
  ## for each transformfield
    for (trans in transformfields) {
    #source alias from previous object
    source_alias <- unname(xmlAttrs(getNodeSet(xml_query, paste0("//INCLUDED_OBJECT[@name='", from_name, "']"))[[1]])['alias'])
    
    ## check NAME attr; 
    ## if there is not such column in xml_query, then add column  - expression is added later in this func
    ## if there is, add only expression 
    name <- xml_attr(trans, "NAME")
    if (length(getNodeSet(xml_query, paste0("//COLUMN[@alias='", name, "']"))) == 0) {
      newXMLNode("COLUMN", attrs = c(name = name, alias = name, source = source_alias, 
                                     value = paste0(source_alias, ".", name)), 
                 parent = getNodeSet(xml_query, "//SELECT")[1])
    } 
    
    ## add EXPRESSION to column      
    expr_value <- xml_attr(trans, "EXPRESSION")
    if (!is.na(expr_value) && expr_value != name) { #if @expression is the same as @name, dont put in the expression, 
      ## get number of all expressions on that column + 1 for new level
      expr_level <- 1 + length(getNodeSet(xml_query, paste0("//SELECT/COLUMN[@name='", name,"' and @alias='", name, "' and @source='", source_alias, "']/EXPRESSION")))
      
      ## column name replacement in expression value
      for (fi in 1:length(finds)) {
        expr_value <- gsub(paste0("([^a-zA-Z0-9]*)(", finds[fi], ")([^a-zA-Z0-9]*)"), 
                           paste0("\\1",repls[fi],"\\3"), expr_value, ignore.case = T)
      }
      
      ## parent node - column to add expression to
      column_node <- getNodeSet(xml_query, paste0("//SELECT/COLUMN[@name='", name, "' and @alias='", name, "' and @source='", source_alias, "']"))[[1]]
      
      ## add EXPRESSION node to xml_query
      newXMLNode("EXPRESSION", attrs = c(value = expr_value, level = expr_level, object = xml_attr(xml_node, "NAME")),
                 parent = column_node)
      
      ## cocoon the expresion on existing column value
      old_value <- xmlAttrs(column_node)['value']
      new_value <- gsub(paste0("([^a-zA-Z0-9]*)(", name, ")([^a-zA-Z0-9]*)"),
                        paste0("\\1",old_value,"\\3"), expr_value, ignore.case = T) #pattern=name, repl=old_value, x=expr_value
      addAttributes(column_node, value = new_value)
      
      #TODO: tu pre kazdy column v expr pozert value v xml_query a nahradit ho - bude treba v expr ako je za lookupom
      # - pre kazdy column v tomto objekte loop? - iny byt nemoze a inak ich z expr nemam ako vytiahnut - rovnako ako hore, ale nahradzujem z @value
      #TODO: mysli na vhniezdovanie expressions
      #TODO: refactor this function, so it has less code since it makes some of it 2times, make it more fluent and readable, less getNodeSet - when it is in right order
    }
  }
  
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

## not used. 
# ## utility func to trace back alias of source table for column names that are specified in later objects like expressions
# trace_source <- function(object_name) {
#   ret <- NULL
#   from_object <- subset(connectors_unique_df, to_instance == object_name)$from_instance
#   if (length(from_object) > 1) { #problem - vetvenie, dva do jednoho ako pri union/join, STOP! alebo trace len do jednej vetvy
#     print("error8001")
#   } else if (length(from_object) == 0) { #nie je uz ziadny predchadzajuci objekt, return alias
#     ret <- unname(xmlAttrs(getNodeSet(xml_query, paste0("//INCLUDED_OBJECT[@name='", object_name, "']"))[[1]])['alias']) #vrati vektor attributov, druhy je alias
#   } else if (length(from_object) == 1) { #jeden predchadzajuci, call itself
#     ret <- trace_source(from_object)
#   } else { #necakane
#     print("error8002")
#   }
#   #print(ret)
#   return(ret)
# }