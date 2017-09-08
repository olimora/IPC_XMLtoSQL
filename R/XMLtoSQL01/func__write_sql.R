###############################################################
#
# functions used to build SQL query in character vector representation 
# from XML representation build by this programm
# params: 
#   xml_query - XML doc with query repres.
#
##################

xml_to_sql <- function(xml_query) {
  select_part <- build_select_part(xml_query)
  from_part <- build_from_part(xml_query)
  where_part <- build_where_part(xml_query)
  
  whole_query <- paste(select_part, from_part, where_part, sep = "\n")
  return(whole_query)
}

build_select_part <- function(xml_query) {
  select_part <- "SELECT"
  # columns to select
  columns <- getNodeSet(xml_query, "//COLUMN")
  for (i in 1:length(columns)) {
    attrs <- xmlAttrs(columns[i][[1]])
    added_row <- paste0(attrs['source'], ".", attrs['name'], " AS ", attrs['alias'])
    #added_row <- ""
    # build coulmn with expressions applied
    exprs <- getNodeSet(xml_query, paste0("//COLUMN[@name='", attrs['name'], "' and @alias='", attrs['alias'], "' and @source='", attrs['source'], "']",
                                          "/EXPRESSION"))
    if (!is.null(exprs[1][[1]])) {
      for (j in 1:length(exprs)) { 
        # TODO:? add expressions by lvl in loop. cocoon around it, replace to ORACLE syntax
        # if lvl == 1 / j == 1, add source.
        added_row <- paste0(added_row, " --lvl=", unname(xmlAttrs(exprs[j][[1]])['level']), ": ", unname(xmlAttrs(exprs[j][[1]])['value']), "; ")
      }
    }
    
    if (i == 1) { #prvy, bez ciarky na zaciatku (koniec predchadzajuceho)
      select_part <- paste(select_part, "\n", paste0(added_row))
    } else { #ostatne
      select_part <- paste(select_part, "\n", paste0(", ", added_row))
    }
  }
  return(select_part)
}

build_from_part <- function(xml_query) {
  from_part <- "FROM"
  # tables and joins
  tables <- getNodeSet(xml_query, "//TABLE")
  for (i in 1:length(tables)) {
    attrs <- xmlAttrs(tables[i][[1]])
    scheme <- get_scheme_for_table(attrs['name'])
    added_row <- paste0(scheme, ".", attrs['name'], " AS ", attrs['alias'])
    if (i == 1) { # prva tabulka, bez joinu
      from_part <- paste(from_part, added_row)
    } else { # join
      from_part <- paste(from_part, paste0("JOIN ", added_row, "\n")) #TODO: "on ... and ..." 
    }
  }
  return(from_part)
}

build_where_part <- function(xml_query) {
  where_part <- "WHERE 1=1"
  # conditions
  conditions <- getNodeSet(xml_query, "//CONDITION")
  for (i in 1:length(conditions)) {
    attrs <- xmlAttrs(conditions[i][[1]])
    where_part <- paste(where_part, "\n", paste0(" AND ", attrs[1]))
  }
  return(where_part)
}

get_scheme_for_table <- function(table_name) {
  ret <- gsub("_.+", "", table_name, ignore.case = T)
  return(ret)
}