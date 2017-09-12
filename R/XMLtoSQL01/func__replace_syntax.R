###############################################################
#
# functions used to replace expressions to ORACLE syntax
# params: 
#   xml_query - XML doc with query repres.
#
##################

# not used yet - to be used when revriting database functions to oracle syntax 
# to_oracle_syntax <- function(input) {
#   input <- gsub("\\$\\$", "$", input)
#   input <- gsub("&lt;", "<", input) 
#   input <- gsub("&gt;", ">", input)
#   return(input)
# }


# :UDF.DEFAULTSTRINGNULL(val) /=/ CASE WHEN val IS NULL OR ltrim(val) = '' THEN 'XNA' ELSE ltrim(rtrim(val))
# ltrim(val) /=/ ltrim() - no change
# rtrim(val) /=/ rtrim() - no change
# isnull(val) /=/ val IS NULL