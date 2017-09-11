###############################################################
#
# functions used to replace expressions to ORACLE syntax
# params: 
#   xml_query - XML doc with query repres.
#
##################

# :UDF.DEFAULTSTRINGNULL(val) /=/ CASE WHEN val IS NULL OR ltrim(val) = '' THEN 'XNA' ELSE ltrim(rtrim(val))
# ltrim(val) /=/ ltrim() - no change
# rtrim(val) /=/ rtrim() - no change
# isnull(val) /=/ val IS NULL