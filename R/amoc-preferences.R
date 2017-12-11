
# Use the R6 class library
library(igraph)
# Chancks to see if a preference fulfils the requirements



setOldClass("igraph") #Required to use igraph in field listing
preference <- setRefClass("preference", 
                       fields = 
                         list(
                           desired_order = "igraph", 
                           consent_order = "igraph", 
                           undesired_order = "igraph"),
                       methods = 
                         list(
                           is_valid = function() {
                             class(desired_order) == "igraph" &&
                               class(consent_order) == "igraph" &&
                               class(undesired_order) == "igraph" &&
                               length(intersect(names(V(desired_order)),names(V(consent_order)))) == 0 &&
                                 length(intersect(names(V(desired_order)),names(V(undesired_order)))) == 0 &&
                                 length(intersect(names(V(consent_order)),names(V(undesired_order)))) == 0
                           },
                           desired_options = function() {
                             names(V(desired_order))
                           },
                           consent_options = function() {
                             names(V(consent_order))
                           },
                           undesired_options = function() {
                             names(V(undesired_order))
                           },
                           all_options = function() {
                              union(union(desired_options(),consent_options()),undesired_options())
                           }
                        )
                      )

