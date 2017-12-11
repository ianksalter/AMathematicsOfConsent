
library(igraph)

#setup
desired_order_1 <- graph_from_literal(Sushi,Chinese)
consent_order_1 <- graph_from_literal(Turkish)
undesired_order_1 <- graph_from_literal(Indian)
preference_1 <- preference(desired_order = desired_order_1,
                           consent_order=consent_order_1,
                           undesired_order=undesired_order_1)

desired_order_2 <- graph_from_literal(Sushi+--Indian)
consent_order_2 <- graph_from_literal(Turkish)
undesired_order_2 <- graph_from_literal(Chinese)


test_that("Preference Creation and access functions",{

  # Check a vaild preorder passes is_vaild test
  expect_that(
    preference_1$is_valid(), 
    is_true()
  )
  
  # Check that incorrect preference constructor arguments throws error
  expect_that(
    preference(desired_order = "a",
               consent_order=consent_order_1,
               undesired_order=undesired_order_1), 
    throws_error()
  )
  
  # Check that incorrect preference constructor without named arguments 
  # generates warning
  expect_warning(
    preference("a","b","c")
  )
  expect_warning(
    preference(desired_order_1,consent_order_1,undesired_order_1) #Even though arguments are valid!!!
  )
  
  #Check that overlapping orders fail is valid testtest
  bad_preference_1 <- preference(desired_order = desired_order_1,
                                  consent_order=consent_order_1,
                                  undesired_order=undesired_order_2)
  expect_that(
    bad_preference_1$is_valid(),
    is_false()
  )
  
  # Check that all_options works as expected
  expect_that(
    setequal(
      preference_1$desired_options(),
      c("Sushi","Chinese")),
    is_true()
  )
  
  # Check that all_options works as expected
  expect_that(
    setequal(
      preference_1$consent_options(),
      c("Turkish")),
    is_true()
  )
  
  # Check that all_options works as expected
  expect_that(
    setequal(
      preference_1$undesired_options(),
      c("Indian")),
    is_true()
  )
  
  # Check that all_options works as expected
  expect_that(
    setequal(
      preference_1$all_options(),
      c("Sushi","Chinese","Turkish","Indian")),
    is_true()
  )
  
})

# expect_that(1 ^ 1, equals(1))
# expect_that(2 ^ 2, equals(4))
# 
# 
# expect_that(2 == 1, is_false())
# 
# expect_that(1, is_a('numeric'))
# 
# expect_that(print('Hello World!'), prints_text('Hello World!'))
# 
# expect_that(log('a'), throws_error())
# 
# expect_that(factorial(16), takes_less_than(1))


# Graph examles from the tutorial here

# library(igraph)
# g1 <- graph(edges=c(1,2, 2,3,3,1), n=3, directed = F) 
# plot(g1)
# 
# class(g1)
# g1
# 
# 
# g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=10 )
# 
# plot(g2)   
# 
# g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) # named vertices
# 
# # When the edge list has vertex names, the number of nodes is not needed
# 
# plot(g3)
# 
# g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
#              
#              isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  
# 
# # In named graphs we can specify isolates by providing a list of their names.
# 
# 
# 
# plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
#      
#      vertex.frame.color="gray", vertex.label.color="black", 
#      
#      vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 