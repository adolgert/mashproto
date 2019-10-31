################################################################################
# prototyping a queue
################################################################################

library(data.table)
rm(list=ls());gc()

# rules: we don't delete rows, we just set them to null events
# we really don't want to append rows.

# make a queue for simple continuous-time models
# this means that the events change state all at once
# no state "blips", also no enter and exit functions; it's a pure expression of the math;
# that's why its called simpleCT (simple continuous time)

# makes the event queue
make_queue_simpleCT <- function(max=100L){
  qenv <- new.env(hash = FALSE,size = 1L)
  assign(x = "q",
         value = data.table::data.table(
           tEvent = rep(1e6,max),
           tag = rep("null",max),
           data = list(list())
         ),envir = qenv,
         inherits = FALSE)
  return(qenv)
}

# addEvent2Q function
addEvent2Q.simpleCT <- function(qenv,tEvent,tag,data = list(list())){
  stopifnot(class(qenv$q)[1]=="data.table",is.double(tEvent),is.character(tag))
  
  # try to insert into a null row; append rows otherwise
  if(qenv$q[,any(tag=="null")]){
    # assigns to existing null row in place such that no new memory allocation/copying is triggered
    data.table::set(x = qenv$q,i = qenv$q[tag=="null",which=TRUE][1],j = 1:3,value = list(tEvent,tag,data))
  } else {
    # if we are forced to, we append rows to the data.table
    qenv$q <- rbind(qenv$q,data.table::data.table(tEvent=tEvent,tag=tag,data=data))
  }
  # sort according to increasing firing times in place
  data.table::setorder(qenv$q,tEvent)
}

queue <- make_queue_simpleCT(max = 3)



queue$q
addEvent2Q.simpleCT(qenv = queue,tEvent = 3,tag = "test1")
addEvent2Q.simpleCT(qenv = queue,tEvent = 1.323,tag = "test2")
addEvent2Q.simpleCT(qenv = queue,tEvent = 3,tag = "test3",data = list(list(a=5,b=232.2,c="hello, there")))

# this guy should force a copy
addEvent2Q.simpleCT(qenv = queue,tEvent = 5.23,tag = "test4",data = list(list(alpha=complex(1))))




# test code

# max = 10
# q <- data.table::data.table(
#   tEvent = rep(1e6,max),
#   tag = rep("null",max),
#   data = list(list())
# )
# # tracemem(q)
# 
# # .Internal(inspect(q))
# 
# data.table::set(x = q,i = q[tag=="null",which=TRUE][1],j = 1:3,value = list(5,"test1",list(list(b=5,c=32))))
# data.table::set(x = q,i = q[tag=="null",which=TRUE][1],j = 1:3,value = list(7,"test1",list(list(alpha=323))))
# data.table::setorder(q,tEvent)
# 
# # untracemem(q)
# # .Internal(inspect(q))
# 
# # q[tag == "null"][1] <- data.table(tEvent = 5,tag = "test1",data = list(list(b=5,c=32)))
# # q[tag == "null"][1] <- data.table(tEvent = 2,tag = "test2",data = list(list(alpha=2312)))
# 
# 
# null_event <- data.table::data.table(tEvent = .Machine$double.xmax,tag = "null",data = vector("list",1))
# 
# 
# 
# # both these assignments work
# q[q[tag=="null",which=TRUE][1],c("tEvent","tag","data") := list(3,"test1",vector("list",1))]
# data.table::set(x = q,i = q[tag=="null",which=TRUE][1],j = 1:3,value = list(3,"test1",vector("list",1)))
# 
# data.table::setorder(q,tEvent)
# 
# 
# addEvent2Q.simpleCT <- function(q,tEvent,tag,data = list(list())){
#   # might be unnecessary
#   stopifnot(class(q)[1]=="data.table",is.double(tEvent),is.character(tag))
#   # insert to blank space
#   if(q[,any(tag=="null")]){
#     # q[tag == "null"][1] <- event
#     data.table::set(x = q,i = q[tag=="null",which=TRUE][1],j = 1:3,value = list(tEvent,tag,data))
#     # need to append rows
#   } else {
#     stop("DT is full\n")
#     # we might be able to use a join here rather than rbind for complete in-place modification. see section 3.5 of https://franknarf1.github.io/r-tutorial/_book/tables.html
#     # q <- rbind(q,event)
#   }
#   data.table::setorder(q,tEvent)
# }
# 
# 
# addEvent2Q.simpleCT(q = q,tEvent = 3,tag = "test1")





################################################################################
# prototyping PfSI
################################################################################

# this guy is generic, gets the mosy -> human transmission prob for this person
get_b <- function(x) {
  UseMethod("get_b")
}

# this guy is generic, gets the human -> mosy transmission prob for this person
get_c <- function(x) {
  UseMethod("get_c")
}

# this returns the pfsi CoI (course of infection) object
# state: (S,I,P)
# b: (0,1)
# c: (0,1)
make_pfsi_CoI <- function(state = character(),b = double(), c = double()){
  stopifnot(is.character(state),is.double(b),is.double(c))
  x <- list()
  x$state <- state
  x$b <- b
  x$c <- c
  x$queue <- queue()
  class(x) <- "pfsi_CoI"
}
