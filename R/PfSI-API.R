################################################################################
# prototyping a queue
################################################################################



# make a queue for simple continuous-time models
# this means that the events change state all at once
# no state "blips", also no enter and exit functions; it's a pure expression of the math;
# that's why its called simpleCT (simple continuous time)
make_queue_simpleCT <- function(max=100L){
  q <- data.table::data.table(
    tEvent = rep(.Machine$double.xmax,max),
    tag = rep("null",max),
    data = rep(vector("list",1),max)
  )
}

max = 10
q <- data.table::data.table(
  tEvent = rep(.Machine$double.xmax,max),
  tag = rep("null",max),
  data = rep(vector("list",1),max)
)


q[tag == "null"][1] <- data.table(tEvent = 5,tag = "test1",data = list(list(b=5,c=32)))
q[tag == "null"][1] <- data.table(tEvent = 2,tag = "test2",data = list(list(alpha=2312)))


# both these assignments work
q[q[tag=="null",which=TRUE][1],c("tEvent","tag","data") := list(3,"test1",vector("list",1))]
data.table::set(x = q,i = q[tag=="null",which=TRUE][1],j = 1:3,value = list(3,"test1",vector("list",1)))

data.table::setorder(q,tEvent)


addEvent2Q.simpleCT <- function(q,tEvent,tag,data = list()){
  # might be unnecessary
  stopifnot(class(event)[1]=="data.table")
  # insert to blank space
  if(q[,any(tag=="null")]){
    # q[tag == "null"][1] <- event
    data.table::set(x = q,i = ,j = )
    # need to append rows
  } else {
    # we might be able to use a join here rather than rbind for complete in-place modification. see section 3.5 of https://franknarf1.github.io/r-tutorial/_book/tables.html
    q <- rbind(q,event)
  }
  data.table::setorder(q,tEvent)
}


addEvent2Q.simpleCT(q = q,event = data.table(tEvent = 3,tag = "test1",data = list()))



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
