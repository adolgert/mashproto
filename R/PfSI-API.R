################################################################################
# prototyping a queue
################################################################################

# make a queue for simple continuous-time models
# this means that the events change state all at once
# no state "blips", also no enter and exit functions; it's a pure expression of the math;
# that's why its called simpleCT (simple continuous time)
make_queue_simpleCT <- function(){
  
}


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
