#Number 9 and 10
state <- c("MO","KS","KS","KS","MO","NE","OK","MO","KS","NE","NE","OK","MO","KS","KS")
state_factor <- factor(state)
state_factor
str(state_factor)#shows the levels numerically easier to see
state_factor2 <- factor(state, levels = c("OK","KS","MO","NE"))#switching the order of the state categories
state_factor2
str(state_factor2)#you can see the difference from the orginal state_factor and shows the reordering
prod <- c(2341, 9873, 23933, 1999, 7214, 9089, 10999, 2389, 5435, 5757, 2521, 7333, 21909, 15110, 4369)
tapply(prod, state, mean)#mean of production for each state
tapply(prod, state, max)#max of production for each state
tapply(prod, state, min)#min of production for each state
