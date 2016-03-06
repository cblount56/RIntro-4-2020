rankhospital <- function (stateTarget, outcomeTarget, num) {
##Read outcome data
outcome <<- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
##num <- num
names(outcome)[11] <- "Heart.Attack.Mortality" 
names(outcome)[17] <- "Heart.Failure.Mortality"
names(outcome)[23] <- "Pneumonia.Mortality" 
##
	STATES <- unique(outcome[ , 7])
	OUTCOME_TARGETS <- c("Heart.Attack", "Heart.Failure", "Pneumonia") 
	##REDUCED_DATA_COLS <- c("Hospital.Name", "City", "State", "Heart.Attack", "Heart.Failure", "Pneumonia")
##
##Check that stateTarget is one of states(50) or one of the territories(District of Columbia, Guam, Puerto Rico, Virgin Islands) is valid
##
		if(!(stateTarget %in% STATES)) 	{
			stop(paste('Error in best(',stateTarget,'): invalid State'))
								}
##
##Check the outcomeTarget is valid
##
		else if(!(outcomeTarget %in% OUTCOME_TARGETS)) 	{
			stop(paste('Error in best(',outcomeTarget,'): invalid Outcome'))
										} 
##> OUTCOME$State[1]  NOTE this command cuts a column from datafram and the index points to each member in the column, maybe useful for what we need.
##[1] "AL"
##paste(cbind(OUTCOME$State,OUTCOME$Heart.Attack)) produces vector of States and then vector of Heart.Attack Mortality numbers
##Return hospital name in that state with the lowest 30-day death 
		else	{	
			OUTCOME1 <- na.omit(subset(outcome[c(2,6,7,11,17,23)]))
			##OUTCOME1 <- subset(outcome[c(2,6,7,11,17,23)])  use to examine impact of na.omit
			OUTCOME1 <- OUTCOME1[order(OUTCOME1$State, OUTCOME1$Hospital.Name) , ]
			OUTCOME <- subset(OUTCOME1, State == stateTarget, select=c(1,2,3,4,5,6))   ##NOW CHOOSE ONLY THE TARGET STATE DATA
			if ( outcomeTarget == "Heart.Attack"  && num != "worst")  {  OUTCOME <- OUTCOME[order(OUTCOME$Heart.Attack.Mortality) , ]}
			if ( outcomeTarget == "Heart.Attack"  && num == "worst"){  OUTCOME <- OUTCOME[order(OUTCOME$Heart.Attack.Mortality, decreasing=T) , ]}
			if ( outcomeTarget == "Heart.Failure"  && num != "worst")    {  OUTCOME <- OUTCOME[order(OUTCOME$Heart.Failure.Mortality) , ]}
			if ( outcomeTarget == "Heart.Failure"  && num == "worst")    {  OUTCOME <- OUTCOME[order(OUTCOME$Heart.Failure.Mortality, decreasing=T) , ]}
			if ( outcomeTarget == "Pneumonia" && num != "worst" )  {  OUTCOME <- OUTCOME[order(OUTCOME$Pneumonia.Mortality) , ]}
			if ( outcomeTarget == "Pneumonia" && num == "worst" )  {  OUTCOME <- OUTCOME[order(OUTCOME$Pneumonia.Mortality, decreasing=T) , ]}
			}
##head(OUTCOME,1)  this allows you to look at all data for one row in the dataframe.
	if(num == "worst" || num == "best") {num = 1 }  ##this handles the cases were input for num is either "worst" or "best".  Note - this impacts sort command and index of outcome to print to screen.
OUTCOME[num,1]
##head(OUTCOME, 100)   used to examine the actual rows of data being processed.
}
##rankhospital("TX", "Heart.Failure", 4)
