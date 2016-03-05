best <- function(stateTarget, outcomeTarget)	{
##Read outcome data
##stateTarget <<- stateTarget
##outcomeTarget <<- outcomeTarget
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
i <- NULL
columnTarget <- NULL
names(outcome)[11] <- "Heart.Attack.Mortality" 
names(outcome)[17] <- "Heart.Failure.Mortality"
names(outcome)[23] <- "Pneumonia.Mortality" 
##y <- NULL
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
##paste(cbind(OUTCOME$State,OUTCOME$Heart.Attack)) produces vector of States and then vector of H.eart.Attack Mortality numbers
##Return hospital name in that state with the lowest 30-day death 
		else	{	
			OUTCOME1 <- na.omit(subset(outcome[c(2,6,7,11,17,23)]))
			OUTCOME <- subset(OUTCOME1, State == stateTarget, select=c(1,2,3,4,5,6))   ##NOW CHOOSE ONLY THE TARGET STATE DATA
			if ( outcomeTarget == "Heart.Attack"  )  {  OUTCOME <- OUTCOME[order(OUTCOME$Heart.Attack.Mortality,OUTCOME$Hospital.Name) , ]}
			if ( outcomeTarget == "Heart.Failure"  )  {  OUTCOME <- OUTCOME[order(OUTCOME$Heart.Failure.Mortality,OUTCOME$Hospital.Name) , ]}
			if ( outcomeTarget == "Pneumonia"  )  {  OUTCOME <- OUTCOME[order(OUTCOME$Pneumonia.Mortality,OUTCOME$Hospital.Name) , ]}
			}
##head(OUTCOME,1)  this allows you to look at all data for one row in the dataframe.
OUTCOME[1,1]
}

##sample output confirmed
##testbest("TX", "Heart.Failure")
##> best("TX", "Heart.Attack")
##[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
##> best("TX", "Heart.Failure")
##[1] "HARRIS COUNTY HOSPITAL DISTRICT"  this does not agree with sample in assignment, will "debug" if time permits
##> best("MD", "Heart.Attack")
##[1] "JOHNS HOPKINS HOSPITAL, THE"
##> best("MD", "Pneumonia")
##[1] "CALVERT MEMORIAL HOSPITAL"
##> best("BB", "Heart.Attack")
##Error in best("BB", "Heart.Attack") : Error in best( BB ): invalid State
##> best("NY", "Hert.Attack")
##Error in best("NY", "Hert.Attack") : 
##  Error in best( Hert.Attack ): invalid Outcome
##> 
