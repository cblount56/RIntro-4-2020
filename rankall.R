rankalltest <- function (outcomeTarget, num) {
##Read outcome data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("Not Available"))
num <<- num
z <- NULL
names(outcome)[11] <- "Heart.Attack.Mortality" 
names(outcome)[17] <- "Heart.Failure.Mortality"
names(outcome)[23] <- "Pneumonia.Mortality" 
##
STATES <- unique(outcome[ , 7])
OUTCOME_TARGETS <- c("Heart.Attack", "Heart.Failure", "Pneumonia") 
##
##Check the outcomeTarget is valid
##
if(!(outcomeTarget %in% OUTCOME_TARGETS)) {
stop(paste('Error in best(',outcomeTarget,'): invalid Outcome'))
}
## new logic, just developed - 3/7/16 09:02 EST - Charles Blount, Jr.
##  need to replicate this for each target outcome (Heart Attack, Heart Failure and Pneumonia.
## STATES index to compare for State == "AL"  example
##myvars <- c("State", "Hospital.Name", "Heart.Attack.Mortality")
##newdata <- OUTCOME[myvars]
##newdata1 <- subset(newdata, State == "AL", select = c(State, Hospital.Name, Heart.Attack.Mortality))
##sort(newdata1$Heart.Attack.Mortality)
##z <- sort(newdata1$Heart.Attack.Mortality)
##STATES[1]    prints the state abbreviation
##z[5]  ## not required, but used to validate the values (high, low, i'th rank)
##z list thesorted list of rates.
##[1] "13.3" "14.2" "14.3" "14.5" "14.6" "14.7" "14.7" "14.9" "15.0" "15.0" "15.0" "15.2" "15.2" "15.2" "15.2" "15.3" "15.4" "15.6" "15.6" "15.7" "15.8" "15.8" "15.8" "15.9" "16.0" "16.1" "16.2"
##[28] "16.3" "16.4" "16.5" "16.6" "16.7" "16.7" "16.7" "16.8" "16.8" "16.9" "17.0" "17.1" "17.1" "17.1" "17.3" "17.3" "17.5" "17.7" "17.8" "18.0" "18.1" "18.5" "19.3" "19.6"
##y <- match(z, newdata1$Heart.Attack.Mortality) ##produces the index list of matches (rates:row number in newdata1
##[1] 12 NA 42  2 35 47 47  4  3  3  3 11 11 11 11 31 NA 33 33  1 14 14 14 40 43  6 44 18 46  7 30  9  9  9 17 17 38 51 10 10 10  8  8 27 29  5 NA 19 28 32 24
##newdata1[12,2] = the hospital name for the 12 row which contains the lowest mortality rate for Heart attacks in AL
##newdata1[12,1] = AL, newdata1[12,3] = 13.3
else	{
	OUTCOME1 <- na.omit(subset(outcome[c(2,6,7,11,17,23)]))
##	OUTCOME1 <- subset(outcome[c(2,6,7,11,17,23)])  ##use to examine impact of na.omit
	OUTCOME <<- OUTCOME1[order(OUTCOME1$State, OUTCOME1$Hospital.Name)  , ]
##
 	for (i in 1:54){
		if ( outcomeTarget == "Heart.Attack") { myvars <- c("State", "Hospital.Name", "Heart.Attack.Mortality")}
		if ( outcomeTarget == "Heart.Failure") { myvars <- c("State", "Hospital.Name", "Heart.Failure.Mortality")}
		if ( outcomeTarget == "Pneumonia") { myvars <- c("State", "Hospital.Name", "Pneumonia.Mortality")}
			newdata <- OUTCOME[myvars]
		if ( outcomeTarget == "Heart.Attack"){
			newdata1 <- subset(newdata, State == STATES[[i]], select = c(State, Hospital.Name, Heart.Attack.Mortality))
			z <- sort(newdata1$Heart.Attack.Mortality)
			y <- match(z, newdata1$Heart.Attack.Mortality) ##produces the index list of matches (rates:row number in newdata1
			writeLines(paste('Hospital Name ',newdata1[y[num],2],' State',newdata1[y[num],1],'Mortality Rate ', newdata1[y[num],3]))
								}  ##end of Heart Attack commands
		if ( outcomeTarget == "Heart.Failure") {
			newdata1 <- subset(newdata, State == STATES[i], select = c(State, Hospital.Name, Heart.Failure.Mortality))
			z <- sort(newdata1$Heart.Failure.Mortality)
			y <- match(z, newdata1$Heart.Failure.Mortality) ##produces the index list of matches (rates:row number in newdata1
			writeLines(paste('Hospital Name ',newdata1[y[num],2],' State',newdata1[y[num],1],'Mortality Rate ', newdata1[y[num],3]))
									}  ##end of Heart Failure commands
		if ( outcomeTarget == "Pneumonia") {
			newdata1 <- subset(newdata, State == STATES[i], select = c(State, Hospital.Name, Pneumonia.Mortality))
			z <- sort(newdata1$Pneumonia.Mortality)
			y <- match(z, newdata1$Pneumonia.Mortality) ##produces the index list of matches (rates:row number in newdata1
			writeLines(paste('Hospital Name ',newdata1[y[num],2],' State',newdata1[y[num],1],'Mortality Rate ', newdata1[y[num],3]))
								}  ##end of Pneumonia commands
				}  ##  next STATE[i]
	}##end of ELSE after ensuring NO input errors
##
##
##some other logic to consider for loop control
##for (i in seq(along=good[,1])){##need to test nobs >= threshhold store data for corr calculation
##if(good[i,2] >= threshhold){
##head(z, 50)
	}
##rankalltest("Heart.Attack", 1)