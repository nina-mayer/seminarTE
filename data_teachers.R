library(dplyr)


### load data set

teachers_full <- get(load("./teachers.RData"))

### bring data set in desired shape

teachers_treat <- distinct(data.frame(teachers_full$State, teachers_full$YearCBrequired))

# only consider states that adopted treatment

teachers_treat <- na.omit(teachers_treat)

# as treatment adoption times are the same for some states, use adoption time in
# months -> provided by Paglayan (2019) in Appendix B
# drop "DC" as no additional info given

teachers_treat <- teachers_treat[-c(4),]

treat_times <- c("06/1970", "07/1976", "06/1965", "11/1969", "01/1975", "07/1970", 
                 "07/1975", "07/1971", "01/1984", "07/1973", "05/1971", "11/1965",
                 "07/1969", "10/1969", "07/1966", "07/1972", "08/1971", "03/1969",
                 "04/1987", "08/1975", "07/1968", "04/1969", "09/1967", "04/1984",
                 "06/1971", "08/1969", "10/1970", "05/1966", "05/1970", "03/1978",
                 "07/1967", "08/1968", "10/1959")

# check for duplicated adoption times
sum(duplicated(treat_times)) # 0 -> no duplicated times

teachers_treat <- data.frame(state = teachers_treat$teachers_full.State, time = treat_times)
