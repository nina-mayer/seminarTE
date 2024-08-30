library(dplyr)
library(ggplot2)
library(RColorBrewer)

rm(list = ls())

### load data set

teachers_full <- get(load("./teachers.RData"))





### bring data set in desired shape

teachers_treat <- distinct(data.frame(teachers_full$State, teachers_full$YearCBrequired))

# only consider states that adopted treatment

teachers_treat <- na.omit(teachers_treat)

# as treatment adoption times are the same for some states, use adoption time in
# months -> provided by Paglayan (2019) in Appendix B
# drop "DC" as no additional info given and "WI" because not enough data before

teachers_treat <- teachers_treat[-c(4,34),]

treat_times <- c("06/1970", "07/1976", "06/1965", "11/1969", "01/1975", "07/1970", 
                 "07/1975", "07/1971", "01/1984", "07/1973", "05/1971", "11/1965",
                 "07/1969", "10/1969", "07/1966", "07/1972", "08/1971", "03/1969",
                 "04/1987", "08/1975", "07/1968", "04/1969", "09/1967", "04/1984",
                 "06/1971", "08/1969", "10/1970", "05/1966", "05/1970", "03/1978",
                 "07/1967", "08/1968")

# check for duplicated adoption times
sum(duplicated(treat_times)) # 0 -> no duplicated times

teachers_treat <- data.frame(state = teachers_treat$teachers_full.State, time = treat_times)





### update teachers_full

# delete all states that are not in teachers_treat
# delete years < 1959 as no info on teacher salary
# delete all columns correlated to treatment time
# delete all columns with (almost) only NAs

teachers_full <- teachers_full %>%
  filter(State %in% teachers_treat$state) %>%
  filter(year >= 1959) %>%
  select(-c(Stateid, YearCBrequired, CBstatusby1990, CBrequired_SY, CBeverrequired, idmap,
            ppexpend, lnppexpend, lnavgteachsal, lnavginstrucsal, lnnonwageppexpend,
            agr, perinc, pnwht, purban, ESWI)) %>%
  mutate(across(avginstrucsal, ~replace(., is.na(.), mean(., na.rm = TRUE))), 
         .by = State) %>%
  relocate(State)





### plot average teacher salary (figure 2)

# filter states for clarity: CT(first adopter), NE(last adopter), AK, FL, CA, NY, OH, WA
sel_states <- c("CT", "NE", "AK", "FL", "CA", "NY", "OH", "WA")
data_plot <- teachers_full %>% 
  filter(State %in% sel_states) %>%
  select(c(State, year, avgteachsal))

# add average of all states
avg_all <- teachers_full %>% 
  group_by(year) %>%
  summarise(avgteachsal = mean(avgteachsal))
all <- rep("All States", times = 42)
avg_all <- cbind(avg_all, State = all)
data_plot <- rbind(data_plot, avg_all)

# some more preparations
data_plot$State <- factor(data_plot$State, levels = c("All States", "AK", "CA", "CT", "FL", "NE", "NY", "OH", "WA"))
colors <- c("red3", brewer.pal(8, name = "Blues"))
my_theme <- theme(
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 16),
  plot.title = element_text(size = 22),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 16))

# plot
jpeg("avgteachsal.jpg", units = "in", width = 14, height = 8, res = 800)
avgteachsal <- ggplot(data = data_plot, mapping = aes(x = year, y = avgteachsal, group = State, color = State)) +
  geom_line(linewidth = 1) + theme_bw() + scale_color_manual(values = colors) +
  labs(y = "Average Teacher Salary", x = "Year", title = "Average Teacher Salary by Year") +
  my_theme
dev.off()

avgteachsal
