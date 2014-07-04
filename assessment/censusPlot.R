# aggregate.plot
# median of worked weeks aggregated by hispanic origin and grouped by education
worked.weeks <- qs$weeks_worked_in_year.cat
aggregate.plot(x = worked.weeks, by = list(Hispanic = qs$hispanic_origin.cat, Education = qs$education.cat), 
               FUN = "median", ylim = c(0, 6), bar.col = c("tomato", "tan1", "yellow", "lawngreen", "lightskyblue", 
                                                           "hotpink3", "slategray", "plum2", "lightsalmon4"), legend = FALSE)
legend(x = 50, y = 6, legend = c("All other", "Central or South American", "Chicano", "Cuban", "Do not Know", 
                                 "Mexican-American", "Mexican (Mexicano)", "Other Spanish", "Puerto Rican"), 
       fill = c("tomato", "tan1", "yellow", "lawngreen", "lightskyblue", "hotpink3", "slategray", "plum2", "lightsalmon4"))

