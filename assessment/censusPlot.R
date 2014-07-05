# epicalc package
# aggregate.plot
# median of worked weeks aggregated by education
worked.weeks <- qs$weeks_worked_in_year.cat
aggregate.plot(x = worked.weeks, by = list(Education = qs$education.cat), FUN = "median")
aggregate.plot(x = qs$weeks_worked_in_year.cat, by = qs$education.cat, grouping = qs$hispanic_origin.cat,  FUN = "median")
points(qs$education.cat[qs$hispanic_origin.cat == 6], qs$weeks_worked_in_year.cat[qs$hispanic_origin == 6], col = "blue")

# median of worked weeks aggregated by hispanic origin, grouped by education
worked.weeks <- qs$weeks_worked_in_year.cat
aggregate.plot(x = worked.weeks, by = list(Hispanic = qs$hispanic_origin.cat, Education = qs$education.cat), 
               FUN = "median", ylim = c(0, 6), bar.col = c("tomato", "tan1", "yellow", "lawngreen", "lightskyblue", 
                                                           "hotpink3", "slategray", "plum2", "lightsalmon4"), legend = FALSE)
legend(x = 50, y = 6, legend = c("All other", "Central or South American", "Chicano", "Cuban", "Do not Know", 
                                 "Mexican-American", "Mexican (Mexicano)", "Other Spanish", "Puerto Rican"), 
       fill = c("tomato", "tan1", "yellow", "lawngreen", "lightskyblue", "hotpink3", "slategray", "plum2", "lightsalmon4"))

# median of education aggregated by migration change, grouped by hispanic origin
education <- qs$education.cat
aggregate.plot(x = education, by = list(Migration.Change = qs$migration_code_change_in_msa.cat, Hispanic = 
                                          qs$hispanic_origin.cat), FUN = "median", ylim = c(0, 18), bar.col = 
                 c("salmon", "wheat", "paleturquoise3", "pink2", "slategray3", "khaki", "lightcoral", "seagreen3", 
                   "khaki4"), legend = FALSE)
legend(8, 16, legend = c("Abroad to MSA", "Abroad to nonMSA", "MSA to MSA", "MSA to nonMSA", "Nonmover", 
                                 "NonMSA to MSA", "NonMSA to nonMSA", "Not identifiable", "Not in universe"), 
       fill = c("salmon", "wheat", "paleturquoise3", "pink2", "slategray3", "khaki", "lightcoral", "seagreen3", "khaki4"))



## This is a VERSUCH ##
niceCensus = function() {
  df <- census
  names(df) <- gsub(" ", ".", names(df))
  df
}

processAndAggregateEducationDataForOriginGroup = function(groupName) {
  dataf <- niceCensus()
  df = subset(dataf, dataf[, "hispanic.origin"] == groupName)
  df <- makeEducationCat(df)
  aggregate(weeks.worked.in.year ~ education.cat, df, FUN=mean)
  aggregate(education.cat ~ age, df, FUN=median)
}

plotEducationAndWorkAggregates = function(data, l=FALSE, col="blue") {
  if (l == FALSE) {
    plot(x=data$age, y=data$education.cat, type="l", col=col)
  } else {
    lines(x=data$age, y=data$education.cat, col=col)
  }
}

plotThem = function() {
  plotGroup <- function(name, notFirst=FALSE, col="blue") {
    plotEducationAndWorkAggregates(processAndAggregateEducationDataForOriginGroup(name), notFirst, col)
  }
  
  plotGroup('Mexican-American')
  plotGroup('Mexican (Mexicano)', TRUE, "red")
  plotGroup('Puerto Rican', TRUE, "green")
  plotGroup('Central or South American', TRUE, "purple")
  plotGroup('Chicano', TRUE, "black")
}

plot(df$education, df$weeks_worked_in_year.cat, pch = 2, cex = 2)
points(df2$education, df2$weeks_worked_in_year.cat, pch = 6, cex = 2)
