plot_bucketize_age <- function(census) {

  image = ggplot(census, aes(x=age)) + 
    geom_histogram(aes(fill = ..count..)) +  
    scale_fill_gradient("Frequency", low = "grey", high = "black") +
    ylab('Frequency') +
    xlab('Age')
  
  ggsave(file="census_age.svg", plot=last_plot(), width=10, height=8)
}

plot_bucketize_wageperhour <- function(census) {
  image = ggplot(census, aes(x=wageperhour)) + 
    geom_histogram(aes(fill = ..count..)) +  
    scale_fill_gradient("Frequency", low = "grey", high = "black") +
    ylab('Frequency') +
    xlab('Wage per Hour')
  
  ggsave(file="census_wageperhour.svg", plot=last_plot(), width=10, height=8)
}
