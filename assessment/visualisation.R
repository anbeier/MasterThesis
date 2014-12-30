plot_bucketize_age <- function(census) {

  image = ggplot(census, aes(x=age)) + 
    geom_histogram(aes(fill = ..count..)) +  
    scale_fill_gradient("Count", low = "grey", high = "black") +
    ylab('Count') +
    xlab('Age')
  
  ggsave(file="census_age.svg", plot=last_plot(), width=10, height=8)
}

plot_bucketize_wageperhour <- function(census) {
  census$x = round(as.numeric(census$wageperhour) / 100, 2)
  image = ggplot(census, aes(x=x)) + 
    geom_histogram(aes(fill = ..count..)) +  
    scale_y_sqrt() + 
    scale_fill_gradient("Square Root of Count", low = "grey48", high = "black") +
    ylab('Square Root of Count') +
    xlab('Wage per Hour')
  
  ggsave(file="census_wageperhour.svg", plot=last_plot(), width=10, height=8)
}
