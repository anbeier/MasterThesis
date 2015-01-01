
plot_origin_age <- function(census) {

  image = ggplot(census, aes(x=age)) + 
    geom_histogram(aes(fill = ..count..)) +  
    scale_fill_gradient("Count", low = "grey", high = "black") +
    ylab('Count') +
    xlab('Age') +
    theme(text = element_text(size=20))
  
  ggsave(file="census_age.svg", plot=last_plot(), width=10, height=5)
}

plot_origin_wageperhour <- function(census) {
  census$x = round(as.numeric(census$wageperhour) / 100, 2)
  image = ggplot(census, aes(x=x)) + 
    geom_histogram(aes(fill = ..count..)) +  
    scale_y_sqrt() + 
    scale_fill_gradient("Count", low = "grey48", high = "black") +
    ylab('Count') +
    xlab('Wage per Hour') +
    theme(text = element_text(size=20))
  
  ggsave(file="census_wageperhour.svg", plot=last_plot(), width=10, height=5)
}

plot_bucketized_age <- function(census_processed) {
  # modify values of age
  census_processed$dat = gsub('i', 'age', gsub('age','',census_processed$age))
  census_processed$dat = factor(census_processed$dat, levels = c('-4.1<=age<3',
                               '3<=age<10.1',
                               '10.1<=age<17.2',
                               '17.2<=age<24.3',
                               '24.3<=age<31.4',
                               '31.4<=age<38.5',
                               '38.5<=age<45.6',
                               '45.6<=age<52.7',
                               '52.7<=age<59.8',
                               '59.8<=age<66.9',
                               '66.9<=age<74',
                               '74<=age<81.1',
                               '81.1<=age<88.2',
                               '88.2<=age<95.3'))
  
  image = ggplot(census_processed, aes(x=dat)) + 
    geom_histogram(aes(fill = ..count..)) +  
    scale_fill_gradient("Count", low = "grey58", high = "black") +
    ylab('Count') +
    xlab('') +
    theme(text = element_text(size=20),
          axis.text.x  = element_text(angle=45, vjust=0.5, size=15))
  
  ggsave(file="census_age_bucketized.svg", plot=last_plot(), width=10, height=5)
}

plot_bucketized_wageperhour <- function(census_processed) {
  # modify wage per hour
  census_processed$dat = gsub('i', 'WPH', gsub('wageperhour','',census_processed$wageperhour))
  census_processed$dat[census_processed$dat=='0<=WPH<50'] = '0<=WPH<0.5'
  census_processed$dat[census_processed$dat=='450<=WPH<500'] = '4.5<=WPH<5'
  census_processed$dat[census_processed$dat=='650<=WPH<700'] = '6.5<=WPH<7'
  census_processed$dat[census_processed$dat=='>1400'] = 'WPH>14'
  census_processed$dat[census_processed$dat=='800<=WPH<850'] = '8<=WPH<8.5'
  census_processed$dat[census_processed$dat=='500<=WPH<550'] = '5<=WPH<5.5'
  census_processed$dat[census_processed$dat=='600<=WPH<650'] = '6<=WPH<6.5'
  census_processed$dat[census_processed$dat=='400<=WPH<450'] = '4<=WPH<4.5'
  census_processed$dat[census_processed$dat=='850<=WPH<900'] = '8.5<=WPH<9'
  census_processed$dat[census_processed$dat=='900<=WPH<950'] = '9<=WPH<9.5'
  census_processed$dat[census_processed$dat=='750<=WPH<800'] = '7.5<=WPH<8'
  census_processed$dat[census_processed$dat=='550<=WPH<600'] = '5.5<=WPH<6'
  census_processed$dat[census_processed$dat=='200<=WPH<250'] = '2<=WPH<2.5'
  census_processed$dat[census_processed$dat=='300<=WPH<350'] = '3<=WPH<3.5'
  census_processed$dat[census_processed$dat=='250<=WPH<300'] = '2.5<=WPH<3'
  census_processed$dat[census_processed$dat=='100<=WPH<150'] = '1<=WPH<1.5'
  census_processed$dat[census_processed$dat=='150<=WPH<200'] = '1.5<=WPH<2'
  census_processed$dat[census_processed$dat=='350<=WPH<400'] = '3.5<=WPH<4'
  census_processed$dat[census_processed$dat=='50<=WPH<100'] = '0.5<=WPH<1'
  census_processed$dat[census_processed$dat=='700<=WPH<750'] = '7<=WPH<7.5'
  
  census_processed$dat = factor(census_processed$dat, levels = c('0<=WPH<0.5',
                               '0.5<=WPH<1',
                               '1<=WPH<1.5',
                               '1.5<=WPH<2',
                               '2<=WPH<2.5',
                               '2.5<=WPH<3',
                               '3<=WPH<3.5',
                               '3.5<=WPH<4',
                               '4<=WPH<4.5',
                               '4.5<=WPH<5',
                               '5<=WPH<5.5',
                               '5.5<=WPH<6',
                               '6<=WPH<6.5',
                               '6.5<=WPH<7',
                               '7<=WPH<7.5',
                               '7.5<=WPH<8',
                               '8<=WPH<8.5',
                               '8.5<=WPH<9',
                               '9<=WPH<9.5',
                               'WPH>14'))
  
    image = ggplot(census_processed, aes(x=dat)) + 
      geom_histogram(aes(fill = ..count..)) +  
      scale_y_sqrt() + 
      scale_fill_gradient("Count", low = "grey48", high = "black") +
      ylab('Count') +
      xlab('') +
      theme(text = element_text(size=20),
            axis.text.x  = element_text(angle=45, vjust=0.5, size=15))
  
  ggsave(file="census_wageperhour_bucketized.svg", plot=last_plot(), width=10, height=5)
}

plot_count_qualified <- function(clqList, delta) {
  fn.bayes = paste(paste('tpch_delta', delta, sep=''), '_alpha0.5/bayes/mcc/results.csv', sep= '')
  fn.svm = paste(paste('tpch_delta', delta, sep=''), '_alpha0.5/svm/mcc/results.csv', sep= '')
  
  # read result.csv
  b = read.csv(fn.bayes, header=T, sep=',')
  s = read.csv(fn.svm, header=T, sep=',')

  b = data.frame(name='MCC > 0.6', method='bayes', qs=unique(b$index))
  s = data.frame(name='MCC > 0.6', method='svm', qs=unique(s$index))
  df = rbind(b, s)
  
  b0 = data.frame(name='Discovered', method='bayes', qs=seq(1, length(clqList), by=1))
  s0 = data.frame(name='Discovered', method='svm', qs=seq(1, length(clqList), by=1))
  df0 = rbind(b0, s0)
  
  dat = rbind(df, df0)
  
  image = ggplot(dat, aes(x=method, fill = name)) +
    geom_histogram(position=position_dodge()) +
    scale_fill_manual("Quasi-clique", values=c('skyblue4','gray8')) +
    theme(text = element_text(size=18)) +
    xlab('Classification Method') +
    ylab('Number of Quasi-cliques')
  
  ggsave(file="count_qualified.svg", plot=last_plot(), width=10, height=5)
}
