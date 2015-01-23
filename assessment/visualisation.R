
plot_origin_age <- function(census) {

  image = ggplot(census, aes(x=age)) + 
    geom_histogram(aes(fill = ..count..), binwidth = 1) +  
    scale_fill_gradient("Count", low = "grey", high = "black") +
    ylab('Count') +
    xlab('Age') +
    theme(text = element_text(size=20))
  
  ggsave(file="census_age.svg", plot=last_plot(), width=10, height=5)
}

plot_origin_wageperhour <- function(census) {
  census$x = round(as.numeric(census$wageperhour) / 100, 2)
  image = ggplot(census, aes(x=x)) + 
    geom_histogram(aes(fill = ..count..), binwidth = 1) +  
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

plot_count_qualified <- function(delta, fqsFile) {
  cliques = readTPCHCliques(fqsFile)
  #cliques = readingQuasiCliques(fqsFile) 
  
  x = 'tpch_delta'
  fn.bayes = paste(paste(x, delta, sep=''), '_alpha0.5_corrected/bayes/mcc/results.csv', sep= '')
  fn.svm = paste(paste(x, delta, sep=''), '_alpha0.5_corrected/svm/mcc/results.csv', sep= '')
  
  # read result.csv
  b = read.csv(fn.bayes, header=T, sep=',')
  s = read.csv(fn.svm, header=T, sep=',')

  b = data.frame(method='bayes', name='MCC > 0.6', qs=length(unique(b$index)), name='total', qs=length(cliques))
  s = data.frame(method='svm', name='MCC > 0.6', qs=length(unique(s$index)), name='total', qs=length(cliques))
  rbind(b, s)
}

countRows <- function(x) {
  length(unique(x))
}

countColsByClique <- function(d) {
  aggregate(d$target, by=list(d$index), FUN=countRows)
}

avgMCCByClique <- function(d) {
  d$mcc = as.numeric(as.character(d$mcc))
  aggregate(d$mcc, by=list(d$index), FUN=mean)
}

makeLevels <- function(inds) {
  inds = sort(as.integer(inds))
  paste('QS', inds, sep='_')
}

addNotInCols <- function(d1, d2) {
  # add cliques in d1 but not in d2 to d2
  toadd = setdiff(unique(d1$index), unique(d2$index))
  for(x in toadd) {
    d2[nrow(d2) + 1,] = c(x, 0, NA, d2$method[1])
  }
  return(d2)
}

findQSHavingSameNCols <- function(df) {
  isSameNCols <- function(x) {
    if(x[1] == x[2]) {
      return(TRUE)
    }
    return(FALSE)
  }
    
  agg = aggregate(df$ncol, by=list(df$index), FUN=isSameNCols)
  agg$Group.1[agg$x == TRUE]
}

readMCCResults <- function(dataName, delta) {
  prefix = paste(dataName, "delta", sep='_')
  fn.bayes = ""
  fn.svm = ""
  if(dataName=='tpch') {
    fn.bayes = paste(paste(prefix, delta, sep=''), '_alpha0.5_corrected/bayes/mcc/results.csv', sep= '')
    fn.svm = paste(paste(prefix, delta, sep=''), '_alpha0.5_corrected/svm/mcc/results.csv', sep= '')
  } else if(dataName=='census') {
    fn.bayes = paste(paste(prefix, delta, sep=''), '_alpha0.5/bayes/mcc/results.csv', sep= '')
    fn.svm = paste(paste(prefix, delta, sep=''), '_alpha0.5/svm/mcc/results.csv', sep= '')
  }

  # read result.csv
  b = read.csv(fn.bayes, header=T, sep=',')
  s = read.csv(fn.svm, header=T, sep=',')
  
  list(bayes=b, svm=s)
}

adjustCliquesFromBothClassifiers <- function(d1, d2) {
  b.agg = addNotInCols(d2, d1)
  s.agg = addNotInCols(d1, d2)
  rbind(b.agg, s.agg)
}

modifyDataTypeForPlot <- function(dat) {
  dat$qs = factor(paste('QS', dat$index, sep='_'), levels=makeLevels(unique(dat$index)))
  dat$mcc = round(as.numeric(dat$mcc), 4)
  dat$ncol = as.integer(dat$ncol)
  return(dat)
}

determineYMin <- function(x) {
  if(is.na(match(0, as.integer(x)))) {
    return(1)
  } else {
    return(0)
  }
}

plot_MCCResults <- function(method, delta, yminMCC=0.9) {
  ls = readMCCResults(method, delta)
  
  b.agg = merge(countColsByClique(ls$bayes), avgMCCByClique(ls$bayes), by='Group.1')
  names(b.agg) = c('index', 'ncol', 'mcc')
  b.agg$method = 'bayes'
  s.agg = merge(countColsByClique(ls$svm), avgMCCByClique(ls$svm), by='Group.1')
  names(s.agg) = c('index', 'ncol', 'mcc')
  s.agg$method = 'svm'
  
  dat = adjustCliquesFromBothClassifiers(b.agg, s.agg)
  dat = modifyDataTypeForPlot(dat)
  
  # determine y axis for number of target columns
  #ymin = determineYMin(dat$ncol)
  
  image = ggplot(dat, aes(x=qs, y=ncol, fill = method)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(name="Classifier", values=c('midnightblue','palevioletred')) +
    coord_cartesian(ylim=seq(0, as.integer(max(dat$ncol)), by=1)) +
    theme(text = element_text(size=16), 
          #legend.position='none',
          legend.title=element_blank(),
          axis.text.x  = element_text(angle=45, vjust=0.5, size=13)) +
    xlab('Discovered Qualified Quasi-cliques') +
    ylab('Number of Identified Target Column')
  
  ggsave(file="target_cols.svg", plot=last_plot(), width=6, height=5)
  
  
  # extract quasi-cliques having same identified target columns from diff model types
  dat = subset(dat, dat$index %in% findQSHavingSameNCols(dat))
  
  image = ggplot(dat, aes(x=qs, y=mcc, fill = method)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(name="Classifier", values=c('midnightblue','palevioletred')) +
    coord_cartesian(ylim=seq(yminMCC, 1, by=0.1)) +
    theme(text = element_text(size=16), 
          #legend.position='none',
          legend.title=element_blank(),
          axis.text.x  = element_text(angle=45, vjust=0.5, size=13)) +
    xlab('Discovered Qualified Quasi-cliques') +
    ylab('Mean MCC Value of Target Columns')
  
  ggsave(file="target_mcc.svg", plot=last_plot(), width=6, height=5)
}

plot_TargetCols_census07 <- function(delta) {
  ls = readMCCResults("census", delta)
  
  b.agg = merge(countColsByClique(ls$bayes), avgMCCByClique(ls$bayes), by='Group.1')
  names(b.agg) = c('index', 'ncol', 'mcc')
  b.agg$method = 'bayes'
  s.agg = merge(countColsByClique(ls$svm), avgMCCByClique(ls$svm), by='Group.1')
  names(s.agg) = c('index', 'ncol', 'mcc')
  s.agg$method = 'svm'
  
  dat = adjustCliquesFromBothClassifiers(b.agg, s.agg)
  dat = modifyDataTypeForPlot(dat)
  
  # separate dat into 2 subsets
  dat = dat[order(as.integer(dat$index)),]
  dat1 = head(dat, 64)
  dat2 = dat[!dat$index %in% dat1$index,]
  
  image1 = ggplot(dat1, aes(x=qs, y=ncol, fill = method)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(name="Classifier", values=c('midnightblue','palevioletred')) +
    coord_cartesian(ylim=seq(0, as.integer(max(dat$ncol)), by=1)) +
    theme(text = element_text(size=16), 
          legend.position='none',
          legend.title=element_blank(),
          axis.text.x  = element_text(angle=90, vjust=0.5, size=12)) +
    xlab('Discovered Qualified Quasi-cliques') +
    ylab('Number of Identified Target Column')
  
  image2 = ggplot(dat2, aes(x=qs, y=ncol, fill = method)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(name="Classifier", values=c('midnightblue','palevioletred')) +
    coord_cartesian(ylim=seq(0, as.integer(max(dat$ncol)), by=1)) +
    theme(text = element_text(size=16), 
          legend.position='none',
          legend.title=element_blank(),
          axis.text.x  = element_text(angle=90, vjust=0.5, size=12)) +
    xlab('Discovered Qualified Quasi-cliques') +
    ylab('Number of Identified Target Column')
  
  image = grid.arrange(image1, image2, ncol=2)
  
  dat = subset(dat, dat$index %in% findQSHavingSameNCols(dat))
  
  image = ggplot(dat, aes(x=qs, y=mcc, fill = method)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(name="Classifier", values=c('midnightblue','palevioletred')) +
    coord_cartesian(ylim=seq(0.6, 1, by=0.1)) +
    theme(text = element_text(size=16), 
          legend.position='none',
          legend.title=element_blank(),
          axis.text.x  = element_text(angle=90, vjust=0.5, size=13)) +
    xlab('Discovered Qualified Quasi-cliques') +
    ylab('Mean MCC Value of Target Columns')
  
  ggsave(file="target_mcc.svg", plot=last_plot(), width=6, height=5)
}

plot_3d <- function(df) {
  p = ggplot(df,aes(migrationcodechangeinreg, stateofpreviousresidence,
                    shape = migrationcodemovewithinreg)) +
    scale_shape_manual(name='migration code move within reg', 
                       values = 0:length(unique(df$migrationcodemovewithinreg))) +
    geom_point(size=6) +
    xlab('state of previous residence') +
    ylab('migration code change in reg') +
    theme(text = element_text(size=16), 
          axis.text.x  = element_text(angle=45, vjust=0.5, size=13))
  
  ggsave(file="3d.svg", plot=last_plot(), width=20, height=16)
}

plot_tpch_quasi_clique_results <- function() {
  gammas = c(0.7, 0.8, 0.9)
  for (gamma in gammas) {
    c = getCliquesForDataset('tpch', gamma)
    data = data.frame(index = 1:length(c), size=sapply(c, length))
    image = ggplot(data=data, aes(x=factor(index), y=size, fill=size)) + 
      geom_bar(stat="identity", position="dodge") + 
      scale_fill_gradient(low="gray49", high = "black") +
      theme(text = element_text(size=16), 
            legend.position='none',
            legend.title=element_blank(),
            axis.text.x  = element_text(angle=90, vjust=0.5, size=13)) +
      xlab('Quasi-Clique Index') +
      ylab('Size of Quasi-Clique')
    
    ggsave(file=paste('tpch-cliques-', gamma, '.svg', sep=''), plot=last_plot(), width=6, height=5)
  }
}

plot_census_quasi_clique_results <- function() {
  gammas = c(0.7, 0.8, 0.9)
  for (gamma in gammas) {
    c = getCliquesForDataset('census', gamma)
    data = data.frame(index = 1:length(c), size=sapply(c, length))
    image = ggplot(data=data, aes(x=index, y=size, fill=size)) + 
      geom_bar(stat="identity", position="dodge") + 
      scale_fill_gradient(low="gray49", high = "black") +
      theme(text = element_text(size=16), 
            legend.position='none',
            legend.title=element_blank(),
            axis.text.x  = element_text(angle=90, vjust=0.5, size=13)) +
      xlab('Quasi-Clique Index') +
      ylab('Size of Quasi-Clique')
    
    ggsave(file=paste('census-cliques-', gamma, '.svg', sep=''), plot=last_plot(), width=6, height=5)
  }
}

find_nice_pruned_cliques <- function(delta = 0.7) {
  max_size = 4
  min_size = 3
  cliques = getCliquesForDataset('census', delta)
  files = list.files(paste('census_delta', delta, '_alpha0.5-pruning', sep=''), full.names = TRUE, pattern = '.csv$')
  for (file in files) {
    index = as.numeric(gsub('.*\\/(\\d+).csv$', "\\1", file))
    clique = cliques[[index]]
    data = read.csv(file)
    if (nrow(data[data$pruningPossible == FALSE, ]) > 0) {
      next
    }
    allParts = c()
    for (cols in data$pruned) {
      parts = strsplit(cols, "\\|")[[1]]
      allParts = unique(sort(c(allParts, parts)))
    }
    
    if (length(allParts) != length(clique)) {
      print(paste(index, 'has', length(allParts), 'parts, but need', length(clique)))
      next
    }
    
    if (length(clique) >= min_size && length(clique) <= max_size) {
      print(paste('candidate clique', index)) 
    } else {
      print(paste('shitty match', index, 'has', length(clique)))
    }
  }
}
