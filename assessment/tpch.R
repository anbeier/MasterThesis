readTPCHData <- function(datafile) {
  load(datafile)
  return(tpch)
}

readTPCHCliques <- function(fqsfile) {
  qs = NULL
  headers <- c('CUSTOMER.','ORDERS.','NATION.','PARTSUPP.','PART.','SUPPLIER.','REGION.','LINEITEM.')
  lines = readLines(fqsfile, encoding='UTF-8')
  for(l in lines) {
    str = unlist(strsplit(l, '--'))
    for(h in headers) {
      str = gsub(h, '', str, fixed=T)
    }
    qs = append(qs, list(str))
  }
  return(qs)
}

hashColumns <- function(data, columnIndices) {
  for(i in columnIndices) {
    vec = as.character(data[,i])
    vechash = unlist(lapply(vec, 
                            function(x) {
                              hs = digest(x, algo=c('md5'), serialize=F)
                              substring(hs, 1, 3)
                            }))
    data[,i] = vechash
    print(paste('hash of column', paste(i, 'done.')))
  }
  return(data)
}

extractFirstNChars <- function(data, columnIndices, nchar) {
  for(i in columnIndices) {
    data[,i] = substring(as.character(data[,i]), 1, nchar)
  }
  return(data)
}

readSchemata <- function() {
  library(digest)
  customer = read.csv('CUSTOMER.csv', header=F, sep=';')
  names(customer) = setColumnNames('customer')
  customer = hashColumns(customer, c(2, 3, 8))
  customer = extractFirstNChars(customer, c(5), 3)
  
  orders = read.csv('ORDERS.csv', header=F, sep=';')
  names(orders) = setColumnNames('orders')
  orders = hashColumns(orders, c(7, 9))
  orders = extractFirstNChars(orders, c(5), 7)
  
  nation = read.csv('NATION.csv', header=F, sep=';')
  names(nation) = setColumnNames('nation')
  nation = hashColumns(nation, c(4))
  
  partsupp = read.csv('PARTSUPP.csv', header=F, sep=';')
  names(partsupp) = setColumnNames('partsupp')
  partsupp = hashColumns(partsupp, c(5))
  
  part = read.csv('PART.csv', header=F, sep=';')
  names(part) = setColumnNames('part')
  part = hashColumns(part, c(2, 9))
  
  supplier = read.csv('SUPPLIER.csv', header=F, sep=';')
  names(supplier) = setColumnNames('supplier')
  supplier = hashColumns(supplier, c(2, 3, 7))
  supplier = extractFirstNChars(supplier, c(5), 3)
  
  region = read.csv('REGION.csv', header=F, sep=';')
  names(region) = setColumnNames('region')
  region = hashColumns(region, c(3))
  
  lineitem = read.csv('LINEITEM.csv', header=F, sep=';')
  names(lineitem) = setColumnNames('lineitem')
  lineitem = hashColumns(lineitem, c(16))
  lineitem = extractFirstNChars(lineitem, c(11, 12, 13), 7)
  
  list(customer=customer, orders=orders, nation=nation, partsupp=partsupp, part=part, supplier=supplier, region=region, line=line)
}


setColumnNames <- function(schema) {
  if(schema == 'customer') {
    return(c('CUSTKEY','C_NAME','C_ADDRESS','NATIONKEY','C_PHONE','C_ACCTBAL','C_MKTSEGMENT','C_COMMENT'))
  }
  else if(schema == 'orders') {
    return(c('ORDERKEY','CUSTKEY','O_ORDERSTATUS','O_TOTALPRICE','O_ORDERDATE','O_ORDERPRIORITY','O_CLERK','O_SHIPPRIORITY','O_COMMENT'))
  }
  else if(schema == 'nation') {
    return(c('NATIONKEY','N_NAME','REGIONKEY','N_COMMENT'))
  }
  else if(schema == 'partsupp') {
    return(c('PARTKEY','SUPPKEY','PS_AVAILQTY','PS_SUPPLYCOST','PS_COMMENT'))
  }
  else if(schema == 'part') {
    return(c('PARTKEY','P_NAME','P_MFGR','P_BRAND','P_TYPE','P_SIZE','P_CONTAINER','P_RETAILPRICE','P_COMMENT'))
  }
  else if(schema == 'supplier') {
    return(c('SUPPKEY','S_NAME','S_ADDRESS','NATIONKEY','S_PHONE','S_ACCTBAL','S_COMMENT'))
  }
  else if(schema == 'region') {
    return(c('REGIONKEY','R_NAME','R_COMMENT'))
  }
  else if(schema == 'lineitem') {
    return(c('ORDERKEY','PARTKEY','SUPPKEY','L_LINENUMBER','L_QUANTITY','L_EXTENDEDPRICE','L_DISCOUNT','L_TAX','L_RETURNFLAG','L_LINESTATUS'
             ,'L_SHIPDATE','L_COMMITDATE','L_RECEIPTDATE','L_SHIPINSTRUCT','L_SHIPMODE','L_COMMENT'))
  }
}

getDistinctNumber <- function(columnValue) {
  length(unique(columnValue))
}

renameJoinedDatasets <- function(df) {
  # PARTKEY
  names(df)[1] = 'L_PARTKEY'
  df$PS_PARTKEY = df$L_PARTKEY
  df$P_PARTKEY = df$L_PARTKEY
  
  # SUPPKEY
  names(df)[2] = 'PS_SUPPKEY'
  df$L_SUPPKEY = df$PS_SUPPKEY
  df$S_SUPPKEY = df$PS_SUPPKEY
  
  # ORDERKEY
  names(df)[3] = 'O_ORDERKEY'
  df$L_ORDERKEY = df$O_ORDERKEY
  
  # CUSTKEY
  names(df)[4] = 'O_CUSTKEY'
  df$C_CUSTKEY = df$O_CUSTKEY
  
  # REGIONKEY
  names(df)[5] = 'N_REGIONKEY'
  names(df)[37] = 'R_REGIONKEY'
  
  # NATIONKEY
  names(df)[6] = 'N_NATIONKEY'
  names(df)[38] = 'S_NATIONKEY'
  df$C_NATIONKEY = df$N_NATIONKEY
  
  return(df)  # columns containing .x or .y should be removed.
}

joinSchemata <- function(schemata.vector) {
  j1 = merge(customer, nation, by='NATIONKEY')
  j1 = merge(j1, region, by='REGIONKEY')
  j1 = merge(j1, orders, by='CUSTKEY')
  j1 = merge(j1, lineitem, by='ORDERKEY')
  print('finished join part 1.')
  
  j2 = merge(supplier, nation, by='NATIONKEY')
  j2 = merge(j2, region, by='REGIONKEY')
  j2 = merge(j2, partsupp, by='SUPPKEY')
  j2 = merge(j2, part, by='PARTKEY')
  print('finished join part 2.')
  
  j = merge(j1, j2, by=c('PARTKEY', 'SUPPKEY'))
  
  j = renameJoinedDatasets(j)
}

modifyDataTypes <- function(data) {
  for(i in 1:ncol(data)) {
    data[,i] = as.factor(as.character(data[,i]))
  }
  
  # to numeric and then bucketize
  numericColumns = c('ACCTBAL','PRICE','QUANTITY','DISCOUNT','TAX','QTY','COST','SIZE')
  for(x in numericColumns) {
    inds = grep(x, names(data), ignore.case = T)
    for(i in inds) {
      data[,i] = as.numeric(as.character(data[,i]))
    }
  }
  data = convertNumericColumnToFactor(data)
  return(data)
}