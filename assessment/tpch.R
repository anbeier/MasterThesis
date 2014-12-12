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
                              substring(hs, 1, 5)
                            }))
    data[,i] = vechash
    print(paste('hash of column', paste(i, 'done.')))
  }
  return(data)
}

readSchemata <- function() {
  customer = read.csv('CUSTOMER.csv', header=F, sep=';')
  names(customer) = setColumnNames('customer')
  customer = hashColumns(customer, c(3, 8))
  customer$C_PHONE = substring(as.character(customer$C_PHONE), 1, 3)
  
  orders = read.csv('ORDERS.csv', header=F, sep=';')
  names(orders) = setColumnNames('orders')
  orders = hashColumns(orders, c(9))
  orders$O_ORDERDATE = substring(as.character(orders$O_ORDERDATE), 1, 7)
  
  nation = read.csv('NATION.csv', header=F, sep=';')
  names(nation) = setColumnNames('nation')
  nation = hashColumns(nation, c(4))
  
  partsupp = read.csv('PARTSUPP.csv', header=F, sep=';')
  names(partsupp) = setColumnNames('partsupp')
  partsupp = hashColumns(partsupp, c(5))
  
  part = read.csv('PART.csv', header=F, sep=';')
  names(part) = setColumnNames('part')
  part = hashColumns(part, c(9))
  
  supplier = read.csv('SUPPLIER.csv', header=F, sep=';')
  names(supplier) = setColumnNames('supplier')
  supplier = hashColumns(supplier, c(3, 7))
  supplier$S_PHONE = substring(as.character(supplier$S_PHONE), 1, 3)
  
  region = read.csv('REGION.csv', header=F, sep=';')
  names(region) = setColumnNames('region')
  region = hashColumns(region, c(3))
  
  lineitem = read.csv('LINEITEM.csv', header=F, sep=';')
  names(lineitem) = setColumnNames('lineitem')
  lineitem = hashColumns(lineitem, c(16))
  lineitem$L_SHIPDATE = substring(as.character(lineitem$L_SHIPDATE), 1, 7)
  lineitem$L_COMMITDATE = substring(as.character(lineitem$L_COMMITDATE), 1, 7)
  lineitem$L_RECEIPTDATE = substring(as.character(lineitem$L_RECEIPTDATE), 1, 7)
  
  list(customer=customer, orders=orders, nation=nation, partsupp=partsupp, part=part, supplier=supplier, region=region, line=line)
}
setColumnNames <- function(schema) {
  if(schema == 'customer') {
    return(c('CUSTKEY','C_NAME','C_ADDRESS','NATIONKEY','C_PHONE','C_ACCTBAL','C_MKTSEGMENT','C_COMMENT'))
  }
  else if(schema == 'orders') {
    return(c('ORDERKEY','CUSTKEY','O_ORDERSTATUS','O_TOTALPRICE','O_ORDERDATE','O_PRDERPRIORITY','O_CLERK','O_SHIPPRIORITY','O_COMMENT'))
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