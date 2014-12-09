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

readSchemata <- function() {
  customer = read.csv('CUSTOMER.csv', header=F, sep=';')
  names(customer) = setColumnNames('customer')
  
  orders = read.csv('ORDERS.csv', header=F, sep=';')
  names(orders) = setColumnNames('orders')
  
  nation = read.csv('NATION.csv', header=F, sep=';')
  names(nation) = setColumnNames('nation')
  
  partsupp = read.csv('PARTSUPP.csv', header=F, sep=';')
  names(partsupp) = setColumnNames('partsupp')
  
  part = read.csv('PART.csv', header=F, sep=';')
  names(part) = setColumnNames('part')
  
  supplier = read.csv('SUPPLIER.csv', header=F, sep=';')
  names(supplier) = setColumnNames('supplier')
  
  region = read.csv('REGION.csv', header=F, sep=';')
  names(region) = setColumnNames('region')
  
  lineitem = read.csv('LINEITEM.csv', header=F, sep=';')
  names(lineitem) = setColumnNames('lineitem')
  
  list(customer=customer, orders=orders, nation=nation, partsupp=partsupp, part=part, supplier=supplier, region=region, line=line)
}
setColumnNames <- function(schema) {
  if(schema == 'customer') {
    return(c('CUSTKEY','NAME','ADDRESS','NATIONKEY','PHONE','ACCTBAL','MKTSEGMENT','COMMENT'))
  }
  else if(schema == 'orders') {
    return(c('ORDERKEY','CUSTKEY','ORDERSTATUS','TOTALPRICE','ORDERDATE','PRDERPRIORITY','CLERK','SHIPPRIORITY','COMMENT'))
  }
  else if(schema == 'nation') {
    return(c('NATIONKEY','NAME','REGIONKEY','COMMENT'))
  }
  else if(schema == 'partsupp') {
    return(c('PARTKEY','SUPPKEY','AVAILQTY','SUPPLYCOST','COMMENT'))
  }
  else if(schema == 'part') {
    return(c('PARTKEY','NAME','MFGR','BRAND','TYPE','SIZE','CONTAINER','RETAILPRICE','COMMENT'))
  }
  else if(schema == 'supplier') {
    return(c('SUPPKEY','NAME','ADDRESS','NATIONKEY','PHONE','ACCTBAL','COMMENT'))
  }
  else if(schema == 'region') {
    return(c('REGIONKEY','NAME','COMMENT'))
  }
  else if(schema == 'lineitem') {
    return(c('ORDERKEY','PARTKEY','SUPPKEY','LINENUMBER','QUANTITY','EXTENDEDPRICE','DISCOUNT','TAX','RETURNFLAG','LINESTATUS'
             ,'SHIPDATE','COMMITDATE','RECEIPTDATE','SHIPINSTRUCT','SHIPMODE','COMMENT'))
  }
}