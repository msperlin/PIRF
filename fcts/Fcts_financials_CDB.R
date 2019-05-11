invest_cdb_lca <- function(type.invest = 'CDB',
                           type.indexing = 'CDI',
                           percent.index = NULL,
                           date.buy = as.Date('2005-01-01'),
                           date.sell = as.Date('2018-12-31'),
                           contract.duration = 'lifetime',
                           value.first.buy = 1,
                           value.monthly.buy = 0,
                           cdi.file = 'data/main datasets/CDI.rds') {

  require(tidyverse)
  require(lubridate)

  if (is.null(percent.index)) {

    percent.index <- switch(type.indexing,
                            'CDI' = 1,
                            'IPCA' = 0.025,
                            'prefixado' = 0.1)
  }

  # set date vec for purchases by expiration
  if (contract.duration == 'lifetime') {
    date.repurchases <- c(date.buy, date.sell)
  } else {
    date.repurchases <- unique(c(seq(date.buy, date.sell,
                                     by = contract.duration),
                                 date.sell))
  }

  n.reaplications <- length(date.repurchases) - 1

  temp.value.first.buy <- value.first.buy

  # do reaplications of capital
  df.invest.out <- tibble()
  for (i.repurchase in seq(n.reaplications)) {

    temp.date.buy <- date.repurchases[i.repurchase]
    temp.date.sell <- date.repurchases[i.repurchase+1]

    # do first investment
    df.invest.first <- invest_single_cdb_lca(type.invest = type.invest,
                                             type.indexing = type.indexing,
                                             percent.index = percent.index,
                                             date.buy = temp.date.buy,
                                             date.sell = temp.date.sell,
                                             value.buy = temp.value.first.buy,
                                             cdi.file = cdi.file )

    # do others investments
    temp.date.buy.vec <- seq( temp.date.buy %m+% months(1),
                              temp.date.sell %m+% months(0), by = 'month')

    l.args <- list(type.invest = type.invest,
                   id.operation = 1:length(temp.date.buy.vec),
                   type.indexing = type.indexing,
                   percent.index = percent.index,
                   date.buy = temp.date.buy.vec,
                   date.sell = temp.date.sell,
                   value.buy = value.monthly.buy,
                   cdi.file = cdi.file)

    df.invest.others <- bind_rows(
      pmap(.l = l.args,
           .f = invest_single_cdb_lca)
    )

    df.invest.temp <- bind_rows(df.invest.first, df.invest.others) %>%
      mutate(asset.code2 = asset.code)

    # group by date and get portfolio value
    tab.invest <- df.invest.temp %>%
      group_by(id, asset.code2, ref.month) %>%
      summarise_all(.funs = last) %>%
      group_by(asset.code2, ref.month) %>%
      summarise(port.net.value = sum(port.net.value),
                port.nominal.value = sum(port.nominal.value),
                cost.IOF = sum(cost.IOF),
                cost.IR = sum(cost.IR) ) %>%
      ungroup() %>%
      mutate(type.invest = type.invest,
             contract.duration = contract.duration)

    temp.value.first.buy <- last(tab.invest$port.net.value)

    df.invest.out <- bind_rows(df.invest.out,
                               tab.invest)

    #browser()

  }

  return(df.invest.out)

}


invest_single_cdb_lca <- function(type.invest = 'CDB',
                                  type.indexing = 'CDI',
                                  id.operation = NA,
                                  percent.index = NULL,
                                  date.buy = as.Date('2010-01-01'),
                                  date.sell = Sys.Date(),
                                  value.buy = 1,
                                  cdi.file = 'data/main datasets/CDI.rds') {

  require(tidyverse)

  if (value.buy == 0) {
    return(tibble())
  }

  if ( !(type.invest %in% c('CDB', 'LCA')) ) {
    stop('type.invest should be "CDB" or "LCA"')
  }

  if ( !(type.indexing %in% c('CDI', 'IPCA', 'prefixado')) ) {
    stop('type.indexing should be "CDI", "IPCA" or "prefixado"')
  }

  if (is.null(percent.index)) {

    percent.index <- switch(type.indexing,
                            'CDI' = 1,
                            'IPCA' = 0,
                            'prefixado' = 0.1)
  }

  my.subname <- switch(type.indexing,
                       'CDI' = paste0(scales::percent(percent.index), ' CDI'),
                       'IPCA' = paste0('IPCA + ',
                                       scales::percent(percent.index)),
                       'prefixado' = paste0('Prefixado ',
                                            scales::percent(percent.index), ' aa'))

  if (type.invest == 'LCA') {
    do.ir = FALSE
    do.iof = FALSE
    my.name <- paste0('LCA/LCI ',
                      my.subname)
  } else if (type.invest == 'CDB') {
    do.ir = TRUE
    do.iof = TRUE
    my.name <- paste0('CDB ',
                      my.subname)
  }

  if (type.indexing == 'CDI') {
    df.invest <- read_rds(cdi.file) %>%
      rename(ref.month = date,
             index.nom.ret = value) %>%
      filter(ref.month >= date.buy,
             ref.month <= date.sell) %>%
      mutate(percent.index = percent.index,
             nom.ret = percent.index*index.nom.ret/100)
  } else if (type.indexing == 'IPCA') {
    df.invest <- read_rds('data/main datasets/IPCA.rds') %>%
      rename(ref.month = date,
             index.nom.ret.am = value) %>%
      select(-IPCA.am, - IPCA.aa) %>%
      filter(ref.month >= date.buy,
             ref.month <= date.sell) %>%
      mutate(percent.index = percent.index,
             index.nom.ret.aa = (1+index.nom.ret.am/100)^12 -1,
             nom.ret.aa = (1+index.nom.ret.aa)*(1+percent.index) - 1 ,
             nom.ret = (1+nom.ret.aa)^(1/12) -1)
  } else if (type.indexing == 'prefixado') {
    df.invest <- read_rds(cdi.file) %>%
      rename(ref.month = date,
             index.nom.ret = value) %>%
      filter(ref.month >= date.buy,
             ref.month <= date.sell) %>%
      mutate(percent.index = percent.index,
             nom.ret.aa = percent.index ,
             nom.ret = (1+nom.ret.aa)^(1/12) -1)
  }


  # calculate B3 custody costs (no custody costs for CDB)
  df.invest$custody.cost <- 0

  df.invest <- df.invest %>%
    mutate(asset.code = my.name,
           n.days = as.numeric(df.invest$ref.month - min(df.invest$ref.month)),
           ir.rate = if_else(rep(do.ir, nrow(df.invest)), calc.ir.rate(n.days), 0),
           iof.rate = if_else(rep(do.iof, nrow(df.invest)), calc.iof.rate(n.days), 0),
           price.bid = cumprod(1 + nom.ret),
           price.ask = cumprod(1 + nom.ret))

  # do purchases
  df.invest$value.purchases <- 0
  df.invest$value.purchases[1] <-value.buy

  df.invest <- invest_build_port(df.invest) %>%
    mutate(id = id.operation)

  return(df.invest)

}
