#' Calculates tax for capital gains
#'
#' This function will use the current Brazilian tax rate for capital gains based
#' on number of days of investment. See <https://blog.rico.com.vc/imposto-renda-investimentos> for details.
#'
#' @param n.days Number of days since investment
#'
#' @return A tax rate (e.g. 0.15)
#' @export
#'
#' @examples
#'
#' print(calc.ir.rate(150))
calc.ir.rate <- function(n.days = 1:1000) {

  out <- numeric(length = length(n.days))

  out[n.days <= 6*30] <- 0.225
  out[ (n.days > 6*30)&(n.days <= 12*30)] <- 0.20
  out[ (n.days > 12*30)&(n.days <= 24*30)] <- 0.175
  out[ (n.days > 24*30)] <- 0.15

  return(out)
}

#' Calculates tax rate for IOF (imposto operacoes financeiras)
#'
#' Based on number of days, calculates the IOF rate.
#' More details at <http://www.planalto.gov.br/ccivil_03/_Ato2007-2010/2007/Decreto/D6306.htm>
#'
#' @inheritParams calc.ir.rate
#'
#' @return A vector of tax rates (e.g. 0.50 )
#' @export
#'
#' @examples
#' print(calc.iof.rate(15))
calc.iof.rate <- function(n.days = 1:100 ) {

  vec.diff <- c(0, rep(c(-4,-3,-3), 10))

  per.out <- (100+cumsum(vec.diff))/100

  iof.out <- rep(0, length(n.days))

  idx.1 <- which(n.days <= 30)
  idx.2 <- na.omit(match(n.days, 0:30))

  iof.out[idx.1] <- per.out[idx.2]

  return(iof.out)
}


my.cum.ret.funds <- function(ret.in,
                             ticker,
                             ref.date,
                             comecota.perc = 0.15,
                             value.invested = 1,
                             monthly.purchases = 0,
                             cost.carregamento = 0,
                             do.come.cota = TRUE) {

  require(purrr)

  unique.tickers <- unique(ticker)
  ret.in[is.na(ret.in)] <- 0

  my.fct <- function(...) {

    n.obs <- length(ret.in)

    cum.ret.out <- numeric(length = length(ret.in))
    for (i in (1:n.obs)) {

      if (i == 1) {
        cum.ret.out[i] <- value.invested*(1+ret.in[i])
      } else {

        month.now <- lubridate::month(ref.date[i])

        cum.ret.out[i] <- cum.ret.out[i-1]*(1+ret.in[i])

        if ((do.come.cota)&(month.now %in% c(6,12))) { # pay comecota

          if (i == 6) {
            cum.ret.comp <- value.invested
          } else {
            cum.ret.comp <- cum.ret.out[i-6]
          }

          comecota.value = (cum.ret.out[i] - cum.ret.comp)*comecota.perc

          cum.ret.out[i] <- cum.ret.out[i-1]*(1+ret.in[i]) -
            comecota.value +
            monthly.purchases*(1-cost.carregamento)
        }
      }

    }

    # last come cota adjustement (paying come-cota for last returns)
    if (do.come.cota) {
      if (n.obs >= 6) {
        per.last <- n.obs %% 6
      } else {
        per.last <- n.obs- 1
      }

      cum.ret.out[n.obs] <- cum.ret.out[n.obs] -
        (cum.ret.out[n.obs] - cum.ret.out[n.obs-per.last])*comecota.perc
    }

    # do iof
    iof.tab <- calc.iof.rate(1:n.obs)
    cum.ret.out <- cum.ret.out - (cum.ret.out - value.invested)*iof.tab

    return(cum.ret.out)

  }

  l <- pmap(.l = list(ret.in = split(x = ret.in, f = ticker),
                      ref.date = split(x = ref.date, f = ticker),
                      do.come.cota = rep(do.come.cota, length(unique.tickers)) ),
            .f = my.fct)

  cum.ret <- do.call(what = c, args = l)

  return(cum.ret)

}

#' Simulation of a bond investment (Tesouro Direto)
#'
#' This function will simulate an investment in a bond from Tesouro Direto (Brazilian government bonds)
#' and, based on inputs, calculate the nominal and net value of the portfolio. All costs, including
#' tax rate and operations costs are incorporated in the analysis
#'
#' @param TD.to.invest Type/name of bond to invest (default = "Tesouro IPCA+ 2024"). See
#'   function available.bonds() for a list of bonds.
#' @param type.invest Defines the rules for investing ('firstday', 'minprice', 'lastprice').
#'   The 'firstday' case will always invest cash in the first business day of the month. The 'minprice'
#'   option will invest in the lowest available price in the month. The 'maxprice' will invest in the day
#'   with the highest price.
#' @param date.buy Date of first buy
#' @param date.sell Date of final sell (liquidation date)
#' @param custody.cost.aa Annual custody cost from the exchange (B3). Default = 0.25% of investment value per year.
#' @param value.first.buy Value ($) of first buy (default = 1)
#' @param value.monthly.buy Value ($) of monthly purchases
#' @param TD.file A .rds file with data from Tesouro Direto
#'
#' @return
#' @export
#'
#' @examples
invest_TD <- function(TD.to.invest,
                      type.invest = 'firstday',
                      date.buy = as.Date('2000-01-01'),
                      date.sell = Sys.Date(),
                      custody.cost.aa = 0.0025,
                      value.first.buy = 1,
                      value.monthly.buy = 0,
                      TD.file = 'data/main datasets/TD.rds') {

  require(lubridate)
  require(tidyverse)

  # check args
  if (!file.exists(TD.file)) {
      stop(paste0('File', TD.file, ' doesnt exist..') )
  }

  df.TD <- read_rds(TD.file)

  available.TDs <- unique(df.TD$asset.code2)

  if (!(TD.to.invest %in% available.TDs)) {
    my.msg <-  paste0('Cant find ', TD.to.invest, ' in database.\n',
                      'Available TDs:\n\n', paste0(available.TDs,
                                                   collapse = '\n '))

    cat(my.msg)
    stop(paste0('Cant find ', TD.to.invest, ' in database.\n'))
  }

  # build month vec
  df.in <- df.TD %>%
    filter(asset.code2 == TD.to.invest,
           ref.date >= date.buy,
           ref.date <= date.sell) %>%
    mutate(ref.month = as.Date(format(ref.date, '%Y-%m-01')))

  # check days of purchases
  if (type.invest == 'firstday') {

    df.purchase.dates <- df.in %>%
      group_by(ref.month) %>%
      summarise(purchase.dates = min(ref.date))

  } else if (type.invest == 'minprice') {

    df.purchase.dates <- df.in %>%
      group_by(ref.month) %>%
      summarise(min.price = min(price.bid),
                purchase.dates = ref.date[which.min(price.bid)])

  } else if (type.invest == 'maxprice') {
    df.purchase.dates <- df.in %>%
      group_by(ref.month) %>%
      summarise(min.price = min(price.bid),
                purchase.dates = ref.date[which.max(price.bid)])
  }

  # do first buy
  df.invest.first <- invest_single_TD(df.in = df.in,
                                      id.operation = 0, # first buy
                                      date.buy = min(df.in$ref.date),
                                      date.sell = max(df.in$ref.date),
                                      custody.cost.aa = custody.cost.aa,
                                      value.buy = value.first.buy)

  # do all other buys
  if (value.monthly.buy != 0) {

    l.arg <- list(df.in = list(df.in),
                  id.operation = 1:nrow(df.purchase.dates),
                  date.buy = df.purchase.dates$purchase.dates,
                  date.sell = max(df.in$ref.date),
                  value.buy = value.monthly.buy)

    l.out <- pmap(.l = l.arg, .f = invest_single_TD)
    df.invest.others <- bind_rows(l.out)

  } else {

    df.invest.others <- tibble()

  }

  # bind first and others
  df.invest <- bind_rows(df.invest.first,
                         df.invest.others)

  # group by date and get portfolio value
  tab.invest <- df.invest %>%
    group_by(id, asset.code2, ref.month) %>%
    summarise_all(.funs = last) %>%
    group_by(asset.code2, ref.month) %>%
    summarise(port.net.value = sum(port.net.value),
              cost.IOF = sum(cost.IOF),
              cost.IR = sum(cost.IR) ) %>%
    ungroup() %>%
    mutate(type.invest = type.invest)

  return(tab.invest)

}

invest_single_TD <- function(df.in,
                             id.operation = NA,
                             date.buy = min(df.in$ref.date),
                             date.sell = max(df.in$ref.date),
                             custody.cost.aa = 0.0025,
                             value.buy = 1) {

  require(lubridate)
  require(tidyverse)

  if (value.buy == 0) return(tibble())

  custody.cost.as <- (1 + custody.cost.aa)^(1/2) - 1

  df.in <- df.in %>%
    filter(ref.date >= date.buy,
           ref.date <= date.sell) %>%
    ungroup()

  df.firstdates <- df.in %>%
    group_by(ref.month = as.Date(format(ref.date, '%Y-%m-01'))) %>%
    summarise(first.date = min(ref.date)) %>%
    mutate(month = month(ref.month))

  df.refdate <- df.firstdates %>%
    filter(month %in% c(7,1)) %>%
    slice(-1) # remove first month


  # calculate B3 custody costs
  df.in$custody.cost <- 0
  idx <- df.in$ref.date %in% (df.refdate$first.date)
  df.in$custody.cost[idx] <- custody.cost.as

  df.in <- df.in %>%
    mutate(n.days = as.numeric(df.in$ref.date - min(df.in$ref.date)),
           ir.rate = calc.ir.rate(n.days),
           iof.rate = calc.iof.rate(n.days),
           asset.code = asset.code2)

  # do purchases
  df.in$value.purchases <- 0
  df.in$value.purchases[1] <- value.buy

  df.in <- invest_build_port(df.in) %>%
    mutate(id = id.operation)

  return(df.in)

}

invest_poup <- function(date.buy = as.Date('2010-01-01'),
                        date.sell = Sys.Date(),
                        value.first.buy = 1,
                        value.monthly.buy = 0,
                        poup.file = 'data/main datasets/Poupanca.rds') {

  require(tidyverse)

  my.name <- 'Caderneta de Poupança'

  df.invest <- read_rds(poup.file) %>%
    rename(ref.month = date,
           index.nom.ret = value) %>%
    filter(ref.month >= date.buy,
           ref.month <= date.sell) %>%
    mutate(percent.index = NA,
           nom.ret = index.nom.ret)

  do.ir <- FALSE

  # calculate B3 custody costs (no custody costs for poupança)
  df.invest$custody.cost <- 0

  df.invest <- df.invest %>%
    mutate(asset.code = my.name,
           asset.code2 = my.name,
           n.days = as.numeric(df.invest$ref.month - min(df.invest$ref.month)),
           ir.rate = if_else(rep(do.ir,
                                 nrow(df.invest)),
                             calc.ir.rate(n.days), 0),
           iof.rate = calc.iof.rate(n.days),
           price.bid = cumprod(1 + nom.ret),
           price.ask = price.bid)

  # do purchases
  df.invest$value.purchases <- 0
  idx <- df.invest$ref.month %in% df.invest$ref.month
  df.invest$value.purchases[idx] <- value.monthly.buy
  df.invest$value.purchases[1] <-value.first.buy

  df.invest <- invest_build_port (df.invest)

  return(df.invest)

}

invest_build_port <- function(df.invest) {
  df.invest <- df.invest %>%
    mutate(qtd.purchased = value.purchases/price.ask, # buy at ask price
           cost.portfolio = cumsum(value.purchases),
           qtd = cumsum(qtd.purchased),
           port.nominal.value = qtd*price.bid, # sell at bid price
           pm = cost.portfolio/qtd,
           cost.custody = port.nominal.value*custody.cost,
           cost.IOF = iof.rate*(price.bid - pm)*qtd,
           cost.IR = ir.rate*( (price.bid - pm)*qtd - cost.IOF),
           port.net.value = port.nominal.value -
             cost.custody -
             cost.IOF -
             cost.IR)

  return(df.invest)
}
