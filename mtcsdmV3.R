rm(list = ls())
library(data.tree)
library(CGE)

compute.dc <- function(node, p) {
  compute.price_dc <- function(node, p) {
    if (isLeaf(node)) {
      tmp.name <- node$name
      dc <- 1
      names(dc) <- tmp.name
      return(list(
        price = p[tmp.name],
        dc = dc
      ))
    }

    p_dc <- lapply(node$children, compute.price_dc, p)
    the.input.p <- sapply(p_dc, function(x) x$p)
    child.dc <- lapply(p_dc, function(x) x$dc)
    switch(node$type,
      "CES" = {
        the.input.coef <- CES_A(node$sigma, node$alpha, node$beta, the.input.p)
      },
      "CD" = {
        the.input.coef <- CD_A(node$alpha, node$beta, the.input.p)
      },
      "Leontief" = {
        the.input.coef <- node$a
      },
      "FIN" = , "money" = , "divident" = , "bond" = ,
      "tax" = {
        # if (!is.null(node$velocity)) node$rate <- node$rate *
        # tmp.subject.name <- names(node$rate)
        # if (is.null(tmp.subject.name)) stop("is.null(tmp.subject.name)")
        # if (any(is.na(p[tmp.subject.name]))) stop("any(is.na(p[tmp.subject.name]))")
        # the.input.coef <- c(1, the.input.p[1] * node$rate / p[tmp.subject.name])
        the.input.coef <- c(1, the.input.p[1] * node$rate / the.input.p[-1])
      },
      stop("Li: wrong type.")
    )

    price <- sum(the.input.p * the.input.coef)

    dc <- c()
    for (k in 1:length(the.input.coef)) {
      dc <- c(dc, unlist(child.dc[[k]]) * the.input.coef[k])
    }

    return(list(
      price = price,
      dc = dc
    ))
  }

  p_dc <- compute.price_dc(node, p)
  dc <- p_dc$dc
  result <- p * 0
  result[names(dc)] <- dc
  return(result)
}


mtcsdmV3 <- function(._labor.supply.CHN = 20, ._labor.supply.ROW = 20,
                     ._stock.supply.CHN = 30, ._stock.supply.ROW = 30,
                     ._tax.receipt.supply.CHN = 20, ._tax.receipt.supply.ROW = 20,
                     ._bond.supply.CHN = 2, ._bond.supply.ROW = 2,
                     ._money.supply.CHN = 100, ._money.supply.ROW = 100,

                     ._consumption.rate.CHN = 0.5, ._consumption.rate.ROW = 0.5,

                     ._sigma.industry.CHN = -0.5, ._sigma.industry.ROW = -0.5,
                     ._alpha.industry.CHN = 20, ._alpha.industry.ROW = 20,
                     ._beta.industry.domestic.product.CHN = 0.6, ._beta.industry.domestic.product.ROW = 0.6,
                     ._beta.industry.imported.product.CHN = 0.4, ._beta.industry.imported.product.ROW = 0.4,

                     ._industry.labor.coef.CHN = 0.1, ._industry.labor.coef.ROW = 0.1,
                     ._industry.ratio_tax_labor.CHN = 0.1, ._industry.ratio_tax_labor.ROW = 0.1,
                     ._industry.ratio_dividend_labor.CHN = 0.1, ._industry.ratio_dividend_labor.ROW = 0.1,

                     ._sigma.consumer.CHN = -0.1, ._sigma.consumer.ROW = -0.1,
                     ._alpha.consumer.CHN = 20, ._alpha.consumer.ROW = 20,
                     ._beta.consumer.domestic.product.CHN = 0.6, ._beta.consumer.domestic.product.ROW = 0.6,
                     ._beta.consumer.imported.product.CHN = 0.4, ._beta.consumer.imported.product.ROW = 0.4,

                     ._domestic.investment.rate.CHN = 0.7, ._domestic.investment.rate.ROW = 0.7,

                     ._tax.rate.CHN_ROW = 0.1, ._tax.rate.ROW_CHN = 0.1,
                     ._interest.rate.CHN = 1e-5, ._interest.rate.ROW = 1e-5,
                     return.ds = FALSE,
                     ...) {
  account.zero <- c(
    product.CHN = 0, labor.CHN = 0, bond.CHN = 0,
    tax.CHN = 0, dividend.CHN = 0, imported.product.CHN_ROW = 0,
    product.ROW = 0, labor.ROW = 0, bond.ROW = 0, tax.ROW = 0,
    dividend.ROW = 0, imported.product.ROW_CHN = 0,
    money.CHN = 0, money.ROW = 0
  )

  # supply
  s.industry.CHN <- s.consumer.CHN <- s.investor.CHN <- s.foreign.trade.CHN_ROW <- s.industry.ROW <- s.consumer.ROW <- s.investor.ROW <- s.foreign.trade.ROW_CHN <- account.zero *
    NA
  s.consumer.CHN["labor.CHN"] <- ._labor.supply.CHN * ._consumption.rate.CHN
  s.consumer.CHN["dividend.CHN"] <- ._stock.supply.CHN * ._consumption.rate.CHN
  s.consumer.CHN["tax.CHN"] <- ._tax.receipt.supply.CHN * ._consumption.rate.CHN
  s.consumer.CHN["money.CHN"] <- ._money.supply.CHN

  s.investor.CHN["labor.CHN"] <- ._labor.supply.CHN * (1 - ._consumption.rate.CHN)
  s.investor.CHN["dividend.CHN"] <- ._stock.supply.CHN * (1 - ._consumption.rate.CHN)
  s.investor.CHN["tax.CHN"] <- ._tax.receipt.supply.CHN * (1 - ._consumption.rate.CHN)
  s.investor.CHN["bond.CHN"] <- ._bond.supply.CHN

  s.consumer.ROW["labor.ROW"] <- ._labor.supply.ROW * ._consumption.rate.ROW
  s.consumer.ROW["dividend.ROW"] <- ._stock.supply.ROW * ._consumption.rate.ROW
  s.consumer.ROW["tax.ROW"] <- ._tax.receipt.supply.ROW * ._consumption.rate.ROW
  s.consumer.ROW["money.ROW"] <- ._money.supply.ROW

  s.investor.ROW["labor.ROW"] <- ._labor.supply.ROW * (1 - ._consumption.rate.ROW)
  s.investor.ROW["dividend.ROW"] <- ._stock.supply.ROW * (1 - ._consumption.rate.ROW)
  s.investor.ROW["tax.ROW"] <- ._tax.receipt.supply.ROW * (1 - ._consumption.rate.ROW)
  s.investor.ROW["bond.ROW"] <- ._bond.supply.ROW

  index.of.money <- which(names(account.zero) == "money.CHN" | names(account.zero) == "money.ROW")
  sector.names <- c(
    "industry.CHN", "consumer.CHN", "investor.CHN",
    "foreign.trade.CHN_ROW", "industry.ROW", "consumer.ROW", "investor.ROW",
    "foreign.trade.ROW_CHN"
  )


  # demand structure tree ---------------------------------------------------

  ## industry.CHN

  ds.industry.CHN <- Node$new("industry.CHN", type = "money", rate = c(money.CHN = ._interest.rate.CHN))

  ds.industry.CHN$AddChild("ip1.industry.CHN", type = "Leontief", a = c(1, ._industry.labor.coef.CHN))$
    AddChild("ip1.1.industry.CHN",
    type = "CES", sigma = ._sigma.industry.CHN, alpha = ._alpha.industry.CHN,
    beta = c(
      ._beta.industry.domestic.product.CHN,
      ._beta.industry.imported.product.CHN
    )
  )$AddSibling("ip1.2.industry.CHN", type = "tax", rate = c(
    tax.CHN = ._industry.ratio_tax_labor.CHN,
    dividend.CHN = ._industry.ratio_dividend_labor.CHN
  ))$
    parent$
    AddSibling("money.CHN")

  FindNode(ds.industry.CHN, "ip1.1.industry.CHN")$
    AddChild("product.CHN")$AddSibling("imported.product.CHN_ROW")

  FindNode(ds.industry.CHN, "ip1.2.industry.CHN")$
    AddChild("labor.CHN")$AddSibling("tax.CHN")$AddSibling("dividend.CHN")

  ## industry.ROW
  ds.industry.ROW <- Node$new("industry.ROW", type = "money", rate = c(money.ROW = ._interest.rate.ROW))

  ds.industry.ROW$AddChild("ip1.industry.ROW", type = "Leontief", a = c(1, ._industry.labor.coef.ROW))$
    AddChild("ip1.1.industry.ROW",
    type = "CES", sigma = ._sigma.industry.ROW, alpha = ._alpha.industry.ROW,
    beta = c(
      ._beta.industry.domestic.product.ROW,
      ._beta.industry.imported.product.ROW
    )
  )$AddSibling("ip1.2.industry.ROW", type = "tax", rate = c(
    tax.ROW = ._industry.ratio_tax_labor.ROW,
    dividend.ROW = ._industry.ratio_dividend_labor.ROW
  ))$
    parent$
    AddSibling("money.ROW")

  FindNode(ds.industry.ROW, "ip1.1.industry.ROW")$
    AddChild("product.ROW")$AddSibling("imported.product.ROW_CHN")

  FindNode(ds.industry.ROW, "ip1.2.industry.ROW")$
    AddChild("labor.ROW")$AddSibling("tax.ROW")$AddSibling("dividend.ROW")

  ## consumer.CHN
  ds.consumer.CHN <- Node$new("consumer.CHN", type = "money", rate = ._interest.rate.CHN)
  ds.consumer.CHN$AddChild("ip1.consumer.CHN",
    type = "CES",
    sigma = ._sigma.consumer.CHN,
    alpha = ._alpha.consumer.CHN,
    beta = c(
      ._beta.consumer.domestic.product.CHN,
      ._beta.consumer.imported.product.CHN
    )
  )$AddChild("product.CHN")$AddSibling("imported.product.CHN_ROW")$
    parent$
    AddSibling("money.CHN")

  ## consumer.ROW
  ds.consumer.ROW <- Node$new("consumer.ROW", type = "money", rate = ._interest.rate.ROW)
  ds.consumer.ROW$AddChild("ip1.consumer.ROW",
    type = "CES",
    sigma = ._sigma.consumer.ROW,
    alpha = ._alpha.consumer.ROW,
    beta = c(
      ._beta.consumer.domestic.product.ROW,
      ._beta.consumer.imported.product.ROW
    )
  )$AddChild("product.ROW")$AddSibling("imported.product.ROW_CHN")$
    parent$
    AddSibling("money.ROW")

  ## investor.CHN
  ds.investor.CHN <- Node$new("investor.CHN", type = "FIN", rate = (1 - ._domestic.investment.rate.CHN) /
    ._domestic.investment.rate.CHN)
  ds.investor.CHN$
    AddChild("ip1.investor.CHN",
    type = "money",
    rate = ._interest.rate.CHN
  )$
    AddChild("ip1.1.investor.CHN",
    type = "CES",
    sigma = ._sigma.industry.CHN,
    alpha = ._alpha.industry.CHN,
    beta = c(
      ._beta.industry.domestic.product.CHN,
      ._beta.industry.imported.product.CHN
    )
  )$
    AddChild("product.CHN")$AddSibling("imported.product.CHN_ROW")$
    parent$
    AddSibling("money.CHN")

  ds.investor.CHN$AddChild("ip2", type = "money", rate = ._interest.rate.ROW)$
    AddChild("bond.ROW")$AddSibling("money.ROW")

  ## investor.ROW
  ds.investor.ROW <- Node$new("investor.ROW", type = "FIN", rate = (1 - ._domestic.investment.rate.ROW) /
    ._domestic.investment.rate.ROW)
  ds.investor.ROW$
    AddChild("ip1.investor.ROW",
    type = "money",
    rate = ._interest.rate.ROW
  )$
    AddChild("ip1.1.investor.ROW",
    type = "CES",
    sigma = ._sigma.industry.ROW,
    alpha = ._alpha.industry.ROW,
    beta = c(
      ._beta.industry.domestic.product.ROW,
      ._beta.industry.imported.product.ROW
    )
  )$
    AddChild("product.ROW")$AddSibling("imported.product.ROW_CHN")$
    parent$
    AddSibling("money.ROW")

  ds.investor.ROW$AddChild("ip2", type = "money", rate = ._interest.rate.CHN)$
    AddChild("bond.CHN")$AddSibling("money.CHN")

  # foreign.trade.CHN_ROW
  ds.foreign.trade.CHN_ROW <- Node$new("foreign.trade.CHN_ROW", type = "money", rate = ._interest.rate.CHN)
  ds.foreign.trade.CHN_ROW$AddChild("ip1.foreign.trade.CHN_ROW",
    type = "tax",
    rate = ._tax.rate.CHN_ROW
  )$
    AddChild("product.ROW")$AddSibling("tax.CHN")$
    parent$
    AddSibling("money.ROW")

  # foreign.trade.ROW_CHN
  ds.foreign.trade.ROW_CHN <- Node$new("foreign.trade.ROW_CHN", type = "money", rate = ._interest.rate.ROW)
  ds.foreign.trade.ROW_CHN$AddChild("ip1.foreign.trade.ROW_CHN",
    type = "tax",
    rate = ._tax.rate.ROW_CHN
  )$
    AddChild("product.CHN")$AddSibling("tax.ROW")$
    parent$
    AddSibling("money.CHN")

  ds.sector <- list(
    ds.industry.CHN,
    ds.consumer.CHN,
    ds.investor.CHN,
    ds.foreign.trade.CHN_ROW,
    ds.industry.ROW,
    ds.consumer.ROW,
    ds.investor.ROW,
    ds.foreign.trade.ROW_CHN
  )

  # run sdm -----------------------------------------------------------------
  ge <- sdm(
    A = function(state) {
      p <- c(state$p)
      names(p) <- names(account.zero)

      result <- sapply(ds.sector, compute.dc, p)
      return(result)
    },
    B = {
      b.industry.CHN <- b.consumer.CHN <- b.investor.CHN <- b.foreign.trade.CHN_ROW <- b.industry.ROW <- b.consumer.ROW <- b.investor.ROW <- b.foreign.trade.ROW_CHN <- account.zero
      b.industry.CHN["product.CHN"] <- 1
      b.foreign.trade.CHN_ROW["imported.product.CHN_ROW"] <- 1
      b.industry.ROW["product.ROW"] <- 1
      b.foreign.trade.ROW_CHN["imported.product.ROW_CHN"] <- 1
      cbind(
        b.industry.CHN, b.consumer.CHN, b.investor.CHN,
        b.foreign.trade.CHN_ROW, b.industry.ROW, b.consumer.ROW,
        b.investor.ROW, b.foreign.trade.ROW_CHN
      )
    },
    S0Exg = {
      cbind(
        s.industry.CHN, s.consumer.CHN, s.investor.CHN,
        s.foreign.trade.CHN_ROW, s.industry.ROW, s.consumer.ROW,
        s.investor.ROW, s.foreign.trade.ROW_CHN
      )
    },
    # numberOfPeriods = 2000,
    # maxIteration = 1,
    # ts = TRUE,
    priceAdjustmentVelocity = 0.05,
    ...
  )


  ge$sector.names <- sector.names
  ge$commodity.names <- names(account.zero)
  names(ge$p) <- ge$commodity.names
  names(ge$z) <- ge$sector.names
  colnames(ge$S) <- ge$sector.names
  rownames(ge$S) <- ge$commodity.names

  ge$D <- diag(ge$p) %*% ge$A %*% diag(ge$z)
  if (return.ds) ge$ds.sector <- ds.sector


  ge$p.real <- ge$p
  ge$p["money.CHN"] <- ge$p["money.CHN"] / ._interest.rate.CHN
  ge$p["money.ROW"] <- ge$p["money.ROW"] / ._interest.rate.ROW

  ge$e <- NULL
  ge$e.ROW <- ge$p["money.ROW"] / ge$p["money.CHN"]
  ge$p <- ge$p / ge$p["money.CHN"]

  ge$demand.value.matrix <- diag(ge$p.real) %*% ge$A %*% diag(ge$z)
  colnames(ge$demand.value.matrix) <- ge$sector.names
  rownames(ge$demand.value.matrix) <- ge$commodity.names
  return(ge)
}

# ge <- mtcsdmV3(return.ds = T)

ge <- mtcsdmV3()
