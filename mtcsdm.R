# Title: A Monetary Two-Country Structural Dynamic Model (v1.0)
# Author: LI Wu. 2019, 12, 28.
# Reference: LI Wu (2019, ISBN: 9787521804225) General Equilibrium and Structural Dynamics: Perspectives of New Structural Economics. Beijing: Economic Science Press. (In Chinese)

library(CGE)
mtcsdm <- function(._labor.supply.CHN = 20, ._labor.supply.ROW = 20,
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
                   ._interest.rate.CHN = 1e-5, ._interest.rate.ROW = 1e-5) {
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

  A.pre <- function(state) {
    p <- state$p
    names(p) <- names(account.zero)
    a.industry.CHN <- a.consumer.CHN <- a.investor.CHN <- a.foreign.trade.CHN_ROW <- a.industry.ROW <- a.consumer.ROW <- a.investor.ROW <- a.foreign.trade.ROW_CHN <- account.zero
    a.industry.CHN["money.CHN"] <- a.consumer.CHN["money.CHN"] <-
      a.investor.CHN["money.CHN"] <- a.foreign.trade.CHN_ROW["money.ROW"] <-
      a.industry.ROW["money.ROW"] <- a.consumer.ROW["money.ROW"] <-
      a.investor.ROW["money.ROW"] <- a.foreign.trade.ROW_CHN["money.CHN"] <- -1
    a.industry.CHN[c("product.CHN", "imported.product.CHN_ROW")] <- CES_A(
      ._sigma.industry.CHN,
      ._alpha.industry.CHN,
      rbind(
        ._beta.industry.domestic.product.CHN,
        ._beta.industry.imported.product.CHN
      ), p[c(
        "product.CHN",
        "imported.product.CHN_ROW"
      )]
    )
    a.industry.CHN["labor.CHN"] <- ._industry.labor.coef.CHN
    a.industry.CHN["tax.CHN"] <- ._industry.ratio_tax_labor.CHN *
      a.industry.CHN["labor.CHN"] * p["labor.CHN"] / p["tax.CHN"]
    a.industry.CHN["dividend.CHN"] <- ._industry.ratio_dividend_labor.CHN *
      a.industry.CHN["labor.CHN"] * p["labor.CHN"] / p["dividend.CHN"]

    consumer.income.CHN <- sum(ifelse(is.na(s.consumer.CHN),
      0, s.consumer.CHN
    ) * p)
    tmp.consumption.bundle <- CES_A(
      ._sigma.consumer.CHN,
      ._alpha.consumer.CHN,
      rbind(._beta.consumer.domestic.product.CHN, ._beta.consumer.imported.product.CHN),
      p[c("product.CHN", "imported.product.CHN_ROW")]
    )
    a.consumer.CHN[c("product.CHN", "imported.product.CHN_ROW")] <- tmp.consumption.bundle

    investor.income.CHN <- sum(ifelse(is.na(s.investor.CHN),
      0, s.investor.CHN
    ) * p)
    abroad.investment.rate.CHN <- 1 - ._domestic.investment.rate.CHN
    tmp.investment.bundle <- CES_A(
      ._sigma.industry.CHN,
      ._alpha.industry.CHN, 
      rbind(._beta.industry.domestic.product.CHN, 
            ._beta.industry.imported.product.CHN),
      p[c("product.CHN", "imported.product.CHN_ROW")]
    )
    a.investor.CHN[c("product.CHN", "imported.product.CHN_ROW")] <- tmp.investment.bundle *
      investor.income.CHN * ._domestic.investment.rate.CHN / sum(tmp.investment.bundle *
        p[c("product.CHN", "imported.product.CHN_ROW")])
    a.investor.CHN[c("bond.ROW")] <- investor.income.CHN *
      abroad.investment.rate.CHN / p[c("bond.ROW")]

    a.foreign.trade.CHN_ROW["product.ROW"] <- 1
    a.foreign.trade.CHN_ROW["tax.CHN"] <- p["product.ROW"] * ._tax.rate.CHN_ROW / p["tax.CHN"]

    a.industry.ROW[c("product.ROW", "imported.product.ROW_CHN")] <- CES_A(
      ._sigma.industry.ROW,
      ._alpha.industry.ROW, rbind(
        ._beta.industry.domestic.product.ROW,
        ._beta.industry.imported.product.ROW
      ), p[c(
        "product.ROW",
        "imported.product.ROW_CHN"
      )]
    )
    a.industry.ROW["labor.ROW"] <- ._industry.labor.coef.ROW
    a.industry.ROW["tax.ROW"] <- ._industry.ratio_tax_labor.ROW *
      a.industry.ROW["labor.ROW"] * p["labor.ROW"] / p["tax.ROW"]
    a.industry.ROW["dividend.ROW"] <- ._industry.ratio_dividend_labor.ROW *
      a.industry.ROW["labor.ROW"] * p["labor.ROW"] / p["dividend.ROW"]
    consumer.income.ROW <- sum(ifelse(is.na(s.consumer.ROW),
      0, s.consumer.ROW
    ) * p)
    tmp.consumption.bundle <- CES_A(
      ._sigma.consumer.ROW,
      ._alpha.consumer.ROW,
      rbind(._beta.consumer.domestic.product.ROW, ._beta.consumer.imported.product.ROW),
      p[c("product.ROW", "imported.product.ROW_CHN")]
    )
    a.consumer.ROW[c("product.ROW", "imported.product.ROW_CHN")] <- tmp.consumption.bundle
    investor.income.ROW <- sum(ifelse(is.na(s.investor.ROW),
      0, s.investor.ROW
    ) * p)
    abroad.investment.rate.ROW <- 1 - ._domestic.investment.rate.ROW
    tmp.investment.bundle <- CES_A(
      ._sigma.industry.ROW,
      ._alpha.industry.ROW, 
      rbind(._beta.industry.domestic.product.ROW, 
            ._beta.industry.imported.product.ROW),
      p[c("product.ROW", "imported.product.ROW_CHN")]
    )
    a.investor.ROW[c("product.ROW", "imported.product.ROW_CHN")] <- tmp.investment.bundle *
      investor.income.ROW * ._domestic.investment.rate.ROW / sum(tmp.investment.bundle *
        p[c("product.ROW", "imported.product.ROW_CHN")])
    a.investor.ROW[c("bond.CHN")] <- investor.income.ROW *
      abroad.investment.rate.ROW / p[c("bond.CHN")]

    a.foreign.trade.ROW_CHN["product.CHN"] <- 1
    a.foreign.trade.ROW_CHN["tax.ROW"] <- p["product.CHN"] * ._tax.rate.ROW_CHN / p["tax.ROW"]

    cbind(
      a.industry.CHN, a.consumer.CHN, a.investor.CHN,
      a.foreign.trade.CHN_ROW, a.industry.ROW, a.consumer.ROW,
      a.investor.ROW, a.foreign.trade.ROW_CHN
    )
  }


  ge <- sdm(
    moneyOwnerIndex = which(sector.names == "consumer.CHN" | sector.names == "consumer.ROW"),
    moneyIndex = index.of.money,
    pExg = {
      tmp <- rep(NA, length(account.zero))
      names(tmp) <- names(account.zero)
      tmp["money.CHN"] <- ._interest.rate.CHN
      tmp["money.ROW"] <- ._interest.rate.ROW
      tmp
    },
    A = function(state) {
      p <- state$p
      tmp.A <- A.pre(state)
      nonnegativeA <- tmp.A
      nonnegativeA[nonnegativeA < 0] <- 0
      Indx <- which(tmp.A < 0, arr.ind = T)
      for (k in 1:nrow(Indx)) {
        tmp.A[Indx[k, 1], Indx[k, 2]] <- t(p) %*% nonnegativeA[
          ,
          Indx[k, 2]
        ] / (-tmp.A[Indx[k, 1], Indx[k, 2]])
      }
      tmp.A
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
    numberOfPeriods = 2000,
    maxIteration = 1,
    ts = TRUE
  )


  ge$sector.names <- sector.names
  ge$commodity.names <- names(account.zero)
  names(ge$p) <- ge$commodity.names
  names(ge$z) <- ge$sector.names
  colnames(ge$S) <- ge$sector.names
  rownames(ge$S) <- ge$commodity.names

  ge$D <- diag(ge$p) %*% ge$A %*% diag(ge$z)
  return(ge)
}

ge <- mtcsdm()
ge$p
ge$z
ge$e
