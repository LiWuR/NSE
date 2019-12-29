library(CGE)

SCD_2_2.policy.tax <- function(time, state,
                               state.history) {
  if ((time >= 100) && (time <= 109)) {
    state$S[2, 2] <- 0.6
    state$S[2, 1] <- 0.4
  } else {
    state$S[2, 2] <- 1
    state$S[2, 1] <- 0
  }

  state
}

SCD_2_2.policy.foreign_aid <- function(time, state, state.history) {
  if ((time >= 100) && (time <= 109)) {
    state$S[2, 2] <- 3
  } else {
    state$S[2, 2] <- 1
  }

  state
}

SCD_2_2.input <- function(a = 15, b = 20,
                          policy = NULL,
                          z0 = c(1, 1), p0 = c(1, 1)) {
  A.last <- NULL
  sdm(
    A = function(state) {
      alpha1 <- rbind(5, 1)
      alpha2 <- rbind(15, 1)

      Beta <- matrix(c(
        0.5, 1,
        0.5, 0
      ), 2, 2, TRUE)

      product.input <- ifelse(is.null(A.last), 1, A.last[1, 1]) * state$z[1]
      if (product.input <= a) {
        result <- A.last <- CD_A(alpha1, Beta, state$p)
      } else if (product.input > b) {
        result <- A.last <- CD_A(alpha2, Beta, state$p)
      } else {
        result <- A.last <- (b - product.input) / (b - a) * CD_A(alpha1, Beta, state$p) +
          (product.input - a) / (b - a) * CD_A(alpha2, Beta, state$p)
      }

      result
    },
    B = matrix(c(
      1, 0,
      0, 1
    ), 2, 2, TRUE),
    S0Exg = matrix(c(
      NA, NA,
      NA, 1
    ), 2, 2, TRUE),
    GRExg = 0,
    z0 = z0,
    p0 = p0,
    maxIteration = 1,
    numberOfPeriods = 200,
    ts = TRUE,
    policy = policy,
    priceAdjustmentVelocity = 0.4
  )
}

ge.a <- SCD_2_2.input()
plot(ge.a$ts.z[, 1], type = "l")

ge.b <- SCD_2_2.input(z0 = c(18, 1), p0 = c(1, 9))
plot(ge.b$ts.z[, 1], type = "l")

ge.c <- SCD_2_2.input(policy = SCD_2_2.policy.tax)
plot(ge.c$ts.z[, 1], type = "l")

ge.d <- SCD_2_2.input(a = 30, b = 35, policy = SCD_2_2.policy.foreign_aid)
plot(ge.d$ts.z[, 1], type = "l")
