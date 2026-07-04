rm(list = ls())

setwd("C:/AnalyticFin/Projects/QuantLibGauss")

.libPaths(c(
  "C:/AnalyticFin/Projects/QuantLibGauss/library/windows",
  .libPaths()
))

library(QuantLib)
devtools::load_all()
devtools::document()

qlg_eval_date("2008-09-18")

curve <- qlg_build_bond_discount_curve()

bond_fx <- qlg_fixed_rate_bond()
bond_fx <- qlg_set_bond_pricing_engine(bond_fx, curve)

summary <- qlg_bond_summary(bond_fx)
summary

ytm <- qlg_bond_yield(bond_fx)
price <- qlg_bond_price_from_yield(bond_fx, ytm)
ytm_check <- qlg_bond_yield_from_price(bond_fx, price)

ytm
price
ytm_check

qlg_bond_dv01(bond_fx, ytm)
qlg_bond_pv01(bond_fx, ytm)

py <- qlg_bond_price_yield_curve(bond_fx)

py
plot(
  py$yield,
  py$clean_price,
  type = "l",
  xlab = "Yield",
  ylab = "Clean Price",
  main = "Bond Price-Yield Curve"
)

qlg_bond_cashflow_table(bond_fx)
coupon_info <- qlg_bond_coupon_info(bond_fx)
coupon_info

coupon_info |>
  dplyr::filter(is_previous_coupon)

coupon_info |>
  dplyr::filter(is_next_coupon)


qlg_bond_settlement_info(bond_fx)


qlg_bond_sensitivity_table(bond_fx)

sens <- qlg_bond_sensitivity_table(bond_fx)

ggplot2::ggplot(sens, ggplot2::aes(x = shift_bp, y = clean_price)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Yield shift (bp)",
    y = "Clean price",
    title = "Bond price sensitivity"
  )


curve <- qlg_build_bond_discount_curve()

qlg_bond_zspread(
  bond = bond_fx,
  discount_curve = curve
)
qlg_bond_zspread(
  bond = bond_fx,
  discount_curve = curve,
  clean_price = Bond_cleanPrice(bond_fx)
)
clean0 <- Bond_cleanPrice(bond_fx)

qlg_bond_zspread(
  bond = bond_fx,
  discount_curve = curve,
  clean_price = clean0 - 1
)
z <- qlg_bond_zspread(
  bond = bond_fx,
  discount_curve = curve,
  clean_price = clean0 - 1
)

z * 10000

qlg_bond_summary(bond_fx)
qlg_bond_coupon_info(bond_fx)
qlg_bond_settlement_info(bond_fx)
qlg_bond_sensitivity_table(bond_fx)
qlg_bond_zspread(bond_fx, curve)

curve <- qlg_build_bond_discount_curve()


index <- USDLibor(
  Period("Semiannual"),
  YieldTermStructureHandle(curve)
)
curve <- qlg_build_bond_discount_curve()

index <- USDLibor(
  Period("Semiannual"),
  YieldTermStructureHandle(curve)
)


qlg_eval_date("2008-09-18")
qlg_eval_date_get()

curve <- qlg_build_bond_discount_curve()
bond_zero <- qlg_zero_coupon_bond(curve)

qlg_bond_summary(bond_zero, curve)
qlg_bond_cashflow_table(bond_zero)
qlg_bond_coupon_info(bond_zero)
qlg_bond_settlement_info(bond_zero)

nodes <- qlg_example_ois_nodes()
ois <- qlg_ois_curve(nodes)

ois$table
ois$curve

bond_curve <- qlg_build_bond_discount_curve()
swap_curve <- qlg_build_swap_curve()
hazard_quote <- qlg_quote_handle(0.02)
discount_quote <- qlg_quote_handle(0.03)
engine <- qlg_cds_midpoint_engine(
  hazard_rate = 0.02,
  recovery_rate = 0.40,
  discount_rate = 0.03,
  reference_date = "2024-01-02"
)


# -------------------------------------------------------------------------
# Option examples
# -------------------------------------------------------------------------

cat("\n=== Option examples ===\n")

european_option_trade <- tibble::tibble(
  spot = 100,
  strike = 100,
  maturity_date = "2025-01-15",
  option_type = "call",
  valuation_date = "2024-01-15",
  risk_free_rate = 0.03,
  dividend_yield = 0.01,
  volatility = 0.20
)

european_option <- qlg_make_european_option_from_trade(
  european_option_trade
)

cat("\nEuropean option summary:\n")
print(qlg_option_summary(european_option))


american_option_trade <- tibble::tibble(
  spot = 100,
  strike = 100,
  maturity_date = "2025-01-15",
  option_type = "put",
  valuation_date = "2024-01-15",
  risk_free_rate = 0.03,
  dividend_yield = 0.01,
  volatility = 0.20,
  steps = 200L
)

american_option <- qlg_make_american_option_from_trade(
  american_option_trade
)

cat("\nAmerican option summary:\n")
print(qlg_option_summary(american_option))


barrier_option_trade <- tibble::tibble(
  spot = 100,
  strike = 100,
  maturity_date = "2025-01-15",
  barrier = 90,
  barrier_type = "down_out",
  rebate = 0,
  option_type = "call",
  valuation_date = "2024-01-15",
  risk_free_rate = 0.03,
  dividend_yield = 0.01,
  volatility = 0.20
)

barrier_option <- qlg_make_barrier_option_from_trade(
  barrier_option_trade
)

cat("\nBarrier option summary:\n")
print(qlg_option_summary(barrier_option))