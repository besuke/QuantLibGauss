qlg_fixed_rate_bond <- function(
  issue_date = "2007-05-15",
  maturity_date = "2017-05-15",
  coupon_rate = 0.045,
  face_amount = 100,
  settlement_days = 3,
  frequency = QuantLib::Period("Semiannual"),
  calendar = QuantLib::UnitedStates("GovernmentBond"),
  accrual_day_counter = QuantLib::ActualActual("Bond"),
  payment_convention = "ModifiedFollowing",
  schedule_convention = "Unadjusted",
  maturity_convention = "Unadjusted",
  date_generation = "Backward",
  end_of_month = FALSE,
  redemption = 100,
  schedule_frequency = frequency
) {
  issue_date_ql <- qlg_date(issue_date)
  maturity_date_ql <- qlg_date(maturity_date)

  if (is.character(date_generation)) {
    date_generation <- QuantLib::copyToR(
      QuantLib::DateGeneration(),
      date_generation
    )
  }

  schedule <- QuantLib::Schedule(
    issue_date_ql,
    maturity_date_ql,
    schedule_frequency,
    calendar,
    schedule_convention,
    maturity_convention,
    date_generation,
    end_of_month
  )

  QuantLib::FixedRateBond(
    settlement_days,
    face_amount,
    schedule,
    coupon_rate,
    accrual_day_counter,
    payment_convention,
    redemption,
    issue_date_ql
  )
}
