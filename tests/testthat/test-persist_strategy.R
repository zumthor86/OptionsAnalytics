test_that("Strategy saving and loading works", {
  save_strategy(strategy, tmp_strat_file)

  strat_from_file <- load_strategy(tmp_strat_file)

  expect_identical(strategy, strat_from_file)
})

teardown({
  unlink(tmp_strat_file)
})
