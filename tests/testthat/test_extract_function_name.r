
test_fun <- function(x) {
  print(10 + x)
}

test_that("general test", {
  out <- extract_function_name(test_fun)
  expect_equal(out, "test_fun")
})

test_that("function undefined", {
  expect_equal(extract_function_name(undef), NA_character_)
})
