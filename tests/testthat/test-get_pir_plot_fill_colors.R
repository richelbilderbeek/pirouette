test_that("c", {
  fill_colors <- get_pir_plot_fill_colors()
  expect_silent(get_pir_plot_fill_colors())

  expect_equal(names(fill_colors), get_tree_and_model_values())
  expect_equal(as.character(fill_colors["true_generative"]), "#FF3333") # Red
  expect_equal(as.character(fill_colors["twin_generative"]), "#F99F55") # Orange
  expect_equal(as.character(fill_colors["true_candidate"]), "#3333FF") # Blue
  expect_equal(as.character(fill_colors["twin_candidate"]), "#559988") # Green
})
