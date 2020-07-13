test_that("c", {
  line_colors <- get_pir_plot_line_colors()
  expect_silent(get_pir_plot_line_colors())

  expect_equal(names(line_colors), get_tree_and_model_values())
  expect_equal(as.character(line_colors["true_generative"]), "#FF0000") # Red
  expect_equal(as.character(line_colors["twin_generative"]), "#E77E22") # Orange
  expect_equal(as.character(line_colors["true_candidate"]), "#0000FF") # Blue
  expect_equal(as.character(line_colors["twin_candidate"]), "#229955") # Green
})
