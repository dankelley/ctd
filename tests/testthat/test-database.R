test_that("getDatabaseName() works", {
    expect_equal(getDatabaseName(username="kelley"), "~/kelley.db")
    expect_equal(getDatabaseName("foo", "bar"), "~/foo_bar.db")
    expect_equal(getDatabaseName("foo", "bar", "01"), "~/foo_bar_01.db")
})
