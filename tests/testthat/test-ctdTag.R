test_that("getDatabaseName() works", {
    expect_equal("foo_bar_01.db", getDatabaseName("foo", "bar", "01"))
})
