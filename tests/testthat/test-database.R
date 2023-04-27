test_that("getDatabaseName() works", {
    expect_equal("./kelley.db", getDatabaseName(username="kelley"))
    expect_equal("./foo_bar.db", getDatabaseName("foo", "bar"))
    expect_equal("./foo_bar_01.db", getDatabaseName("foo", "bar", "01"))
})
