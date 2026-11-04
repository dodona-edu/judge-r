context({
  testcaseAssert("Test code sees R's default packages", function(studentEnv) {
    all(paste0("package:", c("stats", "graphics", "grDevices", "utils", "datasets", "methods")) %in% search())
  })
})
