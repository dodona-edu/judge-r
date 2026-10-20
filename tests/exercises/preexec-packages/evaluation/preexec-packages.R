context({
  testcase("Package attached in preExec", {
    testEqual("kept", function(studentEnv) studentEnv$kept, 2L)
  })
}, preExec = {
  library(dplyr)
})
