library(RUnit)
library(CodeDepends)

testSuite = defineTestSuite(name = "CodeDepends",
  dirs = getwd(),
  testFileRegexp="^test.*\\.R$",
  testFuncRegexp="^test.*")

tests = runTestSuite(testSuite)
printTextProtocol(tests, showDetails=FALSE)
