library(RSelenium)
remDr <- RSelenium::remoteDriver(
  remoteServerAddr = "localhost",
  port = 9999L,
  browserName = "firefox"
)

remDr$getStatus()

remDr$open()


remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "name", value = "q")
webElem$getElementAttribute("name")


remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "id", value = "lst-ib")