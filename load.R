getwd()
setwd("~/DSP_camp/")
thm <- function() theme(text=element_text(size = 15, family = "STHeiti"))
library(gdata)
library(reshape2)

#導入92~94受雇人員人數&薪資
employee92_95 <- read.csv("92_95employee_data.csv", fileEncoding = "BIG5")
employee97_99 <- read.csv("97_99employee_data.csv", fileEncoding = "BIG5")
employee100_101 <- read.csv("100_101employee_data.csv", fileEncoding = "BIG5")
employee102_104 <- read.csv("102_104employee_data.csv", fileEncoding = "BIG5")
#92~104初任人員薪資
first92_95 <- read.csv("92_95first_data.csv", fileEncoding = "BIG5")
first97_99 <- read.csv("97_99first_data.csv", fileEncoding = "BIG5")
first100_101 <- read.csv("100_101first_data.csv", fileEncoding = "BIG5")
first102_104 <- read.csv("102_104first_data.csv", fileEncoding = "BIG5")
#92~104工作時數
worktime92_95 <- read.csv("92_95worktime_data.csv", fileEncoding = "BIG5")
worktime97_99 <- read.csv("97_99worktime_data.csv", fileEncoding = "BIG5")
worktime100_101 <- read.csv("100_101worktime_data.csv", fileEncoding = "BIG5")
worktime102_104 <- read.csv("102_104worktime_data.csv", fileEncoding = "BIG5")
#工作滿意度
happy_data <- read.csv("happiness.csv", fileEncoding = "BIG5")

