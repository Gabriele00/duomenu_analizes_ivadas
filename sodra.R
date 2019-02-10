X <- read.csv(url(""))


temp <- tempfile()
download.file("http://atvira.sodra.lt/imones/downloads/2018/monthly-2018.csv.zip",temp)
data <- read.csv(unz(temp, "monthly-2018.csv"))
unlink(temp)

temp
