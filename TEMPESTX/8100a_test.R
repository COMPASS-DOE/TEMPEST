
d <- read.csv("8100_data/test.csv")

library(lubridate)
d$Date <- ymd_hms(d$Date_IV)
d$Port. <- as.factor(d$Port.)
d <- tibble::as_tibble(d)

library(ggplot2)
ggplot(d, aes(Date, Exp_Flux, color = Port.)) + geom_point() + facet_grid(File.Name~., scales="free") + ggtitle("Flux")

