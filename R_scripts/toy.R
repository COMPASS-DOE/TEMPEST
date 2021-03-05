
library(tibble)
toy <- tibble(time = 1:4,
              A = c(1, 2, 3, 4),
              B = c(2, 4, 3, 0),
              C = c(0, 1, -1, 0))

library(ggplot2)
ggplot(toy, aes(time, A)) + geom_line() +
    geom_line(aes(y = B), color = "red") +
    geom_line(aes(y = C), color = "blue")

# This toy dataset has a variable in each column
# This is convenient for *people* but not for computers
# For efficient data analysis and visualization, we could like our data to be LONG or TIDY
# https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html

# We want to reshape this dataset
# R has a reshape command that sucks so let's use tidyr!

library(tidyr)
toy_long <- gather(toy, sensor, value, A, B, C)
toy_long <- gather(toy, sensor, value, -time)  # another way: "except for the time column"

# We can also spread the data:
# spread(toy_long, sensor, value)
# spread(toy_long, time, value)

# NOW we are cooking with gas:
ggplot(toy_long, aes(time, value, color = sensor)) + geom_line()

