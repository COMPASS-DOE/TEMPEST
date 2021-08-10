sapflow <- read_csv("~/Desktop/sapflow.csv")

test <- reactiveFileReader(1000,
                           session,
                           filePath = "~/Desktop/test_reactive.csv",
                           readFunc = read.csv)
