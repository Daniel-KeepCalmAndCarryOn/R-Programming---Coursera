source("Part2.R")
corr <- function(directory, threshold = 0){
    full_directory <- paste("C:/Users/Daniel/OneDrive/MFE Application/Online courses/R Programming/Week 2/", directory, sep = "")
    setwd(full_directory)
    df <- complete("specdata", 1:332)
    larger_than_threshold <- df$nobs > threshold
    id <- df$id[larger_than_threshold]
    corr <- c()
    for(i in id){
        three_digit_num <- formatC(i, flag = 0, width = 3)
        filename <- paste(three_digit_num, "csv", sep = ".")
        data <- read.cs
        v(filename)
        comp <- complete.cases(data[["sulfate"]], data[["nitrate"]])
        sulfate <- data[["sulfate"]][comp]
        nitrate <- data[["nitrate"]][comp]
        corr <- append(corr, cor(sulfate, nitrate)) 
    }
    return(corr)
}
