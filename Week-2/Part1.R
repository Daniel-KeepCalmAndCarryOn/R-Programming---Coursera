pollutantmean <- function(directory, pollutant, id = 1:332){
    full_directory <- paste("C:/Users/Daniel/OneDrive/MFE Application/Online courses/R Programming/Week 2/", directory, sep = "")
    setwd(full_directory)
    sum <- 0
    count <- 0
    for(i in id){
        three_digit_num <- formatC(i, flag = 0, width = 3)
        filename <- paste(three_digit_num, "csv", sep = ".")
        data <- read.csv(filename)
        na <- is.na(data[[pollutant]])
        sum <- sum + sum(data[[pollutant]][!na])
        count <- count + length(data[[pollutant]][!na])
    }
    mean <- sum/count
}

