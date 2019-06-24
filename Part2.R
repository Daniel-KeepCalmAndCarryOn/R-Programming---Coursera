complete <- function(directory, id = 1:332){
    complete_num <- c()
    full_directory <- paste("C:/Users/Daniel/OneDrive/MFE Application/Online courses/R Programming/Week 2/", directory, sep = "")
    setwd(full_directory)
    for(i in id){
        three_digit_num <- formatC(i, flag = 0, width = 3)
        filename <- paste(three_digit_num, "csv", sep = ".")
        data <- read.csv(filename)
        comp <- complete.cases(data[["sulfate"]], data[["nitrate"]])
        complete_num <- append(complete_num, sum(comp))
    }
    data.frame(id = id, nobs = complete_num)
}