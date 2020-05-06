library(feather)

exportToFeather <- function(dataframe, fPath) {
    write_feather(dataframe, fPath)
}

importFeatherToDataframe <- function(fPath) {
    return(read_feather(fPath))
}

featherExample <- function() {
    # load dummy data
    data("mtcars")
    mtcars$car_names <- rownames(mtcars)
    rownames(mtcars) <- c()
    # print first rows of our dummy dataframe
    print(head(mtcars))
    print('AND')

    exportToFeather(mtcars, 'plop.feather')

    df = importFeatherToDataframe('plop.feather')
    print(head(df))
}