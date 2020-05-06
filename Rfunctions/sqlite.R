library(RSQLite)

# this function creates OR appends data to a SQLite database file
exportOrAppendToDB <- function(dataframe, tableName, dbPath) {
    db = dbConnect(RSQLite::SQLite(), dbname=dbPath)
    dbWriteTable(db, tableName, dataframe, append=T)
}

# this function imports all tables from a SQLite database file
# into dataframes and return them as a list
importDbToDataframes <- function(dbPath) {
    con <- dbConnect(drv=RSQLite::SQLite(), dbname=dbPath)
    # list all tables
    tables <- dbListTables(con)
    # exclude sqlite_sequence (contains table information)
    tables <- tables[tables != 'sqlite_sequence']

    lDataFrames <- vector('list', length=length(tables))
    # create a data.frame for each table
    for (i in seq(along=tables)) {
        lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
    }
    return(lDataFrames)
}

dbStuffExample <- function() {
    # load dummy data
    data("mtcars")
    mtcars$car_names <- rownames(mtcars)
    rownames(mtcars) <- c()
    # print first rows of our dummy dataframe
    print(head(mtcars))
    print('AND')

    # export it to a SQLite3 file
    exportOrAppendToDB(mtcars, 'plop', 'exp.db')

    # import all tables from this file into a list of dataframes
    l = importDbToDataframes('exp.db')
    # check that first rows are correct
    print(head(l[[1]]))
}