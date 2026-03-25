# load data & perform initial cleaning
load_data <- function(file_path, removed_columns = NULL) {
    # Read the CSV file
    data_frame <- read.csv(file_path)

    # Remove specified columns
    if (!is.null(removed_columns)) {
        data_frame <- data_frame[, !colnames(data_frame) %in% removed_columns]
    }

    # Note: Do NOT call na.omit() here - it silently removes rows with any NA
    # (e.g., products with missing categories). Handle NAs explicitly in merge logic instead.
    return(data_frame)
}

# remove multiple columns from a data frame
remove_column <- function(data_frame, column_name) {
    data_frame <- data_frame[, !colnames(data_frame) %in% column_name]
    return(data_frame)
}

# install and load necessary packages
install_and_load <- function(){
    # Set the CRAN mirror:
    local({r <- getOption("repos")
        r["CRAN"] <- "https://cran.rstudio.com/"
        options(repos = r)}
    )
    # Install the packages used in this workbook:
    packages <- c("C50", "ggplot2", "gmodels", "Hmisc", "randomForest", "rsample", "e1071", "tidyr")
    for (i in packages) {
        if (!require(i, character.only = TRUE)) {
            install.packages(i, dependencies = TRUE)
            if (!require(i, character.only = TRUE)) {
                stop(paste("Package", i, "could not be loaded even after installation."))
            }
        }
    }
}