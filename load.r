# load data & perform initial cleaning
load_data <- function(file_path, removed_columns = NULL) {
    # Read the CSV file
    data_frame <- read.csv(file_path)

    # Remove specified columns
    if (!is.null(removed_columns)) {
        data_frame <- data_frame[, !colnames(data_frame) %in% removed_columns]
    }

    # Perform initial cleaning (e.g., remove NA values)
    data_frame <- na.omit(data_frame)
    return(data_frame)
}

merge_frames <- function(frame1, frame2, by_column) {
    # Merge two data frames based on a common column
    merged_frame <- merge(frame1, frame2, by = by_column)
    return(merged_frame)
}

