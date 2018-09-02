#' Reads data from a file
#'
#' This function first checks if the file exists. If it does, it reads the data and converts it into a tibble. If the file does not exist, it will throw an error. Note that this filename can be conveniently created using \code{make_filename} function included in this package.
#'
#' @param filename Name of the file(character string) from which to read the data.
#'
#' @return This function returns a tibble contianing the data.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df 
#'
#' @examples
#' file_2013 <- make_filename(2013)
#' file_2013 <- paste0(system.file("extdata", package = "fars"), "/",file_2013)
#' fars_read(file_2013)
#' 
#' @export

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates a filename from year input
#'
#' This function takes year as input and creates a filename using that year.
#'
#' @param year year from which to create the name of the file
#'
#' @return This function returns name of the file containing year.
#'
#' @examples
#' make_filename(1980)
#' 
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads data from a file for each year, subsets month and year and saves it into a list
#'
#' This function reads in a vector of years. For each year, it makes a filename, reads the data from that file, adds a year column to the data, selects, month and year variables, and finally stores that into an element of list.
#'
#' @param years a single year or a vector containing multiple years
#'
#' @return This function returns a list containing month and year from each years data
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select 
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_read_years(2012)
#' 
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                file <- paste0(system.file("extdata", package = "fars"), "/",file)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = YEAR) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Creates a table of number of observations for each month and year
#'
#' This function uses the list created by \code{fars_read_years}, calculates the number of observations for each month and year pair and generates a tabular output of this data.
#'
#' @param years a single year or a vector containing multiple years
#'
#' @return This function returns a table of number of observations for each month and year
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df 
#' @importFrom magrittr %>%
#'
#' @examples
#' library(magrittr)
#' fars_summarize_years(2013)
#' 
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Creates a map of number of fatalities for a given state and year
#'
#' This function uses state number and year as input and if fatality data exists, it puts this data on a map
#'
#' @param state.num Identifier for state number
#' @param year a single year for which to plot the data
#'
#' @return This function returns a map of fatalities for a given state and year
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter 
#' @importFrom graphics points 
#' @importFrom maps map
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_map_state(1, 2015)
#' 
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        filename <- paste0(system.file("extdata", package = "fars"), "/",filename)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
