#' Read data from file
#'
#' Reads data from a csv file and returns a data frame tbl object.
#'
#' @param filename input string
#' @return data frame tbl with data read from provided file
#'     filename.  File filename is expected to be in .csv format.
#'     Function is stopped if the provided
#'     filename does not exist.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'
#' fars_read(my_file)
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

#' Make a filename from a given year
#'
#' Create a filename from a given year which includes the
#'   year in the correct locationin the name.
#'
#' @param year integer or string
#'
#' @return string, specialized filename containing year
#'
#' @examples
#'
#' make_filename(1992)
#'
#' make_filename("1992")
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Convert a vector of years to a list of filenames and imports them
#'
#' Converts a vector years to a list of filenames using the make_filename
#'   function.  Produces errors for invalid years in the input vector.
#'
#' @param years vector of integer or string
#'
#' @return list of string
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#'
#' fars_read_years(c(1993,1995,2001,2016))
#'
#' fars_read_years(c("1993",1995,"2001",2016))
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Organizes data by year and month
#'
#' Reads data with year and month components, returns a count
#'   of data entries for each month in each year, and organizes
#'   the return data frame using a separate column for
#'   each year.
#'
#' @param years vector of years, integer or character
#'
#' @return data frame
#'
#' @importFrom dplyr bind_rows group_by summrize %>%
#' @importFrom tidyr spread
#'
#' @examples
#'
#' fars_summarize_years(c("1993",1995,"2001",2016))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Read and plots accidents in a given state and year on a map.
#'
#' Read data from a file.  Plots accidents in a given state
#'   and year on a map.  Returns error if the given year is
#'   invalid.  Prints message if there are no accidents to plot.
#'
#'   @param state.num integer or character
#'
#'   @param year integer or character
#'
#'   @return None
#'
#'   @importFrom maps map
#'   @importFrom graphics point
#'
#'   @seealso \code{\link{make_filename}} \code{\link{fars_read}}
#'   @examples
#'
#'   fars_map_state(23,1994)
#'
#'   @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
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
