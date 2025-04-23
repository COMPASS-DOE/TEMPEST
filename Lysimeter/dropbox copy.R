
#' Download and read a data file from Dropbox
#'
#' @param filename A Dropbox filename, e.g. returned by \code{drop_dir}
#' @param token A dropbox token
#' @param read_function A function to read the downloaded file with
#' @param ... Other parameters to be passed to \code{read_function}
#' @description This function downloads and reads (via a caller-supplied
#' function) a file on Dropbox.
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @author Ben Bond-Lamberty
read_file_dropbox <- function(filename, token, read_function, ...) {
    # We don't want users to need rdrop2 to use this package (i.e. we don't
    # want to put it in DESCRIPTION's Imports:), so check for availability
    if(requireNamespace("rdrop2", quietly = TRUE)) {
        # download to temp file
        tf <- tempfile()
        rdrop2::drop_download(filename, local_path = tf,
                              dtoken = token, overwrite = TRUE)
        read_function(tf, ...)
    } else {
        stop("rdrop2 package is not available")
    }
}


#' Read a directory of files
#'
#' @param datadir A directory, either in Dropbox or local
#' @param pattern Filename regex pattern
#' @param read_function The file-read function to use
#' @param dropbox_token Optional Dropbox token
#' @param progress_bar Optional progress bar to call while reading
#' @param ... Other parameters to be passed to \code{read_function}
#' @description A general-purpose function to read data files, either from
#' Dropbox or locally. The function identifies files based on a pattern,
#' reads data from each file, and binds them together Callers
#' pass a `read_function` that handles the actual file read.
#' @return All data files in directory, read and concatenated
#' in a \code{\link[tibble]{tibble}}.
#' @importFrom dplyr bind_rows
#' @note This is not intended to be called directly by users; use
#' \code{\link{process_sapflow_dir}}, etc., instead.
#' @note We use \code{\link[dplyr]{bind_rows}} for its much greater efficiency
#' over \code{\link{rbind}}
#' @author Ben Bond-Lamberty
process_dir <- function(datadir, pattern, read_function,
                        dropbox_token = NULL,
                        progress_bar = NULL, ...) {

    local <- is.null(dropbox_token)

    # Get our file list, either locally or in Dropbox
    if(local) {
        s_files <- list.files(datadir, pattern = pattern, full.names = TRUE)
    } else {
        # We don't want users to need rdrop2 to use this package (i.e. we don't
        # want to put it in DESCRIPTION's Imports:), so check for availability
        if(requireNamespace("rdrop2", quietly = TRUE)) {
            # Generate list of 'current' (based on token) files
            s_dir <- rdrop2::drop_dir(datadir, dtoken = dropbox_token)
            s_files <- grep(s_dir$path_display, pattern = pattern, value = TRUE)
        } else {
            stop("rdrop2 package is not available")
        }
    }

    # Function called by lapply below; handles progress bar and calls file reader
    f <- function(filename, read_function, token, total_files) {
        if(!is.null(progress_bar)) progress_bar(1 / total_files)
        # Read file, either locally or from Dropbox
        if(local) {
            read_function(filename, ...)
        } else {
            read_file_dropbox(filename, dropbox_token, read_function, ...)
        }
    }
    x <- lapply(s_files, f, read_function, dropbox_token, length(s_files))
    bind_rows(x)
}
