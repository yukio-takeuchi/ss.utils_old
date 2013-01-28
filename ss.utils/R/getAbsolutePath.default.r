getAbsolutePath.default<-
function (pathname, workDirectory = getwd(), expandTilde = FALSE,
    ...)
{
    getName <- function(pathname, removeSuffix = FALSE, ...) {
        components <- strsplit(pathname, split = "[/\\]")[[1]]
        len <- length(components)
        if (len == 0) {
            return("")
        }
        name <- components[len]
        if (name == ".") {
            return("")
        }
        reg <- regexpr("^.:", name)
        if (reg != -1) {
            name <- substring(name, attr(reg, "match.length") +
                1)
        }
        if (removeSuffix) {
            name <- gsub("[.][^.]*$", "", name)
        }
        name
    }
    if (length(pathname) > 1) {
        throw("Argument 'pathname' must be a single character string: ",
            paste(pathname, collapse = ", "))
    }
    if (is.na(pathname)) {
        naValue <- as.character(NA)
        return(naValue)
    }
    if (is.null(pathname)) {
        pathname <- "."
    }
    if (!isAbsolutePath(pathname)) {
        workDirectory <- strsplit(workDirectory, split = "[/\\]")[[1]]
        name <- getName(pathname)
        if (name == "" || name == ".")
            name <- NULL
        pathname <- strsplit(pathname, split = "[/\\]")[[1]]
        len <- length(pathname)
        if (len != 0) {
            pathname <- pathname[-len]
        }
        pathname <- c(workDirectory, pathname, name)
        pathname <- paste(pathname, sep = "", collapse = .Platform$file.sep)
        pathname <- filePath(pathname, removeUps = TRUE)
    }
    if (expandTilde) {
        pathname <- file.path(dirname(pathname), basename(pathname))
    }
    isWindowsUNC <- (regexpr("^//", pathname) != -1)
    pathname <- gsub("//*", "/", pathname)
    if (isWindowsUNC) {
        pathname <- paste("/", pathname, sep = "")
    }
    pathname
}
