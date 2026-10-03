# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-03
# --------------------------------------------

# Machinery shared by the four database readers (HMD, JMD, AHMD, CHMD).
# It holds the one HTTP fetch and the file-name map, the one download loop,
# the object assembly, the input checks, the HMD country and data-type
# catalogues, and the small HTML table parser behind `availableHMD`. The
# readers differ only in their URL shape and in the extra availability
# rules, which stay in their own files.

#' Save Output in the working directory
#' @param out Output file
#' @inheritParams ReadHMD
#' @param prefix File name prefix for the saved object, e.g. "HMD".
#' @return No return value, called for side effects
#' @noRd
save_output <- function(out, show, prefix) {
  fn  <- paste0(prefix, "_", out$input$what) # file name
  assign(fn, value = out)
  save(list = fn, file = paste0(fn, ".Rdata"))
  if (show) save_message()
}


#' Print message when saving an object
#' @return No return value, called for side effects
#' @noRd
save_message <- function() {
  wd  <- getwd()
  n   <- nchar(wd)
  wd_ <- paste0("...", substring(wd, first = n - 45, last = n))
  message(paste("\nThe dataset is saved in your working directory:\n  ", wd_),
          appendLF = FALSE)
  message("\nDownload completed!\n")
}


#' HMD file name for a data type and interval
#' @inheritParams ReadHMD
#' @return A character string with the file name stub, without the \code{.txt}
#'   extension. \code{NULL} if the data type is not one offered by HMD.
#' @noRd
hmd_file_name <- function(what, interval) {

  if (what == "e0" & interval == "1x1") {
    which_file <- "E0per"

  } else if (what == "e0c" & interval == "1x1") {
    which_file <- "E0coh"

  } else if (what == "population" & startsWith(interval, "5")) {
    # The 5-year product is a separate file: Population5.txt, 24 rows per year
    # with the age groups 0, 1-4, 5-9, ... (verified on SWE, ACT and CAN).
    which_file <- "Population5"

  } else {
    which_file <- switch(
      what,
      births     = "Births",
      population = "Population",
      Dx_lexis   = "Deaths_lexis",
      Ex_lexis   = "Exposures_lexis",
      Dx         = paste0("Deaths_", interval),
      Ex         = paste0("Exposures_", interval),
      mx         = paste0("Mx_", interval),
      LT_f       = paste0("fltper_", interval),
      LT_m       = paste0("mltper_", interval),
      LT_t       = paste0("bltper_", interval),
      e0         = paste0("E0per_", interval),
      Exc        = paste0("cExposures_", interval),
      mxc        = paste0("cMx_", interval),
      LT_fc      = paste0("fltcoh_", interval),
      LT_mc      = paste0("mltcoh_", interval),
      LT_tc      = paste0("bltcoh_", interval),
      e0c        = paste0("E0coh_", interval)
    )
  }

  return(which_file)
}


#' Function to Download Data for a one Country
#' @inheritParams ReadHMD
#' @param country HMD country code for the selected country. Character;
#' @param session A login cookie string returned by \code{hmd_session()}, or
#'   \code{NULL} for databases that need no authentication.
#' @param link the main link to the database.
#' @return A data.frame containing demographic data, or \code{NULL} when the
#'   download fails. Failures are reported with a message, never an error.
#' @noRd
read_hmd_file <- function(what, country, interval, session = NULL, link){

  which_file <- hmd_file_name(what = what, interval = interval)

  if (is.null(which_file)) {
    message("\n", what, " is not a data type available in HMD.\n",
            "Try one of these options:\n",
            paste(hmd_indices(), collapse = ", "))
    return(NULL)
  }

  if (link %in% c("https://www.mortality.org/File/GetDocument/hmd.v6/",
                  "https://www.ipss.go.jp/p-toukei/JMD/")) {
    interlude <- "/STATS/"

  } else {
    interlude <- "/"
  }

  path <- paste0(link, country, interlude, which_file, ".txt")
  res  <- fetch_text(url = path, session = session)

  if (!is.null(res$error)) {
    message(res$error)
    return(NULL)
  }

  if (isTRUE(res$html)) {
    message("\nThe response for ", path, " is an HTML page, not a data file.",
            "\nThe login to the database failed or the session has expired.")
    return(NULL)
  }

  if (is.null(res$text)) {
    message("\nThe server returned an empty response for ", path, ".")
    return(NULL)
  }

  con <- textConnection(res$text)
  on.exit(close(con))

  dat <- tryCatch(
    read.table(con, skip = 2, header = TRUE, na.strings = "."),
    error = function(e) {
      message("\nThe ", what, " data for ", country, " in the ", interval,
              " format could not be parsed. Looked here:\n", path,
              "\nThe parser reported: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(dat)) {
    return(NULL)
  }

  out <- cbind(country, dat)

  if (any(interval %in% c("1x1", "1x5", "1x10")) &
      !any(what %in% c("births", "Dx_lexis", "Ex_lexis", "e0", "e0c")) &
      !is.null(dat$Age)) {
    # One age per row, taken as-is from the file; the open interval "110+"
    # becomes the integer 110.
    out$Age <- as.integer(sub("\\+$", "", as.character(dat$Age)))
  }

  return(out)
}


#' Country codes
#' @return A character vector with the HMD country codes.
#' @noRd
hmd_countries <- function() {
  c("AUS","AUT","BEL","BGR","BLR",
    "CAN","CHL","HRV","CHE","CZE",
    "DEUTNP","DEUTE", "DEUTW","DNK","ESP",
    "EST","FIN","FRATNP","FRACNP","GRC",
    "HUN", "HKG", "IRL","ISL", "ISR",
    "ITA","JPN","KOR","LTU","LUX",
    "LVA","NLD","NOR","NZL_NP","NZL_MA",
    "NZL_NM","POL","PRT","RUS","SVK",
    "SVN","SWE","TWN","UKR","GBR_NP",
    "GBRTENW", "GBRCENW","GBR_SCO","GBR_NIR","USA")
}

#' Data formats
#' @return A character vector with the available age and time intervals.
#' @noRd
data_format <- function() c("1x1", "1x5", "1x10", "5x1", "5x5","5x10")



#' HMD Indices
#' @return A character vector with the available data types.
#' @noRd
hmd_indices <- function() c("births", "population", "Dx_lexis", "Ex_lexis", "Dx",
                            "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0",
                            "mxc", "Exc", "LT_fc", "LT_mc", "LT_tc", "e0c")


#' Download one data file for every region of a database
#'
#' The single download loop behind \code{ReadHMD}, \code{ReadJMD},
#' \code{ReadAHMD} and \code{ReadCHMD}. It logs in once when a session is
#' supplied, walks the regions in order, and binds the returned rows into
#' one data frame. A region that cannot be downloaded is reported with a
#' message and skipped, never raised as an error.
#' @param regions Character vector of country or region codes to download.
#' @param what,interval The data type and the age/time format.
#' @param session Login cookies from \code{hmd_session}, or \code{NULL}
#'   for the databases that need no login.
#' @param link Base URL of the database, up to the part before the region
#'   code.
#' @param label Column name that carries the region or country code.
#' @param row_labels Optional vector of labels shown to the user, one per
#'   region. JMD needs it: its folders are JIS numbers, not prefecture
#'   names. Defaults to \code{regions}.
#' @param show Logical; show the progress bar and the per-region line.
#' @return A data frame with the collected rows, or \code{NULL} when
#'   nothing could be downloaded.
#' @noRd
download_regions <- function(regions,
                             what,
                             interval,
                             session = NULL,
                             link,
                             label = "country",
                             row_labels = regions,
                             show = TRUE) {

  nr <- length(regions)

  if (show) {
    pb <- startpb(0, nr + 1)
    on.exit(closepb(pb))
    setpb(pb, 0)
  }

  chunks <- vector(mode = "list", length = nr)

  for (i in seq_len(nr)) {
    if (show) {
      setpb(pb, i)
      cat(paste("      :Downloading", row_labels[i], "    "))
    }

    # Single-bracket assignment: [[i]] <- NULL would delete the slot.
    chunks[i] <- list(tryCatch(
      read_hmd_file(
        what     = what,
        country  = regions[i],
        interval = interval,
        session  = session,
        link     = link
      ),
      error = function(e) {
        message("\nThe ", what, " data for ", row_labels[i],
                " could not be read. The download reported: ",
                conditionMessage(e))
        NULL
      }
    ))

    if (!is.null(chunks[[i]])) {
      # The folder code identifies the file, not the rows.
      chunks[[i]][["country"]] <- row_labels[i]
    }
  }

  D <- bind_region_rows(chunks = chunks, label = label)

  if (show) {
    setpb(pb, nr + 1)
  }

  return(D)
}


#' Stack the per-region download results into one data frame
#'
#' The loop above collects one data frame per region. Binding them all at
#' once keeps the cost linear; growing a data frame row by row inside the
#' loop would copy the whole table on every region. Regions that failed
#' are dropped, so the caller can test for an empty result.
#' @param chunks A list of data frames, or \code{NULL} entries for the
#'   regions that could not be downloaded.
#' @param label Column name that carries the region or country code.
#' @return A data frame with the collected rows, or \code{NULL} when no
#'   chunk carried data.
#' @noRd
bind_region_rows <- function(chunks, label = "country") {
  keep <- !vapply(chunks, is.null, logical(1))

  if (!any(keep)) {
    return(NULL)
  }

  out <- do.call(what = rbind, args = chunks[keep])

  if (label != "country" && "country" %in% colnames(out)) {
    colnames(out)[colnames(out) == "country"] <- label
  }

  return(out)
}


#' Assemble the object returned by a database reader
#'
#' Builds the \code{ReadHMD}-family object out of the downloaded rows,
#' deriving the year and age coverage from them, and writes the file when
#' the user asked for a copy.
#' @param data A data frame with the downloaded rows.
#' @param input List with the input values of the reader call.
#' @param prefix File name prefix for the saved object, e.g. \code{"HMD"}.
#' @param class S3 class of the object, e.g. \code{"ReadHMD"}.
#' @param show Logical; show the progress bar and the per-region line.
#' @return A \code{ReadHMD}-family object.
#' @noRd
new_read_object <- function(data, input, prefix, class, show = TRUE) {
  out <- list(input         = input,
              data          = data,
              download.date = date(),
              years         = sort(unique(data$Year)),
              ages          = unique(data$Age))
  out <- structure(class = class, out)

  if (isTRUE(input$save)) {
    save_output(out = out, show = show, prefix = prefix)
  }

  return(out)
}


#' What age(s) are we looking at?
#' @inheritParams ReadHMD
#' @param x An object of class \code{"ReadHMD"}.
#' @return A scalar or character indicating age groups
#' @noRd
age_message <- function(what, x) {
  if (any(what %in% c("e0", "e0c"))) {
    0
    
  } else if (what %in% c("births")){
    "all ages"
    
  } else {
    paste(x$ages[1], "--", rev(x$ages)[1])
  }
}


#' Check the input common to the four database readers
#'
#' The readers share one shape of input: a single data type, a single
#' interval, and at least one region. These checks run before any HTTP
#' request, so a dead download is stopped with a clear message.
#' @param x A list with the input arguments of a reader call.
#' @param database Name of the database, used in the error messages.
#' @param what_set Character vector of the data types the database serves.
#' @return No return value, called for side effects.
#' @noRd
check_reader_input <- function(x, database, what_set) {
  if (length(x$what) != 1) {
    stop("Please specify exactly one data type in 'what'. You supplied ",
         length(x$what), " values: ", paste(x$what, collapse = ", "),
         call. = FALSE)
  }

  if (length(x$interval) != 1) {
    stop("Please specify exactly one interval. You supplied ",
         length(x$interval), " values: ", paste(x$interval, collapse = ", "),
         call. = FALSE)
  }

  if (!any(x$interval %in% data_format())) {
    stop("The interval ", x$interval, " does not exist in ", database, ". ",
         "Try one of these options:\n", paste(data_format(), collapse = ", "),
         call. = FALSE)
  }

  if (!any(x$what %in% what_set)) {
    stop(x$what, " does not exist in ", database, ". ",
         "Try one of these options:\n", paste(what_set, collapse = ", "),
         call. = FALSE)
  }

  return(invisible(NULL))
}


#' Check the regions of a database reader against the codes it serves
#'
#' @param regions Character vector of region codes supplied by the user.
#' @param known Character vector of the accepted codes.
#' @param label How the codes are called in the message, e.g. "region code".
#' @return No return value, called for side effects.
#' @noRd
check_reader_regions <- function(regions, known, label = "region code") {
  if (length(regions) == 0) {
    stop("Please specify at least one region in 'regions'.", call. = FALSE)
  }

  bad <- regions[!(regions %in% known)]

  if (length(bad) > 0) {
    stop("Unknown ", label, "(s) in 'regions': ", paste(bad, collapse = ", "),
         ".\nTry one or more of these options:\n",
         paste(known, collapse = ", "), call. = FALSE)
  }

  return(invisible(NULL))
}


#' Read the first HTML table of a page into a data frame
#'
#' Parses one simple \code{<table>} out of the page with regular
#' expressions, which keeps the package free of an HTML parsing dependency
#' for the single table it needs. The first row is the header; a row with
#' fewer cells than the header is padded with missing values, the way a
#' browsable table with an empty cell would parse. Markup inside a cell is
#' dropped, so a linked country name comes back as its text.
#' @param html Page source as a single character string.
#' @return A data frame, or \code{NULL} when the page carries no table.
#' @noRd
parse_html_table <- function(html) {
  if (is.null(html) || !grepl("<table", html, ignore.case = TRUE)) {
    return(NULL)
  }

  table_html <- regmatches(
    x = html,
    m = regexpr(pattern = "(?s)<table.*?</table>", text = html, perl = TRUE)
  )

  if (length(table_html) == 0) {
    return(NULL)
  }

  rows_html <- regmatches(
    x = table_html,
    m = gregexpr(pattern = "(?s)<tr.*?</tr>", text = table_html, perl = TRUE)
  )[[1]]

  rows <- lapply(rows_html, FUN = parse_html_row)
  rows <- rows[vapply(rows, length, integer(1)) > 0]

  if (length(rows) < 2) {
    return(NULL)
  }

  n_col <- max(vapply(rows, length, integer(1)))
  body  <- rows[-1]
  m     <- matrix(NA_character_, nrow = length(body), ncol = n_col)

  for (i in seq_along(body)) {
    m[i, seq_along(body[[i]])] <- body[[i]]
  }

  out <- as.data.frame(m, stringsAsFactors = FALSE)
  header <- trimws(gsub(pattern = "\\s+", replacement = " ", x = rows[[1]]))
  colnames(out) <- header

  return(out)
}


#' Read the cells of one table row
#'
#' @param row_html The \code{<tr>} markup of a single row.
#' @return A character vector with the cell text, tags and entities
#'   resolved.
#' @noRd
parse_html_row <- function(row_html) {
  cells <- regmatches(
    x = row_html,
    m = gregexpr(pattern = "(?s)<t[dh][^>]*>.*?</t[dh]>", text = row_html,
                 perl = TRUE)
  )[[1]]

  if (length(cells) == 0 || identical(cells, character(0))) {
    return(character(0))
  }

  out <- gsub(pattern = "(?s)<[^>]*>", replacement = "", x = cells,
              perl = TRUE)
  out <- decode_html_entities(x = out)
  out <- trimws(gsub(pattern = "\\s+", replacement = " ", x = out))

  return(out)
}


#' Resolve the few HTML entities an HMD page uses
#'
#' @param x A character vector of cell text.
#' @return The same vector with the named and numeric entities decoded.
#' @noRd
decode_html_entities <- function(x) {
  entities <- c("&amp;" = "&", "&lt;" = "<", "&gt;" = ">",
                "&quot;" = "\"", "&#39;" = "'", "&apos;" = "'",
                "&nbsp;" = " ")

  for (entity in names(entities)) {
    x <- gsub(pattern = entity, replacement = entities[[entity]], x = x,
              fixed = TRUE)
  }

  # Numeric references, e.g. &#8211; for an en dash.
  m <- gregexpr(pattern = "&#[0-9]+;", text = x)

  for (i in seq_along(x)) {
    match_i <- regmatches(x[i], m[i])[[1]]

    if (length(match_i) == 0 || identical(match_i, character(0))) {
      next
    }

    for (code_text in match_i) {
      code <- as.integer(gsub(pattern = "[^0-9]", replacement = "", x = code_text))
      x[i] <- sub(pattern = code_text,
                  replacement = intToUtf8(code),
                  x = x[i],
                  fixed = TRUE)
    }
  }

  return(x)
}
