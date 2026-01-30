# Wind Data Processing Shiny App
# Processes CSV/Excel/TXT files with wind speed, direction, and temperature data
# Performs hourly averaging with vector averaging for wind direction

library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(readxl)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Detect delimiter in a text file
detect_delimiter <- function(file_path, n_lines = 10) {
  # Read first few lines
  lines <- readLines(file_path, n = n_lines, warn = FALSE)
  if (length(lines) == 0) return(",")

  # Common delimiters to check
  delimiters <- c(",", "\t", ";", "|", " ")
  delimiter_names <- c("comma", "tab", "semicolon", "pipe", "space")

  # Count occurrences of each delimiter
  counts <- sapply(delimiters, function(d) {
    # Count in each line and check consistency
    line_counts <- sapply(lines, function(l) length(gregexpr(d, l, fixed = TRUE)[[1]]))
    # Filter out -1 (no match)
    line_counts[line_counts < 0] <- 0
    # Return median count (most consistent)
    if (all(line_counts == 0)) return(0)
    median(line_counts)
  })

  # Pick delimiter with highest consistent count
  if (max(counts) == 0) return(",")  # default to comma
  best_idx <- which.max(counts)
  return(delimiters[best_idx])
}

#' Read data file (CSV, Excel, or delimited text)
read_data_file <- function(file_path, file_ext) {
  file_ext <- tolower(file_ext)

  if (file_ext %in% c("xls", "xlsx")) {
    # Read Excel file
    data <- read_excel(file_path, na = c("", "NA", "N/A", "null", "-"))
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else if (file_ext == "csv") {
    # Read CSV file
    data <- read.csv(file_path, stringsAsFactors = FALSE,
                     check.names = FALSE, na.strings = c("", "NA", "N/A", "null", "-"))
  } else {
    # TXT or other - detect delimiter
    delimiter <- detect_delimiter(file_path)
    data <- read.delim(file_path, sep = delimiter, stringsAsFactors = FALSE,
                       check.names = FALSE, na.strings = c("", "NA", "N/A", "null", "-"))
  }

  return(data)
}

#' Parse various date/time formats into POSIXct
#' Handles: POSIX, mm-dd-YY, dd-mm-YYYY, ISO 8601, separate columns, etc.
parse_datetime_flexible <- function(data, date_col = NULL, time_col = NULL,
                                     year_col = NULL, month_col = NULL,
                                     day_col = NULL, hour_col = NULL,
                                     minute_col = NULL) {

  # Case 1: Separate date component columns
  if (!is.null(year_col) && !is.null(month_col) && !is.null(day_col) && !is.null(hour_col)) {
    year_vals <- data[[year_col]]
    month_vals <- data[[month_col]]
    day_vals <- data[[day_col]]
    hour_vals <- data[[hour_col]]
    minute_vals <- if (!is.null(minute_col) && minute_col != "") data[[minute_col]] else rep(0, nrow(data))

    # Handle 2-digit years
    year_vals <- as.numeric(year_vals)
    year_vals <- ifelse(year_vals < 100, ifelse(year_vals > 50, 1900 + year_vals, 2000 + year_vals), year_vals)

    datetime_str <- sprintf("%04d-%02d-%02d %02d:%02d:00",
                            as.numeric(year_vals),
                            as.numeric(month_vals),
                            as.numeric(day_vals),
                            as.numeric(hour_vals),
                            as.numeric(minute_vals))
    return(as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT"))
  }

  # Case 2: Single datetime column or date + time columns
  if (!is.null(date_col)) {
    datetime_str <- as.character(data[[date_col]])

    # If separate time column exists, combine them
    if (!is.null(time_col) && time_col != "" && time_col %in% names(data)) {
      datetime_str <- paste(datetime_str, as.character(data[[time_col]]))
    }

    return(parse_datetime_string(datetime_str))
  }

  return(NULL)
}

#' Parse datetime strings in various formats
parse_datetime_string <- function(datetime_str) {
  datetime_str <- trimws(datetime_str)
  n <- length(datetime_str)
  result <- rep(as.POSIXct(NA), n)

  # Try each format pattern
  formats_to_try <- list(
    # ISO 8601 formats
    list(pattern = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z?$",
         format = "%Y-%m-%dT%H:%M:%S"),
    list(pattern = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}[+-]\\d{2}:\\d{2}$",
         format = "%Y-%m-%dT%H:%M:%S"),

    # YYYY-mm-dd HH:MM:SS
    list(pattern = "^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}\\s+\\d{1,2}:\\d{2}:\\d{2}$",
         format = c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S")),

    # YYYY-mm-dd HH:MM
    list(pattern = "^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}\\s+\\d{1,2}:\\d{2}$",
         format = c("%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M")),

    # mm-dd-YY HH:MM:SS or mm-dd-YYYY HH:MM:SS
    list(pattern = "^\\d{1,2}[-/]\\d{1,2}[-/]\\d{2,4}\\s+\\d{1,2}:\\d{2}:\\d{2}$",
         format = c("%m-%d-%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S",
                    "%m-%d-%y %H:%M:%S", "%m/%d/%y %H:%M:%S")),

    # mm-dd-YY HH:MM
    list(pattern = "^\\d{1,2}[-/]\\d{1,2}[-/]\\d{2,4}\\s+\\d{1,2}:\\d{2}$",
         format = c("%m-%d-%Y %H:%M", "%m/%d/%Y %H:%M",
                    "%m-%d-%y %H:%M", "%m/%d/%y %H:%M")),

    # mm dd YY HH:MM:SS (space separated)
    list(pattern = "^\\d{1,2}\\s+\\d{1,2}\\s+\\d{2,4}\\s+\\d{1,2}:\\d{2}:\\d{2}$",
         format = c("%m %d %Y %H:%M:%S", "%m %d %y %H:%M:%S")),

    # ddmmYYYYHHMMSS (no separators)
    list(pattern = "^\\d{14}$",
         format = "%d%m%Y%H%M%S"),

    # mmddYYYYHHMMSS (no separators, US format)
    list(pattern = "^\\d{14}$",
         format = "%m%d%Y%H%M%S"),

    # ddmmYYYYHHMM (no separators, no seconds)
    list(pattern = "^\\d{12}$",
         format = c("%d%m%Y%H%M", "%m%d%Y%H%M")),

    # POSIX timestamp (numeric)
    list(pattern = "^\\d{9,10}$",
         format = "posix")
  )

  # First check for AM/PM format
  am_pm_idx <- grepl("(AM|PM|am|pm)", datetime_str, ignore.case = TRUE)
  if (any(am_pm_idx)) {
    am_pm_strings <- datetime_str[am_pm_idx]
    # Try common AM/PM formats
    am_pm_formats <- c("%m-%d-%Y %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p",
                       "%m-%d-%y %I:%M:%S %p", "%m/%d/%y %I:%M:%S %p",
                       "%m-%d-%Y %I:%M %p", "%m/%d/%Y %I:%M %p",
                       "%m-%d-%y %I:%M %p", "%m/%d/%y %I:%M %p",
                       "%Y-%m-%d %I:%M:%S %p", "%Y/%m/%d %I:%M:%S %p",
                       "%Y-%m-%d %I:%M %p", "%Y/%m/%d %I:%M %p")
    for (fmt in am_pm_formats) {
      parsed <- as.POSIXct(am_pm_strings, format = fmt, tz = "Etc/GMT")
      if (sum(!is.na(parsed)) > sum(!is.na(result[am_pm_idx]))) {
        result[am_pm_idx] <- parsed
      }
    }
  }

  # Process non-AM/PM strings
  remaining_idx <- is.na(result)
  if (any(remaining_idx)) {
    remaining_str <- datetime_str[remaining_idx]

    for (fmt_info in formats_to_try) {
      if (fmt_info$format[1] == "posix") {
        # Handle POSIX timestamps
        numeric_idx <- grepl("^\\d{9,10}$", remaining_str)
        if (any(numeric_idx)) {
          posix_vals <- as.numeric(remaining_str[numeric_idx])
          parsed_posix <- as.POSIXct(posix_vals, origin = "1970-01-01", tz = "Etc/GMT")
          temp_result <- result[remaining_idx]
          temp_result[numeric_idx] <- parsed_posix
          result[remaining_idx] <- temp_result
        }
      } else {
        # Try each format string
        for (fmt in fmt_info$format) {
          still_na <- is.na(result[remaining_idx])
          if (any(still_na)) {
            temp_str <- remaining_str[still_na]
            # Remove 'Z' suffix for ISO format
            temp_str <- gsub("Z$", "", temp_str)
            # Remove timezone offset
            temp_str <- gsub("[+-]\\d{2}:\\d{2}$", "", temp_str)
            parsed <- as.POSIXct(temp_str, format = fmt, tz = "Etc/GMT")

            temp_result <- result[remaining_idx]
            temp_still_na <- is.na(temp_result)
            temp_result[temp_still_na] <- parsed
            result[remaining_idx] <- temp_result
            remaining_idx <- is.na(result)
            remaining_str <- datetime_str[remaining_idx]
          }
        }
      }
    }
  }

  return(result)
}

#' Detect units from column name
detect_units <- function(col_name, data_values = NULL) {
  col_lower <- tolower(col_name)

  # Wind speed units
  if (grepl("mph|mi.*h|mile", col_lower)) return(list(type = "speed", unit = "mph"))
  if (grepl("km.*h|kmh|kph", col_lower)) return(list(type = "speed", unit = "kmh"))
  if (grepl("m.*s|ms|mps|meter.*sec", col_lower)) return(list(type = "speed", unit = "ms"))
  if (grepl("knot|kt|kn", col_lower)) return(list(type = "speed", unit = "knots"))
  if (grepl("ft.*s|fps", col_lower)) return(list(type = "speed", unit = "fts"))


  # Temperature units
  if (grepl("\\(f\\)|_f$|fahrenheit|deg.*f|\\[f\\]", col_lower)) return(list(type = "temp", unit = "F"))
  if (grepl("\\(c\\)|_c$|celsius|deg.*c|\\[c\\]|centigrade", col_lower)) return(list(type = "temp", unit = "C"))
  if (grepl("\\(k\\)|_k$|kelvin|deg.*k|\\[k\\]", col_lower)) return(list(type = "temp", unit = "K"))

  # If speed/temp keyword found but no unit, try to infer from values
  if (grepl("speed|wind.*spd|ws|wspd", col_lower)) {
    if (!is.null(data_values)) {
      median_val <- median(as.numeric(data_values), na.rm = TRUE)
      # Heuristic: m/s typically < 30, mph typically < 100, km/h typically < 150
      if (!is.na(median_val)) {
        if (median_val < 25) return(list(type = "speed", unit = "ms"))
        if (median_val < 80) return(list(type = "speed", unit = "mph"))
        return(list(type = "speed", unit = "kmh"))
      }
    }
    return(list(type = "speed", unit = "unknown"))
  }

  if (grepl("temp|tmp|air.*t|t_air", col_lower)) {
    if (!is.null(data_values)) {
      median_val <- median(as.numeric(data_values), na.rm = TRUE)
      # Heuristic: Celsius typically -40 to 50, Fahrenheit -40 to 120
      if (!is.na(median_val)) {
        if (median_val > 200) return(list(type = "temp", unit = "K"))
        if (median_val > 50) return(list(type = "temp", unit = "F"))
        return(list(type = "temp", unit = "C"))
      }
    }
    return(list(type = "temp", unit = "unknown"))
  }

  if (grepl("dir|wdir|wd|direction|heading|bearing", col_lower)) {
    return(list(type = "direction", unit = "degrees"))
  }

  return(list(type = "unknown", unit = "unknown"))
}

#' Convert wind speed to mph
convert_to_mph <- function(value, from_unit) {
  switch(tolower(from_unit),
         "mph" = value,
         "ms" = value * 2.23694,
         "m/s" = value * 2.23694,
         "kmh" = value * 0.621371,
         "km/h" = value * 0.621371,
         "kph" = value * 0.621371,
         "knots" = value * 1.15078,
         "kn" = value * 1.15078,
         "kt" = value * 1.15078,
         "fts" = value * 0.681818,
         "ft/s" = value * 0.681818,
         value  # default: assume already mph or unknown
  )
}

#' Convert temperature to Fahrenheit
convert_to_fahrenheit <- function(value, from_unit) {
  switch(toupper(from_unit),
         "F" = value,
         "C" = value * 9/5 + 32,
         "K" = (value - 273.15) * 9/5 + 32,
         value  # default: assume already F or unknown
  )
}

#' Vector average for wind direction
#' Uses unit vector decomposition method
vector_average_direction <- function(directions, speeds = NULL) {
  # Remove NAs
  valid_idx <- !is.na(directions)
  if (sum(valid_idx) == 0) return(NA)

  dirs <- directions[valid_idx]

  # Convert to radians (meteorological convention: 0 = N, 90 = E)
  dirs_rad <- dirs * pi / 180

  # If speeds provided, use speed-weighted averaging
  if (!is.null(speeds)) {
    spds <- speeds[valid_idx]
    valid_spd <- !is.na(spds) & spds > 0
    if (sum(valid_spd) == 0) {
      # Fall back to unweighted if no valid speeds
      u <- mean(sin(dirs_rad))
      v <- mean(cos(dirs_rad))
    } else {
      dirs_rad <- dirs_rad[valid_spd]
      spds <- spds[valid_spd]
      u <- sum(spds * sin(dirs_rad)) / sum(spds)
      v <- sum(spds * cos(dirs_rad)) / sum(spds)
    }
  } else {
    u <- mean(sin(dirs_rad))
    v <- mean(cos(dirs_rad))
  }

  # Convert back to degrees
  avg_dir <- atan2(u, v) * 180 / pi

  # Normalize to 0-360
  if (avg_dir < 0) avg_dir <- avg_dir + 360

  return(avg_dir)
}

#' Perform hourly averaging on the dataset
hourly_average <- function(data, datetime_col, speed_col, dir_col, temp_col = NULL,
                           speed_unit, temp_unit = NULL) {

  # Ensure datetime is POSIXct
  data$datetime <- data[[datetime_col]]
  if (!inherits(data$datetime, "POSIXct")) {
    data$datetime <- as.POSIXct(data$datetime, tz = "Etc/GMT")
  }

  # Create hour floor for grouping
  data$hour_group <- floor_date(data$datetime, unit = "hour")

  # Get numeric values
  data$wind_speed <- as.numeric(data[[speed_col]])
  data$wind_dir <- as.numeric(data[[dir_col]])

  if (!is.null(temp_col) && temp_col != "" && temp_col %in% names(data)) {
    data$temperature <- as.numeric(data[[temp_col]])
    has_temp <- TRUE
  } else {
    has_temp <- FALSE
  }

  # Group by hour and calculate averages
  hourly_data <- data %>%
    group_by(hour_group) %>%
    summarise(
      wind_speed_avg = mean(wind_speed, na.rm = TRUE),
      wind_dir_avg = vector_average_direction(wind_dir, wind_speed),
      temp_avg = if (has_temp) mean(temperature, na.rm = TRUE) else NA_real_,
      n_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(hour_group)

  # Convert units
  hourly_data$wind_speed_mph <- convert_to_mph(hourly_data$wind_speed_avg, speed_unit)

  if (has_temp && !is.null(temp_unit)) {
    hourly_data$temp_f <- convert_to_fahrenheit(hourly_data$temp_avg, temp_unit)
  } else {
    hourly_data$temp_f <- NA_real_
  }

  # Round direction to nearest degree
  hourly_data$wind_dir_deg <- round(hourly_data$wind_dir_avg, 0)

  # Prepare output (no POSIX timestamp, just formatted datetime)
  output <- data.frame(
    DateTime = format(hourly_data$hour_group, "%Y-%m-%d %H:%M"),
    Wind_Speed_mph = round(hourly_data$wind_speed_mph, 2),
    Wind_Direction_deg = hourly_data$wind_dir_deg,
    stringsAsFactors = FALSE
  )

  if (has_temp) {
    output$Temperature_F <- round(hourly_data$temp_f, 1)
  }

  return(output)
}

# ============================================================================
# SHINY UI
# ============================================================================

ui <- fluidPage(
  titlePanel("Wind Data Processor"),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # File upload
      fileInput("file", "Upload Data File",
                accept = c(".csv", ".txt", ".xls", ".xlsx",
                           "text/csv", "text/plain",
                           "application/vnd.ms-excel",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),

      hr(),

      # Date/Time configuration
      h4("Date/Time Configuration"),

      radioButtons("datetime_mode", "Date/Time Format:",
                   choices = list(
                     "Single datetime column" = "single",
                     "Separate date and time columns" = "date_time",
                     "Separate component columns (Y, M, D, H, min)" = "components"
                   ),
                   selected = "single"),

      conditionalPanel(
        condition = "input.datetime_mode == 'single'",
        selectInput("datetime_col", "DateTime Column:", choices = NULL)
      ),

      conditionalPanel(
        condition = "input.datetime_mode == 'date_time'",
        selectInput("date_col", "Date Column:", choices = NULL),
        selectInput("time_col", "Time Column:", choices = NULL)
      ),

      conditionalPanel(
        condition = "input.datetime_mode == 'components'",
        selectInput("year_col", "Year Column:", choices = NULL),
        selectInput("month_col", "Month Column:", choices = NULL),
        selectInput("day_col", "Day Column:", choices = NULL),
        selectInput("hour_col", "Hour Column:", choices = NULL),
        selectInput("minute_col", "Minute Column (optional):", choices = NULL)
      ),

      hr(),

      # Wind speed configuration
      h4("Wind Speed"),
      selectInput("speed_col", "Wind Speed Column:", choices = NULL),
      selectInput("speed_unit", "Speed Units:",
                  choices = list(
                    "mph (miles per hour)" = "mph",
                    "m/s (meters per second)" = "ms",
                    "km/h (kilometers per hour)" = "kmh",
                    "knots" = "knots",
                    "ft/s (feet per second)" = "fts"
                  ),
                  selected = "mph"),

      hr(),

      # Wind direction configuration
      h4("Wind Direction"),
      selectInput("dir_col", "Wind Direction Column:", choices = NULL),

      hr(),

      # Temperature configuration (optional)
      h4("Temperature (Optional)"),
      selectInput("temp_col", "Temperature Column:", choices = NULL),
      selectInput("temp_unit", "Temperature Units:",
                  choices = list(
                    "Fahrenheit (°F)" = "F",
                    "Celsius (°C)" = "C",
                    "Kelvin (K)" = "K"
                  ),
                  selected = "F"),

      hr(),

      actionButton("process", "Process Data", class = "btn-primary btn-lg"),

      hr(),

      downloadButton("download", "Download Results")
    ),

    mainPanel(
      width = 8,

      tabsetPanel(
        tabPanel("Preview",
                 h4("Uploaded Data Preview"),
                 DTOutput("preview_table"),
                 hr(),
                 verbatimTextOutput("parse_info")
        ),

        tabPanel("Processed Data",
                 h4("Hourly Averaged Data"),
                 p("Wind direction: vector averaged | Wind speed & temperature: scalar averaged"),
                 DTOutput("result_table"),
                 hr(),
                 h4("Summary Statistics"),
                 verbatimTextOutput("summary_stats")
        ),

        tabPanel("Help",
                 h4("Supported File Formats"),
                 tags$ul(
                   tags$li("CSV - comma-separated values"),
                   tags$li("TXT - auto-detects delimiter (comma, tab, semicolon, pipe, space)"),
                   tags$li("XLS - Excel 97-2003 format"),
                   tags$li("XLSX - Excel 2007+ format")
                 ),
                 h4("Supported Date/Time Formats"),
                 tags$ul(
                   tags$li("POSIX timestamp (Unix epoch seconds)"),
                   tags$li("ISO 8601: YYYY-MM-DDTHH:MM:SSZ"),
                   tags$li("US format: MM-DD-YY HH:MM:SS or MM-DD-YYYY HH:MM:SS"),
                   tags$li("Separators: dashes (-), slashes (/), spaces, or none"),
                   tags$li("Compact: ddmmYYYYHHMMSS or mmddYYYYHHMMSS"),
                   tags$li("12-hour format with AM/PM"),
                   tags$li("Separate columns for year, month, day, hour, minute")
                 ),
                 h4("Wind Speed Units Supported"),
                 tags$ul(
                   tags$li("mph - miles per hour"),
                   tags$li("m/s - meters per second"),
                   tags$li("km/h - kilometers per hour"),
                   tags$li("knots"),
                   tags$li("ft/s - feet per second")
                 ),
                 h4("Temperature Units Supported"),
                 tags$ul(
                   tags$li("Fahrenheit (°F)"),
                   tags$li("Celsius (°C)"),
                   tags$li("Kelvin (K)")
                 ),
                 h4("Averaging Methods"),
                 tags$ul(
                   tags$li(strong("Wind Direction:"), " Vector averaging using u/v component decomposition"),
                   tags$li(strong("Wind Speed:"), " Scalar (arithmetic) mean"),
                   tags$li(strong("Temperature:"), " Scalar (arithmetic) mean")
                 ),
                 h4("Output"),
                 p("The processed data includes:"),
                 tags$ul(
                   tags$li("DateTime (YYYY-MM-DD HH:MM)"),
                   tags$li("Wind speed in mph"),
                   tags$li("Wind direction in degrees (0-360)"),
                   tags$li("Temperature in Fahrenheit (if provided)")
                 ),
                 p(em("Note: All times are treated as local standard time year-round (no DST adjustment)."))
        )
      )
    )
  )
)

# ============================================================================
# SHINY SERVER
# ============================================================================

server <- function(input, output, session) {

  # Reactive value to store uploaded data
  uploaded_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)

  # Handle file upload
  observeEvent(input$file, {
    req(input$file)

    # Get file extension
    file_ext <- tools::file_ext(input$file$name)

    # Read the file based on type
    tryCatch({
      data <- read_data_file(input$file$datapath, file_ext)
      uploaded_data(data)

      col_names <- names(data)
      col_choices <- setNames(col_names, col_names)
      col_choices_optional <- c("(none)" = "", col_choices)

      # Update all select inputs with column names
      updateSelectInput(session, "datetime_col", choices = col_choices, selected = col_names[1])
      updateSelectInput(session, "date_col", choices = col_choices, selected = col_names[1])
      updateSelectInput(session, "time_col", choices = col_choices_optional)

      updateSelectInput(session, "year_col", choices = col_choices)
      updateSelectInput(session, "month_col", choices = col_choices)
      updateSelectInput(session, "day_col", choices = col_choices)
      updateSelectInput(session, "hour_col", choices = col_choices)
      updateSelectInput(session, "minute_col", choices = col_choices_optional)

      updateSelectInput(session, "speed_col", choices = col_choices)
      updateSelectInput(session, "dir_col", choices = col_choices)
      updateSelectInput(session, "temp_col", choices = col_choices_optional)

      # Try to auto-detect columns based on names
      for (i in seq_along(col_names)) {
        col_name <- col_names[i]
        detected <- detect_units(col_name, data[[col_name]])

        if (detected$type == "speed") {
          updateSelectInput(session, "speed_col", selected = col_name)
          if (detected$unit != "unknown") {
            unit_map <- c("mph" = "mph", "ms" = "ms", "m/s" = "ms",
                          "kmh" = "kmh", "km/h" = "kmh", "kph" = "kmh",
                          "knots" = "knots", "kn" = "knots", "kt" = "knots",
                          "fts" = "fts", "ft/s" = "fts")
            if (detected$unit %in% names(unit_map)) {
              updateSelectInput(session, "speed_unit", selected = unit_map[detected$unit])
            }
          }
        }

        if (detected$type == "direction") {
          updateSelectInput(session, "dir_col", selected = col_name)
        }

        if (detected$type == "temp") {
          updateSelectInput(session, "temp_col", selected = col_name)
          if (detected$unit != "unknown") {
            updateSelectInput(session, "temp_unit", selected = detected$unit)
          }
        }
      }

      # Try to auto-detect datetime columns
      for (col_name in col_names) {
        col_lower <- tolower(col_name)
        if (grepl("date|time|timestamp|datetime|dt", col_lower)) {
          if (grepl("time", col_lower) && !grepl("date|stamp", col_lower)) {
            updateSelectInput(session, "time_col", selected = col_name)
          } else {
            updateSelectInput(session, "datetime_col", selected = col_name)
            updateSelectInput(session, "date_col", selected = col_name)
          }
        }
        if (grepl("^year$|^yr$|^yyyy$", col_lower)) {
          updateSelectInput(session, "year_col", selected = col_name)
        }
        if (grepl("^month$|^mon$|^mm$", col_lower)) {
          updateSelectInput(session, "month_col", selected = col_name)
        }
        if (grepl("^day$|^dd$", col_lower)) {
          updateSelectInput(session, "day_col", selected = col_name)
        }
        if (grepl("^hour$|^hr$|^hh$", col_lower)) {
          updateSelectInput(session, "hour_col", selected = col_name)
        }
        if (grepl("^minute$|^min$", col_lower)) {
          updateSelectInput(session, "minute_col", selected = col_name)
        }
      }

    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })

  # Preview table
  output$preview_table <- renderDT({
    req(uploaded_data())
    datatable(head(uploaded_data(), 100),
              options = list(scrollX = TRUE, pageLength = 10),
              caption = "First 100 rows of uploaded data")
  })

  # Parse info
  output$parse_info <- renderPrint({
    req(uploaded_data())
    data <- uploaded_data()
    cat("File Information:\n")
    cat(sprintf("  Rows: %d\n", nrow(data)))
    cat(sprintf("  Columns: %d\n", ncol(data)))
    cat("\nColumn Names:\n")
    for (col in names(data)) {
      cat(sprintf("  - %s\n", col))
    }
  })

  # Process data
  observeEvent(input$process, {
    req(uploaded_data())

    data <- uploaded_data()

    tryCatch({
      # Parse datetime based on selected mode
      if (input$datetime_mode == "single") {
        data$parsed_datetime <- parse_datetime_flexible(data, date_col = input$datetime_col)
      } else if (input$datetime_mode == "date_time") {
        data$parsed_datetime <- parse_datetime_flexible(data,
                                                         date_col = input$date_col,
                                                         time_col = input$time_col)
      } else {
        data$parsed_datetime <- parse_datetime_flexible(data,
                                                         year_col = input$year_col,
                                                         month_col = input$month_col,
                                                         day_col = input$day_col,
                                                         hour_col = input$hour_col,
                                                         minute_col = input$minute_col)
      }

      # Check if parsing was successful
      valid_dates <- sum(!is.na(data$parsed_datetime))
      if (valid_dates == 0) {
        showNotification("Could not parse any dates. Please check the date/time format.", type = "error")
        return()
      }

      if (valid_dates < nrow(data)) {
        showNotification(sprintf("Warning: %d of %d rows had unparseable dates",
                                 nrow(data) - valid_dates, nrow(data)), type = "warning")
      }

      # Filter out rows with invalid dates
      data <- data[!is.na(data$parsed_datetime), ]

      # Perform hourly averaging
      result <- hourly_average(
        data = data,
        datetime_col = "parsed_datetime",
        speed_col = input$speed_col,
        dir_col = input$dir_col,
        temp_col = if (input$temp_col != "") input$temp_col else NULL,
        speed_unit = input$speed_unit,
        temp_unit = if (input$temp_col != "") input$temp_unit else NULL
      )

      processed_data(result)

      showNotification(sprintf("Successfully processed %d hours of data", nrow(result)), type = "message")

      # Switch to results tab
      updateTabsetPanel(session, "tabsetPanel", selected = "Processed Data")

    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
    })
  })

  # Result table
  output$result_table <- renderDT({
    req(processed_data())
    datatable(processed_data(),
              options = list(scrollX = TRUE, pageLength = 25),
              caption = "Hourly averaged wind data")
  })

  # Summary statistics
  output$summary_stats <- renderPrint({
    req(processed_data())
    data <- processed_data()

    cat("Summary Statistics:\n")
    cat(sprintf("  Total hours: %d\n", nrow(data)))
    cat(sprintf("  Date range: %s to %s\n", min(data$DateTime), max(data$DateTime)))
    cat("\nWind Speed (mph):\n")
    cat(sprintf("  Min: %.2f\n", min(data$Wind_Speed_mph, na.rm = TRUE)))
    cat(sprintf("  Max: %.2f\n", max(data$Wind_Speed_mph, na.rm = TRUE)))
    cat(sprintf("  Mean: %.2f\n", mean(data$Wind_Speed_mph, na.rm = TRUE)))
    cat("\nWind Direction (degrees):\n")
    cat(sprintf("  Most common quadrant: %s\n",
                c("N", "E", "S", "W")[which.max(table(cut(data$Wind_Direction_deg,
                                                          breaks = c(0, 90, 180, 270, 360),
                                                          labels = c("N", "E", "S", "W"),
                                                          include.lowest = TRUE)))]))

    if ("Temperature_F" %in% names(data) && sum(!is.na(data$Temperature_F)) > 0) {
      cat("\nTemperature (°F):\n")
      cat(sprintf("  Min: %.1f\n", min(data$Temperature_F, na.rm = TRUE)))
      cat(sprintf("  Max: %.1f\n", max(data$Temperature_F, na.rm = TRUE)))
      cat(sprintf("  Mean: %.1f\n", mean(data$Temperature_F, na.rm = TRUE)))
    }
  })

  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0("wind_data_hourly_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(processed_data())
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
