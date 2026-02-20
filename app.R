# Wind Data Processing Shiny App
# Processes CSV/Excel/TXT files with wind speed, direction, and temperature data
# Performs hourly averaging with vector averaging for wind direction


library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(readxl)
library(shinyBS)
library(base64enc)

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
                                     minute_col = NULL, metDST) {

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

    return(as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "PST8"))
  }

  # Case 2: Single datetime column or date + time columns
  if (!is.null(date_col)) {
    datetime_str <- as.character(data[[date_col]])

    # If separate time column exists, combine them
    if (!is.null(time_col) && time_col != "" && time_col %in% names(data)) {
      datetime_str <- paste(datetime_str, as.character(data[[time_col]]))
    }
    return(parse_datetime_string(datetime_str, metDST == "LDT"))
  }

  return(NULL)
}

#' Parse datetime strings in various formats
# Fixed: removed duplicate outer function definition that caused unclosed brace syntax error
parse_datetime_string <- function(datetime_str, adjust_dst) {

  datetime_str <- trimws(datetime_str)
  n <- length(datetime_str)
  result <- rep(as.POSIXct(NA), n)

  # Detect and handle timezone abbreviations
  daylight_tz_pattern <- "-0.0{0,2}$|\\s+(PDT|EDT|CDT|MDT|ADT|AKDT|-0.0{0,2})$"
  standard_tz_pattern <- "-0.0{0,2}$|\\s+(PST|EST|CST|MST|AST|AKST|HST|UTC|GMT|-0.0{0,2})$"

  # Remove timezone abbreviations from strings before parsing
  datetime_str <- gsub(daylight_tz_pattern, "", datetime_str, ignore.case = TRUE)
  datetime_str <- gsub(standard_tz_pattern, "", datetime_str, ignore.case = TRUE)
  datetime_str <- trimws(datetime_str)

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

      parsed <- as.POSIXct(am_pm_strings, format = fmt, tz = "PST8")
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

          parsed_posix <- as.POSIXct(posix_vals, origin = "1970-01-01", tz = "PST8")
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
            parsed <- as.POSIXct(temp_str, format = fmt, tz = "PST8")

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

  # Apply daylight saving adjustment: subtract 1 hour for daylight time zones
  # This converts daylight time to standard time
  # Fixed: use any() for vector comparison instead of && with bare !is.na()
  if (adjust_dst && any(!is.na(result))) {
    result <- result - 3600
  }

  # Fix 2-digit years that weren't properly converted
  # R's %y format sometimes doesn't work as expected, so we fix years < 100
  valid_idx <- !is.na(result)
  if (any(valid_idx)) {
    years <- as.numeric(format(result[valid_idx], "%Y"))
    needs_fix <- years < 100
    if (any(needs_fix)) {
      # Convert: 0-50 -> 2000-2050, 51-99 -> 1951-1999
      fix_idx <- which(valid_idx)[needs_fix]
      for (i in fix_idx) {
        # Extract components and rebuild with correct year
        yr <- as.numeric(format(result[i], "%Y"))
        new_year <- if (yr <= 50) 2000 + yr else 1900 + yr
        # Rebuild the datetime string with correct year
        new_dt_str <- format(result[i], paste0(new_year, "-%m-%d %H:%M:%S"))
        result[i] <- as.POSIXct(new_dt_str, format = "%Y-%m-%d %H:%M:%S", tz = "PST8")
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
    data$datetime <- as.POSIXct(data$datetime, tz = "PST8")
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

  # Prepare hourly-averaged output
  output <- data.frame(
    DateTime = format(hourly_data$hour_group, "%Y-%m-%d %H:%M"),
    Wind_Speed_mph = round(hourly_data$wind_speed_mph, 2),
    Wind_Direction_deg = hourly_data$wind_dir_deg,
    stringsAsFactors = FALSE
  )

  if (has_temp) {
    output$Temperature_F <- round(hourly_data$temp_f, 1)
  }

  # Build sub-hourly (per-observation) output with converted units
  # for downstream plotting at finer time resolution
  subhourly <- data.frame(
    DateTime = format(data$datetime, "%Y-%m-%d %H:%M"),
    Wind_Speed_mph = round(convert_to_mph(data$wind_speed, speed_unit), 2),
    Wind_Direction_deg = round(data$wind_dir, 0),
    stringsAsFactors = FALSE
  )
  if (has_temp) {
    subhourly$Temperature_F <- round(convert_to_fahrenheit(data$temperature, temp_unit), 1)
  }

  # Return list so pollutant section can access both resolutions
  return(list(hourly = output, subhourly = subhourly))
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

      # Meteorological site name
      textInput("met_data_site", "Meteorological Site Name:",
                placeholder = "e.g., KVNY, KLAX"),

      hr(),

      # Meteorological data timestamp convention
      radioButtons("met_timestamp_convention",
                   tags$span("Are meteorological data timestamps at start or end of the hour?",
                             icon("info-circle", id = "met_ts_info")),
                   choices = list("Start" = "start", "End" = "end"),
                   selected = "end",
                   inline = TRUE),
      bsTooltip("met_ts_info",
                "Air quality agencies usually timestamp data at the START of the hour while meteorological data sources usually timestamp data at the END of the hour.",
                placement = "right", trigger = "hover"),


		radioButtons("met_timeZone",
						   tags$span("Meteorological data timestamps are in:",
									 icon("info-circle", id = "met_TZ_info")),
						   choices = list("Local Standard Time" = "LST", "Local Daylight Savings Time" = "LDT"),
						   selected = "LST",
						   inline = TRUE),
			  bsTooltip("met_TZ_info",
						"Cannot accept met data that are from a different timezone.",
						placement = "right", trigger = "hover"),
      hr(),

      # Long format configuration
      h4("Data Format"),
      checkboxInput("is_long_format", "Long format (parameters in rows)", value = FALSE),

      conditionalPanel(
        condition = "input.is_long_format == true",
        wellPanel(
          selectInput("param_col", "Parameter/Variable Column:", choices = NULL),
          selectInput("value_col", "Value Column:", choices = NULL),
          selectInput("unit_col", "Units Column (optional):", choices = NULL),
          selectInput("qc_col", "QC Flag Column (optional):", choices = NULL),
          uiOutput("qc_flag_selector"),
          actionButton("apply_reshape", "Apply & Reshape Data", class = "btn-info")
        )
      ),

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
        data$parsed_datetime <- parse_datetime_flexible(data, date_col = input$datetime_col, metDST = input$met_timeZone)
      } else if (input$datetime_mode == "date_time") {
        data$parsed_datetime <- parse_datetime_flexible(data,
                                                         date_col = input$date_col,
                                                         time_col = input$time_col,
														 metDST = input$met_timeZone)
      } else {
        data$parsed_datetime <- parse_datetime_flexible(data,
                                                         year_col = input$year_col,
                                                         month_col = input$month_col,
                                                         day_col = input$day_col,
                                                         hour_col = input$hour_col,
                                                         minute_col = input$minute_col,
														 metDST = input$met_timeZone)
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

      # Fixed: result is now a list; use $hourly for row count
      showNotification(sprintf("Successfully processed %d hours of data", nrow(result$hourly)), type = "message")

      # Switch to results tab
      updateTabsetPanel(session, "tabsetPanel", selected = "Processed Data")

    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
    })
  })

  # Result table
  output$result_table <- renderDT({
    req(processed_data())
    # Fixed: access $hourly since hourly_average returns a list
    datatable(processed_data()$hourly,
              options = list(scrollX = TRUE, pageLength = 25),
              caption = "Hourly averaged wind data")
  })

  # Summary statistics
  output$summary_stats <- renderPrint({
    req(processed_data())
    data <- processed_data()$hourly

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
      # Fixed: access $hourly since hourly_average now returns a list
      write.csv(processed_data()$hourly, file, row.names = FALSE)
    }
  )

  # Download handler - sub-hourly data
  output$download_subhourly <- downloadHandler(
    filename = function() {
      paste0("wind_data_subhourly_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(processed_data())
      write.csv(processed_data()$subhourly, file, row.names = FALSE)
    }
  )

  # ========================================================================
  # POLLUTANT DATA SECTION
  # ========================================================================

  pollutant_raw <- reactiveVal(NULL)
  merged_result <- reactiveVal(NULL)
  pollutant_plot_path <- reactiveVal(NULL)

  # Handle pollutant file upload
  observeEvent(input$pollutant_file, {
    req(input$pollutant_file)
    tryCatch({
      data <- read.csv(input$pollutant_file$datapath, stringsAsFactors = FALSE,
                        check.names = FALSE,
                        na.strings = c("", "NA", "N/A", "null", "-"))
      col_names <- names(data)
      col_lower <- tolower(col_names)

      # Normalize column names to canonical forms
      param_idx <- which(col_lower %in% c("parameter", "param"))
      if (length(param_idx) > 0) names(data)[param_idx[1]] <- "parameter"

      conc_idx <- which(col_lower %in% c("sample_measurement", "conc", "concentration"))
      if (length(conc_idx) > 0) names(data)[conc_idx[1]] <- "sample_measurement"

      unit_idx <- which(col_lower %in% c("units_of_measure", "units", "unit"))
      if (length(unit_idx) > 0) names(data)[unit_idx[1]] <- "units_of_measure"

      date_idx <- which(col_lower %in% c("date_lt_shifted_to_selected_timezone",
                                           "date_lt", "date"))
      if (length(date_idx) > 0) names(data)[date_idx[1]] <- "date_LT_shifted_to_selected_timezone"

      # Prefer SiteName_nopoc if available; fall back to SiteName / site_name / site
      nopoc_idx <- which(col_lower %in% c("sitename_nopoc"))
      if (length(nopoc_idx) > 0) {
        names(data)[nopoc_idx[1]] <- "SiteName"
      } else {
        site_idx <- which(col_lower %in% c("sitename", "site_name", "site"))
        if (length(site_idx) > 0) names(data)[site_idx[1]] <- "SiteName"
      }

      pollutant_raw(data)
      showNotification(sprintf("Pollutant file loaded: %d rows, %d columns",
                               nrow(data), ncol(data)), type = "message")
    }, error = function(e) {
      showNotification(paste("Error reading pollutant file:", e$message), type = "error")
    })
  })

  # SiteName selector (single selection)
  output$site_name_selector <- renderUI({
    req(pollutant_raw())
    data <- pollutant_raw()
    if (!("SiteName" %in% names(data))) return(NULL)
    sites <- sort(unique(as.character(data$SiteName)))
    sites <- sites[!is.na(sites) & sites != ""]
    selectInput("selected_site", "Select Pollutant Site:",
                choices = sites, selected = sites[1], multiple = FALSE)
  })

  # Parameter selector (multiple selection, shows units)
  output$pollutant_param_selector <- renderUI({
    req(pollutant_raw(), input$selected_site)
    data <- pollutant_raw()
    if (!("parameter" %in% names(data))) return(NULL)
    site_data <- data[data$SiteName == input$selected_site, ]
    params <- sort(unique(as.character(site_data$parameter)))
    params <- params[!is.na(params) & params != ""]
    param_labels <- sapply(params, function(p) {
      units <- unique(site_data$units_of_measure[site_data$parameter == p])
      units <- units[!is.na(units) & units != ""]
      if (length(units) > 0) paste0(p, " (", paste(units, collapse = "/"), ")") else p
    })
    checkboxGroupInput("selected_params", "Select Parameter(s):",
                        choices = setNames(params, param_labels))
  })

  # Date selector based on met data date range
  output$pollutant_date_selector <- renderUI({
    req(pollutant_raw(), input$selected_site, input$selected_params, processed_data())
    poll_data <- pollutant_raw()
    met_data <- processed_data()$hourly

    met_dates <- as.POSIXct(met_data$DateTime, format = "%Y-%m-%d %H:%M", tz = "PST8")
    met_min <- min(met_dates, na.rm = TRUE)
    met_max <- max(met_dates, na.rm = TRUE)

    site_data <- poll_data[poll_data$SiteName == input$selected_site &
                            poll_data$parameter %in% input$selected_params, ]
    site_data$parsed_date <- as.POSIXct(site_data$date_LT_shifted_to_selected_timezone, tz="PST8") # parse_datetime_string(as.character(site_data$date_LT_shifted_to_selected_timezone),adjust_dst = FALSE)

    site_data <- site_data[!is.na(site_data$parsed_date) &
                            site_data$parsed_date >= met_min &
                            site_data$parsed_date <= met_max, ]
    site_data$sample_measurement <- as.numeric(site_data$sample_measurement)
    site_data <- site_data[is.finite(site_data$sample_measurement), ]

    if (nrow(site_data) == 0) {
      return(p(em("No pollutant data with finite values in the met data date range.")))
    }

    avail_dates <- sort(unique(as.Date(site_data$parsed_date)))
    date_choices <- setNames(as.character(avail_dates),
                              format(avail_dates, "%b %d, %Y (%a)"))
    checkboxGroupInput("selected_poll_dates", "Select Date(s):",
                        choices = date_choices, selected = as.character(avail_dates))
  })

  # Process pollutant data and generate plot with wind overlay
  # Reactive helper: compute the max pollutant value across ALL sites for the
  # selected parameters and dates so every per-site plot shares the same Y-axis.
  global_y_max <- reactive({
    req(pollutant_raw(), input$selected_params, input$selected_poll_dates)
    poll_data <- pollutant_raw()
    sel_params <- input$selected_params
    sel_dates  <- as.Date(input$selected_poll_dates)

    date_min <- as.POSIXct(paste0(min(sel_dates), " 00:00"), tz = "PST8")
    date_max <- as.POSIXct(paste0(max(sel_dates), " 23:00"), tz = "PST8")
    all_hours <- seq(from = date_min, to = date_max, by = "hour")


    sub <- poll_data[poll_data$parameter %in% sel_params, ]
    sub$parsed_date <- as.POSIXct(sub$date_LT_shifted_to_selected_timezone, tz="PST8") # parse_datetime_string(as.character(sub$date_LT_shifted_to_selected_timezone),adjust_dst = FALSE)
    sub <- sub[!is.na(sub$parsed_date) &
               sub$parsed_date %in% all_hours, ]
    sub$sample_measurement <- as.numeric(sub$sample_measurement)
    sub <- sub[is.finite(sub$sample_measurement), ]

    if (nrow(sub) == 0) return(1)
    max(sub$sample_measurement, na.rm = TRUE)
  })

  # Helper function containing the plot generation logic
  generate_pollutant_plot <- function() {
    req(pollutant_raw(), processed_data(),
        input$selected_site, input$selected_params, input$selected_poll_dates)
    tryCatch({
      poll_data <- pollutant_raw()
      met_hourly <- processed_data()$hourly
      met_subhourly <- processed_data()$subhourly
      site_name <- input$selected_site
      sel_params <- input$selected_params
      sel_dates <- as.Date(input$selected_poll_dates)
	  
	# Build complete hourly time grid: midnight on first selected day
      # through 11 PM on last selected day
      date_min <- as.POSIXct(paste0(min(sel_dates), " 00:00"), tz = "PST8")
      date_max <- as.POSIXct(paste0(max(sel_dates), " 23:00"), tz = "PST8")
      all_hours <- seq(from = date_min, to = date_max, by = "hour")

      met_site <- if (!is.null(input$met_data_site) && input$met_data_site != "") {
        input$met_data_site
      } else {
        "MetSite"
      }

      # Filter pollutant data
      poll_sub <- poll_data[poll_data$SiteName == site_name &
                             poll_data$parameter %in% sel_params, ]
      poll_sub$parsed_date <- as.POSIXct(poll_sub$date_LT_shifted_to_selected_timezone, tz="PST8") # parse_datetime_string(as.character(poll_sub$date_LT_shifted_to_selected_timezone),adjust_dst = FALSE)
      poll_sub <- poll_sub[!is.na(poll_sub$parsed_date) &
                            poll_sub$parsed_date %in% all_hours, ]
      if (nrow(poll_sub) == 0) {
        showNotification("No pollutant data after filtering.", type = "error")
        return()
      }

      # Record units per parameter
      param_units <- list()
      for (p in sel_params) {
        u <- unique(poll_sub$units_of_measure[poll_sub$parameter == p])
        u <- u[!is.na(u) & u != ""]
        param_units[[p]] <- if (length(u) > 0) u[1] else ""
      }

      poll_sub$sample_measurement <- as.numeric(poll_sub$sample_measurement)

      # Reshape pollutant to wide format (one row per hour)
      reshape_df <- data.frame(
        date = poll_sub$parsed_date,
        parameter = poll_sub$parameter,
        sample_measurement = poll_sub$sample_measurement,
        stringsAsFactors = FALSE
      )

      reshape_df$date_hour <- floor_date(reshape_df$date, unit = "hour")
      wide_poll <- reshape_df %>%
        group_by(date_hour, parameter) %>%
        summarise(sample_measurement = mean(sample_measurement, na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = parameter, values_from = sample_measurement) %>%
        as.data.frame()

      # Build complete hourly time grid: midnight on first selected day
      # through 11 PM on last selected day
      date_min <- as.POSIXct(paste0(min(sel_dates), " 00:00"), tz = "PST8")
      date_max <- as.POSIXct(paste0(max(sel_dates), " 23:00"), tz = "PST8")
      all_hours <- seq(from = date_min, to = date_max, by = "hour")
      time_grid <- data.frame(date_hour = all_hours, stringsAsFactors = FALSE)

      # Left-join pollutant data onto grid (gaps remain NA)
      poll_grid <- merge(time_grid, wide_poll, by = "date_hour", all.x = TRUE)
      poll_grid <- poll_grid[order(poll_grid$date_hour), ]
      n_hours <- nrow(poll_grid)

      if (n_hours == 0) {
        showNotification("No data to plot.", type = "error")
        return()
      }

      # Prepare wind data: prefer sub-hourly, fall back to hourly
      # Parse wind datetimes
      wind_df <- NULL

      if (!is.null(met_subhourly) && nrow(met_subhourly) > 0) {
        sh <- met_subhourly
        sh$date <- as.POSIXct(sh$DateTime, format = "%Y-%m-%d %H:%M", tz = "PST8")
        sh <- sh[sh$date >= date_min & sh$date <= (date_max + 3600), ]
        if (nrow(sh) > 0) {
          wind_df <- data.frame(date = sh$date, stringsAsFactors = FALSE)
          if ("Wind_Gust_mph" %in% names(sh))
            wind_df$gust <- as.numeric(sh$Wind_Gust_mph)
          if ("Wind_Speed_mph" %in% names(sh))
            wind_df$ws <- as.numeric(sh$Wind_Speed_mph)
          wind_df <- wind_df[order(wind_df$date), ]
        }
      }

      # Fall back to hourly if sub-hourly not available or empty
      if (is.null(wind_df) || nrow(wind_df) == 0) {
        hr <- met_hourly
        hr$date <- as.POSIXct(hr$DateTime, format = "%Y-%m-%d %H:%M", tz = "PST8")
        hr <- hr[hr$date >= date_min & hr$date <= (date_max + 3600), ]
        if (nrow(hr) > 0) {
          wind_df <- data.frame(date = hr$date, stringsAsFactors = FALSE)
          if ("Wind_Gust_mph" %in% names(hr))
            wind_df$gust <- as.numeric(hr$Wind_Gust_mph)
          if ("Wind_Speed_mph" %in% names(hr))
            wind_df$ws <- as.numeric(hr$Wind_Speed_mph)
          wind_df <- wind_df[order(wind_df$date), ]
        }
      }

      # Compute fractional positions for wind data on the hourly grid
      # Each hour bar occupies [i-1, i) on the x-axis (0-indexed from left)
      # Wind points map: x = (date - date_min) / 3600
      if (!is.null(wind_df) && nrow(wind_df) > 0) {
        wind_df$x <- as.numeric(difftime(wind_df$date, date_min, units = "hours"))
      }

      # Store for preview table
      merged_result(poll_grid)

      # Event label for title
      evt1 <- paste(format(date_min, "%b %d"), "-",
                    format(date_max, "%b %d, %Y"))

      # Plot filename
      safe_site <- gsub("[^A-Za-z0-9_-]", "_", site_name)
      safe_met <- gsub("[^A-Za-z0-9_-]", "_", met_site)
      plot_filename <- paste0(safe_site, "_", safe_met, "_",
                              paste(sel_params, collapse = "_"), ".png")
      plot_path <- file.path(tempdir(), plot_filename)

      # Identify PM10 / PM2.5 columns in the wide pollutant grid
      pm10_col <- NULL; pm25_col <- NULL
      for (cn in names(poll_grid)) {
        if (grepl("^PM10$|^PM10 ", cn, ignore.case = TRUE) && is.null(pm10_col))
          pm10_col <- cn
        if (grepl("^PM2\\.5$|^PM2\\.5 ", cn, ignore.case = TRUE) && is.null(pm25_col))
          pm25_col <- cn
      }

      # Build bar values vectors (NA stays NA for gaps)
      if (!is.null(pm10_col)) {
        bar_primary <- as.numeric(poll_grid[[pm10_col]])
      } else {
        bar_primary <- as.numeric(poll_grid[[sel_params[1]]])
      }

      bar_secondary <- NULL
      if (!is.null(pm10_col) && !is.null(pm25_col)) {
        bar_secondary <- as.numeric(poll_grid[[pm25_col]])
      } else if (is.null(pm10_col) && length(sel_params) > 1 &&
                 sel_params[2] %in% names(poll_grid)) {
        bar_secondary <- as.numeric(poll_grid[[sel_params[2]]])
      }

      # Determine labels and colors
      if (!is.null(pm10_col)) {
        concSpec <- "PM10"
        txtCols <- rgb(0, 0, 0.8, alpha = 0.7)
        y_label <- expression(paste("PM, ", mu, "g/m"^3))
      } else {
        concSpec <- sel_params[1]
        txtCols <- rgb(0, 0, 0.8, alpha = 0.7)
        y_label <- if (param_units[[sel_params[1]]] != "") {
          paste0(sel_params[1], ", ", param_units[[sel_params[1]]])
        } else sel_params[1]
      }

      if (!is.null(bar_secondary)) {
        sec_name <- if (!is.null(pm25_col)) "PM2.5" else sel_params[2]
        concSpec <- c(concSpec, sec_name)
        txtCols <- c(txtCols, rgb(0.5, 0.8, 1, alpha = 0.8))
      }

      # Y-axis max for bars — use global max across ALL sites (with 1.4x buffer)
      g_max <- global_y_max()
      y_max <- g_max * 1.4
      if (!is.finite(y_max) || y_max == 0) y_max <- 1

      # Wind y-axis max
      wind_max <- 1
      if (!is.null(wind_df) && nrow(wind_df) > 0) {
        gust_vals <- if ("gust" %in% names(wind_df)) wind_df$gust else NA
        ws_vals <- if ("ws" %in% names(wind_df)) wind_df$ws else NA
        wind_max <- max(c(gust_vals, ws_vals), na.rm = TRUE)
        if (!is.finite(wind_max) || wind_max == 0) wind_max <- 1
      }

      # --- Generate the PNG ---
      png(plot_path, width = 1980, height = 1200, pointsize = 24)
      par(mar = c(8, 8, 3, 8), mgp = c(5, 2, 0))

      # Replace NA with 0 for barplot (barplot can't handle NA heights),
      # but track which are true gaps
      bar_primary_plot <- ifelse(is.na(bar_primary), 0, bar_primary)

      # Use numeric barplot: bars at positions 0.5, 1.5, ..., n-0.5
      bp <- barplot(bar_primary_plot, space = 0, border = FALSE,
                    col = ifelse(is.na(bar_primary), "transparent", txtCols[1]),
                    xlab = "", ylab = y_label,
                    main = paste(site_name, evt1),
                    cex.lab = 3, cex.axis = 3, xaxt = "n",
                    ylim = c(0, y_max), cex.main = 3,
                    xlim = c(0, n_hours))

      # X-axis tick labels at regular intervals
      tick_seq <- seq(1, n_hours, by = max(1, floor(n_hours / 6)))
      axis(1, at = tick_seq - 0.5,
           labels = format(poll_grid$date_hour[tick_seq], "%b %d\n%I %p"),
           cex.axis = 2, las = 2)

      # Overlay secondary bars if present
      if (!is.null(bar_secondary)) {
        bar_sec_plot <- ifelse(is.na(bar_secondary), 0, bar_secondary)
        barplot(bar_sec_plot, space = c(0.5, rep(1, n_hours - 1)),
                width = rep(0.5, n_hours), border = FALSE,
                col = ifelse(is.na(bar_secondary), "transparent", txtCols[2]),
                names.arg = rep("", n_hours),
                xlab = "", ylab = "", main = "", axes = FALSE, add = TRUE)
      }

      # Overlay wind data as lines on right axis
      par(new = TRUE)
      plot(NULL, xlim = c(0, n_hours), ylim = c(0, wind_max * 1.1),
           xlab = "", ylab = "", axes = FALSE, type = "n")

      if (!is.null(wind_df) && nrow(wind_df) > 0) {
        # Plot gust line with gaps (NA produces gaps in type="l")
        if ("gust" %in% names(wind_df)) {
          lines(wind_df$x, wind_df$gust, col = 6, lwd = 3, type = "l")
        }
        # Plot avg wind speed line with gaps
        if ("ws" %in% names(wind_df)) {
          lines(wind_df$x, wind_df$ws, col = 1, lwd = 3, type = "l")
        }
      }

      axis(4, cex.axis = 2, col = 6, col.ticks = 6, col.axis = 6)

      # Wind threshold
      abline(h = 25, lty = 3, lwd = 3, col = 1)

      # Build legend
      leg_fill <- c(txtCols, NA, NA, NA)
      leg_lty <- c(rep(NA, length(txtCols)), 1, 1, 3)
      leg_lwd <- c(rep(NA, length(txtCols)), 3, 3, 3)
      leg_col <- c(rep(NA, length(txtCols)), 6, 1, 1)
      leg_txtcol <- c(txtCols, 6, 1, 1)
      gust_label <- paste0("Max gust @ ", met_site)
      ws_label <- paste0("Avg wind @ ", met_site)
      leg_labels <- c(concSpec, gust_label, ws_label, "Wind threshold")

      # Dynamically choose number of legend columns based on label widths
      # to avoid overflow when site names or labels are long
      max_label_chars <- max(nchar(leg_labels))
      n_items <- length(leg_labels)
      leg_ncol <- if (max_label_chars > 30 || n_items > 6) {
        1
      } else if (max_label_chars > 18 || n_items > 4) {
        2
      } else {
        3
      }

      legend("topleft", ncol = leg_ncol,
             fill = leg_fill, lty = leg_lty, border = FALSE,
             lwd = leg_lwd, col = leg_col, text.col = leg_txtcol,
             legend = leg_labels,
             box.lty = 0, cex = 2.5, bg = "transparent",
             x.intersp = 0.6, seg.len = 0.6)

      box()
      mtext("Wind Speed (mph)", side = 4, line = 4, cex = 2.5, col = 1)
      dev.off()

      pollutant_plot_path(plot_path)
      showNotification("Plot generated successfully!", type = "message")
      updateTabsetPanel(session, "tabsetPanel", selected = "Pollutant Plot")

    }, error = function(e) {
      showNotification(paste("Error processing pollutant data:", e$message),
                       type = "error", duration = 10)
    })
  }

  # Trigger plot generation from the button
  observeEvent(input$process_pollutant, {
    generate_pollutant_plot()
  })

  # Auto-regenerate plot when site, parameters, or dates change
  observeEvent(list(input$selected_site, input$selected_params, input$selected_poll_dates), {
    # Only auto-regenerate if the plot has been generated at least once
    req(pollutant_plot_path())
    generate_pollutant_plot()
  })

  # Render the pollutant plot as base64 image
  output$pollutant_plot_ui <- renderUI({
    req(pollutant_plot_path())
    plot_path <- pollutant_plot_path()
    if (!file.exists(plot_path)) return(NULL)
    raw_data <- readBin(plot_path, "raw", file.info(plot_path)$size)
    b64 <- base64encode(raw_data)
    tags$img(src = paste0("data:image/png;base64,", b64),
             style = "max-width:100%; height:auto;")
  })

  # Merged data preview table
  output$merged_data_table <- renderDT({
    req(merged_result())
    datatable(merged_result(),
              options = list(scrollX = TRUE, pageLength = 25),
              caption = "Merged meteorological + pollutant data")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
