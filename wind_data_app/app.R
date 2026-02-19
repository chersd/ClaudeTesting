# Wind Data Processing Shiny App
# Processes CSV/Excel/TXT files with wind speed, direction, and temperature data
# Performs hourly averaging with vector averaging for wind direction

library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(readxl)
library(tidyr)
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

#' Remove comment lines from a file and return clean temp file path
remove_comments <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)

  # Remove lines starting with comment characters (after trimming whitespace)
  comment_patterns <- c("^\\s*#", "^\\s*\\*", "^\\s*//", "^\\s*;\\s*[^0-9]")

  clean_lines <- lines
  for (pattern in comment_patterns) {
    clean_lines <- clean_lines[!grepl(pattern, clean_lines)]
  }

  # Remove empty lines at start and end
  while (length(clean_lines) > 0 && trimws(clean_lines[1]) == "") {
    clean_lines <- clean_lines[-1]
  }
  while (length(clean_lines) > 0 && trimws(clean_lines[length(clean_lines)]) == "") {
    clean_lines <- clean_lines[-length(clean_lines)]
  }

  # Write to temp file
  temp_file <- tempfile(fileext = ".txt")
  writeLines(clean_lines, temp_file)
  return(temp_file)
}

#' Read data file (CSV, Excel, or delimited text)
read_data_file <- function(file_path, file_ext) {
  file_ext <- tolower(file_ext)

  if (file_ext %in% c("xls", "xlsx")) {
    # Read Excel file (comments handled differently - skip rows starting with #)
    data <- read_excel(file_path, na = c("", "NA", "N/A", "null", "-"))
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    # Remove any rows where first column starts with comment char
    if (nrow(data) > 0 && ncol(data) > 0) {
      first_col <- as.character(data[[1]])
      comment_rows <- grepl("^\\s*[#*]", first_col)
      data <- data[!comment_rows, , drop = FALSE]
    }
  } else if (file_ext == "csv") {
    # Remove comments first
    clean_file <- remove_comments(file_path)
    data <- read.csv(clean_file, stringsAsFactors = FALSE,
                     check.names = FALSE, na.strings = c("", "NA", "N/A", "null", "-"))
    unlink(clean_file)
  } else {
    # TXT or other - remove comments and detect delimiter
    clean_file <- remove_comments(file_path)
    delimiter <- detect_delimiter(clean_file)
    data <- read.delim(clean_file, sep = delimiter, stringsAsFactors = FALSE,
                       check.names = FALSE, na.strings = c("", "NA", "N/A", "null", "-"))
    unlink(clean_file)
  }

  return(data)
}

#' Detect if data is in long format
#' Returns TRUE if a parameter/variable column is detected
detect_long_format <- function(data) {
  col_names_lower <- tolower(names(data))

  # Look for parameter/variable column indicators
  param_patterns <- c("param", "variable", "var_name", "measure", "metric",
                      "indicator", "pollutant", "species", "analyte")

  for (pattern in param_patterns) {
    if (any(grepl(pattern, col_names_lower))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Find likely column for a given purpose in long-format data
find_likely_column <- function(col_names, patterns) {
  col_names_lower <- tolower(col_names)
  for (pattern in patterns) {
    matches <- grep(pattern, col_names_lower, value = FALSE)
    if (length(matches) > 0) {
      return(col_names[matches[1]])
    }
  }
  return(NULL)
}

#' Reshape long format data to wide format
reshape_long_to_wide <- function(data, datetime_col, param_col, value_col,
                                  unit_col = NULL, qc_col = NULL, valid_qc_flags = NULL) {

  # Filter by QC flags if specified
  if (!is.null(qc_col) && qc_col != "" && !is.null(valid_qc_flags) && length(valid_qc_flags) > 0) {
    data <- data[data[[qc_col]] %in% valid_qc_flags, , drop = FALSE]
  }

  # Create unique parameter names (include units if available)
  if (!is.null(unit_col) && unit_col != "" && unit_col %in% names(data)) {
    # Combine parameter and unit for column names
    data$param_with_unit <- paste0(data[[param_col]], "_", data[[unit_col]])
  } else {
    data$param_with_unit <- data[[param_col]]
  }

  # Keep only needed columns
  cols_to_keep <- c(datetime_col, "param_with_unit", value_col)
  data_subset <- data[, cols_to_keep, drop = FALSE]
  names(data_subset) <- c("datetime", "parameter", "value")

  # Convert value to numeric
  data_subset$value <- as.numeric(data_subset$value)

  # Pivot to wide format
  wide_data <- data_subset %>%
    group_by(datetime, parameter) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = parameter, values_from = value)

  # Rename datetime column back
  names(wide_data)[1] <- datetime_col

  return(as.data.frame(wide_data))
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
    return(as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = ""))
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

  # Detect and handle timezone abbreviations
  # Daylight time zones need -1 hour adjustment to convert to standard time
  daylight_tz_pattern <- "\\s+(PDT|EDT|CDT|MDT|ADT|AKDT)$"
  standard_tz_pattern <- "\\s+(PST|EST|CST|MST|AST|AKST|HST|UTC|GMT)$"

  # Track which entries need daylight adjustment
  needs_dst_adjustment <- grepl(daylight_tz_pattern, datetime_str, ignore.case = TRUE)

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
      parsed <- as.POSIXct(am_pm_strings, format = fmt, tz = "")
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
          parsed_posix <- as.POSIXct(posix_vals, origin = "1970-01-01", tz = "")
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
            parsed <- as.POSIXct(temp_str, format = fmt, tz = "")

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
  if (any(needs_dst_adjustment & !is.na(result))) {
    result[needs_dst_adjustment] <- result[needs_dst_adjustment] - 3600  # subtract 1 hour (3600 seconds)
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
        result[i] <- as.POSIXct(new_dt_str, format = "%Y-%m-%d %H:%M:%S", tz = "")
      }
    }
  }

  return(result)
}

#' Detect units from column name
detect_units <- function(col_name, data_values = NULL) {
  col_lower <- tolower(col_name)

  # Wind gust detection (check before general speed)
  if (grepl("gust|gst|peak.*wind|max.*wind|wind.*max|wind.*peak", col_lower)) {
    # Check for units in gust column name
    if (grepl("mph|mi.*h|mile", col_lower)) return(list(type = "gust", unit = "mph"))
    if (grepl("km.*h|kmh|kph", col_lower)) return(list(type = "gust", unit = "kmh"))
    if (grepl("m.*s|ms|mps|meter.*sec", col_lower)) return(list(type = "gust", unit = "ms"))
    if (grepl("knot|kt|kn", col_lower)) return(list(type = "gust", unit = "knots"))
    if (grepl("ft.*s|fps", col_lower)) return(list(type = "gust", unit = "fts"))
    return(list(type = "gust", unit = "unknown"))
  }

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
                           gust_col = NULL, speed_unit, gust_unit = NULL, temp_unit = NULL) {

  # Ensure datetime is POSIXct
  data$datetime <- data[[datetime_col]]
  if (!inherits(data$datetime, "POSIXct")) {
    data$datetime <- as.POSIXct(data$datetime, tz = "")
  }

  # Create hour floor for grouping
  data$hour_group <- floor_date(data$datetime, unit = "hour")

  # Get numeric values - handle case where speed_col might be empty (gust only)
  has_speed <- !is.null(speed_col) && speed_col != "" && speed_col %in% names(data)
  if (has_speed) {
    data$wind_speed <- as.numeric(data[[speed_col]])
  } else {
    data$wind_speed <- NA_real_
  }

  # Handle direction - might not exist if only gust data
  has_dir <- !is.null(dir_col) && dir_col != "" && dir_col %in% names(data)
  if (has_dir) {
    data$wind_dir <- as.numeric(data[[dir_col]])
  } else {
    data$wind_dir <- NA_real_
  }

  # Handle gust data
  has_gust <- !is.null(gust_col) && gust_col != "" && gust_col %in% names(data)
  if (has_gust) {
    data$wind_gust <- as.numeric(data[[gust_col]])
  }

  # Handle temperature
  has_temp <- !is.null(temp_col) && temp_col != "" && temp_col %in% names(data)
  if (has_temp) {
    data$temperature <- as.numeric(data[[temp_col]])
  }

  # Group by hour and calculate averages
  hourly_data <- data %>%
    group_by(hour_group) %>%
    summarise(
      wind_speed_avg = if (has_speed) mean(wind_speed, na.rm = TRUE) else NA_real_,
      wind_dir_avg = if (has_dir && has_speed) vector_average_direction(wind_dir, wind_speed)
                     else if (has_dir) vector_average_direction(wind_dir) else NA_real_,
      gust_max = if (has_gust) max(wind_gust, na.rm = TRUE) else NA_real_,
      temp_avg = if (has_temp) mean(temperature, na.rm = TRUE) else NA_real_,
      n_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(hour_group)

  # Handle infinite values from max() on empty data
  if (has_gust) {
    hourly_data$gust_max[is.infinite(hourly_data$gust_max)] <- NA_real_
  }

  # Convert units
  if (has_speed) {
    hourly_data$wind_speed_mph <- convert_to_mph(hourly_data$wind_speed_avg, speed_unit)
  }

  if (has_gust) {
    gust_unit_to_use <- if (!is.null(gust_unit) && gust_unit != "") gust_unit else speed_unit
    hourly_data$gust_mph <- convert_to_mph(hourly_data$gust_max, gust_unit_to_use)
  }

  if (has_temp && !is.null(temp_unit)) {
    hourly_data$temp_f <- convert_to_fahrenheit(hourly_data$temp_avg, temp_unit)
  }

  # Round direction to nearest degree
  if (has_dir) {
    hourly_data$wind_dir_deg <- round(hourly_data$wind_dir_avg, 0)
  }

  # Calculate u and v wind vector components (using output speed in mph)
  # u = east-west component (positive = wind from west)
  # v = north-south component (positive = wind from south)
  if (has_speed && has_dir) {
    dir_rad <- hourly_data$wind_dir_avg * pi / 180
    hourly_data$u_mph <- hourly_data$wind_speed_mph * sin(dir_rad)
    hourly_data$v_mph <- hourly_data$wind_speed_mph * cos(dir_rad)
  }

  # Prepare hourly output (no POSIX timestamp, just formatted datetime)
  output <- data.frame(
    DateTime = format(hourly_data$hour_group, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )

  # Add columns based on what data is available
  if (has_speed) {
    output$Wind_Speed_mph <- round(hourly_data$wind_speed_mph, 2)
  }

  if (has_dir) {
    output$Wind_Direction_deg <- hourly_data$wind_dir_deg
  }

  # Add u and v components if both speed and direction are available
  if (has_speed && has_dir) {
    output$U_mph <- round(hourly_data$u_mph, 3)
    output$V_mph <- round(hourly_data$v_mph, 3)
  }

  if (has_gust) {
    output$Wind_Gust_mph <- round(hourly_data$gust_mph, 2)
  }

  if (has_temp) {
    output$Temperature_F <- round(hourly_data$temp_f, 1)
  }

  # Prepare sub-hourly output with consistent column names, units, and date format
  subhourly <- data.frame(
    DateTime = format(data$datetime, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )

  if (has_speed) {
    subhourly$Wind_Speed_mph <- round(convert_to_mph(data$wind_speed, speed_unit), 2)
  }

  if (has_dir) {
    subhourly$Wind_Direction_deg <- round(data$wind_dir, 0)
  }

  if (has_speed && has_dir) {
    dir_rad_sub <- data$wind_dir * pi / 180
    speed_mph_sub <- convert_to_mph(data$wind_speed, speed_unit)
    subhourly$U_mph <- round(speed_mph_sub * sin(dir_rad_sub), 3)
    subhourly$V_mph <- round(speed_mph_sub * cos(dir_rad_sub), 3)
  }

  if (has_gust) {
    gust_unit_to_use <- if (!is.null(gust_unit) && gust_unit != "") gust_unit else speed_unit
    subhourly$Wind_Gust_mph <- round(convert_to_mph(data$wind_gust, gust_unit_to_use), 2)
  }

  if (has_temp && !is.null(temp_unit)) {
    subhourly$Temperature_F <- round(convert_to_fahrenheit(data$temperature, temp_unit), 1)
  }

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
      selectInput("speed_col", "Wind Speed Column (optional if gust provided):", choices = NULL),
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

      # Wind gust configuration (optional)
      h4("Wind Gust (Optional)"),
      selectInput("gust_col", "Wind Gust Column:", choices = NULL),
      uiOutput("gust_unit_ui"),

      hr(),

      # Temperature configuration (optional)
      h4("Temperature (Optional)"),
      selectInput("temp_col", "Temperature Column:", choices = NULL),
      uiOutput("temp_unit_ui"),

      hr(),

      actionButton("process", "Process Data", class = "btn-primary btn-lg"),

      hr(),

      downloadButton("download", "Download Hourly Results"),
      downloadButton("download_subhourly", "Download Sub-Hourly Results"),

      hr(),

      # ---- Pollutant Data Section ----
      h3("Pollutant Data"),

      fileInput("pollutant_file", "Upload Pollutant CSV (Long Format)",
                accept = c(".csv", "text/csv")),

      uiOutput("site_name_selector"),
      uiOutput("pollutant_param_selector"),
      uiOutput("pollutant_date_selector"),

      actionButton("process_pollutant", "Process & Plot Pollutant Data",
                    class = "btn-success btn-lg")
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
                 h4("Sub-Hourly Data (Standardized)"),
                 p("Original time resolution with consistent column names, date format, and units (mph / °F)"),
                 DTOutput("subhourly_table"),
                 hr(),
                 h4("Summary Statistics"),
                 verbatimTextOutput("summary_stats")
        ),

        tabPanel("Pollutant Plot",
                 h4("Pollutant vs Wind Data"),
                 uiOutput("pollutant_plot_ui"),
                 hr(),
                 h4("Merged Data Preview"),
                 DTOutput("merged_data_table")
        ),

        tabPanel("Help",
                 h4("Supported File Formats"),
                 tags$ul(
                   tags$li("CSV - comma-separated values"),
                   tags$li("TXT - auto-detects delimiter (comma, tab, semicolon, pipe, space)"),
                   tags$li("XLS - Excel 97-2003 format"),
                   tags$li("XLSX - Excel 2007+ format")
                 ),
                 p(em("Comment lines starting with #, *, or // are automatically ignored.")),

                 h4("Long Format Data"),
                 p("If your data is in long format (parameters in rows rather than columns):"),
                 tags$ul(
                   tags$li("Check 'Long format' checkbox"),
                   tags$li("Select the Parameter/Variable column (contains variable names)"),
                   tags$li("Select the Value column (contains measurements)"),
                   tags$li("Optionally select Units column"),
                   tags$li("Optionally select QC Flag column and choose which flags are valid"),
                   tags$li("Click 'Apply & Reshape Data' to convert to wide format")
                 ),

                 h4("Supported Date/Time Formats"),
                 tags$ul(
                   tags$li("POSIX timestamp (Unix epoch seconds)"),
                   tags$li("ISO 8601: YYYY-MM-DDTHH:MM:SSZ"),
                   tags$li("US format: MM-DD-YY HH:MM:SS or MM-DD-YYYY HH:MM:SS"),
                   tags$li("Separators: dashes (-), slashes (/), spaces, or none"),
                   tags$li("Compact: ddmmYYYYHHMMSS or mmddYYYYHHMMSS"),
                   tags$li("12-hour format with AM/PM"),
                   tags$li("Separate columns for year, month, day, hour, minute"),
                   tags$li("Timezone abbreviations: PST, EST, CST, MST, PDT, EDT, CDT, MDT, etc.")
                 ),
                 p(em("Daylight time zones (PDT, EDT, CDT, MDT) are automatically converted to standard time.")),
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
                   tags$li(strong("Wind Gust:"), " Maximum value per hour"),
                   tags$li(strong("Temperature:"), " Scalar (arithmetic) mean")
                 ),
                 h4("Output"),
                 p("The processed data includes (columns shown only if data available):"),
                 tags$ul(
                   tags$li("DateTime (YYYY-MM-DD HH:MM)"),
                   tags$li("Wind speed in mph"),
                   tags$li("Wind direction in degrees (0-360)"),
                   tags$li("U_mph - east-west wind vector component (positive = from west)"),
                   tags$li("V_mph - north-south wind vector component (positive = from south)"),
                   tags$li("Wind gust in mph (hourly maximum)"),
                   tags$li("Temperature in Fahrenheit")
                 ),
                 p(em("Note: U and V components are included when both speed and direction are available.")),
                 p(em("At least wind speed OR wind gust must be provided. Temperature and direction are optional.")),
                 p(em("All output times are in local standard time. Daylight time inputs are adjusted automatically."))
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

      updateSelectInput(session, "speed_col", choices = col_choices_optional)
      updateSelectInput(session, "dir_col", choices = col_choices_optional)
      updateSelectInput(session, "gust_col", choices = col_choices_optional)
      updateSelectInput(session, "temp_col", choices = col_choices_optional)

      # Track detected units for gust and temp
      detected_gust_unit <- NULL
      detected_temp_unit <- NULL

      # Try to auto-detect columns based on names
      for (i in seq_along(col_names)) {
        col_name <- col_names[i]
        detected <- detect_units(col_name, data[[col_name]])

        unit_map <- c("mph" = "mph", "ms" = "ms", "m/s" = "ms",
                      "kmh" = "kmh", "km/h" = "kmh", "kph" = "kmh",
                      "knots" = "knots", "kn" = "knots", "kt" = "knots",
                      "fts" = "fts", "ft/s" = "fts")

        if (detected$type == "speed") {
          updateSelectInput(session, "speed_col", selected = col_name)
          if (detected$unit != "unknown" && detected$unit %in% names(unit_map)) {
            updateSelectInput(session, "speed_unit", selected = unit_map[detected$unit])
          }
        }

        if (detected$type == "gust") {
          updateSelectInput(session, "gust_col", selected = col_name)
          if (detected$unit != "unknown" && detected$unit %in% names(unit_map)) {
            detected_gust_unit <<- unit_map[detected$unit]
          }
        }

        if (detected$type == "direction") {
          updateSelectInput(session, "dir_col", selected = col_name)
        }

        if (detected$type == "temp") {
          updateSelectInput(session, "temp_col", selected = col_name)
          if (detected$unit != "unknown") {
            detected_temp_unit <<- detected$unit
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

      # Setup long-format selectors
      updateSelectInput(session, "param_col", choices = col_choices)
      updateSelectInput(session, "value_col", choices = col_choices)
      updateSelectInput(session, "unit_col", choices = col_choices_optional)
      updateSelectInput(session, "qc_col", choices = col_choices_optional)

      # Auto-detect long format and pre-select likely columns
      is_long <- detect_long_format(data)
      updateCheckboxInput(session, "is_long_format", value = is_long)

      if (is_long) {
        # Try to find parameter column
        param_match <- find_likely_column(col_names, c("param", "variable", "var_name",
                                                        "measure", "pollutant", "analyte"))
        if (!is.null(param_match)) {
          updateSelectInput(session, "param_col", selected = param_match)
        }

        # Try to find value column
        value_match <- find_likely_column(col_names, c("value", "result", "concentration",
                                                        "reading", "measurement", "data"))
        if (!is.null(value_match)) {
          updateSelectInput(session, "value_col", selected = value_match)
        }

        # Try to find unit column
        unit_match <- find_likely_column(col_names, c("unit", "uom", "units"))
        if (!is.null(unit_match)) {
          updateSelectInput(session, "unit_col", selected = unit_match)
        }

        # Try to find QC column
        qc_match <- find_likely_column(col_names, c("qc", "flag", "quality", "valid",
                                                     "status", "qualifier"))
        if (!is.null(qc_match)) {
          updateSelectInput(session, "qc_col", selected = qc_match)
        }
      }

    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })

  # Reactive for working data (original or reshaped from long format)
  working_data <- reactiveVal(NULL)

  # Keep working_data in sync with uploaded_data for wide format

  observeEvent(uploaded_data(), {
    if (!input$is_long_format) {
      working_data(uploaded_data())
    }
  })

  # Dynamic QC flag selector based on selected QC column
  output$qc_flag_selector <- renderUI({
    req(uploaded_data(), input$qc_col)
    if (input$qc_col == "") return(NULL)

    data <- uploaded_data()
    if (!(input$qc_col %in% names(data))) return(NULL)

    # Get unique QC flag values
    qc_values <- unique(as.character(data[[input$qc_col]]))
    qc_values <- qc_values[!is.na(qc_values) & qc_values != ""]
    qc_values <- sort(qc_values)

    if (length(qc_values) == 0) return(NULL)

    checkboxGroupInput("valid_qc_flags", "Select Valid QC Flags:",
                       choices = qc_values,
                       selected = qc_values)  # Default: all selected
  })

  # Conditional gust units selector - only show if gust column is selected
  output$gust_unit_ui <- renderUI({
    req(input$gust_col)
    if (input$gust_col == "") return(NULL)

    selectInput("gust_unit", "Gust Units:",
                choices = list(
                  "Same as wind speed" = "",
                  "mph (miles per hour)" = "mph",
                  "m/s (meters per second)" = "ms",
                  "km/h (kilometers per hour)" = "kmh",
                  "knots" = "knots",
                  "ft/s (feet per second)" = "fts"
                ),
                selected = "")
  })

  # Conditional temperature units selector - only show if temp column is selected
  output$temp_unit_ui <- renderUI({
    req(input$temp_col)
    if (input$temp_col == "") return(NULL)

    selectInput("temp_unit", "Temperature Units:",
                choices = list(
                  "Fahrenheit (°F)" = "F",
                  "Celsius (°C)" = "C",
                  "Kelvin (K)" = "K"
                ),
                selected = "F")
  })

  # Handle reshape button for long format data
  observeEvent(input$apply_reshape, {
    req(uploaded_data(), input$is_long_format)
    req(input$param_col, input$value_col, input$datetime_col)

    data <- uploaded_data()

    tryCatch({
      # Determine which datetime column to use based on mode
      datetime_col <- switch(input$datetime_mode,
                             "single" = input$datetime_col,
                             "date_time" = input$date_col,
                             "components" = input$year_col)

      # Get valid QC flags
      valid_qc <- if (!is.null(input$valid_qc_flags)) input$valid_qc_flags else NULL

      # Reshape from long to wide
      wide_data <- reshape_long_to_wide(
        data = data,
        datetime_col = datetime_col,
        param_col = input$param_col,
        value_col = input$value_col,
        unit_col = if (input$unit_col != "") input$unit_col else NULL,
        qc_col = if (input$qc_col != "") input$qc_col else NULL,
        valid_qc_flags = valid_qc
      )

      # Store reshaped data
      working_data(wide_data)

      # Update column selectors with new wide-format columns
      col_names <- names(wide_data)
      col_choices <- setNames(col_names, col_names)
      col_choices_optional <- c("(none)" = "", col_choices)

      updateSelectInput(session, "datetime_col", choices = col_choices, selected = col_names[1])
      updateSelectInput(session, "date_col", choices = col_choices, selected = col_names[1])
      updateSelectInput(session, "speed_col", choices = col_choices_optional)
      updateSelectInput(session, "dir_col", choices = col_choices_optional)
      updateSelectInput(session, "gust_col", choices = col_choices_optional)
      updateSelectInput(session, "temp_col", choices = col_choices_optional)

      # Try to auto-detect columns in reshaped data
      for (col_name in col_names) {
        detected <- detect_units(col_name, wide_data[[col_name]])
        if (detected$type == "speed") {
          updateSelectInput(session, "speed_col", selected = col_name)
        }
        if (detected$type == "gust") {
          updateSelectInput(session, "gust_col", selected = col_name)
        }
        if (detected$type == "direction") {
          updateSelectInput(session, "dir_col", selected = col_name)
        }
        if (detected$type == "temp") {
          updateSelectInput(session, "temp_col", selected = col_name)
        }
      }

      showNotification(sprintf("Reshaped data: %d rows, %d columns", nrow(wide_data), ncol(wide_data)),
                       type = "message")

    }, error = function(e) {
      showNotification(paste("Error reshaping data:", e$message), type = "error")
    })
  })

  # Preview table - show working data if available, otherwise uploaded data
  output$preview_table <- renderDT({
    # Prefer working_data if available (reshaped), otherwise show uploaded
    data_to_show <- working_data()
    if (is.null(data_to_show)) {
      data_to_show <- uploaded_data()
    }
    req(data_to_show)

    caption_text <- if (!is.null(working_data()) && input$is_long_format) {
      "Reshaped data (wide format) - First 100 rows"
    } else {
      "First 100 rows of uploaded data"
    }

    datatable(head(data_to_show, 100),
              options = list(scrollX = TRUE, pageLength = 10),
              caption = caption_text)
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
    # Use working_data if available (reshaped long format), otherwise uploaded_data
    data <- working_data()
    if (is.null(data)) {
      data <- uploaded_data()
    }

    # Validation: Check if data exists
    if (is.null(data) || nrow(data) == 0) {
      showNotification("No data loaded. Please upload a valid data file.", type = "error")
      return()
    }

    # Validation: Check if we have at least speed OR gust data
    has_speed <- !is.null(input$speed_col) && input$speed_col != "" && input$speed_col %in% names(data)
    has_gust <- !is.null(input$gust_col) && input$gust_col != "" && input$gust_col %in% names(data)

    if (!has_speed && !has_gust) {
      showNotification("Warning: No wind speed or gust column selected. Please select at least one wind data column.",
                       type = "error")
      return()
    }

    # Validation: Check if selected columns exist in data
    col_names <- names(data)
    warnings_list <- c()

    if (has_speed && !all(sapply(data[[input$speed_col]], function(x) is.na(x) || is.numeric(as.numeric(x))))) {
      warnings_list <- c(warnings_list, "Wind speed column contains non-numeric values")
    }

    if (has_gust && !all(sapply(data[[input$gust_col]], function(x) is.na(x) || is.numeric(as.numeric(x))))) {
      warnings_list <- c(warnings_list, "Wind gust column contains non-numeric values")
    }

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
        showNotification("Could not parse any dates. Please check the date/time column selection and format. This may not be the correct input file.",
                         type = "error", duration = 10)
        return()
      }

      if (valid_dates < nrow(data)) {
        pct_failed <- round((nrow(data) - valid_dates) / nrow(data) * 100, 1)
        showNotification(sprintf("Warning: %d of %d rows (%.1f%%) had unparseable dates",
                                 nrow(data) - valid_dates, nrow(data), pct_failed),
                         type = "warning", duration = 8)
      }

      # Filter out rows with invalid dates
      data <- data[!is.na(data$parsed_datetime), ]

      # Additional validation after filtering
      if (nrow(data) == 0) {
        showNotification("No valid data rows remaining after date parsing. Please check if this is the correct file.",
                         type = "error")
        return()
      }

      # Perform hourly averaging
      result <- hourly_average(
        data = data,
        datetime_col = "parsed_datetime",
        speed_col = if (has_speed) input$speed_col else NULL,
        dir_col = input$dir_col,
        temp_col = if (!is.null(input$temp_col) && input$temp_col != "") input$temp_col else NULL,
        gust_col = if (has_gust) input$gust_col else NULL,
        speed_unit = input$speed_unit,
        gust_unit = if (has_gust && !is.null(input$gust_unit) && input$gust_unit != "") input$gust_unit else NULL,
        temp_unit = if (!is.null(input$temp_col) && input$temp_col != "" && !is.null(input$temp_unit)) input$temp_unit else NULL
      )

      # Check if result has any data columns beyond DateTime
      if (ncol(result$hourly) <= 1) {
        showNotification("Warning: No wind data columns in output. Please verify column selections.",
                         type = "warning")
      }

      processed_data(result)

      # Build success message (inspect the hourly data frame)
      hourly_result <- result$hourly
      data_types <- c()
      if ("Wind_Speed_mph" %in% names(hourly_result)) data_types <- c(data_types, "speed")
      if ("Wind_Direction_deg" %in% names(hourly_result)) data_types <- c(data_types, "direction")
      if ("Wind_Gust_mph" %in% names(hourly_result)) data_types <- c(data_types, "gust")
      if ("Temperature_F" %in% names(hourly_result)) data_types <- c(data_types, "temperature")

      showNotification(sprintf("Successfully processed %d hours of data (%s)",
                               nrow(hourly_result), paste(data_types, collapse = ", ")),
                       type = "message")

      # Switch to results tab
      updateTabsetPanel(session, "tabsetPanel", selected = "Processed Data")

    }, error = function(e) {
      showNotification(paste("Error processing data. Please check if this is the correct file format:", e$message),
                       type = "error", duration = 10)
    })
  })

  # Result table (hourly)
  output$result_table <- renderDT({
    req(processed_data())
    datatable(processed_data()$hourly,
              options = list(scrollX = TRUE, pageLength = 25),
              caption = "Hourly averaged wind data")
  })

  # Sub-hourly result table
  output$subhourly_table <- renderDT({
    req(processed_data())
    datatable(processed_data()$subhourly,
              options = list(scrollX = TRUE, pageLength = 25),
              caption = "Sub-hourly data (original resolution, standardized columns and units)")
  })

  # Summary statistics
  output$summary_stats <- renderPrint({
    req(processed_data())
    data <- processed_data()$hourly

    cat("Summary Statistics:\n")
    cat(sprintf("  Total hours: %d\n", nrow(data)))
    cat(sprintf("  Date range: %s to %s\n", min(data$DateTime), max(data$DateTime)))

    if ("Wind_Speed_mph" %in% names(data) && sum(!is.na(data$Wind_Speed_mph)) > 0) {
      cat("\nWind Speed (mph):\n")
      cat(sprintf("  Min: %.2f\n", min(data$Wind_Speed_mph, na.rm = TRUE)))
      cat(sprintf("  Max: %.2f\n", max(data$Wind_Speed_mph, na.rm = TRUE)))
      cat(sprintf("  Mean: %.2f\n", mean(data$Wind_Speed_mph, na.rm = TRUE)))
    }

    if ("Wind_Direction_deg" %in% names(data) && sum(!is.na(data$Wind_Direction_deg)) > 0) {
      cat("\nWind Direction (degrees):\n")
      dir_table <- table(cut(data$Wind_Direction_deg,
                             breaks = c(0, 90, 180, 270, 360),
                             labels = c("N", "E", "S", "W"),
                             include.lowest = TRUE))
      if (length(dir_table) > 0) {
        cat(sprintf("  Most common quadrant: %s\n", c("N", "E", "S", "W")[which.max(dir_table)]))
      }
    }

    if ("Wind_Gust_mph" %in% names(data) && sum(!is.na(data$Wind_Gust_mph)) > 0) {
      cat("\nWind Gust (mph):\n")
      cat(sprintf("  Min: %.2f\n", min(data$Wind_Gust_mph, na.rm = TRUE)))
      cat(sprintf("  Max: %.2f\n", max(data$Wind_Gust_mph, na.rm = TRUE)))
      cat(sprintf("  Mean: %.2f\n", mean(data$Wind_Gust_mph, na.rm = TRUE)))
    }

    if ("Temperature_F" %in% names(data) && sum(!is.na(data$Temperature_F)) > 0) {
      cat("\nTemperature (°F):\n")
      cat(sprintf("  Min: %.1f\n", min(data$Temperature_F, na.rm = TRUE)))
      cat(sprintf("  Max: %.1f\n", max(data$Temperature_F, na.rm = TRUE)))
      cat(sprintf("  Mean: %.1f\n", mean(data$Temperature_F, na.rm = TRUE)))
    }
  })

  # Download handler - hourly data
  output$download <- downloadHandler(
    filename = function() {
      paste0("wind_data_hourly_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(processed_data())
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

      site_idx <- which(col_lower %in% c("sitename", "site_name", "site"))
      if (length(site_idx) > 0) names(data)[site_idx[1]] <- "SiteName"

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

    met_dates <- as.POSIXct(met_data$DateTime, format = "%Y-%m-%d %H:%M", tz = "")
    met_min <- min(met_dates, na.rm = TRUE)
    met_max <- max(met_dates, na.rm = TRUE)

    site_data <- poll_data[poll_data$SiteName == input$selected_site &
                            poll_data$parameter %in% input$selected_params, ]
    site_data$parsed_date <- parse_datetime_string(
      as.character(site_data$date_LT_shifted_to_selected_timezone))

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

  # Process pollutant data, merge with met data, and generate plot
  observeEvent(input$process_pollutant, {
    req(pollutant_raw(), processed_data(),
        input$selected_site, input$selected_params, input$selected_poll_dates)
    tryCatch({
      poll_data <- pollutant_raw()
      met_hourly <- processed_data()$hourly
      site_name <- input$selected_site
      sel_params <- input$selected_params
      sel_dates <- as.Date(input$selected_poll_dates)
      met_site <- if (!is.null(input$met_data_site) && input$met_data_site != "") {
        input$met_data_site
      } else {
        "MetSite"
      }

      # Filter pollutant data
      poll_sub <- poll_data[poll_data$SiteName == site_name &
                             poll_data$parameter %in% sel_params, ]
      poll_sub$parsed_date <- parse_datetime_string(
        as.character(poll_sub$date_LT_shifted_to_selected_timezone))
      poll_sub <- poll_sub[!is.na(poll_sub$parsed_date) &
                            as.Date(poll_sub$parsed_date) %in% sel_dates, ]
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

      # Reshape to wide format
      reshape_df <- data.frame(
        date = poll_sub$parsed_date,
        SiteName = poll_sub$SiteName,
        parameter = poll_sub$parameter,
        sample_measurement = poll_sub$sample_measurement,
        stringsAsFactors = FALSE
      )
      wide_poll <- reshape_df %>%
        group_by(date, SiteName, parameter) %>%
        summarise(sample_measurement = mean(sample_measurement, na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = parameter, values_from = sample_measurement) %>%
        as.data.frame()

      # Parse met dates and floor pollutant dates for merging
      met_hourly$date <- as.POSIXct(met_hourly$DateTime,
                                      format = "%Y-%m-%d %H:%M", tz = "")
      wide_poll$date <- floor_date(wide_poll$date, unit = "hour")

      # Merge
      MLVB_mesowest2 <- merge(met_hourly, wide_poll, by = "date",
                               all.x = FALSE, all.y = FALSE)
      if (nrow(MLVB_mesowest2) == 0) {
        showNotification("No matching dates between met and pollutant data.",
                         type = "error")
        return()
      }

      # Map wind columns
      if ("Wind_Gust_mph" %in% names(MLVB_mesowest2))
        MLVB_mesowest2$gust_mesowest <- MLVB_mesowest2$Wind_Gust_mph
      if ("Wind_Speed_mph" %in% names(MLVB_mesowest2))
        MLVB_mesowest2$ws_mesowest <- MLVB_mesowest2$Wind_Speed_mph

      merged_result(MLVB_mesowest2)
      MLVB_mesowest2 <- MLVB_mesowest2[order(MLVB_mesowest2$date), ]

      # Event label for title
      evt1 <- paste(format(min(MLVB_mesowest2$date), "%b %d"), "-",
                    format(max(MLVB_mesowest2$date), "%b %d, %Y"))

      # Plot filename
      safe_site <- gsub("[^A-Za-z0-9_-]", "_", site_name)
      safe_met <- gsub("[^A-Za-z0-9_-]", "_", met_site)
      plot_filename <- paste0(safe_site, "_", safe_met, "_",
                              paste(sel_params, collapse = "_"), ".png")
      plot_path <- file.path(tempdir(), plot_filename)
      n_rows <- nrow(MLVB_mesowest2)

      # Find PM10 / PM2.5 columns
      pm10_col <- NULL; pm25_col <- NULL
      for (cn in names(MLVB_mesowest2)) {
        if (grepl("^PM10$|^PM10_", cn) && is.null(pm10_col)) pm10_col <- cn
        if (grepl("^PM2\\.5$|^PM2\\.5_", cn) && is.null(pm25_col)) pm25_col <- cn
      }

      if (!is.null(pm10_col)) {
        MLVB_mesowest2$PM10_all <- as.numeric(MLVB_mesowest2[[pm10_col]])
        MLVB_mesowest2$PM10_all[is.na(MLVB_mesowest2$PM10_all)] <- 0
      }
      if (!is.null(pm25_col)) {
        MLVB_mesowest2$PM2.5_all <- as.numeric(MLVB_mesowest2[[pm25_col]])
        MLVB_mesowest2$PM2.5_all[is.na(MLVB_mesowest2$PM2.5_all)] <- 0
      }

      if (is.null(MLVB_mesowest2$gust_mesowest))
        MLVB_mesowest2$gust_mesowest <- NA_real_
      if (is.null(MLVB_mesowest2$ws_mesowest))
        MLVB_mesowest2$ws_mesowest <- NA_real_

      # --- Generate the PNG ---
      png(plot_path, width = 1980, height = 1200, pointsize = 24)
      par(mar = c(8, 8, 3, 8), mgp = c(5, 2, 0))

      if (!is.null(pm10_col)) {
        # PM10 bar chart
        txtCols <- rgb(0, 0, 0.8, alpha = 0.7)
        concSpec <- "PM10"
        y_max <- max(MLVB_mesowest2$PM10_all, na.rm = TRUE) * 1.5
        if (!is.finite(y_max) || y_max == 0) y_max <- 1

        barplot(MLVB_mesowest2$PM10_all ~ MLVB_mesowest2$date,
                space = 0, border = FALSE, col = txtCols, xlab = "",
                names.arg = rep("", n_rows),
                ylab = expression(paste("PM, ", mu, "g/m"^3)),
                main = paste(site_name, evt1),
                cex.lab = 3, cex.axis = 3, xaxt = "n",
                ylim = c(0, y_max), cex.main = 3, xlim = c(0, n_rows))
        tick_seq <- seq(1, n_rows, by = max(1, floor(n_rows / 4)))
        axis(1, at = tick_seq - 0.5,
             labels = format(MLVB_mesowest2$date[tick_seq], "%b %d\n%I %p"),
             cex.axis = 2, las = 2)

        # Overlay PM2.5 if present
        if (!is.null(pm25_col) &&
            is.finite(mean(MLVB_mesowest2$PM2.5_all, na.rm = TRUE))) {
          barplot(MLVB_mesowest2$PM2.5_all ~ MLVB_mesowest2$date,
                  space = c(0.5, rep(1, n_rows - 1)),
                  width = rep(0.5, n_rows), border = FALSE,
                  col = rgb(0.5, 0.8, 1, alpha = 0.8),
                  names.arg = rep("", n_rows),
                  xlab = "", ylab = "", main = "", axes = FALSE, add = TRUE)
          concSpec <- c(concSpec, "PM2.5")
          txtCols <- c(txtCols, rgb(0.5, 0.8, 1, alpha = 0.8))
        }
      } else {
        # Generic: first selected parameter as bars
        first_param <- sel_params[1]
        first_col <- NULL
        for (cn in names(MLVB_mesowest2)) {
          if (cn == first_param) { first_col <- cn; break }
        }
        if (is.null(first_col)) first_col <- first_param

        prim_vals <- as.numeric(MLVB_mesowest2[[first_col]])
        prim_vals[is.na(prim_vals)] <- 0
        MLVB_mesowest2$primary_conc <- prim_vals

        txtCols <- rgb(0, 0, 0.8, alpha = 0.7)
        concSpec <- first_param
        unit_label <- if (param_units[[first_param]] != "") {
          paste0(first_param, ", ", param_units[[first_param]])
        } else first_param

        y_max <- max(prim_vals, na.rm = TRUE) * 1.5
        if (!is.finite(y_max) || y_max == 0) y_max <- 1

        barplot(MLVB_mesowest2$primary_conc ~ MLVB_mesowest2$date,
                space = 0, border = FALSE, col = txtCols, xlab = "",
                names.arg = rep("", n_rows), ylab = unit_label,
                main = paste(site_name, evt1),
                cex.lab = 3, cex.axis = 3, xaxt = "n",
                ylim = c(0, y_max), cex.main = 3, xlim = c(0, n_rows))
        tick_seq <- seq(1, n_rows, by = max(1, floor(n_rows / 4)))
        axis(1, at = tick_seq - 0.5,
             labels = format(MLVB_mesowest2$date[tick_seq], "%b %d\n%I %p"),
             cex.axis = 2, las = 2)

        # Second parameter overlay if present
        if (length(sel_params) > 1) {
          sec_param <- sel_params[2]
          if (sec_param %in% names(MLVB_mesowest2)) {
            sec_vals <- as.numeric(MLVB_mesowest2[[sec_param]])
            sec_vals[is.na(sec_vals)] <- 0
            if (is.finite(mean(sec_vals, na.rm = TRUE))) {
              barplot(sec_vals ~ MLVB_mesowest2$date,
                      space = c(0.5, rep(1, n_rows - 1)),
                      width = rep(0.5, n_rows), border = FALSE,
                      col = rgb(0.5, 0.8, 1, alpha = 0.8),
                      names.arg = rep("", n_rows),
                      xlab = "", ylab = "", main = "", axes = FALSE, add = TRUE)
              concSpec <- c(concSpec, sec_param)
              txtCols <- c(txtCols, rgb(0.5, 0.8, 1, alpha = 0.8))
            }
          }
        }
      }

      # Overlay gust line
      par(new = TRUE)
      gust_max <- max(MLVB_mesowest2$gust_mesowest, na.rm = TRUE)
      if (!is.finite(gust_max)) gust_max <- 1
      plot((1:n_rows) - 0.5, MLVB_mesowest2$gust_mesowest,
           type = "l", xlab = "", ylab = "", col = 6, axes = FALSE,
           lwd = 3, ylim = c(0, gust_max * 1.1), xlim = c(0, n_rows))
      axis(4, cex.axis = 2, col = 6, col.ticks = 6, col.axis = 6)

      # Overlay average wind speed line
      lines((1:n_rows) - 0.5, MLVB_mesowest2$ws_mesowest, col = 1, lwd = 3)

      # Wind threshold
      abline(h = 25, lty = 3, lwd = 3, col = 1)

      # Legend
      legend("topleft", ncol = 3,
             fill = c(txtCols, NA, NA, NA),
             lty = c(rep(NA, length(txtCols)), 1, 1, 3),
             border = FALSE,
             lwd = c(rep(NA, length(txtCols)), 3, 3, 3),
             col = c(rep(NA, length(txtCols)), 6, 1, 1),
             text.col = c(txtCols, 6, 1, 1),
             legend = c(concSpec,
                        paste0("1hr max gust @ ", met_site),
                        paste("Avg wind @", met_site),
                        "Wind threshold"),
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
