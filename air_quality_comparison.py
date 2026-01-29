#!/usr/bin/env python3
"""
Air Quality Data Comparison Tool

This script fetches and compares air quality data from two sources:
1. American Lung Association (ALA) - State of the Air Report data
2. EPA Air Quality System (AQS) - PM2.5 monitoring data

Target Area: Placer County, California
Year: 2024

Author: Generated with Claude
"""

import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
import pandas as pd
import numpy as np
import json
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Union
import time
import logging
import os
from pathlib import Path


# =============================================================================
# LOGGING CONFIGURATION
# =============================================================================

def setup_logging(
    level: Union[int, str] = logging.INFO,
    log_file: Optional[str] = None,
    format_string: Optional[str] = None
) -> logging.Logger:
    """
    Configure logging for the air quality comparison tool.

    Args:
        level: Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
        log_file: Optional path to log file. If None, logs only to console.
        format_string: Custom format string for log messages.

    Returns:
        Configured logger instance.
    """
    logger = logging.getLogger("air_quality")

    # Avoid adding handlers multiple times
    if logger.handlers:
        return logger

    logger.setLevel(level)

    if format_string is None:
        format_string = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"

    formatter = logging.Formatter(format_string)

    # Console handler
    console_handler = logging.StreamHandler()
    console_handler.setLevel(level)
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)

    # File handler (optional)
    if log_file:
        try:
            file_handler = logging.FileHandler(log_file)
            file_handler.setLevel(level)
            file_handler.setFormatter(formatter)
            logger.addHandler(file_handler)
            logger.debug(f"Logging to file: {log_file}")
        except (IOError, OSError) as e:
            logger.warning(f"Could not create log file '{log_file}': {e}")

    return logger


# Initialize default logger
logger = setup_logging()


# =============================================================================
# CUSTOM EXCEPTIONS
# =============================================================================

class AirQualityError(Exception):
    """Base exception for air quality module errors."""
    pass


class APIError(AirQualityError):
    """Raised when an API request fails."""
    def __init__(self, message: str, status_code: Optional[int] = None, response_text: Optional[str] = None):
        self.status_code = status_code
        self.response_text = response_text
        super().__init__(message)


class ValidationError(AirQualityError):
    """Raised when input validation fails."""
    pass


class DataError(AirQualityError):
    """Raised when data processing fails."""
    pass


class ConfigurationError(AirQualityError):
    """Raised when configuration is invalid."""
    pass


# =============================================================================
# CONSTANTS
# =============================================================================

DEFAULT_REQUEST_TIMEOUT = 30  # seconds
MAX_RETRIES = 3
RETRY_BACKOFF_FACTOR = 1.0  # seconds
RATE_LIMIT_DELAY = 0.5  # seconds between API calls


class EPAAirQualityFetcher:
    """
    Fetches PM2.5 data from EPA's Air Quality System (AQS) API.

    API Documentation: https://aqs.epa.gov/aqsweb/documents/data_api.html
    """

    BASE_URL = "https://aqs.epa.gov/data/api"

    # Placer County, California FIPS codes
    STATE_CODE = "06"  # California
    COUNTY_CODE = "061"  # Placer County

    # PM2.5 parameter codes
    PM25_LOCAL = "88101"  # PM2.5 - Local Conditions
    PM25_FRM = "88502"  # PM2.5 Raw Data (Federal Reference Method)

    def __init__(self, email: str, api_key: str, timeout: int = DEFAULT_REQUEST_TIMEOUT):
        """
        Initialize the EPA API client.

        Args:
            email: Registered email address for EPA AQS API
            api_key: API key obtained from EPA AQS registration
            timeout: Request timeout in seconds (default: 30)

        To obtain an API key, register at:
        https://aqs.epa.gov/aqsweb/documents/data_api.html#signup

        Raises:
            ValidationError: If email or api_key is empty or invalid.
        """
        # Validate credentials
        self._validate_credentials(email, api_key)

        self.email = email
        self.api_key = api_key
        self.timeout = timeout
        self._session = self._create_session()

        logger.debug(f"EPAAirQualityFetcher initialized for email: {email[:3]}***")

    def _validate_credentials(self, email: str, api_key: str) -> None:
        """
        Validate API credentials.

        Args:
            email: Email address to validate
            api_key: API key to validate

        Raises:
            ValidationError: If credentials are invalid.
        """
        if not email or not isinstance(email, str):
            raise ValidationError("Email must be a non-empty string")

        if "@" not in email or "." not in email:
            raise ValidationError(f"Invalid email format: {email}")

        if not api_key or not isinstance(api_key, str):
            raise ValidationError("API key must be a non-empty string")

        if len(api_key) < 10:
            raise ValidationError("API key appears too short to be valid")

        logger.debug("Credentials validated successfully")

    def _create_session(self) -> requests.Session:
        """
        Create a requests session with retry configuration.

        Returns:
            Configured requests.Session with retry logic.
        """
        session = requests.Session()

        retry_strategy = Retry(
            total=MAX_RETRIES,
            backoff_factor=RETRY_BACKOFF_FACTOR,
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["GET"],
            raise_on_status=False
        )

        adapter = HTTPAdapter(max_retries=retry_strategy)
        session.mount("http://", adapter)
        session.mount("https://", adapter)

        logger.debug(f"Session created with {MAX_RETRIES} max retries")
        return session

    def _make_request(self, endpoint: str, params: Dict) -> Dict:
        """
        Make a request to the EPA AQS API with retry logic and timeout.

        Args:
            endpoint: API endpoint path
            params: Query parameters

        Returns:
            Parsed JSON response as dictionary

        Raises:
            APIError: If the request fails after retries.
            ValidationError: If the response is malformed.
        """
        params.update({
            "email": self.email,
            "key": self.api_key
        })

        url = f"{self.BASE_URL}/{endpoint}"
        logger.debug(f"Making request to: {endpoint}")
        logger.debug(f"Parameters: {self._sanitize_params(params)}")

        try:
            response = self._session.get(url, params=params, timeout=self.timeout)

            logger.debug(f"Response status: {response.status_code}")

            # Check for HTTP errors
            if response.status_code != 200:
                error_msg = f"HTTP {response.status_code}: {response.reason}"
                logger.error(f"API request failed: {error_msg}")
                raise APIError(error_msg, response.status_code, response.text[:500])

            # Parse JSON response
            try:
                data = response.json()
            except json.JSONDecodeError as e:
                logger.error(f"Failed to parse JSON response: {e}")
                raise ValidationError(f"Invalid JSON response from API: {e}")

            # Check for EPA API-level errors
            header = data.get("Header", [{}])
            if not header:
                logger.warning("Response missing Header field")
            elif header[0].get("status") == "Failed":
                error_msg = header[0].get("error", "Unknown error")
                logger.error(f"EPA API returned error: {error_msg}")
                raise APIError(f"EPA API Error: {error_msg}")

            logger.debug(f"Request successful, received {len(data.get('Data', []))} records")
            return data

        except requests.exceptions.Timeout:
            logger.error(f"Request timed out after {self.timeout} seconds")
            raise APIError(f"Request timed out after {self.timeout} seconds")

        except requests.exceptions.ConnectionError as e:
            logger.error(f"Connection error: {e}")
            raise APIError(f"Connection failed: {e}")

        except requests.exceptions.RequestException as e:
            logger.error(f"Request failed: {e}")
            raise APIError(f"Request failed: {e}")

    def _sanitize_params(self, params: Dict) -> Dict:
        """
        Create a sanitized copy of params for logging (hide sensitive data).

        Args:
            params: Original parameters dictionary

        Returns:
            Dictionary with sensitive values masked
        """
        sanitized = params.copy()
        if "key" in sanitized:
            sanitized["key"] = "***HIDDEN***"
        if "email" in sanitized:
            email = sanitized["email"]
            sanitized["email"] = email[:3] + "***" if len(email) > 3 else "***"
        return sanitized

    def get_daily_pm25_data(self, year: int = 2024) -> pd.DataFrame:
        """
        Fetch daily PM2.5 data for Placer County.

        Args:
            year: Year to fetch data for (default: 2024)

        Returns:
            DataFrame with daily PM2.5 measurements

        Raises:
            ValidationError: If year is invalid.
            APIError: If the API request fails.
        """
        # Validate year
        self._validate_year(year)

        logger.info(f"Fetching daily PM2.5 data for year {year}")

        params = {
            "param": self.PM25_LOCAL,
            "bdate": f"{year}0101",
            "edate": f"{year}1231",
            "state": self.STATE_CODE,
            "county": self.COUNTY_CODE
        }

        data = self._make_request("dailyData/byCounty", params)

        if not data.get("Data"):
            logger.warning(f"No PM2.5 data found for Placer County in {year}")
            return pd.DataFrame()

        df = pd.DataFrame(data["Data"])
        logger.info(f"Retrieved {len(df)} daily records")

        # Convert date column with error handling
        if "date_local" in df.columns:
            try:
                df["date_local"] = pd.to_datetime(df["date_local"], errors="coerce")
                invalid_dates = df["date_local"].isna().sum()
                if invalid_dates > 0:
                    logger.warning(f"Found {invalid_dates} records with invalid dates")
            except Exception as e:
                logger.error(f"Failed to parse dates: {e}")

        # Validate numeric columns
        df = self._validate_numeric_columns(df)

        return df

    def get_annual_summary(self, year: int = 2024) -> pd.DataFrame:
        """
        Fetch annual PM2.5 summary statistics for Placer County.

        Args:
            year: Year to fetch data for (default: 2024)

        Returns:
            DataFrame with annual PM2.5 summary statistics

        Raises:
            ValidationError: If year is invalid.
            APIError: If the API request fails.
        """
        # Validate year
        self._validate_year(year)

        logger.info(f"Fetching annual PM2.5 summary for year {year}")

        params = {
            "param": self.PM25_LOCAL,
            "bdate": f"{year}0101",
            "edate": f"{year}1231",
            "state": self.STATE_CODE,
            "county": self.COUNTY_CODE
        }

        data = self._make_request("annualData/byCounty", params)

        if not data.get("Data"):
            logger.warning(f"No annual PM2.5 summary found for Placer County in {year}")
            return pd.DataFrame()

        df = pd.DataFrame(data["Data"])
        logger.info(f"Retrieved annual summary with {len(df)} records")

        return df

    def get_monitoring_sites(self) -> pd.DataFrame:
        """
        Get list of PM2.5 monitoring sites in Placer County.

        Returns:
            DataFrame with monitoring site information

        Raises:
            APIError: If the API request fails.
        """
        logger.info("Fetching monitoring sites for Placer County")

        params = {
            "param": self.PM25_LOCAL,
            "state": self.STATE_CODE,
            "county": self.COUNTY_CODE
        }

        data = self._make_request("monitors/byCounty", params)

        if not data.get("Data"):
            logger.warning("No monitoring sites found for Placer County")
            return pd.DataFrame()

        df = pd.DataFrame(data["Data"])
        logger.info(f"Found {len(df)} monitoring sites")

        return df

    def _validate_year(self, year: int) -> None:
        """
        Validate that year is within acceptable range.

        Args:
            year: Year to validate

        Raises:
            ValidationError: If year is invalid.
        """
        current_year = datetime.now().year

        if not isinstance(year, int):
            raise ValidationError(f"Year must be an integer, got {type(year).__name__}")

        if year < 1980:
            raise ValidationError(f"Year {year} is before EPA monitoring began (1980)")

        if year > current_year:
            raise ValidationError(f"Year {year} is in the future (current year: {current_year})")

        logger.debug(f"Year {year} validated")

    def _validate_numeric_columns(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Validate and clean numeric columns in DataFrame.

        Args:
            df: DataFrame to validate

        Returns:
            DataFrame with validated numeric columns
        """
        numeric_cols = ["arithmetic_mean", "sample_measurement", "first_max_value"]

        for col in numeric_cols:
            if col in df.columns:
                # Convert to numeric, coercing errors to NaN
                original_count = len(df)
                df[col] = pd.to_numeric(df[col], errors="coerce")
                nan_count = df[col].isna().sum()

                if nan_count > 0:
                    logger.warning(f"Column '{col}': {nan_count}/{original_count} values could not be converted to numeric")

                # Check for negative values (invalid for PM2.5)
                negative_count = (df[col] < 0).sum()
                if negative_count > 0:
                    logger.warning(f"Column '{col}': {negative_count} negative values detected (invalid for PM2.5)")

        return df


class ALAAirQualityData:
    """
    American Lung Association State of the Air Report Data.

    The ALA publishes annual "State of the Air" reports with air quality grades
    for counties across the United States. This data is compiled from EPA
    monitoring data but presented with ALA's own grading methodology.

    Data Source: https://www.lung.org/research/sota

    Note: The ALA doesn't provide a public API. This class provides:
    1. Methods to parse downloaded ALA report data
    2. Reference information about ALA's grading methodology
    3. Comparison capabilities with EPA raw data
    """

    # ALA Grading Scale for Ozone and Particle Pollution
    # Based on number of unhealthy air days
    GRADE_SCALE = {
        "A": {"min_days": 0, "max_days": 0, "description": "No unhealthy days"},
        "B": {"min_days": 0.3, "max_days": 0.9, "description": "Very few unhealthy days"},
        "C": {"min_days": 1.0, "max_days": 2.0, "description": "Some unhealthy days"},
        "D": {"min_days": 2.1, "max_days": 3.2, "description": "Many unhealthy days"},
        "F": {"min_days": 3.3, "max_days": None, "description": "Excessive unhealthy days"}
    }

    # AQI Breakpoints for PM2.5 (24-hour average, μg/m³)
    PM25_AQI_BREAKPOINTS = {
        "Good": (0, 12.0),
        "Moderate": (12.1, 35.4),
        "Unhealthy for Sensitive Groups": (35.5, 55.4),
        "Unhealthy": (55.5, 150.4),
        "Very Unhealthy": (150.5, 250.4),
        "Hazardous": (250.5, 500.4)
    }

    # Placer County 2024 Data (from State of the Air 2024 Report)
    # Note: The 2024 report covers data from 2020-2022
    # The 2025 report would cover 2021-2023 data
    PLACER_COUNTY_2024_REPORT = {
        "county": "Placer",
        "state": "California",
        "metro_area": "Sacramento-Roseville, CA",
        "report_year": 2024,
        "data_years": "2020-2022",
        "ozone": {
            "grade": "F",
            "high_ozone_days": 23.7,
            "rank_nationwide": None  # Rank among counties with failing grades
        },
        "particle_pollution_24hour": {
            "grade": "F",
            "high_pm25_days": 12.3,
            "rank_nationwide": None
        },
        "particle_pollution_annual": {
            "grade": "Pass",  # Pass/Fail for annual
            "annual_avg_pm25": None,
            "meets_who_guideline": False
        },
        "at_risk_population": {
            "total": 411982,
            "children": 71735,
            "adults_65_plus": 82978,
            "poverty": 29727,
            "lung_disease": 42051,
            "cardiovascular_disease": 31618
        }
    }

    # Supported file extensions for data loading
    SUPPORTED_EXTENSIONS = {'.csv', '.xlsx', '.xls'}

    def __init__(self, data_file: Optional[str] = None):
        """
        Initialize ALA data handler.

        Args:
            data_file: Optional path to downloaded ALA CSV/Excel data file

        Raises:
            ValidationError: If data_file path is invalid.
            DataError: If file cannot be loaded.
        """
        self.data_file = data_file
        self.data = None

        logger.debug("ALAAirQualityData initialized")

        if data_file:
            self.load_data(data_file)

    def load_data(self, filepath: str) -> pd.DataFrame:
        """
        Load ALA data from a downloaded file.

        Args:
            filepath: Path to the data file (CSV or Excel)

        Returns:
            DataFrame with ALA report data

        Raises:
            ValidationError: If filepath is invalid or unsupported format.
            DataError: If file cannot be read or is corrupted.
        """
        logger.info(f"Loading ALA data from: {filepath}")

        # Validate filepath
        if not filepath or not isinstance(filepath, str):
            raise ValidationError("Filepath must be a non-empty string")

        # Check file exists
        path = Path(filepath)
        if not path.exists():
            raise ValidationError(f"File not found: {filepath}")

        if not path.is_file():
            raise ValidationError(f"Path is not a file: {filepath}")

        # Check file extension
        ext = path.suffix.lower()
        if ext not in self.SUPPORTED_EXTENSIONS:
            raise ValidationError(
                f"Unsupported file format '{ext}'. "
                f"Supported formats: {', '.join(self.SUPPORTED_EXTENSIONS)}"
            )

        # Check file is readable
        if not os.access(filepath, os.R_OK):
            raise ValidationError(f"File is not readable: {filepath}")

        # Check file is not empty
        if path.stat().st_size == 0:
            raise DataError(f"File is empty: {filepath}")

        try:
            if ext == '.csv':
                logger.debug("Reading CSV file")
                self.data = pd.read_csv(filepath)
            else:  # .xlsx or .xls
                logger.debug("Reading Excel file")
                self.data = pd.read_excel(filepath)

            # Validate loaded data
            if self.data is None or self.data.empty:
                raise DataError(f"No data found in file: {filepath}")

            logger.info(f"Loaded {len(self.data)} rows, {len(self.data.columns)} columns")
            logger.debug(f"Columns: {list(self.data.columns)}")

            return self.data

        except pd.errors.EmptyDataError:
            raise DataError(f"File contains no data: {filepath}")
        except pd.errors.ParserError as e:
            raise DataError(f"Failed to parse file: {filepath}. Error: {e}")
        except Exception as e:
            logger.error(f"Unexpected error loading file: {e}")
            raise DataError(f"Failed to load file: {filepath}. Error: {e}")

    def get_placer_county_data(self) -> Dict:
        """
        Get ALA data for Placer County.

        Returns:
            Dictionary with ALA report data for Placer County
        """
        logger.debug("Retrieving Placer County ALA data")
        return self.PLACER_COUNTY_2024_REPORT.copy()

    def calculate_grade_from_days(self, unhealthy_days: float) -> str:
        """
        Calculate ALA grade based on number of unhealthy air days.

        Args:
            unhealthy_days: Average number of unhealthy days per year

        Returns:
            Letter grade (A, B, C, D, or F)

        Raises:
            ValidationError: If unhealthy_days is invalid.
        """
        # Validate input
        if unhealthy_days is None:
            raise ValidationError("unhealthy_days cannot be None")

        if not isinstance(unhealthy_days, (int, float)):
            raise ValidationError(
                f"unhealthy_days must be numeric, got {type(unhealthy_days).__name__}"
            )

        # Handle NaN
        if pd.isna(unhealthy_days):
            logger.warning("unhealthy_days is NaN, returning 'N/A'")
            return "N/A"

        # Handle negative values
        if unhealthy_days < 0:
            logger.warning(f"Negative unhealthy_days ({unhealthy_days}), treating as 0")
            unhealthy_days = 0

        logger.debug(f"Calculating grade for {unhealthy_days} unhealthy days")

        if unhealthy_days == 0:
            grade = "A"
        elif unhealthy_days < 1.0:
            grade = "B"
        elif unhealthy_days <= 2.0:
            grade = "C"
        elif unhealthy_days <= 3.2:
            grade = "D"
        else:
            grade = "F"

        logger.debug(f"Calculated grade: {grade}")
        return grade

    def get_pm25_category(self, concentration: float) -> str:
        """
        Get AQI category for a PM2.5 concentration.

        Args:
            concentration: PM2.5 concentration in μg/m³

        Returns:
            AQI category name

        Raises:
            ValidationError: If concentration is invalid.
        """
        # Validate input
        if concentration is None:
            raise ValidationError("concentration cannot be None")

        if not isinstance(concentration, (int, float)):
            raise ValidationError(
                f"concentration must be numeric, got {type(concentration).__name__}"
            )

        # Handle NaN
        if pd.isna(concentration):
            logger.warning("concentration is NaN")
            return "Unknown"

        # Handle negative values
        if concentration < 0:
            logger.warning(f"Negative concentration ({concentration}), invalid for PM2.5")
            return "Invalid"

        logger.debug(f"Getting PM2.5 category for concentration: {concentration}")

        for category, (low, high) in self.PM25_AQI_BREAKPOINTS.items():
            if low <= concentration <= high:
                logger.debug(f"Category: {category}")
                return category

        logger.warning(f"Concentration {concentration} exceeds AQI scale")
        return "Beyond AQI Scale"

    @staticmethod
    def fetch_sota_report_info() -> Dict:
        """
        Provide information about how to access ALA State of the Air data.

        Returns:
            Dictionary with access instructions and URLs
        """
        return {
            "report_name": "State of the Air",
            "publisher": "American Lung Association",
            "website": "https://www.lung.org/research/sota",
            "data_access": {
                "interactive_tool": "https://www.lung.org/research/sota/city-rankings",
                "methodology": "https://www.lung.org/research/sota/about",
                "full_report_pdf": "https://www.lung.org/getmedia/state-of-the-air-report.pdf"
            },
            "notes": [
                "ALA compiles data from EPA monitoring stations",
                "Reports are released annually in April",
                "Data represents 3-year averages (e.g., 2024 report uses 2020-2022 data)",
                "Grades are based on weighted average of unhealthy air days",
                "No public API available - data must be downloaded or scraped"
            ]
        }


class AirQualityComparison:
    """
    Compare air quality data from EPA and American Lung Association sources.
    """

    def __init__(self, epa_email: str = None, epa_api_key: str = None):
        """
        Initialize comparison tool.

        Args:
            epa_email: Email registered with EPA AQS API
            epa_api_key: EPA AQS API key

        Note:
            If EPA credentials are invalid, initialization will still succeed
            but EPA data fetching will fail with a descriptive error.
        """
        self.epa_fetcher = None

        logger.info("Initializing AirQualityComparison")

        if epa_email and epa_api_key:
            try:
                self.epa_fetcher = EPAAirQualityFetcher(epa_email, epa_api_key)
                logger.info("EPA API client initialized successfully")
            except ValidationError as e:
                logger.warning(f"Invalid EPA credentials: {e}")
                logger.warning("EPA data fetching will not be available")
        else:
            logger.info("No EPA credentials provided - EPA data fetching disabled")

        self.ala_data = ALAAirQualityData()
        logger.debug("AirQualityComparison initialization complete")

    def count_unhealthy_days(self, epa_data: pd.DataFrame) -> Dict:
        """
        Count unhealthy air days from EPA data using ALA methodology.

        Args:
            epa_data: DataFrame with EPA daily PM2.5 data

        Returns:
            Dictionary with unhealthy day counts by category

        Raises:
            ValidationError: If epa_data is not a DataFrame.
        """
        logger.info("Counting unhealthy days from EPA data")

        # Validate input
        if not isinstance(epa_data, pd.DataFrame):
            raise ValidationError(
                f"epa_data must be a DataFrame, got {type(epa_data).__name__}"
            )

        if epa_data.empty:
            logger.warning("EPA data is empty")
            return {"error": "No EPA data available"}

        logger.debug(f"Processing {len(epa_data)} records")

        # Get the arithmetic mean column for daily average
        value_col = None
        for col in ["arithmetic_mean", "sample_measurement", "daily_mean"]:
            if col in epa_data.columns:
                value_col = col
                logger.debug(f"Using column '{col}' for PM2.5 values")
                break

        if not value_col:
            logger.error(f"No valid measurement column found. Available: {list(epa_data.columns)}")
            return {"error": "Could not find measurement column in EPA data"}

        counts = {
            "Good": 0,
            "Moderate": 0,
            "Unhealthy for Sensitive Groups": 0,
            "Unhealthy": 0,
            "Very Unhealthy": 0,
            "Hazardous": 0,
            "Total Days": len(epa_data),
            "Invalid/Missing": 0
        }

        for idx, row in epa_data.iterrows():
            try:
                value = row[value_col]

                # Handle missing or invalid values
                if pd.isna(value):
                    counts["Invalid/Missing"] += 1
                    continue

                category = self.ala_data.get_pm25_category(value)
                if category in counts:
                    counts[category] += 1
                elif category in ("Unknown", "Invalid"):
                    counts["Invalid/Missing"] += 1
                    logger.debug(f"Row {idx}: Invalid value {value}")

            except Exception as e:
                logger.warning(f"Error processing row {idx}: {e}")
                counts["Invalid/Missing"] += 1

        # Calculate unhealthy days (USG + Unhealthy + Very Unhealthy + Hazardous)
        unhealthy_total = (
            counts["Unhealthy for Sensitive Groups"] +
            counts["Unhealthy"] +
            counts["Very Unhealthy"] +
            counts["Hazardous"]
        )
        counts["Total Unhealthy Days"] = unhealthy_total

        logger.info(f"Count complete: {unhealthy_total} unhealthy days out of {counts['Total Days']} total")

        if counts["Invalid/Missing"] > 0:
            logger.warning(f"Found {counts['Invalid/Missing']} records with invalid/missing values")

        return counts

    def compare_sources(self, year: int = 2024) -> Dict:
        """
        Compare EPA raw data with ALA reported data.

        Args:
            year: Year to analyze

        Returns:
            Dictionary with comparison results

        Raises:
            ValidationError: If year is invalid.
        """
        logger.info(f"Comparing data sources for year {year}")

        # Validate year
        if not isinstance(year, int):
            raise ValidationError(f"Year must be an integer, got {type(year).__name__}")

        current_year = datetime.now().year
        if year < 1980 or year > current_year:
            raise ValidationError(f"Year must be between 1980 and {current_year}")

        results = {
            "year": year,
            "county": "Placer County, CA",
            "epa_data": None,
            "ala_data": None,
            "comparison": None,
            "generated_at": datetime.now().isoformat()
        }

        # Get ALA data
        try:
            logger.debug("Fetching ALA data")
            ala_info = self.ala_data.get_placer_county_data()
            results["ala_data"] = {
                "source": "American Lung Association State of the Air Report",
                "report_year": ala_info["report_year"],
                "data_years": ala_info["data_years"],
                "pm25_24hr_grade": ala_info["particle_pollution_24hour"]["grade"],
                "high_pm25_days": ala_info["particle_pollution_24hour"]["high_pm25_days"],
                "annual_grade": ala_info["particle_pollution_annual"]["grade"]
            }
            logger.debug("ALA data retrieved successfully")
        except Exception as e:
            logger.error(f"Failed to get ALA data: {e}")
            results["ala_data"] = {"error": f"Failed to retrieve ALA data: {e}"}

        # Get EPA data if API credentials provided
        if self.epa_fetcher:
            try:
                logger.debug(f"Fetching EPA data for year {year}")
                epa_daily = self.epa_fetcher.get_daily_pm25_data(year)

                if not epa_daily.empty:
                    unhealthy_counts = self.count_unhealthy_days(epa_daily)

                    # Calculate statistics with null safety
                    value_col = None
                    for col in ["arithmetic_mean", "sample_measurement"]:
                        if col in epa_daily.columns:
                            value_col = col
                            break

                    mean_pm25 = None
                    max_pm25 = None

                    if value_col and value_col in epa_daily.columns:
                        # Filter out NaN values for calculations
                        valid_values = epa_daily[value_col].dropna()
                        if len(valid_values) > 0:
                            mean_pm25 = float(valid_values.mean())
                            max_pm25 = float(valid_values.max())
                            logger.debug(f"Statistics calculated: mean={mean_pm25:.2f}, max={max_pm25:.2f}")
                        else:
                            logger.warning("No valid PM2.5 values for statistics calculation")

                    results["epa_data"] = {
                        "source": "EPA Air Quality System",
                        "year": year,
                        "total_measurements": len(epa_daily),
                        "valid_measurements": len(valid_values) if value_col else 0,
                        "mean_pm25": mean_pm25,
                        "max_pm25": max_pm25,
                        "unhealthy_day_counts": unhealthy_counts,
                        "calculated_grade": self.ala_data.calculate_grade_from_days(
                            unhealthy_counts.get("Total Unhealthy Days", 0)
                        )
                    }
                    logger.info(f"EPA data processed: {len(epa_daily)} measurements")
                else:
                    logger.warning(f"No EPA data available for year {year}")
                    results["epa_data"] = {"error": "No data available for specified year"}

            except APIError as e:
                logger.error(f"EPA API error: {e}")
                results["epa_data"] = {"error": f"EPA API error: {e}"}
            except ValidationError as e:
                logger.error(f"Validation error: {e}")
                results["epa_data"] = {"error": f"Validation error: {e}"}
            except Exception as e:
                logger.error(f"Unexpected error fetching EPA data: {e}")
                results["epa_data"] = {"error": f"Unexpected error: {e}"}
        else:
            logger.info("EPA API credentials not provided")
            results["epa_data"] = {
                "note": "EPA API credentials not provided",
                "instructions": "Provide email and API key to fetch EPA data",
                "signup_url": "https://aqs.epa.gov/aqsweb/documents/data_api.html#signup"
            }

        # Generate comparison notes
        results["comparison"] = {
            "methodology_differences": [
                "ALA uses 3-year weighted averages; EPA provides raw daily measurements",
                "ALA grades are based on design values, not simple averages",
                "ALA includes all monitoring sites; individual site data may vary",
                "Wildfire smoke impacts can significantly affect year-to-year variation"
            ],
            "key_considerations": [
                "Placer County is in Sacramento metro area, affected by valley air quality",
                "Wildfire seasons can cause significant PM2.5 spikes",
                "Mountain/foothill terrain can trap pollutants"
            ]
        }

        logger.info("Source comparison complete")
        return results

    def generate_report(self, year: int = 2024) -> str:
        """
        Generate a formatted comparison report.

        Args:
            year: Year to analyze

        Returns:
            Formatted string report

        Raises:
            ValidationError: If year is invalid.
        """
        logger.info(f"Generating report for year {year}")

        try:
            comparison = self.compare_sources(year)
        except ValidationError:
            raise
        except Exception as e:
            logger.error(f"Failed to generate comparison data: {e}")
            return f"Error generating report: {e}"

        report = []
        report.append("=" * 70)
        report.append("AIR QUALITY DATA COMPARISON REPORT")
        report.append(f"Placer County, California - {year}")
        report.append("=" * 70)
        report.append(f"Generated: {comparison.get('generated_at', 'N/A')}")
        report.append("")

        # ALA Section
        report.append("-" * 40)
        report.append("AMERICAN LUNG ASSOCIATION DATA")
        report.append("-" * 40)
        ala = comparison["ala_data"]

        if ala and "error" not in ala:
            report.append(f"Report Year: {ala.get('report_year', 'N/A')}")
            report.append(f"Data Period: {ala.get('data_years', 'N/A')}")
            report.append(f"PM2.5 24-Hour Grade: {ala.get('pm25_24hr_grade', 'N/A')}")
            report.append(f"High PM2.5 Days (weighted avg): {ala.get('high_pm25_days', 'N/A')}")
            report.append(f"Annual PM2.5 Grade: {ala.get('annual_grade', 'N/A')}")
        else:
            report.append(f"Error: {ala.get('error', 'Unknown error') if ala else 'No data available'}")
        report.append("")

        # EPA Section
        report.append("-" * 40)
        report.append("EPA AIR QUALITY SYSTEM DATA")
        report.append("-" * 40)
        epa = comparison["epa_data"]

        if not epa:
            report.append("Error: No EPA data available")
        elif "error" in epa:
            report.append(f"Error: {epa['error']}")
        elif "note" in epa:
            report.append(f"Note: {epa['note']}")
            report.append(f"To get EPA data: {epa.get('signup_url', 'N/A')}")
        else:
            report.append(f"Year: {epa.get('year', 'N/A')}")
            report.append(f"Total Measurements: {epa.get('total_measurements', 'N/A')}")
            report.append(f"Valid Measurements: {epa.get('valid_measurements', 'N/A')}")

            mean_pm25 = epa.get('mean_pm25')
            max_pm25 = epa.get('max_pm25')
            report.append(f"Mean PM2.5: {mean_pm25:.2f} μg/m³" if mean_pm25 is not None else "Mean PM2.5: N/A")
            report.append(f"Max PM2.5: {max_pm25:.2f} μg/m³" if max_pm25 is not None else "Max PM2.5: N/A")
            report.append(f"Calculated Grade: {epa.get('calculated_grade', 'N/A')}")

            if "unhealthy_day_counts" in epa:
                report.append("\nUnhealthy Day Breakdown:")
                for category, count in epa["unhealthy_day_counts"].items():
                    report.append(f"  {category}: {count}")
        report.append("")

        # Comparison Notes
        report.append("-" * 40)
        report.append("METHODOLOGY NOTES")
        report.append("-" * 40)
        if comparison.get("comparison") and comparison["comparison"].get("methodology_differences"):
            for note in comparison["comparison"]["methodology_differences"]:
                report.append(f"• {note}")
        else:
            report.append("No methodology notes available")
        report.append("")

        report.append("-" * 40)
        report.append("REGIONAL CONSIDERATIONS")
        report.append("-" * 40)
        if comparison.get("comparison") and comparison["comparison"].get("key_considerations"):
            for note in comparison["comparison"]["key_considerations"]:
                report.append(f"• {note}")
        else:
            report.append("No regional considerations available")

        report.append("")
        report.append("=" * 70)

        logger.info("Report generation complete")
        return "\n".join(report)


def fetch_airnow_data(
    api_key: str,
    zipcode: str = "95661",
    start_date: str = None,
    end_date: str = None,
    timeout: int = DEFAULT_REQUEST_TIMEOUT,
    max_retries: int = MAX_RETRIES
) -> pd.DataFrame:
    """
    Fetch air quality data from AirNow API (alternative to AQS).

    AirNow provides more recent data than AQS and is easier to use.
    API Documentation: https://docs.airnowapi.org/

    Args:
        api_key: AirNow API key (register at https://docs.airnowapi.org/account/request/)
        zipcode: ZIP code for Placer County (95661 is Roseville)
        start_date: Start date (YYYY-MM-DD format)
        end_date: End date (YYYY-MM-DD format)
        timeout: Request timeout in seconds (default: 30)
        max_retries: Maximum number of retry attempts per request (default: 3)

    Returns:
        DataFrame with air quality data

    Raises:
        ValidationError: If input parameters are invalid.
    """
    logger.info("Starting AirNow data fetch")

    # Validate API key
    if not api_key or not isinstance(api_key, str):
        raise ValidationError("API key must be a non-empty string")

    if len(api_key) < 10:
        raise ValidationError("API key appears too short to be valid")

    # Validate zipcode
    if not zipcode or not isinstance(zipcode, str):
        raise ValidationError("Zipcode must be a non-empty string")

    if not zipcode.isdigit() or len(zipcode) != 5:
        raise ValidationError(f"Invalid zipcode format: {zipcode}. Expected 5-digit US zipcode.")

    # Set default dates
    current_year = datetime.now().year
    if not start_date:
        start_date = f"{current_year}-01-01"
    if not end_date:
        end_date = f"{current_year}-12-31"

    # Validate and parse dates
    try:
        start_dt = datetime.strptime(start_date, "%Y-%m-%d")
    except ValueError:
        raise ValidationError(f"Invalid start_date format: {start_date}. Expected YYYY-MM-DD.")

    try:
        end_dt = datetime.strptime(end_date, "%Y-%m-%d")
    except ValueError:
        raise ValidationError(f"Invalid end_date format: {end_date}. Expected YYYY-MM-DD.")

    if start_dt > end_dt:
        raise ValidationError(f"start_date ({start_date}) must be before or equal to end_date ({end_date})")

    # Check date range is reasonable (max 1 year to avoid excessive API calls)
    date_diff = (end_dt - start_dt).days
    if date_diff > 366:
        logger.warning(f"Date range spans {date_diff} days. This may take a while and make many API calls.")

    logger.info(f"Fetching AirNow data for zipcode {zipcode} from {start_date} to {end_date}")
    logger.debug(f"Date range: {date_diff + 1} days")

    base_url = "https://www.airnowapi.org/aq/observation/zipCode/historical/"

    # Create session with retry logic
    session = requests.Session()
    retry_strategy = Retry(
        total=max_retries,
        backoff_factor=RETRY_BACKOFF_FACTOR,
        status_forcelist=[429, 500, 502, 503, 504],
        allowed_methods=["GET"],
        raise_on_status=False
    )
    adapter = HTTPAdapter(max_retries=retry_strategy)
    session.mount("http://", adapter)
    session.mount("https://", adapter)

    all_data = []
    current_date = start_dt
    successful_days = 0
    failed_days = 0
    total_days = date_diff + 1

    while current_date <= end_dt:
        params = {
            "format": "application/json",
            "zipCode": zipcode,
            "date": current_date.strftime("%Y-%m-%dT00-0000"),
            "distance": 25,
            "API_KEY": api_key
        }

        try:
            response = session.get(base_url, params=params, timeout=timeout)

            if response.status_code == 200:
                try:
                    data = response.json()

                    if isinstance(data, list):
                        for record in data:
                            record["date"] = current_date.strftime("%Y-%m-%d")
                            all_data.append(record)
                        successful_days += 1
                        logger.debug(f"{current_date.strftime('%Y-%m-%d')}: Retrieved {len(data)} records")
                    else:
                        logger.warning(f"{current_date.strftime('%Y-%m-%d')}: Unexpected response format")
                        failed_days += 1

                except json.JSONDecodeError as e:
                    logger.warning(f"{current_date.strftime('%Y-%m-%d')}: Invalid JSON response: {e}")
                    failed_days += 1

            elif response.status_code == 429:
                logger.warning(f"{current_date.strftime('%Y-%m-%d')}: Rate limited. Waiting longer...")
                time.sleep(5.0)  # Longer wait on rate limit
                failed_days += 1

            else:
                logger.warning(
                    f"{current_date.strftime('%Y-%m-%d')}: HTTP {response.status_code}: {response.reason}"
                )
                failed_days += 1

        except requests.exceptions.Timeout:
            logger.warning(f"{current_date.strftime('%Y-%m-%d')}: Request timed out")
            failed_days += 1

        except requests.exceptions.ConnectionError as e:
            logger.warning(f"{current_date.strftime('%Y-%m-%d')}: Connection error: {e}")
            failed_days += 1

        except requests.exceptions.RequestException as e:
            logger.warning(f"{current_date.strftime('%Y-%m-%d')}: Request error: {e}")
            failed_days += 1

        except Exception as e:
            logger.error(f"{current_date.strftime('%Y-%m-%d')}: Unexpected error: {e}")
            failed_days += 1

        # Rate limiting - be nice to the API
        time.sleep(RATE_LIMIT_DELAY)

        current_date += timedelta(days=1)

        # Progress logging every 30 days
        days_processed = successful_days + failed_days
        if days_processed % 30 == 0:
            logger.info(f"Progress: {days_processed}/{total_days} days processed")

    # Summary logging
    logger.info(f"AirNow fetch complete: {successful_days} successful, {failed_days} failed out of {total_days} days")

    if failed_days > 0:
        logger.warning(f"{failed_days} days had fetch errors")

    if not all_data:
        logger.warning("No data retrieved from AirNow API")
        return pd.DataFrame()

    df = pd.DataFrame(all_data)
    logger.info(f"Total records retrieved: {len(df)}")

    return df


# Demo/Example usage
def main(debug: bool = False):
    """
    Main function demonstrating the air quality comparison tool.

    Args:
        debug: If True, enable debug-level logging for verbose output.

    Returns:
        AirQualityComparison instance for further use.
    """
    # Configure logging based on debug flag
    if debug:
        setup_logging(level=logging.DEBUG)
        logger.info("Debug mode enabled")
    else:
        setup_logging(level=logging.WARNING)

    print("\n" + "=" * 70)
    print("AIR QUALITY DATA COMPARISON TOOL")
    print("Placer County, California - 2024")
    print("=" * 70 + "\n")

    try:
        # Initialize comparison without API credentials for demo
        comparison = AirQualityComparison()

        # Generate and print report
        report = comparison.generate_report(2024)
        print(report)

    except ValidationError as e:
        print(f"Validation Error: {e}")
        logger.error(f"Validation error in main: {e}")
        return None
    except AirQualityError as e:
        print(f"Air Quality Error: {e}")
        logger.error(f"Air quality error in main: {e}")
        return None
    except Exception as e:
        print(f"Unexpected Error: {e}")
        logger.exception(f"Unexpected error in main: {e}")
        return None

    # Print API access information
    print("\n" + "=" * 70)
    print("HOW TO ACCESS REAL-TIME DATA")
    print("=" * 70)

    print("\n1. EPA AQS API:")
    print("   - Register at: https://aqs.epa.gov/aqsweb/documents/data_api.html#signup")
    print("   - Free API key provided via email")
    print("   - Provides historical monitoring data")

    print("\n2. AirNow API:")
    print("   - Register at: https://docs.airnowapi.org/account/request/")
    print("   - Free API key for personal use")
    print("   - Provides real-time and recent historical data")

    print("\n3. American Lung Association:")
    print("   - Website: https://www.lung.org/research/sota")
    print("   - Interactive rankings tool available")
    print("   - No API - manual data download required")

    print("\n" + "=" * 70)
    print("EXAMPLE CODE USAGE")
    print("=" * 70)

    example_code = '''
# Enable debug logging for troubleshooting:
setup_logging(level=logging.DEBUG, log_file="air_quality.log")

# With EPA API credentials:
try:
    comparison = AirQualityComparison(
        epa_email="your.email@example.com",
        epa_api_key="your_api_key_here"
    )
    results = comparison.compare_sources(2024)
    print(comparison.generate_report(2024))
except ValidationError as e:
    print(f"Invalid credentials: {e}")
except APIError as e:
    print(f"API request failed: {e}")

# Fetch just EPA data with error handling:
try:
    epa = EPAAirQualityFetcher("your.email@example.com", "your_api_key")
    daily_data = epa.get_daily_pm25_data(2024)
    print(daily_data.head())
except ValidationError as e:
    print(f"Invalid input: {e}")
except APIError as e:
    print(f"API error: {e}")

# Get monitoring sites:
sites = epa.get_monitoring_sites()
print(sites)

# Using AirNow API with validation:
try:
    airnow_data = fetch_airnow_data(
        api_key="your_airnow_key",
        zipcode="95661",  # Roseville, Placer County
        start_date="2024-01-01",
        end_date="2024-01-31"  # Smaller range for testing
    )
    pm25_data = airnow_data[airnow_data["ParameterName"] == "PM2.5"]
    print(pm25_data)
except ValidationError as e:
    print(f"Invalid parameters: {e}")

# Load ALA data from file with error handling:
try:
    ala = ALAAirQualityData("path/to/ala_data.csv")
    print(ala.data.head())
except ValidationError as e:
    print(f"File error: {e}")
except DataError as e:
    print(f"Data error: {e}")
'''
    print(example_code)

    # Return comparison object for further use
    return comparison


if __name__ == "__main__":
    import sys

    # Check for --debug flag
    debug_mode = "--debug" in sys.argv or "-d" in sys.argv
    comparison = main(debug=debug_mode)
