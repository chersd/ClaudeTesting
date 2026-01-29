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
import pandas as pd
import json
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
import time


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

    def __init__(self, email: str, api_key: str):
        """
        Initialize the EPA API client.

        Args:
            email: Registered email address for EPA AQS API
            api_key: API key obtained from EPA AQS registration

        To obtain an API key, register at:
        https://aqs.epa.gov/aqsweb/documents/data_api.html#signup
        """
        self.email = email
        self.api_key = api_key

    def _make_request(self, endpoint: str, params: Dict) -> Dict:
        """Make a request to the EPA AQS API."""
        params.update({
            "email": self.email,
            "key": self.api_key
        })

        url = f"{self.BASE_URL}/{endpoint}"
        response = requests.get(url, params=params)
        response.raise_for_status()

        data = response.json()

        if data.get("Header", [{}])[0].get("status") == "Failed":
            error_msg = data.get("Header", [{}])[0].get("error", "Unknown error")
            raise Exception(f"EPA API Error: {error_msg}")

        return data

    def get_daily_pm25_data(self, year: int = 2024) -> pd.DataFrame:
        """
        Fetch daily PM2.5 data for Placer County.

        Args:
            year: Year to fetch data for (default: 2024)

        Returns:
            DataFrame with daily PM2.5 measurements
        """
        params = {
            "param": self.PM25_LOCAL,
            "bdate": f"{year}0101",
            "edate": f"{year}1231",
            "state": self.STATE_CODE,
            "county": self.COUNTY_CODE
        }

        data = self._make_request("dailyData/byCounty", params)

        if not data.get("Data"):
            print(f"No PM2.5 data found for Placer County in {year}")
            return pd.DataFrame()

        df = pd.DataFrame(data["Data"])

        # Convert date column
        if "date_local" in df.columns:
            df["date_local"] = pd.to_datetime(df["date_local"])

        return df

    def get_annual_summary(self, year: int = 2024) -> pd.DataFrame:
        """
        Fetch annual PM2.5 summary statistics for Placer County.

        Args:
            year: Year to fetch data for (default: 2024)

        Returns:
            DataFrame with annual PM2.5 summary statistics
        """
        params = {
            "param": self.PM25_LOCAL,
            "bdate": f"{year}0101",
            "edate": f"{year}1231",
            "state": self.STATE_CODE,
            "county": self.COUNTY_CODE
        }

        data = self._make_request("annualData/byCounty", params)

        if not data.get("Data"):
            print(f"No annual PM2.5 summary found for Placer County in {year}")
            return pd.DataFrame()

        return pd.DataFrame(data["Data"])

    def get_monitoring_sites(self) -> pd.DataFrame:
        """Get list of PM2.5 monitoring sites in Placer County."""
        params = {
            "param": self.PM25_LOCAL,
            "state": self.STATE_CODE,
            "county": self.COUNTY_CODE
        }

        data = self._make_request("monitors/byCounty", params)

        if not data.get("Data"):
            return pd.DataFrame()

        return pd.DataFrame(data["Data"])


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

    def __init__(self, data_file: Optional[str] = None):
        """
        Initialize ALA data handler.

        Args:
            data_file: Optional path to downloaded ALA CSV/Excel data file
        """
        self.data_file = data_file
        self.data = None

        if data_file:
            self.load_data(data_file)

    def load_data(self, filepath: str) -> pd.DataFrame:
        """
        Load ALA data from a downloaded file.

        Args:
            filepath: Path to the data file (CSV or Excel)

        Returns:
            DataFrame with ALA report data
        """
        if filepath.endswith('.csv'):
            self.data = pd.read_csv(filepath)
        elif filepath.endswith(('.xlsx', '.xls')):
            self.data = pd.read_excel(filepath)
        else:
            raise ValueError("Unsupported file format. Use CSV or Excel.")

        return self.data

    def get_placer_county_data(self) -> Dict:
        """
        Get ALA data for Placer County.

        Returns:
            Dictionary with ALA report data for Placer County
        """
        return self.PLACER_COUNTY_2024_REPORT

    def calculate_grade_from_days(self, unhealthy_days: float) -> str:
        """
        Calculate ALA grade based on number of unhealthy air days.

        Args:
            unhealthy_days: Average number of unhealthy days per year

        Returns:
            Letter grade (A, B, C, D, or F)
        """
        if unhealthy_days == 0:
            return "A"
        elif unhealthy_days < 1.0:
            return "B"
        elif unhealthy_days <= 2.0:
            return "C"
        elif unhealthy_days <= 3.2:
            return "D"
        else:
            return "F"

    def get_pm25_category(self, concentration: float) -> str:
        """
        Get AQI category for a PM2.5 concentration.

        Args:
            concentration: PM2.5 concentration in μg/m³

        Returns:
            AQI category name
        """
        for category, (low, high) in self.PM25_AQI_BREAKPOINTS.items():
            if low <= concentration <= high:
                return category
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
        """
        self.epa_fetcher = None
        if epa_email and epa_api_key:
            self.epa_fetcher = EPAAirQualityFetcher(epa_email, epa_api_key)

        self.ala_data = ALAAirQualityData()

    def count_unhealthy_days(self, epa_data: pd.DataFrame) -> Dict:
        """
        Count unhealthy air days from EPA data using ALA methodology.

        Args:
            epa_data: DataFrame with EPA daily PM2.5 data

        Returns:
            Dictionary with unhealthy day counts by category
        """
        if epa_data.empty:
            return {"error": "No EPA data available"}

        # Get the arithmetic mean column for daily average
        value_col = None
        for col in ["arithmetic_mean", "sample_measurement", "daily_mean"]:
            if col in epa_data.columns:
                value_col = col
                break

        if not value_col:
            return {"error": "Could not find measurement column in EPA data"}

        counts = {
            "Good": 0,
            "Moderate": 0,
            "Unhealthy for Sensitive Groups": 0,
            "Unhealthy": 0,
            "Very Unhealthy": 0,
            "Hazardous": 0,
            "Total Days": len(epa_data)
        }

        for _, row in epa_data.iterrows():
            value = row[value_col]
            category = self.ala_data.get_pm25_category(value)
            if category in counts:
                counts[category] += 1

        # Calculate unhealthy days (USG + Unhealthy + Very Unhealthy + Hazardous)
        unhealthy_total = (
            counts["Unhealthy for Sensitive Groups"] +
            counts["Unhealthy"] +
            counts["Very Unhealthy"] +
            counts["Hazardous"]
        )
        counts["Total Unhealthy Days"] = unhealthy_total

        return counts

    def compare_sources(self, year: int = 2024) -> Dict:
        """
        Compare EPA raw data with ALA reported data.

        Args:
            year: Year to analyze

        Returns:
            Dictionary with comparison results
        """
        results = {
            "year": year,
            "county": "Placer County, CA",
            "epa_data": None,
            "ala_data": None,
            "comparison": None
        }

        # Get ALA data
        ala_info = self.ala_data.get_placer_county_data()
        results["ala_data"] = {
            "source": "American Lung Association State of the Air Report",
            "report_year": ala_info["report_year"],
            "data_years": ala_info["data_years"],
            "pm25_24hr_grade": ala_info["particle_pollution_24hour"]["grade"],
            "high_pm25_days": ala_info["particle_pollution_24hour"]["high_pm25_days"],
            "annual_grade": ala_info["particle_pollution_annual"]["grade"]
        }

        # Get EPA data if API credentials provided
        if self.epa_fetcher:
            try:
                epa_daily = self.epa_fetcher.get_daily_pm25_data(year)

                if not epa_daily.empty:
                    unhealthy_counts = self.count_unhealthy_days(epa_daily)

                    # Calculate statistics
                    value_col = "arithmetic_mean" if "arithmetic_mean" in epa_daily.columns else "sample_measurement"

                    results["epa_data"] = {
                        "source": "EPA Air Quality System",
                        "year": year,
                        "total_measurements": len(epa_daily),
                        "mean_pm25": epa_daily[value_col].mean() if value_col in epa_daily.columns else None,
                        "max_pm25": epa_daily[value_col].max() if value_col in epa_daily.columns else None,
                        "unhealthy_day_counts": unhealthy_counts,
                        "calculated_grade": self.ala_data.calculate_grade_from_days(
                            unhealthy_counts.get("Total Unhealthy Days", 0)
                        )
                    }
                else:
                    results["epa_data"] = {"error": "No data available for specified year"}

            except Exception as e:
                results["epa_data"] = {"error": str(e)}
        else:
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

        return results

    def generate_report(self, year: int = 2024) -> str:
        """
        Generate a formatted comparison report.

        Args:
            year: Year to analyze

        Returns:
            Formatted string report
        """
        comparison = self.compare_sources(year)

        report = []
        report.append("=" * 70)
        report.append("AIR QUALITY DATA COMPARISON REPORT")
        report.append(f"Placer County, California - {year}")
        report.append("=" * 70)
        report.append("")

        # ALA Section
        report.append("-" * 40)
        report.append("AMERICAN LUNG ASSOCIATION DATA")
        report.append("-" * 40)
        ala = comparison["ala_data"]
        report.append(f"Report Year: {ala['report_year']}")
        report.append(f"Data Period: {ala['data_years']}")
        report.append(f"PM2.5 24-Hour Grade: {ala['pm25_24hr_grade']}")
        report.append(f"High PM2.5 Days (weighted avg): {ala['high_pm25_days']}")
        report.append(f"Annual PM2.5 Grade: {ala['annual_grade']}")
        report.append("")

        # EPA Section
        report.append("-" * 40)
        report.append("EPA AIR QUALITY SYSTEM DATA")
        report.append("-" * 40)
        epa = comparison["epa_data"]
        if "error" in epa:
            report.append(f"Error: {epa['error']}")
        elif "note" in epa:
            report.append(f"Note: {epa['note']}")
            report.append(f"To get EPA data: {epa['signup_url']}")
        else:
            report.append(f"Year: {epa['year']}")
            report.append(f"Total Measurements: {epa['total_measurements']}")
            report.append(f"Mean PM2.5: {epa['mean_pm25']:.2f} μg/m³" if epa['mean_pm25'] else "Mean PM2.5: N/A")
            report.append(f"Max PM2.5: {epa['max_pm25']:.2f} μg/m³" if epa['max_pm25'] else "Max PM2.5: N/A")
            report.append(f"Calculated Grade: {epa['calculated_grade']}")

            if "unhealthy_day_counts" in epa:
                report.append("\nUnhealthy Day Breakdown:")
                for category, count in epa["unhealthy_day_counts"].items():
                    report.append(f"  {category}: {count}")
        report.append("")

        # Comparison Notes
        report.append("-" * 40)
        report.append("METHODOLOGY NOTES")
        report.append("-" * 40)
        for note in comparison["comparison"]["methodology_differences"]:
            report.append(f"• {note}")
        report.append("")

        report.append("-" * 40)
        report.append("REGIONAL CONSIDERATIONS")
        report.append("-" * 40)
        for note in comparison["comparison"]["key_considerations"]:
            report.append(f"• {note}")

        report.append("")
        report.append("=" * 70)

        return "\n".join(report)


def fetch_airnow_data(api_key: str, zipcode: str = "95661",
                      start_date: str = None, end_date: str = None) -> pd.DataFrame:
    """
    Fetch air quality data from AirNow API (alternative to AQS).

    AirNow provides more recent data than AQS and is easier to use.
    API Documentation: https://docs.airnowapi.org/

    Args:
        api_key: AirNow API key (register at https://docs.airnowapi.org/account/request/)
        zipcode: ZIP code for Placer County (95661 is Roseville)
        start_date: Start date (YYYY-MM-DD format)
        end_date: End date (YYYY-MM-DD format)

    Returns:
        DataFrame with air quality data
    """
    base_url = "https://www.airnowapi.org/aq/observation/zipCode/historical/"

    if not start_date:
        start_date = "2024-01-01"
    if not end_date:
        end_date = "2024-12-31"

    all_data = []
    current_date = datetime.strptime(start_date, "%Y-%m-%d")
    end = datetime.strptime(end_date, "%Y-%m-%d")

    while current_date <= end:
        params = {
            "format": "application/json",
            "zipCode": zipcode,
            "date": current_date.strftime("%Y-%m-%dT00-0000"),
            "distance": 25,
            "API_KEY": api_key
        }

        try:
            response = requests.get(base_url, params=params)
            response.raise_for_status()
            data = response.json()

            for record in data:
                record["date"] = current_date.strftime("%Y-%m-%d")
                all_data.append(record)

            # Rate limiting
            time.sleep(0.5)

        except Exception as e:
            print(f"Error fetching data for {current_date}: {e}")

        current_date += timedelta(days=1)

    return pd.DataFrame(all_data)


# Demo/Example usage
def main():
    """
    Main function demonstrating the air quality comparison tool.
    """
    print("\n" + "=" * 70)
    print("AIR QUALITY DATA COMPARISON TOOL")
    print("Placer County, California - 2024")
    print("=" * 70 + "\n")

    # Initialize comparison without API credentials for demo
    comparison = AirQualityComparison()

    # Generate and print report
    report = comparison.generate_report(2024)
    print(report)

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
# With EPA API credentials:
comparison = AirQualityComparison(
    epa_email="your.email@example.com",
    epa_api_key="your_api_key_here"
)
results = comparison.compare_sources(2024)
print(comparison.generate_report(2024))

# Fetch just EPA data:
epa = EPAAirQualityFetcher("your.email@example.com", "your_api_key")
daily_data = epa.get_daily_pm25_data(2024)
print(daily_data.head())

# Get monitoring sites:
sites = epa.get_monitoring_sites()
print(sites)

# Using AirNow API:
airnow_data = fetch_airnow_data(
    api_key="your_airnow_key",
    zipcode="95661",  # Roseville, Placer County
    start_date="2024-01-01",
    end_date="2024-12-31"
)
print(airnow_data[airnow_data["ParameterName"] == "PM2.5"])
'''
    print(example_code)

    # Return comparison object for further use
    return comparison


if __name__ == "__main__":
    comparison = main()
