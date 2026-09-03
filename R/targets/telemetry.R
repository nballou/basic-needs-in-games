# Vendored from data/open-play-v1.2.5/R/helpers.R (the Zenodo archive's own
# helpers file, distinct from this repo's R/helpers.R). Copied verbatim
# because _targets.R needs a statically-sourceable copy that exists before
# the archive is downloaded.

# Parse a "GMT+HHMM"/"GMT-HHMM" style offset string into seconds.
offset_secs <- function(z) {
  z <- toupper(sub("^GMT", "", z))
  m <- regexec("^([+-])(\\d{2})(\\d{2})$", z)
  p <- regmatches(z, m)
  sapply(
    p,
    \(x) {
      if (length(x)) {
        (ifelse(x[2] == "-", -1, 1)) *
          (as.numeric(x[3]) * 3600 + as.numeric(x[4]) * 60)
      } else {
        0
      }
    }
  )
}

# Map country and standard time offset to IANA timezone.
get_iana_timezone <- function(country, local_timezone) {
  base_offset <- offset_secs(local_timezone)

  case_when(
    # US timezones
    country == "US" & base_offset == -18000 ~ "America/New_York",
    country == "US" & base_offset == -21600 ~ "America/Chicago",
    country == "US" & base_offset == -25200 ~ "America/Denver",
    country == "US" & base_offset == -28800 ~ "America/Los_Angeles",
    country == "US" & base_offset == -32400 ~ "America/Anchorage",
    country == "US" & base_offset == -36000 ~ "Pacific/Honolulu",

    # UK timezone
    country == "UK" & base_offset == 0 ~ "Europe/London",

    # UK residents in other timezones
    country == "UK" & base_offset == -18000 ~ "America/New_York",
    country == "UK" & base_offset == -21600 ~ "America/Chicago",
    country == "UK" & base_offset == -25200 ~ "America/Denver",
    country == "UK" & base_offset == -28800 ~ "America/Los_Angeles",
    country == "UK" & base_offset == -32400 ~ "America/Anchorage",
    country == "UK" & base_offset == -36000 ~ "Pacific/Honolulu",

    # OTHER country
    country == "OTHER" & base_offset == -18000 ~ "America/New_York",
    country == "OTHER" & base_offset == -21600 ~ "America/Chicago",
    country == "OTHER" & base_offset == -25200 ~ "America/Denver",
    country == "OTHER" & base_offset == -28800 ~ "America/Los_Angeles",
    country == "OTHER" & base_offset == -36000 ~ "Pacific/Honolulu",
    country == "OTHER" & base_offset == 0 ~ "Europe/London",

    TRUE ~ NA_character_
  )
}

# Vectorized DST-aware offset calculation. Returns the number of seconds to
# ADD to UTC to get local time (accounts for daylight saving time).
#
# Note: R's POSIXct can only have ONE timezone attribute per vector, so we
# cannot create a properly-labeled mixed-timezone column. Instead, we return
# the offset in seconds, and callers should:
#   - Use (utc_timestamp + offset) for hour/date extraction
#   - Be explicit that these are "local time values with UTC labels"
get_dst_offset <- function(utc_timestamp, country, local_timezone) {
  iana_tz <- get_iana_timezone(country, local_timezone)

  offset <- numeric(length(utc_timestamp))
  unique_tzs <- unique(iana_tz[!is.na(iana_tz)])

  for (tz in unique_tzs) {
    mask <- !is.na(iana_tz) & iana_tz == tz
    utc_subset <- utc_timestamp[mask]
    local_times <- with_tz(utc_subset, tzone = tz)
    local_as_utc <- force_tz(local_times, tzone = "UTC")
    offset[mask] <- as.numeric(difftime(local_as_utc, utc_subset, units = "secs"))
  }

  offset
}
