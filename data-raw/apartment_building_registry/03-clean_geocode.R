# Clean results from geocoding

library(dplyr)
library(stringr)
library(purrr)
library
devtools::load_all() # Load package itself to get read_latest_file

apartment_building_registry_geocoded <- read_latest_file(directory = here::here("data-raw", "apartment_building_registry", "geocode_raw"), suffix = "-apartment_building_registry_geocoded.rds", fileext = "rds")

# Check if any records were duplicated
nrow(apartment_building_registry) == nrow(apartment_building_registry_geocoded)

# Check any that had errors
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  mutate(address_geocode_error = map_lgl(address_geocode_error,  ~ !is.null(.x)))

apartment_building_registry_geocoded %>%
  filter(address_geocode_error) %>%
  nrow() == 0

# Check that we have all fields for every record
apartment_building_registry_geocoded %>%
  select(starts_with("bing")) %>%
  visdat::vis_miss()

issues_missing <- apartment_building_registry_geocoded %>%
  select(`_id`, SITE_ADDRESS, PCODE, starts_with("bing")) %>%
  filter(is.na(bing_latitude) | is.na(bing_longitude) | is.na(bing_postal_code)) %>%
  mutate(issue = "missing")

# Requery ones that are missing - some of them come up!
safely_geocode_address <- safely(~ geocode_address(.x, quiet = TRUE), otherwise = NA)

missing_geocoding_filled <- issues_missing %>%
  select(`_id`, SITE_ADDRESS) %>%
  mutate(address_geocode = map(SITE_ADDRESS, function(x) {
    safely_geocode_address(x)
  })) %>%
  mutate(
    address_geocode = map(address_geocode, "result"),
    address_geocode_error = map(address_geocode, "error")
  ) %>%
  unnest(cols = c(address_geocode)) %>%
  select(-address_geocode_error)

missing_geocoding_filled <- missing_geocoding_filled %>%
  filter(!is.na(bing_address))

# Update the missing ones with these, then check for other issues
apartment_building_registry_geocoded <- apartment_building_registry_geocoded %>%
  rows_update(missing_geocoding_filled, by = c("_id", "SITE_ADDRESS"))

# Any still missing

issues_missing <- apartment_building_registry_geocoded %>%
  select(`_id`, SITE_ADDRESS, PCODE, starts_with("bing")) %>%
  filter(is.na(bing_latitude) | is.na(bing_longitude) | is.na(bing_postal_code)) %>%
  mutate(issue = "missing")

# Any with "Low" confidence from the API
issues_low_confidence <- apartment_building_registry_geocoded %>%
  filter(bing_confidence == "Low") %>%
  select(`_id`, SITE_ADDRESS, PCODE, starts_with("bing")) %>%
  mutate(issue = "low confidence")

# Any where PCODE (first three digits of postal code, from the extract) doesn't match the postal code from the API
# Some records are just missing PCODE, so don't count that as an error
issues_pcode_mismatch <- apartment_building_registry_geocoded %>%
  filter(!str_starts(bing_postal_code, PCODE) & !is.na(PCODE)) %>%
  select(`_id`, SITE_ADDRESS, PCODE, starts_with("bing")) %>%
  mutate(issue = "pcode mismatch")

# Combine issues to handle
geocode_issues <- issues_missing %>%
  bind_rows(issues_low_confidence) %>%
  bind_rows(issues_pcode_mismatch) %>%
  group_by(`_id`, SITE_ADDRESS) %>%
  mutate(issue = stringr::str_c(issue, collapse = ", ")) %>%
  ungroup() %>%
  distinct()

geocode_issues %>%
  count(issue)

# Fixing

geocode_issues %>%
  select(issue, SITE_ADDRESS, bing_address, PCODE, bing_postal_code, bing_latitude, bing_longitude) %>% View()

corrections <- tribble(
  ~SITE_ADDRESS, ~manual_address, ~manual_latitude, ~manual_longitude, ~manual_postal_code,
  "10  VENA WAY", "10 Vena Way", 43.75049883092916, -79.5416203305828, NA_character_,
  "245 A  HOWLAND AVE ", "245 Howland Ave", 43.672864274077, -79.41102178778262, NA_character_,
  "190  JAMESON AVE ", "190 Jameson Ave", 43.639510732000055, -79.43710527249598, "M6K 2Z5",
  "6  VENA WAY", "6 Vena Way", 43.75040688113031, -79.54156401524023, NA_character_,
  "6 A  GREENLAW AVE ", "6 A Greenlaw Ave", 43.67267750136975, -79.44646015900271, NA_character_,
  "8  ST THOMAS ST ", "8 St Thomas St", 43.66862519229393, -79.3909378724956, "M5S 2B8",
  "75  FORTY SECOND ST ", "75 Forty Second St", 43.58905081747705, -79.54391345715403, "M8W 3P5",
  "55  FORTY SECOND ST ", "55 Forty Second St", 43.588666362969235, -79.54345093001373, "M8W 3P3",
  "25  ST MARY ST ", "25 St Mary St", 43.66730181819619, -79.38713051466665, "M4Y 2R4",
  "45  FORTY SECOND ST ", "45 Forty Second St", 43.588183636931625, -79.54274833001381, "M8W 3P4",
  "87  FORTY SECOND ST ", "87 Forty Second St", 43.58944029168145, -79.54465887234079, "M8W 3P5",
  "6  TWENTY FOURTH ST ", "6 Twenty Fourth St", 43.59744386947406, -79.52336113001373, "M8V 3N4",
  "15  FORTY THIRD ST ", "15 Forty Third St", 43.59027353747142, -79.54609615885032, "M8W 3P7",
  "150  ROSEMOUNT AVE ", "150 Rosemount Ave", 43.70406251322614, -79.51958339511457, "M9N 3B9",
  "1079  LAWRENCE AVE W", "1079 Lawrence Ave W", 43.711962658834956, -79.46270853001178, "M6A 1C9",
  "2126  VICTORIA PARK AVE ", "2126 Victoria Park Ave", 43.759446681217504, -79.31639663001107, "M1R 1V7",
  "100  CAVELL AVE ", "100 Cavell Ave", 43.61603156284792, -79.49653436628094, "M8V 3V6",
  "1049  LAWRENCE AVE W", "1049 Lawrence Ave W", 43.71240910408016, -79.46043385884825,"M6A 1C3",
  "1055  VICTORIA PARK AVE ", "1055 Victoria Park Ave", 43.70674167901931, -79.29449785884835, "M4B 2J8",
  "750  YORK MILLS RD ", "750 York Mills Rd", 43.753760928757956, -79.36088917249076, "M3B 1W9",
  "8  HECTOR AVE ", "8 Hector Ave", 43.68007403829632, -79.48977327249183, "M6N 2M1",
  "15  THIRTY THIRD ST ", "15 Thirty Third St", 43.59205730857955, -79.53071613016472, "M8W 3G7",
  "1700  FINCH AVE E", "1700 Finch Ave E", 43.79415463471499, -79.35136273016141, "M2J 4X8",
  "3111  EGLINTON AVE E", "3111 Eglinton Ave E", 43.74160813033203, -79.22402243016236, "M1J 2G4",
  "280  ST GEORGE ST ", "280 St George St", 43.67401083908051, -79.40325970132774, "M5R 2P7",
  "355  ST CLAIR AVE W", "355 St Clair Ave W", 43.683933337797, -79.41284060132753, "M5P 1N5",
  "4  SHERBOURNE ST N", "4 Sherbourne St N", 43.674069939072936, -79.37777085899904, "M4W 2T1",
  "7  EDMUND AVE  ", "7 Edmund Ave", 43.68207272020809, -79.40082021482047, "M4V 1H2",
  "651  LAWRENCE AVE W", "651 Lawrence Ave W", 43.71622188811714, -79.44264040132697, "M6A 1A9",
  "3171-3181  EGLINTON AVE E", "3181 Eglinton Ave E", 43.74259578128866, -79.2192437436552, "M1J 2G9",
  "1607  JANE ST ", "1607 Jane St", 43.70080382263062, -79.50267177249151, "M9N 2R8",
  "25  FORTY THIRD ST  ", "25 Forty Third St", 43.590352562200415, -79.54645818783612, "M8W 3P7",
  "1540  VICTORIA PARK AVE ", "1540 Victoria Park Ave", 43.72612787269327, -79.30380707249113, "M1L 4S1",
  "36  CHURCH ST ", "36 Church St", 43.704291691204936, -79.52341414365578, "M9N 1M7",
  "1780  VICTORIA PARK AVE  ", "1780 Victoria Park Ave", 43.73853562429903, -79.30874350132665, "M1R 1S6",
  "9  THIRTY THIRD ST  ", "9 Thirty Third St", 43.59166980852946, -79.530529701329, "M8W 3G7",
  "3125  LAWRENCE AVE E", "3125 Lawrence Ave E", 43.75528312856072, -79.2438524724907, "M1H 1A2",
  "345  MERTON ST  ", "345 Merton St", 43.69797966540959, -79.38567783016298, "M4S 1B5",
  "3131  EGLINTON AVE E ", "3131 Eglinton Ave E", 43.741713479120826, -79.22286610132659, "M1J 2G6",
  "11  THIRTY THIRD ST ", "11 Thirty Third St", 43.59190037912575, -79.53052970132894, "M8W 3G7",
  "4000  YONGE ST ", "4000 Yonge St", 43.7423371280044, -79.40741625899793, "M4N 2N9",
  "551  EGLINTON AVE E", "551 Eglinton Ave E", 43.71046158960812, -79.37863993016272, "M4P 1N8",


)


# corrections from manual inspection of missing geocoded entries
# From google
corrections <- tribble(
  ~SITE_ADDRESS, ~manual_address, ~manual_latitude, ~manual_longitude,
  "2877 A  ELLESMERE RD", "2877 Ellesmere Rd", 43.780633, -79.20349,
  "2  KINGSTON RD", "2 Kingston Rd", 43.774061, -79.183909,
  "2  VENA WAY", "2 Vena Way", 43.7493214854571, -79.54115877106196,
  "360  BLOOR ST W", "360 Bloor St W", 43.666789, -79.405123,
  "21  MAYFAIR AVE", "21 Mayfair Ave", 43.703712, -79.422213,
  "127  ISABELLA ST", "127 Isabella St", 43.669023, -79.377737,
  "74  HUBBARD BLVD", "74 Hubbard Blvd", 43.669107, -79.291128,
)


# TODO: look into dplyr::update

# replace with corrections
corrected_apt_registry <- geocode_apt_registry %>%
  left_join(corrections, by = "SITE_ADDRESS") %>%
  mutate(
    bing_address = coalesce(manual_address, bing_address),
    bing_latitude = if_else(!is.na(manual_latitude), manual_latitude, bing_latitude),
    bing_longitude = if_else(!is.na(manual_longitude), manual_longitude, bing_longitude)
  ) %>%
  select(-manual_address, -manual_latitude, -manual_longitude)

# convert to sf
apt_registry_sf <- corrected_apt_registry %>%
  st_as_sf(coords = c("bing_longitude", "bing_latitude"), crs = 4326)
