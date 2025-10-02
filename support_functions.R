if (!("pak" %in% installed.packages())) {
  install.packages("pak")
}

pak::pak("tidyverse")
pak::pak("httr2")
pak::pak("mapgl")
pak::pak("gt")
pak::pak("gtExtras")
pak::pak("sf")
pak::pak("logger")
pak::pak("glue")
pak::pak("bslib")
pak::pak("fontawesome")

library(httr2)
library(tidyverse)
library(sf)
library(logger)
library(googlesheets4)
library(glue)
library(bslib)
library(mapgl)
library(gt)
library(gtExtras)
library(fontawesome)

client1 <-
  oauth_client(
    id = Sys.getenv("OPENSKY_CLIENT_ID"),
    token_url = "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    secret = Sys.getenv("OPENSKY_CLIENT_SECRET"),
    auth = "header"
  )

get_flight_track <- function(icao24, as_sf = TRUE) {
  tryCatch(
    {
      opensky_response <- request(
        "https://opensky-network.org/api/tracks/all?"
      ) |>
        req_oauth_client_credentials(client1) |>
        req_url_query(icao24 = str_to_lower(icao24)) |>
        req_perform()

      check_remaining_credits <- opensky_response |>
        resp_header("X-Rate-Limit-Remaining")

      log_info("Remaining API credits: {check_remaining_credits}")

      flight_track <- opensky_response |>
        resp_body_json() |>
        as_tibble() |>
        unnest_wider(col = path, names_sep = "_") |>
        rename(
          timestamp = path_1,
          latitude = path_2,
          longitude = path_3,
          baro_altitude = path_4,
          true_track = path_5,
          on_ground = path_6
        ) |>
        mutate(across(c(startTime, endTime, timestamp), \(x) as_datetime(x))) |>
        mutate(callsign = trimws(callsign)) |>
        arrange(timestamp)

      if (as_sf) {
        flight_track <- st_as_sf(
          flight_track,
          coords = c("longitude", "latitude", "baro_altitude"),
          dim = "XYZ",
          crs = 4326
        ) |>
          group_by(icao24, callsign) |>
          summarize(do_union = FALSE) |>
          st_cast("LINESTRING") |>
          st_wrap_dateline()
      }
      return(flight_track)
    },
    error = function(e) {
      log_error(
        "Error occurred when retrieving flight track for {icao24}. {e} Check your icao24 value: {icao24} may be valid but currently inactive."
      )
      flight_position <- tibble(
        icao24 = str_to_lower(icao24),
        on_ground = TRUE
      )
    }
  )
}

get_state_vector <- function(icao24, as_sf = TRUE) {
  tryCatch(
    {
      opensky_response <- request(
        "https://opensky-network.org/api/states/all?"
      ) |>
        req_oauth_client_credentials(client1) |>
        req_url_query(icao24 = str_to_lower(icao24)) |>
        req_perform()

      check_remaining_credits <- opensky_response |>
        resp_header("X-Rate-Limit-Remaining")

      log_info("Remaining API credits: {check_remaining_credits}")

      flight_position <- opensky_response |>
        resp_body_json() |>
        as_tibble() |>
        unnest_wider(col = states, names_sep = "_") |>
        rename(
          icao24 = states_1,
          callsign = states_2,
          origin_country = states_3,
          time_position = states_4,
          last_contact = states_5,
          longitude = states_6,
          latitude = states_7,
          baro_altitude = states_8,
          on_ground = states_9,
          velocity = states_10,
          true_track = states_11,
          vertical_rate = states_12,
          sensors = states_13,
          geo_altitude = states_14,
          squawk = states_15,
          special_purpose = states_16,
          position_source = states_17
        ) |>
        mutate(callsign = trimws(callsign)) |>
        mutate(across(c(time, time_position, last_contact), \(x) {
          as_datetime(x)
        }))

      if (as_sf) {
        flight_position <- st_as_sf(
          flight_position,
          coords = c("longitude", "latitude", "baro_altitude"),
          dim = "XYZ",
          crs = 4326
        )
      }

      return(flight_position)
    },
    error = function(e) {
      log_error(
        "Error occurred when retrieving state vector for {icao24}. {e} Check your icao24 value: {icao24} may be valid but currently inactive."
      )
      flight_position <- tibble(
        icao24 = str_to_lower(icao24),
        on_ground = TRUE
      )
    }
  )
}

get_icao24_from_registration <- function(registration, return_tibble = TRUE) {
  tryCatch(
    {
      icao24 <- request("https://api.adsbdb.com/v0/n-number/") |>
        req_url_path_append(registration) |>
        req_perform() |>
        resp_body_json()

      if (return_tibble) {
        return(as_tibble(icao24$response) |> rename(icao24 = value))
      } else {
        return(icao24$response)
      }
    },
    error = function(e) {
      log_error("{registration} encountered error {e}")
      return(NULL)
    }
  )
}

get_registration_from_icao24 <- function(icao24, return_tibble = TRUE) {
  tryCatch(
    {
      registration <- request("https://api.adsbdb.com/v0/mode-s/") |>
        req_url_path_append(icao24) |>
        req_perform() |>
        resp_body_json()

      if (return_tibble) {
        return(as_tibble(registration$response) |> rename(registration = value))
      } else {
        return(registration$response)
      }
    },
    error = function(e) {
      log_error("{icao24} encountered error {e}")
      return(NULL)
    }
  )
}

get_route_information <- function(callsign) {
  tryCatch(
    {
      json <- list(
        planes = list(list(callsign = callsign, lat = 0, lng = 0))
      )

      route <- request("https://api.adsb.lol/api/0/routeset/") |>
        req_body_json(json) |>
        req_perform() |>
        resp_body_json()

      route <- route[[1]] |> as_tibble()
      if (nrow(route) > 2) {
        stop(
          "Callsign has multiple routes or a multi-leg route. Unable to determine routing."
        )
      }
      route_origin <- route[1, ] |>
        unnest_wider(`_airports`, names_sep = "_") |>
        rename_all(~ glue("origin_{.x}"))
      route_destination <- route[2, ] |>
        unnest_wider(`_airports`, names_sep = "_") |>
        rename_all(~ glue("destination_{.x}"))

      route <- bind_cols(route_origin, route_destination) |>
        rename_all(~ str_replace_all(.x, "__", "_")) |>
        mutate(callsign = callsign)

      return(route)
    },
    error = function(e) {
      log_error("Error getting route info for {callsign} {e}")
      route <- tibble(
        origin_airports_iata = NA,
        origin_airports_name = NA,
        origin_airports_countryiso2 = NA,
        origin_plausible = NA,
        destination_airports_iata = NA,
        destination_airports_name = NA,
        destination_airports_countryiso2 = NA,
        destination_plausible = NA,
      )
      return(route)
    }
  )
}

generate_map_tables <- function(df, specified_callsign, output_html = TRUE) {
  tbl <- df |> filter(callsign == specified_callsign)

  aircraftimg <- tbl$aircraft_image
  aircraftmodel <- tbl$aircraft_model

  callsign <- tbl$callsign
  united_logo <- web_image(
    "https://www.united.com/2500e4e62233fbfe8ac6.unitedLogoNew.svg",
    height = "1em"
  )

  if (is.null(tbl$origin_plausible) | is.na(tbl$origin_plausible)) {
    tbl$origin_plausible <- 0
  }

  if (is.null(tbl$destination_plausible | is.na(tbl$destination_plausible))) {
    tbl$destination_plausible <- 0
  }

  ### Add fallbacks in case route data is inaccurate. API provides a 0/1 metric for plausibility.
  if (tbl$origin_plausible == 1) {
    origin_value_box <- value_box(
      value = tbl$origin_airports_iata,
      title = html(glue(
        "{tbl$origin_airports_name}<br>{tbl$origin_airports_countryiso2}"
      )),
      showcase = fa("plane-departure", height = "4.5em"),
      theme = value_box_theme("bg-gradient-blue-purple")
    )
  } else {
    origin_value_box <- value_box(
      value = glue("{tbl$velocity} m/s"),
      title = "Aircraft Velocity",
      theme = value_box_theme("bg-gradient-blue-purple"),
      showcase = fa("angles-right", height = "3em")
    )
  }
  dest_value_box <- if (tbl$destination_plausible == 1) {
    value_box(
      value = tbl$destination_airports_iata,
      title = html(glue(
        "{tbl$destination_airports_name}<br>{tbl$destination_airports_countryiso2}"
      )),
      showcase = fa("plane-arrival", height = "4.5em"),
      theme = value_box_theme("bg-gradient-blue-purple")
    )
  } else {
    dest_value_box <- value_box(
      value = glue("{tbl$baro_altitude} meters"),
      title = "Altitude",
      theme = value_box_theme("bg-gradient-blue-purple"),
      showcase = if (tbl$vertical_rate > 0) {
        fa("angle-up", height = "3em")
      } else if (tbl$vertical_rate == 0) {
        fa("minus", fill = "#FFFFFF", "3em")
      } else {
        fa("angle-down", height = "3em")
      }
    )
  }

  if (tbl$origin_plausible == 0 | tbl$destination_plausible == 0) {
    reliability <- glue(
      "{fa('triangle-exclamation', fill = '#990000')} <b>Route data for {callsign} is unreliable. Operational information is displayed instead.</b>"
    )
  } else {
    reliability <- glue(
      "{fa('triangle-exclamation', fill = '#FFB343')} <b>Route data for {callsign} is plausible, but may still be outdated/incorrect. Double check using other sources.</b>"
    )
  }

  contact <- tbl$last_contact

  get_flag <- function(x, height = "1em") {
    flag_tbl <- gt:::flag_tbl
    country_names <- gt:::country_names

    x_str <- character(length(x))
    x_str_non_missing <- x[!is.na(x)]
    x_str_non_missing <- vapply(
      seq_along(x_str_non_missing),
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE,
      FUN = function(x) {
        if (grepl(",", x_str_non_missing[x], fixed = TRUE)) {
          countries <- toupper(unlist(strsplit(x_str_non_missing[x], ",\\s*")))
        } else {
          countries <- toupper(x_str_non_missing[x])
        }
        if (is.numeric(height)) {
          height <- paste0(height, "px")
        }
        out <- c()
        for (y in seq_along(countries)) {
          country_i <- toupper(countries[y])
          country_i_len <- nchar(country_i)
          flag_svg <- flag_tbl[
            flag_tbl[[paste0("country_code_", country_i_len)]] == country_i,
          ][["country_flag"]]
          out_y <- gsub(
            "<svg.*?>",
            paste0(
              "<svg xmlns=\"http://www.w3.org/2000/svg\" ",
              "aria-hidden=\"true\" role=\"img\" ",
              "width=\"512\" height=\"512\" ",
              "viewBox=\"0 0 512 512\" ",
              "style=\"vertical-align:-0.125em;",
              "image-rendering:optimizeQuality;",
              "height:",
              height,
              ";",
              "width:",
              height,
              ";",
              "\"",
              ">"
            ),
            flag_svg
          )
          out <- c(out, out_y)
        }
        paste0(
          "<span style=\"white-space:nowrap;\">",
          paste0(out, collapse = ""),
          "</span>"
        )
      }
    )
    x_str[!is.na(x)] <- x_str_non_missing
    x_str[is.na(x)] <- NA_character_
    x_str
  }

  gt_tbl <- tbl |>
    mutate(across(everything(), \(x) as.character(x))) |>
    mutate(
      origin_info = if (origin_plausible == 0 & is.na(origin_airports_name)) {
        as.character(html(glue(
          "{fa('triangle-exclamation', fill = '#990000')} Unable to Determine"
        )))
      } else if (origin_plausible == 0) {
        as.character(html(glue(
          "{fa('triangle-exclamation', fill = '#990000')} {origin_airports_name} {get_flag(origin_airports_countryiso2)}"
        )))
      } else {
        as.character(html(glue(
          "{origin_airports_iata} - {origin_airports_name} {get_flag(origin_airports_countryiso2)}"
        )))
      }
    ) |>
    mutate(
      destination_info = if (
        destination_plausible == 0 & is.na(destination_airports_name)
      ) {
        as.character(html(glue(
          "{fa('triangle-exclamation', fill = '#990000')} Unable to Determine"
        )))
      } else if (destination_plausible == 0) {
        as.character(html(glue(
          "{fa('triangle-exclamation', fill = '#990000')} {destination_airports_name} {get_flag(destination_airports_countryiso2)}"
        )))
      } else {
        as.character(html(glue(
          "{destination_airports_iata} - {destination_airports_name} {get_flag(destination_airports_countryiso2)}"
        )))
      }
    ) |>
    mutate(velocity = glue("{velocity} m/s")) |>
    mutate(true_track = glue("{true_track} degrees")) |>
    mutate(vertical_rate = glue("{vertical_rate} m/s")) |>
    mutate(baro_altitude = glue("{baro_altitude} m")) |>
    mutate(
      amenity_ife = case_when(
        ife == "NO" ~ NA,
        str_detect(ife, "No ") ~ NA,
        str_detect(ife, "AVOD|DTV") & str_detect(ife, "PDE") ~
          glue(
            '{web_image("https://media.united.com/assets/m/6902cf38052408ed/original/x-global-icons-travel-airport-on-demand.svg")} {web_image("https://media.united.com/assets/m/36ec366e93188fda/original/x-global-icons-travel-airport-personal-device.svg")}'
          ),
        str_detect(ife, "AVOD|DTV") ~
          web_image(
            "https://media.united.com/assets/m/6902cf38052408ed/original/x-global-icons-travel-airport-on-demand.svg"
          ),
        str_detect(ife, "Seatback|SEATBACK") ~
          web_image(
            "https://media.united.com/assets/m/6902cf38052408ed/original/x-global-icons-travel-airport-on-demand.svg"
          ),
        str_detect(ife, "PDE") ~
          web_image(
            "https://media.united.com/assets/m/36ec366e93188fda/original/x-global-icons-travel-airport-personal-device.svg"
          )
      )
    ) |>
    mutate(
      amenity_wifi = case_when(
        wifi == "NO" ~ NA,
        .default = web_image(
          "https://media.united.com/assets/m/4d2b44a2bcff27d9/original/x-global-icons-travel-airport-wifi.svg"
        )
      )
    ) |>
    mutate(
      amenity_power = case_when(
        power == "NO" ~ NA,
        power == "F/E+" ~ NA,
        .default = web_image(
          "https://media.united.com/assets/m/3acf299d26de7a3b/original/x-global-icons-travel-airport-in-seat-power.svg"
        )
      )
    ) |>
    mutate(
      amenity_power = case_when(
        str_detect(power, "USB") ~
          glue(
            "{amenity_power} {web_image('https://media.united.com/assets/m/601a3d239d42470/original/USB-icon.svg')}"
          ),
        .default = amenity_power
      )
    ) |>
    mutate(
      amenities = glue("{amenity_wifi} {amenity_power} {amenity_ife}", .na = "")
    ) |>
    select(
      registration,
      aircraft_model,
      delivered,
      config,
      amenities,
      velocity,
      true_track,
      vertical_rate,
      baro_altitude,
      squawk,
      special_purpose,
      origin_info,
      destination_info
    ) |>
    rename(
      "Registration Number" = registration,
      "Aircraft Model" = aircraft_model,
      "Delivery Year" = delivered,
      "Seating Configuration" = config,
      "Amenities On-Board" = amenities,
      "Velocity" = velocity,
      "Climb Rate" = vertical_rate,
      "True Track" = true_track,
      "Barometric Altitude" = baro_altitude,
      "Squawk Code" = squawk,
      "Special Purpose Flight" = special_purpose,
      "Origin Airport" = origin_info,
      "Destination Airport" = destination_info
    ) |>
    pivot_longer(everything()) |>
    mutate(
      category = case_match(
        name,
        "Registration Number" ~ "Aircraft Information",
        "Aircraft Model" ~ "Aircraft Information",
        "Delivery Year" ~ "Aircraft Information",
        "Seating Configuration" ~ "Aircraft Information",
        "Amenities On-Board" ~ "Aircraft Information",
        "Velocity" ~ "Operational Information",
        "True Track" ~ "Operational Information",
        "Climb Rate" ~ "Operational Information",
        "Barometric Altitude" ~ "Operational Information",
        "Squawk Code" ~ "Operational Information",
        "Special Purpose Flight" ~ "Operational Information",
        "Origin Airport" ~ "Route Information",
        "Destination Airport" ~ "Route Information"
      )
    ) |>
    group_by(category) |>
    gt() |>
    tab_header(
      title = html(glue(
        "{united_logo} <b>{callsign}</b><br>{web_image(aircraftimg, height = '9em')}"
      )),
      subtitle = html(glue(
        "{bslib::layout_column_wrap(origin_value_box, dest_value_box)}{reliability}<br>Data as of {contact} UTC"
      ))
    ) |>
    tab_options(table.width = pct(100), container.width = pct(100)) |>
    tab_source_note(
      "Information sourced from the OpenSky Network, adsb.lol, ADSB-DB, and the United Fleet Website's mainline fleet tracker. Information, especially route information, may be inaccurate, and is omitted from the table header when unreliable. Table design by Martin Stavro. Information displayed here, including the medium in which it is displayed, is not affiliated with nor endorsed by United Airlines and is presented here within the provisions of fair use."
    ) |>
    tab_footnote(
      "J = United Polaris seats; F = First Class; PP = Premium Plus; E+ = Economy Plus; Y = Economy.",
      locations = cells_body(
        columns = name,
        rows = name == "Seating Configuration"
      )
    ) |>
    fmt_markdown() |>
    tab_style(
      list(
        cell_text(weight = 500, color = "#FFFFFF"),
        cell_fill(color = "#000000")
      ),
      list(cells_row_groups())
    ) |>
    tab_options(column_labels.hidden = TRUE)

  if (output_html) {
    gt_tbl <- gt_tbl |> as_raw_html()
  }

  return(gt_tbl)
}

generate_addt_info_table <- function(df, specified_registration) {
  tbl <- df |> filter(registration == specified_registration)

  ife_learn_more <- glue("In-Flight Entertainment {fa('circle-question')}")
  wifi_learn_more <- glue("Wi-Fi System {fa('circle-question')}")
  power_learn_more <- glue("Power Available {fa('circle-question')}")

  tbl |>
    select(
      aircraft_model,
      config,
      amenities,
      ife,
      wifi,
      power,
      J,
      F,
      PP,
      `E+`,
      Y
    ) |>
    mutate(across(c("J", "F", "PP", "E+", "Y"), \(x) as.integer(x))) |>
    mutate("Total Seats" = sum(c(J, F, PP, `E+`, Y), na.rm = T)) |>
    mutate(across(everything(), \(x) as.character(x))) |>
    rename(
      "Aircraft Model" = aircraft_model,
      Layout = config,
      "Amenities" = amenities,
      "In-Flight Entertainment" = ife,
      "Wi-Fi System" = wifi,
      "Power Available" = power,
      "Polaris Seats" = J,
      "First Class Seats" = F,
      "Premium Plus Seats" = PP,
      "Economy Plus Seats" = `E+`,
      "Economy Seats" = Y
    ) |>
    pivot_longer(everything()) |>
    mutate(
      category = case_match(
        name,
        "Aircraft Model" ~ "Aircraft Information",
        "Layout" ~ "Aircraft Information",
        "Amenities" ~ "Amenities On-Board",
        "In-Flight Entertainment" ~ "Amenities On-Board",
        "Wi-Fi System" ~ "Amenities On-Board",
        "Power Available" ~ "Amenities On-Board",
        "Polaris Seats" ~ "Seating",
        "First Class Seats" ~ "Seating",
        "Premium Plus Seats" ~ "Seating",
        "Economy Plus Seats" ~ "Seating",
        "Economy Seats" ~ "Seating",
        "Total Seats" ~ "Seating"
      )
    ) |>
    group_by(category) |>
    gt() |>
    tab_options(column_labels.hidden = TRUE) |>
    fmt_markdown() |>
    sub_missing() |>
    tab_header(title = specified_registration) |>
    tab_style(
      list(
        cell_text(weight = 400, color = "#FFFFFF"),
        cell_fill(color = "#000000")
      ),
      list(cells_row_groups(), cells_body(rows = name == "Total Seats"))
    ) |>
    tab_options(table.width = pct(90), table.border.top.style = "hidden") |>
    text_replace(
      "In-Flight Entertainment",
      glue(
        '{bslib::tooltip(
      html(ife_learn_more),
      "Abbreviations: Audio-Video on Demand (AVOD), AVOD with overhead television monitors in economy (AVOD/OVER), Personal Device Entertainment via the United app (PDE), DirecTV (DTV). F/E+ indicates that this amenity is only available in First or Economy Plus; in this case, the amenity is omitted from the icon list as it is not available to all passengers."
    )}'
      )
    ) |>
    text_replace(
      "Wi-Fi System",
      glue(
        '{bslib::tooltip(
      html(wifi_learn_more),
      "Abbreviations: Satellite (Satl), Ku (Ku-band satellite frequency), Ka (Ka-band satellite frequency). F/E+ indicates that this amenity is only available in First or Economy Plus; in this case, the amenity is omitted from the icon list as it is not available to all passengers."
    )}'
      )
    ) |>
    text_replace(
      "Power Available",
      glue(
        '{bslib::tooltip(
      html(power_learn_more),
      "Numerical values represent volts. USB indicates that USB power (either type-A or type-C) is available. F/E+ indicates that this amenity is only available in First or Economy Plus; in this case, the amenity is omitted from the icon list as it is not available to all passengers."
    )}'
      )
    ) |>
    data_color(
      rows = category == "Seating" & name != "Total Seats",
      palette = "ggsci::indigo_material",
      columns = value,
      na_color = "#FFFFFF"
    ) |>
    as_raw_html()
}
