### Process to rebuild United fleet info
source("support_functions.R")

united_fleet_info <- function(sheet) {
  gs4_deauth()

  read_sheet("1ZlYgN_IZmd6CSx_nXnuP0L0PiodapDRx3RmNkIpxXAo", sheet = sheet) |>
    slice(-1) |>
    rename(
      aircraft_model = 1,
      registration = `Reg #`,
      any_of(
        c(
          delivered = "Delivery",
          delivered = "Delivered",
          delivered = "Delivery Yr",
          delivered = "Delvr",
          config = "Configuration",
          config = "Congfiguration"
        )
      ),
      ife = IFE,
      wifi = WiFi,
      power = Power
    ) |>
    select(
      aircraft_model,
      registration,
      delivered,
      config,
      ife,
      wifi,
      power,
      J,
      F,
      PP,
      `E+`,
      Y
    ) |>
    mutate(across(everything(), \(x) as.character(x)))
}

united_full_fleet_info <- bind_rows(map(1:15, \(x) united_fleet_info(x))) |>
  mutate(
    aircraft_image = case_match(
      aircraft_model,
      "319" ~
        "https://media.united.com/assets/m/7c9ce478782db0ed/original/Airbus-A319-2x.png", # this is all United has public facing for 'Airbus'
      "320" ~
        "https://media.united.com/assets/m/7c9ce478782db0ed/original/Airbus-A319-2x.png", # maybe a320/21 exist out there, some more google dorking is needed
      "21N" ~
        "https://media.united.com/assets/m/7c9ce478782db0ed/original/Airbus-A319-2x.png",
      "73G" ~
        "https://media.united.com/assets/m/e1fdecdb14fce86/original/737-MAX-2x.png",
      "738" ~
        "https://media.united.com/assets/m/e1fdecdb14fce86/original/737-MAX-2x.png",
      "38M" ~
        "https://media.united.com/assets/m/e1fdecdb14fce86/original/737-MAX-2x.png",
      "739" ~
        "https://media.united.com/assets/m/e1fdecdb14fce86/original/737-MAX-2x.png",
      "739ER" ~
        "https://media.united.com/assets/m/e1fdecdb14fce86/original/737-MAX-2x.png",
      "39M" ~
        "https://media.united.com/assets/m/e1fdecdb14fce86/original/737-MAX-2x.png",
      "752" ~
        "https://media.united.com/assets/m/4b144548aa973560/original/757-200-2x.png",
      "753" ~
        "https://media.united.com/assets/m/4b144548aa973560/original/757-200-2x.png",
      "763ER" ~
        "https://media.united.com/assets/m/1b2147dcd5318024/original/767-300-2x.png",
      "764ER" ~
        "https://media.united.com/assets/m/1b2147dcd5318024/original/767-300-2x.png",
      "77G" ~
        "https://media.united.com/assets/m/2c8d559fcfcec38f/original/777-300-2x.png",
      "77M" ~
        "https://media.united.com/assets/m/2c8d559fcfcec38f/original/777-300-2x.png",
      "77O" ~
        "https://media.united.com/assets/m/2c8d559fcfcec38f/original/777-300-2x.png",
      "77N" ~
        "https://media.united.com/assets/m/2c8d559fcfcec38f/original/777-300-2x.png",
      "77U" ~
        "https://media.united.com/assets/m/2c8d559fcfcec38f/original/777-300-2x.png",
      "772ER" ~
        "https://media.united.com/assets/m/2c8d559fcfcec38f/original/777-300-2x.png",
      "77W" ~
        "https://media.united.com/assets/m/2c8d559fcfcec38f/original/777-300-2x.png",
      "787-8" ~
        "https://media.united.com/assets/m/4a29a23b8c38ff55/original/787-8-png-2x.png",
      "787-10" ~
        "https://media.united.com/assets/m/4a29a23b8c38ff55/original/787-8-png-2x.png",
      "787-9" ~
        "https://media.united.com/assets/m/4a29a23b8c38ff55/original/787-8-png-2x.png"
    )
  ) |>
  mutate(
    aircraft_model = case_match(
      aircraft_model,
      "319" ~ "Airbus A319",
      "320" ~ "Airbus A320",
      "21N" ~ "Airbus A321neo",
      "73G" ~ "Boeing 737-700",
      "738" ~ "Boeing 737-800",
      "38M" ~ "Boeing 737 MAX 8",
      "739" ~ "Boeing 737-900",
      "739ER" ~ "Boeing 737-900ER",
      "39M" ~ "Boeing 737 MAX 9",
      "752" ~ "Boeing 757-200",
      "753" ~ "Boeing 757-300",
      "763ER" ~ "Boeing 767-300ER",
      "764ER" ~ "Boeing 767-400ER",
      "77G" ~ "Boeing 777-200",
      "77M" ~ "Boeing 777-200",
      "77O" ~ "Boeing 777-200ER",
      "77N" ~ "Boeing 777-200ER",
      "77U" ~ "Boeing 777-200ER",
      "772ER" ~ "Boeing 777-200ER",
      "77W" ~ "Boeing 777-300",
      "787-8" ~ "Boeing 787-8 Dreamliner",
      "787-10" ~ "Boeing 787-10 Dreamliner",
      "787-9" ~ "Boeing 787-9 Dreamliner"
    )
  ) |>
  mutate(
    aircraft_seatmap = case_when(
      aircraft_model == "Airbus A319" & config == "12F/36E+/78Y" ~
        "https://media.united.com/assets/m/6c2619139fcc1674/original/Airbus-319_SeatMap.png",
      aircraft_model == "Airbus A320" & config == "12F/42E+/96Y" ~
        "https://media.united.com/assets/m/77611d0be598e4e2/original/Airbus-320_SeatMap.png",
      aircraft_model == "Airbus A321neo" & config == "20F/57E+/123Y" ~
        "https://media.united.com/assets/m/5f1be20d804eac0b/original/0043-A-Airbus-321-NEO_SeatMap_3850x1100.jpg",
      aircraft_model == "Boeing 737-700" & config == "12F/36E+/78Y" ~
        "https://media.united.com/assets/m/75349c6fec922b74/original/737-700_Seatmap.png",
      aircraft_model == "Boeing 737-800" & config == "16F/42E+/108Y" ~
        "https://media.united.com/assets/m/59d58a3109d4fdcf/original/737_800-V3_Seatmap.png",
      aircraft_model == "Boeing 737-800" & config == "16F/54E+/96Y" ~
        "https://media.united.com/assets/m/478c8a4514af749f/original/737_800-V2_Seatmap.png",
      aircraft_model == "Boeing 737-800" & config == "16F/48E+/102Y" ~
        "https://media.united.com/assets/m/2e41eddf5126cca/original/737_800-V1_Seatmap.png",
      aircraft_model == "Boeing 737 MAX 8" & config == "16F/54E+/96Y" ~
        "https://media.united.com/assets/m/468bc1680a021607/original/737_8_Max_-16_150-_2272px_62colors.webp",
      aircraft_model == "Boeing 737-900" & config == "20F/42E+/117Y" ~
        "https://media.united.com/assets/m/7ecc06665f5eadc5/original/737-900-V1_Seatmap.png",
      aircraft_model == "Boeing 737-900" & config == "20F/45E+/114Y" ~
        "https://media.united.com/assets/m/672e436134393ac8/original/737-900-V3_Seatmap.png",
      aircraft_model == "Boeing 737-900ER" & config == "20F/42E+/117Y" ~
        "https://media.united.com/assets/m/7ecc06665f5eadc5/original/737-900-V1_Seatmap.png",
      aircraft_model == "Boeing 737-900ER" & config == "20F/45E+/114Y" ~
        "https://media.united.com/assets/m/672e436134393ac8/original/737-900-V3_Seatmap.png",
      aircraft_model == "Boeing 737 MAX 9" & config == "20F/45E+/114Y" ~
        "https://media.united.com/assets/m/478424e0a7f686a3/original/737-9_ICR-B3D_B3Di11142017_3850x1100.jpg",
      aircraft_model == "Boeing 737 MAX 9" & config == "20F/48E+/111Y" ~
        "https://media.united.com/assets/m/61c72950276939c2/original/737-900-MAX9_Seatmap.png",
      aircraft_model == "Boeing 757-200" & config == "16J/42E+/118Y" ~
        "https://media.united.com/assets/m/5cd6b8b29de9fe1e/original/0024_757-200_SeatMap_3850x1100.png",
      aircraft_model == "Boeing 757-300" & config == "24F/54E+/156Y" ~
        "https://media.united.com/assets/m/6ad7c68d690247ad/original/757-300_SeatMap.png",
      aircraft_model == "Boeing 767-300ER" & config == "30J/24PP/32E+/113Y" ~
        "https://media.united.com/assets/m/7f46e01710b8c8ee/original/767-300-V3_SeatMap.png",
      aircraft_model == "Boeing 767-300ER" & config == "46J/22PE/43E+/56Y" ~
        "https://media.united.com/assets/m/3a404651615a2ff7/original/767-300-V2_SeatMap.png",
      aircraft_model == "Boeing 767-400ER" & config == "34J/24PE/48E+/125Y" ~
        "https://media.united.com/assets/m/402d979292c41b69/original/767-400_V2_Seatmap.png",
      aircraft_model == "Boeing 777-200" & config == "28F/102E+/234Y" ~
        "https://media.united.com/assets/m/5f45564c1af4723c/original/777-200_V2_SeatMap.png",
      aircraft_model == "Boeing 777-200ER" & config == "32F/124E+/206Y" ~ NA,
      aircraft_model == "Boeing 777-200ER" & config == "50J/24PE/46E+/156Y" ~
        "https://media.united.com/assets/m/135e5704439135f4/original/777-200_V1_SeatMap.png",
      aircraft_model == "Boeing 777-300" & config == "60J/24PE/62E+/204Y" ~
        "https://media.united.com/assets/m/7d514e882a9d133f/original/777-300ER_SeatMap.png",
      aircraft_model == "Boeing 787-8 Dreamliner" &
        config == "28J/21PE/36E+/158Y" ~
        "https://media.united.com/assets/m/c8b1f1d11e6c118/original/787-8_SeatMap.png",
      aircraft_model == "Boeing 787-10 Dreamliner" &
        config == "44J/21PE/54E+/199Y" ~
        "https://media.united.com/assets/m/3ccf19b38c5430c2/original/787-10_SeatMap.png",
      aircraft_model == "Boeing 787-9 Dreamliner" &
        config == "48J/21PE/39E+/149Y" ~
        "https://media.united.com/assets/m/6d72d9a9f884e8d2/original/787-9_SeatMap.png"
    )
  ) |>
  drop_na(aircraft_image)

write_csv(united_full_fleet_info, "data/united_full_fleet_info.csv")

united_icao_list <- united_full_fleet_info |>
  drop_na(registration) |>
  filter(str_starts(registration, "N")) |>
  mutate(registration = trimws(registration)) |>
  pull(registration)
compatible_registrations <- bind_rows(map(
  united_icao_list,
  get_icao24_from_registration
))

write_csv(compatible_registrations, "data/compatible_registrations.csv")
