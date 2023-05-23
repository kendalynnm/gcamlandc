
getwd()
Sys.setenv("PATH" = "C:/Users/morr497/Documents/OneDriveSafeSpace/openjdk-20.0.1_windows-x64_bin/jdk-20.0.1/bin")
system2("java", "-version")

AG_emissions <- read.csv("data_reference/ag_emiss_full_world_real-baseline_no-protected_2100.csv")
BG_emissions <- read.csv("data_reference/bg_emiss_full_world_real-baseline_no-protected_2100.csv")
climate_data <- read.csv("data_reference/climate_data_full_world_real-baseline_no-protected_2100.csv")
gcam_land <- read.csv("data_reference/gcam_land_alloc.csv")
leaf_data <- read.csv("data_reference/leaf_data_full_world_real-baseline_no-protected_2100.csv")
leaf_params <- read.csv("data_reference/leaf_params_full_world_real-baseline_no-protected_2100.csv")
