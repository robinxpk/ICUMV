library(ggplot2)

data_path <- "data/"

dfs <- list()
for (file in list.files(data_path, pattern = "*.Rds")) {
  dfs[[file]] <- tibble::as_tibble(readRDS(paste0(data_path, file)))
}
names(dfs)

# ICU ----
# Probably: Informations on the ICU itself, without any patient info
icu <- dfs$ICU.Rds

# Daily all ----
daily_all <- dfs$daily_all.Rds
daily_all |> dplyr::summarise(.by = c(CombinedID), n = dplyr::n()) |> dplyr::summarise(.by = n) |> dplyr::arrange()

# Daily --- 
daily = dfs$daily.Rds
daily |> dplyr::summarise(.by = c(CombinedID), n = dplyr::n()) |> dplyr::summarise(.by = n) |> dplyr::arrange()

# Excluded_ids ---
excl_ids = dfs$excluded_Ids.Rds

# merged and cleaned data ---
mac_data = dfs$mergedAndCleanedData.Rds
mac_data |> dplyr::summarise(.by = c(CombinedID), n = dplyr::n()) |> dplyr::summarise(.by = n) |> dplyr::arrange()

# Patient --- 
# -> Long format
patient = dfs$patient.Rds
# Every patient contained once
patient |> dplyr::summarise(.by = c(CombinedID), n = dplyr::n()) |> dplyr::summarise(.by = n) |> dplyr::arrange()
