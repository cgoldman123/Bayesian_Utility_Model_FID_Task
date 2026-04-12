# verify_distribution_mapping.R
#
# Encapsulates logic for verifying the distribution label mapping in data.DT
# against the JSON-based distribution metadata found in FID_balance.json files.
#
# Usage:
#   source("verify_distribution_mapping.R")
#   combined_distribution_dataframe <- verify_distribution_mapping(data.DT)
#
# The returned data frame includes:
#   id               - subject/session identifier from JSON metadata
#   distribution     - distribution label from the JSON metadata
#   distribution_DT  - distribution label derived from data.DT
#   identical_dist   - TRUE/FALSE whether the two labels match

verify_distribution_mapping <- function(data.DT,
                                        base_dir = "L:/NPC/DataSink/study-Ironside-2023-TCADPilot/data-original/functional_session") {
  stopifnot(is.data.frame(data.DT))
  stopifnot(is.character(base_dir), length(base_dir) == 1)
  stopifnot(requireNamespace("jsonlite", quietly = TRUE))
  stopifnot(requireNamespace("dplyr", quietly = TRUE))

  # Find all JSON files that contain the balance information for the FID task.
  json_files <- list.files(base_dir, pattern = "FID_balance\\.json$", recursive = TRUE, full.names = TRUE)
  distribution_info_json <- data.frame(id = character(), distribution = character(), stringsAsFactors = FALSE)

  # Parse each JSON file and extract the relevant distribution label metadata.
  for (file in json_files) {
    json_data <- jsonlite::fromJSON(file)
    if (!is.null(json_data$balance)) {
      balance_entries <- json_data$balance
      distribution_info_json <- dplyr::bind_rows(
        distribution_info_json,
        data.frame(
          id = balance_entries$sid,
          distribution = balance_entries$distribution,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Align the data.DT-derived subject/session keys with the JSON-derived keys.
  distribution_info_DT <- data.DT %>%
    dplyr::mutate(subject_short = paste0(substr(subject_id, 1, 5), ifelse(session == 1, "_T0", "_T1"))) %>%
    dplyr::distinct(subject_short, session, distribution, .keep_all = TRUE) %>%
    dplyr::select(subject_short, distribution) %>%
    dplyr::rename(distribution_DT = distribution)

  # Join the two sources and flag mismatches.
  combined_distribution_dataframe <- dplyr::left_join(distribution_info_json,
                                                     distribution_info_DT,
                                                     by = c("id" = "subject_short"))
  combined_distribution_dataframe$identical_dist <- combined_distribution_dataframe$distribution == combined_distribution_dataframe$distribution_DT

  combined_distribution_dataframe
}
