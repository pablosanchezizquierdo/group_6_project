# Define a function to read all sheets from a given Excel file

read_excel_sheets <- function(file_path) {
  sheets <- excel_sheets(file_path)
  map(sheets, ~ read_excel(file_path, sheet = .x))
}

# Function to retrieve the first pathway for a given KO ID

get_first_pathway_name <- function(ko_id) {
  pathway_info <- tryCatch({
    keggGet(ko_id)
  }, error = function(e) return(NA))

  if (!is.na(pathway_info) && !is.null(pathway_info[[1]]$PATHWAY)) {
    # Extracting only the first pathway name
    first_pathway_name <- pathway_info[[1]]$PATHWAY[[1]]
  } else {
    first_pathway_name <- NA
  }
  return(first_pathway_name)
}

calculate_percentage <- function(consensus_sequence) {

  count <- str_count(consensus_sequence, fixed("-")) + str_count(consensus_sequence, fixed("?"))
  total <- nchar(consensus_sequence)
  percentage <- (count / total) * 100

  return(percentage)
}

mer_position_function <- function(consensus_mat) {
  consensus_mat <- consensus_mat |>
    as.data.frame() |>
    mutate(position = row_number()) |>
    pivot_longer(-position, names_to = "position_aa", values_to = "aa") |>
    group_by(position) |>
    mutate(position_score = max(aa)) |>
    select(position, position_score) |>
    distinct(position, .keep_all = TRUE)

  consensus_mat <- data.frame(
    position = consensus_mat$position,
    position_score = consensus_mat$position_score
  )

  mer_position <- consensus_mat |>
    mutate(mer_score = lead(position_score, n=1, default = 0) + lead(position_score, n=1, default = 0) + lead(position_score, n=2, default = 0) + lead(position_score, n=3, default = 0) + lead(position_score, n=4, default = 0) + lead(position_score, n=5, default = 0) + lead(position_score, n=6, default = 0) + lead(position_score, n=7, default = 0) + lead(position_score, n=8, default = 0)) |>
    filter(mer_score == max(mer_score)) |>
    slice_head(n = 1) |>
    select(position) |>
    pull()

  return(mer_position)
}
