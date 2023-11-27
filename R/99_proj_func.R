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
