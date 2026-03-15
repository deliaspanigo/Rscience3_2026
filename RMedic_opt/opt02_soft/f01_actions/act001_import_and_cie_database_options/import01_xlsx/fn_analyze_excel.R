analyze_excel <- function(file_path, sheet_to_analyze = NULL) {
  if (!file.exists(file_path)) stop("The file does not exist")
  if (!grepl("\\.xlsx$", file_path, ignore.case = TRUE)) stop("Not a .xlsx file")
  
  file_size <- file.size(file_path) / (1024 * 1024)
  all_sheets <- readxl::excel_sheets(file_path)
  
  # Si el usuario eligió una hoja, filtramos la lista para procesar solo esa
  sheets_to_process <- if (!is.null(sheet_to_analyze)) {
    if (!(sheet_to_analyze %in% all_sheets)) stop("Selected sheet not found")
    sheet_to_analyze
  } else {
    all_sheets
  }
  
  sheets_info <- lapply(sheets_to_process, function(sheet) {
    # Leemos la hoja completa (es más seguro para obtener dimensiones exactas)
    # Si el archivo es GIGANTE (>100k filas), podrías volver al método chunk
    # pero para archivos normales, read_excel es suficiente.
    tmp_data <- suppressMessages(readxl::read_excel(file_path, sheet = sheet))
    
    list(
      name = sheet,
      rows = nrow(tmp_data),
      columns = ncol(tmp_data)
    )
  })
  
  df_sheets_info <- do.call(rbind.data.frame, sheets_info)
  
  # Devolvemos la estructura que tu código espera
  return(list(
    file_path = file_path,
    file_size_mb = round(file_size, 2),
    sheet_count = length(all_sheets), # Total de hojas en el archivo
    sheets_info = df_sheets_info,
    vector_cols = df_sheets_info[,"columns"],
    vector_rows = df_sheets_info[,"rows"]
  ))
}