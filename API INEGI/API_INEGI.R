# Paquetes
library(httr)
library(rjson)
# install.packages("writexl")
library(writexl)
# install.packages("ggplot2")
library(ggplot2)

# ====== Parámetros ======
token <- "7407aaa9-caa4-b3fa-2bd4-1872e3c5ab9d"
indicadores <- c("737121","737145","742203","742204","742205","742210")

# ====== URLs (HISTÓRICO: desagrega = false, un indicador por llamada) ======
# Formato correcto (nota GEO = "00", no "00000"):
# https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/{id}/es/00/false/BIE-BISE/2.0/{token}?type=json
build_url_hist <- function(id, catalog = c("BIE-BISE","BISE","BIE")) {
  catalog <- match.arg(catalog)
  paste0(
    "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/",
    id, "/es/00/false/", catalog, "/2.0/", token, "?type=json"
  )
}

# ====== GET con reintentos de catálogo y mensajes de error amigables ======
fetch_hist_json <- function(id) {
  catalogs <- c("BIE-BISE","BISE","BIE")
  last_err <- NULL
  for (cat in catalogs) {
    url <- build_url_hist(id, cat)
    resp <- GET(url)
    if (http_error(resp)) {
      # Guarda el texto del error para depurar
      last_err <- tryCatch(content(resp, "text", encoding = "UTF-8"), error = function(e) NA_character_)
      message(sprintf("Catálogo '%s' falló para %s (HTTP %s). Respuesta: %s",
                      cat, id, status_code(resp), substr(last_err, 1, 200)))
      next
    }
    # OK
    datos_txt <- content(resp, "text", encoding = "UTF-8")
    return(rjson::fromJSON(datos_txt))
  }
  stop(sprintf("No se pudo obtener el indicador %s. Última respuesta del servidor: %s",
               id, substr(last_err, 1, 500)))
}

# ====== Parseo: Serie completa -> data.frame ======
serie_a_df <- function(s){
  obs <- s$OBSERVATIONS
  if (is.null(obs) || length(obs) == 0) return(NULL)
  
  time_period <- vapply(obs, function(x) x$TIME_PERIOD, "", USE.NAMES = FALSE)
  obs_value   <- vapply(obs, function(x) x$OBS_VALUE,   NA_character_, USE.NAMES = FALSE)
  obs_status  <- vapply(obs, function(x) x$OBS_STATUS,  NA_character_, USE.NAMES = FALSE)
  obs_note    <- vapply(obs, function(x) x$OBS_NOTE,    NA_character_, USE.NAMES = FALSE)
  cober_geo   <- vapply(obs, function(x) x$COBER_GEO,   NA_character_, USE.NAMES = FALSE)
  
  # nombre del indicador si viene en la respuesta
  indicador_nombre <- NA_character_
  for (k in c("INDICADOR_NOMBRE","TITULO","TITLE","NAME")) {
    if (!is.null(s[[k]])) { indicador_nombre <- as.character(s[[k]]); break }
  }
  
  df <- data.frame(
    indicador         = as.character(s$INDICADOR),
    indicador_nombre  = indicador_nombre,
    freq              = as.character(s$FREQ),     # 8=mensual, 4=trimestral
    unidad            = as.character(s$UNIT),
    unit_mult         = as.character(s$UNIT_MULT),
    topic             = as.character(s$TOPIC),
    source            = as.character(s$SOURCE),
    last_update       = as.character(s$LASTUPDATE),
    time_period       = time_period,              # "YYYY/MM"
    value             = suppressWarnings(as.numeric(obs_value)),
    obs_status        = obs_status,
    obs_note          = obs_note,
    cober_geo         = cober_geo,
    stringsAsFactors  = FALSE
  )
  
  # ---- Columna de fecha (primer mes del periodo) ----
  year <- as.integer(substr(df$time_period, 1, 4))
  code <- as.integer(substr(df$time_period, 6, 7))
  freq_int <- as.integer(df$freq)
  month_first <- ifelse(freq_int == 8, code, (code - 1L) * 3L + 1L)
  # Si prefieres último mes del trimestre: month_first <- ifelse(freq_int == 8, code, code * 3L)
  
  df$date <- as.Date(sprintf("%04d-%02d-01", year, month_first))
  
  df <- df[order(df$date, df$time_period), ]
  rownames(df) <- NULL
  df
}

# ====== Descarga TODA la historia por indicador y apila ======
dfs_por_indicador <- list()
for (id in indicadores) {
  json_list <- fetch_hist_json(id)
  s_list <- json_list$Series
  if (is.null(s_list) || length(s_list) == 0) {
    warning(sprintf("Sin 'Series' para indicador %s", id))
    next
  }
  dfi_list <- lapply(s_list, serie_a_df)
  dfi <- do.call(rbind, dfi_list)
  if (!is.null(dfi) && nrow(dfi) > 0) {
    # Si por alguna razón trae variantes, nos quedamos con el ID solicitado
    dfi <- dfi[dfi$indicador == id, ]
    dfs_por_indicador[[id]] <- dfi
  } else {
    warning(sprintf("Serie vacía para indicador %s", id))
  }
}

# Consolidado largo
df_long <- if (length(dfs_por_indicador) > 0) do.call(rbind, dfs_por_indicador) else NULL

# ====== 1) Excel con una hoja por indicador ======
if (!is.null(df_long) && nrow(df_long) > 0) {
  xlsx_file <- "inegi_series_por_indicador.xlsx"
  write_xlsx(dfs_por_indicador, path = xlsx_file)
  cat("Archivo Excel creado con hojas por indicador ->", xlsx_file, "\n")
} else {
  cat("No se generó Excel: no hay datos.\n")
}

# ====== 2) CSV por indicador (toda la historia) ======
for (id in names(dfs_por_indicador)) {
  csv_path <- paste0("inegi_", id, ".csv")
  write.csv(dfs_por_indicador[[id]], csv_path, row.names = FALSE, na = "")
  cat("CSV creado ->", csv_path, "\n")
}

# ====== 3) Gráficas por indicador (ggplot2) ======
dir_plots <- "plots_inegi"
if (!dir.exists(dir_plots)) dir.create(dir_plots)

for (id in names(dfs_por_indicador)) {
  dfi <- dfs_por_indicador[[id]]
  
  titulo <- if (all(is.na(dfi$indicador_nombre)) || all(trimws(dfi$indicador_nombre) == "")) {
    paste("Indicador", id)
  } else {
    nm <- unique(na.omit(trimws(dfi$indicador_nombre)))
    if (length(nm) == 0) paste("Indicador", id) else paste0(nm[1], " (", id, ")")
  }
  
  p <- ggplot(dfi, aes(x = date, y = value)) +
    geom_line(na.rm = TRUE) +
    labs(
      title = titulo,
      subtitle = "Serie histórica (desagrega=false)",
      x = "Fecha",
      y = paste0("Valor (Unidad: ", ifelse(is.na(dfi$unidad[1]), "NA", dfi$unidad[1]), ")"),
      caption = "Fuente: INEGI API"
    ) +
    theme_minimal(base_size = 12)
  
  print(p)
  png_path <- file.path(dir_plots, paste0("plot_inegi_", id, ".png"))
  ggsave(filename = png_path, plot = p, width = 10, height = 5.5, dpi = 150)
  cat("Gráfica guardada ->", png_path, "\n")
}

# ====== Impresión en data frame (como venías haciéndolo) ======
cat("\n--- df_long (muestra de las últimas 3 filas por indicador) ---\n")
if (!is.null(df_long) && nrow(df_long) > 0) {
  print(do.call(rbind, by(df_long, df_long$indicador, function(d) tail(d, 3))))
} else {
  cat("df_long está vacío.\n")
}


