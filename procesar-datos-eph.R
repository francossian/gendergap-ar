# =============================================================================
# EPH - Procesamiento de microdatos para dashboard de brecha de género
# =============================================================================
# Fuente: INDEC - Encuesta Permanente de Hogares
# https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos
# Bases: usu_individual_TXYY.txt (X = trimestre, YY = año)
#
# Outputs:
#   eph_consolidado.parquet  → solo ocupados con ingreso y horas (compatible
#                              con el modelo Power BI existente / f_ocupadxs)
#   eph_poblacion.parquet    → TODA la población, TODAS las columnas del EPH
#                              (base para análisis de actividad, cuidados, etc.)
# =============================================================================

library(data.table)
library(arrow)
library(stringr)

# -----------------------------------------------------------------------------
# CONFIGURACIÓN
# -----------------------------------------------------------------------------

CARPETA_EPH <- "datos-eph/txt"
CARPETA_OUT <- "datos-eph"

# -----------------------------------------------------------------------------
# TABLAS DE REFERENCIA — etiquetas de región y aglomerado
# -----------------------------------------------------------------------------

REGIONES <- data.table(
  REGION     = c(1L, 40L, 41L, 42L, 43L, 44L),
  region_lab = c("Gran Buenos Aires", "NOA", "NEA", "Cuyo", "Pampeana", "Patagonia")
)

AGLOMERADOS <- data.table(
  AGLOMERADO   = c(2L,3L,4L,5L,6L,7L,8L,9L,10L,12L,13L,14L,15L,17L,
                   18L,19L,20L,22L,23L,25L,26L,27L,29L,30L,31L,32L,
                   33L,34L,36L,38L,91L,93L),
  aglomerado_lab = c(
    "Gran La Plata", "Bahía Blanca-Cerri", "Gran Rosario", "Gran Santa Fe",
    "Gran Paraná", "Posadas", "Gran Resistencia", "Comodoro Rivadavia-Rada Tilly",
    "Gran Mendoza", "Corrientes", "Gran Córdoba", "Concordia", "Formosa",
    "Neuquén-Plottier", "Santiago del Estero-La Banda", "Jujuy-Palpalá",
    "Río Gallegos", "Gran Catamarca", "Gran Salta", "La Rioja", "Gran San Luis",
    "Gran San Juan", "Gran Tucumán-Tafí Viejo", "Santa Rosa-Toay",
    "Ushuaia-Río Grande", "CABA", "Partidos del GBA", "Mar del Plata",
    "Río Cuarto", "San Nicolás-Villa Constitución", "Rawson-Trelew",
    "Viedma-Carmen de Patagones"
  ),
  latitud = c(
    -34.92, -38.72, -32.95, -31.63, -31.73, -27.37, -27.45, -45.87,
    -32.89, -27.47, -31.42, -31.39, -26.18, -38.95, -27.78, -24.19,
    -51.62, -28.47, -24.78, -29.41, -33.30, -31.54, -26.82, -36.62,
    -54.80, -34.60, -34.61, -38.00, -33.13, -33.34, -43.30, -40.81
  ),
  longitud = c(
    -57.95, -62.27, -60.64, -60.70, -60.53, -55.90, -59.03, -67.49,
    -68.85, -58.83, -64.18, -58.02, -58.18, -68.06, -64.26, -65.30,
    -69.22, -65.78, -65.41, -66.86, -66.34, -68.53, -65.22, -64.29,
    -68.30, -58.38, -58.44, -57.55, -64.35, -60.21, -65.10, -62.99
  )
)

# -----------------------------------------------------------------------------
# FUNCIÓN: parsear trimestre y año del nombre de archivo
# -----------------------------------------------------------------------------

parsear_periodo <- function(nombre_archivo) {
  base  <- tools::file_path_sans_ext(basename(nombre_archivo))
  match <- str_extract(toupper(base), "T([1-4])(\\d{2})$")
  if (is.na(match)) {
    warning(paste("No se pudo parsear el período del archivo:", nombre_archivo))
    return(list(trimestre = NA, anio = NA, periodo = NA))
  }
  trimestre <- as.integer(str_sub(match, 2, 2))
  anio      <- as.integer(paste0("20", str_sub(match, 3, 4)))
  periodo   <- paste0(anio, "T", trimestre)
  list(trimestre = trimestre, anio = anio, periodo = periodo)
}

# -----------------------------------------------------------------------------
# FUNCIÓN: leer un trimestre completo (TODAS las columnas)
# -----------------------------------------------------------------------------

leer_trimestre <- function(ruta_archivo) {

  cat("Procesando:", basename(ruta_archivo), "\n")

  per <- parsear_periodo(ruta_archivo)

  dt <- fread(
    ruta_archivo,
    sep          = ";",
    encoding     = "Latin-1",
    colClasses   = "character",   # leer todo como texto primero
    showProgress = FALSE
  )

  setnames(dt, toupper(names(dt)))

  # Columnas numéricas conocidas — se convierten explícitamente
  # El resto queda como character y se recoercerá al consolidar con fill=TRUE
  cols_numericas <- c(
    "P21", "PP3E_TOT", "PP3F_TOT", "CH06", "CH04", "NIVEL_ED", "ESTADO",
    "CAT_OCUP", "CAT_INAC", "PP04A", "PP07H", "PP07C", "PP07J", "PP03D",
    "PONDERA", "PONDIIO", "REGION", "AGLOMERADO", "PP04C", "INTENSI",
    "PP04C99", "PP04G",
    # Ingresos adicionales
    "P47T", "TOT_P12", "T_VI",
    # Variables de no remunerado / cuidados (para el módulo de intro)
    "PP03H", "PP03I", "PP03J",
    # Deciles
    "P_DECCF", "P_RDECCF", "P_GDECCF", "P_PDECCF", "P_IDECCF", "P_ADECCF"
  )
  cols_numericas <- intersect(cols_numericas, names(dt))
  dt[, (cols_numericas) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
     .SDcols = cols_numericas]

  # Agregar columnas de período
  dt[, trimestre := per$trimestre]
  dt[, anio      := per$anio]
  dt[, periodo   := per$periodo]

  # -------------------------------------------------------------------------
  # VARIABLES DERIVADAS (idénticas al script anterior — no rompen PBI)
  # -------------------------------------------------------------------------

  # Solo disponibles para ocupados con datos válidos; NA para el resto
  dt[, ingreso_hora := fifelse(
    !is.na(P21) & !is.na(PP3E_TOT) & PP3E_TOT > 0 & P21 > 0,
    P21 / (PP3E_TOT * 4.3),
    NA_real_
  )]

  dt[, grupo_edad := fcase(
    !is.na(CH06) & CH06 >= 14 & CH06 <= 24, "14-24",
    !is.na(CH06) & CH06 >= 25 & CH06 <= 34, "25-34",
    !is.na(CH06) & CH06 >= 35 & CH06 <= 44, "35-44",
    !is.na(CH06) & CH06 >= 45 & CH06 <= 54, "45-54",
    !is.na(CH06) & CH06 >= 55 & CH06 <= 64, "55-64",
    !is.na(CH06) & CH06 >= 65,              "65+",
    default = NA_character_
  )]

  dt[, sexo := fcase(
    CH04 == 1, "Varón",
    CH04 == 2, "Mujer",
    default = NA_character_
  )]

  dt[, nivel_educativo := fcase(
    NIVEL_ED == 1, "Primario incompleto",
    NIVEL_ED == 2, "Primario completo",
    NIVEL_ED == 3, "Secundario incompleto",
    NIVEL_ED == 4, "Secundario completo",
    NIVEL_ED == 5, "Superior/Univ. incompleto",
    NIVEL_ED == 6, "Superior/Univ. completo",
    NIVEL_ED == 7, "Sin instrucción",
    default = NA_character_
  )]

  dt[, categoria_ocup := fcase(
    CAT_OCUP == 1, "Patrón",
    CAT_OCUP == 2, "Cuenta propia",
    CAT_OCUP == 3, "Asalariado",
    CAT_OCUP == 4, "Familiar sin remuneración",
    default = NA_character_
  )]

  dt[, sector := fcase(
    PP04A == 1, "Estatal",
    PP04A == 2, "Privado",
    default = "Otro"
  )]

  dt[, formalidad := fcase(
    PP07H == 1, "Formal",
    PP07H == 2, "Informal",
    default = NA_character_
  )]

  dt[, tipo_empleo := fcase(
    PP07C == 1, "Transitorio",
    PP07C == 2, "Permanente",
    default = NA_character_
  )]

  dt[, turno := fcase(
    PP07J == 1, "Diurno",
    PP07J == 2, "Nocturno",
    PP07J == 3, "Rotativo",
    default = NA_character_
  )]

  dt[, pluriempleo := fcase(
    PP03D == 1, "Un empleo",
    PP03D >  1, "Más de un empleo",
    default = NA_character_
  )]

  # Rama de actividad — CAES-Mercosur, agrupada por sección
  # Fuente: Clasificador de Actividades Económicas para Encuestas Sociodemográficas
  dt[, PP04B_NUM := suppressWarnings(as.numeric(PP04B_COD))]
  dt[, rama_actividad := fcase(
    PP04B_NUM >= 1    & PP04B_NUM <= 99,   "Agricultura, ganadería y pesca",
    PP04B_NUM >= 100  & PP04B_NUM <= 149,  "Minería",
    PP04B_NUM >= 150  & PP04B_NUM <= 399,  "Industria manufacturera",
    PP04B_NUM >= 400  & PP04B_NUM <= 499,  "Electricidad, gas y agua",
    PP04B_NUM >= 500  & PP04B_NUM <= 599,  "Construcción",
    PP04B_NUM >= 600  & PP04B_NUM <= 699,  "Comercio",
    PP04B_NUM >= 700  & PP04B_NUM <= 799,  "Transporte y logística",
    PP04B_NUM >= 800  & PP04B_NUM <= 899,  "Hotelería y gastronomía",
    PP04B_NUM >= 900  & PP04B_NUM <= 999,  "Información y comunicación",
    PP04B_NUM >= 1000 & PP04B_NUM <= 1099, "Actividades financieras",
    PP04B_NUM >= 1100 & PP04B_NUM <= 1199, "Inmobiliarias",
    PP04B_NUM >= 1200 & PP04B_NUM <= 1299, "Servicios profesionales y técnicos",
    PP04B_NUM >= 1300 & PP04B_NUM <= 1399, "Servicios administrativos",
    PP04B_NUM >= 1400 & PP04B_NUM <= 1499, "Administración pública y defensa",
    PP04B_NUM >= 1500 & PP04B_NUM <= 1599, "Enseñanza",
    PP04B_NUM >= 1600 & PP04B_NUM <= 1699, "Salud",
    PP04B_NUM >= 1700 & PP04B_NUM <= 1799, "Arte y entretenimiento",
    PP04B_NUM >= 1800 & PP04B_NUM <= 1899, "Otros servicios",
    PP04B_NUM >= 1900 & PP04B_NUM <= 1999, "Servicio doméstico",
    PP04B_NUM == 9700,                     "Organismos internacionales",
    default = NA_character_
  )]
  dt[, PP04B_NUM := NULL]  # columna auxiliar, no la necesitamos en el output

  # Carácter ocupacional y calificación — CNO 2001
  # Primer dígito = carácter ocupacional (qué hace), segundo = calificación (nivel requerido)
  # Fuente: Clasificador Nacional de Ocupaciones versión 2001
  dt[, cno_pad := str_pad(as.character(PP04D_COD), width = 5, side = "left", pad = "0")]
  dt[, caracter_ocupacional := fcase(
    str_sub(cno_pad, 1, 1) == "1", "Dirección y gerencia",
    str_sub(cno_pad, 1, 1) == "2", "Profesional",
    str_sub(cno_pad, 1, 1) == "3", "Técnico",
    str_sub(cno_pad, 1, 1) == "4", "Administrativo",
    str_sub(cno_pad, 1, 1) == "5", "Comercio y servicios",
    str_sub(cno_pad, 1, 1) == "6", "Agropecuario",
    str_sub(cno_pad, 1, 1) == "7", "Producción artesanal e industrial",
    str_sub(cno_pad, 1, 1) == "8", "Operación de maquinaria",
    str_sub(cno_pad, 1, 1) == "9", "Trabajos no calificados",
    default = NA_character_
  )]
  dt[, calificacion_ocupacional := fcase(
    str_sub(cno_pad, 2, 2) == "1", "Profesional",
    str_sub(cno_pad, 2, 2) == "2", "Técnico",
    str_sub(cno_pad, 2, 2) == "3", "Operativo",
    str_sub(cno_pad, 2, 2) == "4", "No calificado",
    default = NA_character_
  )]
  dt[, cno_pad := NULL]  # columna auxiliar

  # Variable de condición de actividad (útil para el módulo de intro)
  dt[, condicion_actividad := fcase(
    ESTADO == 1, "Ocupado/a",
    ESTADO == 2, "Desocupado/a",
    ESTADO == 3, "Inactivo/a",
    ESTADO == 4, "Menor de 10 años",
    default = NA_character_
  )]

  return(dt)
}

# -----------------------------------------------------------------------------
# PROCESAMIENTO PRINCIPAL
# -----------------------------------------------------------------------------

if (!dir.exists(CARPETA_OUT)) dir.create(CARPETA_OUT, recursive = TRUE)

archivos <- list.files(
  path        = CARPETA_EPH,
  pattern     = "usu_individual.*\\.txt$",
  full.names  = TRUE,
  ignore.case = TRUE
)

cat("Archivos encontrados:", length(archivos), "\n\n")

lista_trimestres <- lapply(archivos, leer_trimestre)

cat("\nConsolidando todos los trimestres...\n")
eph_total <- rbindlist(lista_trimestres, use.names = TRUE, fill = TRUE)

# Join con etiquetas de región y aglomerado
eph_total[, REGION     := as.integer(REGION)]
eph_total[, AGLOMERADO := as.integer(AGLOMERADO)]
eph_total <- merge(eph_total, REGIONES,    by = "REGION",     all.x = TRUE)
eph_total <- merge(eph_total, AGLOMERADOS, by = "AGLOMERADO", all.x = TRUE)

cat("Registros totales (toda la población):", nrow(eph_total), "\n")
cat("Períodos cubiertos:", paste(sort(unique(eph_total$periodo)), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# OUTPUT 1: eph_poblacion.parquet  — TODA la población, todas las columnas
#           Para el módulo de intro: actividad, cuidados, horas totales
# -----------------------------------------------------------------------------

ruta_pob <- file.path(CARPETA_OUT, "eph_poblacion.parquet")
write_parquet(eph_total, ruta_pob)
cat("\neph_poblacion.parquet guardado:", round(file.size(ruta_pob) / 1024^2, 1), "MB\n")

# -----------------------------------------------------------------------------
# OUTPUT 2: eph_consolidado.parquet — solo ocupados con ingreso y horas > 0
#           Idéntico al output anterior; mantiene compatibilidad con f_ocupadxs
# -----------------------------------------------------------------------------

eph_ocupados <- eph_total[ESTADO == 1 & !is.na(P21) & P21 > 0 &
                           !is.na(PP3E_TOT) & PP3E_TOT > 0]

ruta_ocu <- file.path(CARPETA_OUT, "eph_consolidado_final.parquet")
write_parquet(eph_ocupados, ruta_ocu)
cat("eph_consolidado.parquet guardado:", round(file.size(ruta_ocu) / 1024^2, 1), "MB\n")

cat("\nRegistros en eph_consolidado (ocupados con ingreso):", nrow(eph_ocupados), "\n")
cat("\n¡Listo!\n")
