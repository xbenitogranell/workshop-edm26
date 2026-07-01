library(readxl)
library(DBI)
library(duckdb)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)


## ---- FUNCTION OVERVIEW ---------------------------------------------
#
# - load_seed_database(xlsx_path)
# - extract_data(db, req)
# - get_seed_metadata(db, sites = TRUE, records = TRUE)
# - get_data_by_metadata(db, site_name = NA, country = NA, collection = NA,
#                     collector_analyst = NA, habitat = NA, substrate = NA,
#                        sampletype = NA, year = NA,  month = NA, day = NA,
#                                   longitude_min = NA, longitude_max = NA,
#                                     latitude_min = NA, latitude_max = NA,
#                                       exact = TRUE, include_envir = FALSE)
# - get_data_by_site_id(db, site_id, include_envir = FALSE)
# - get_data_by_record_id(db, record_ids, include_envir = FALSE)
# - get_data_by_sample_id(db, sample_ids, include_envir = FALSE)
# - get_data_by_sampletype(db, sampletype, include_envir = FALSE)
# - get_data_by_country(db, country, include_envir = FALSE)
# - get_data_by_name(db, name, exact = TRUE, include_envir = FALSE)
# - get_data_by_coordinates(db, longitude_min = NA, longitude_max = NA,
#                latitude_min = NA, latitude_max = NA, include_envir = FALSE)
# - get_paleo_data_by_age_bp(db, age_young, age_old, include_envir = FALSE)
# - get_data_by_taxa(db, original_name  = NA, accepted_genus = NA,
#                                accepted_name = NA, exact = TRUE,
#                   include_envir = FALSE, include_zeros  = FALSE)
# - merge_records(x)
#











#' Load the SeeD Excel database into an in-memory DuckDB
#'
#' This function reads all sheets from the SeeD Excel workbook and loads each
#' sheet as a table into an in-memory DuckDB database. The sheet names are
#' used as DuckDB table names.
#'
#' @param xlsx_path Character. Path to the Excel workbook containing
#'   the SeeD database (typically a `.xlsx` file). The file must exist and
#'   have an Excel extension (`.xlsx` or `.xls`).
#'
#' @return
#' A DBI connection object to an in-memory DuckDB database. Each sheet in
#' `xlsx_path` is available as a table with the same name as the sheet.
#'
#' @examples
#' \dontrun{
#' con <- load_seed_database("SeeD_databaser.xlsx")
#' DBI::dbListTables(con)
#' DBI::dbGetQuery(con, "SELECT COUNT(*) FROM sites")
#' }
load_seed_database <- function(xlsx_path) {
    # ---- Input checks ----------------------------------------------------
    if (!is.character(xlsx_path) || length(xlsx_path) != 1L) {
        stop("`xlsx_path` must be a single character string with the path to an Excel file.")
    }

    if (!file.exists(xlsx_path)) {
        stop("Excel file not found: ", xlsx_path)
    }

    ext <- tolower(tools::file_ext(xlsx_path))
    if (!ext %in% c("xlsx", "xls")) {
        stop("`xlsx_path` must point to an Excel file with extension '.xlsx' or '.xls'.")
    }

    xlsx_path <- normalizePath(xlsx_path, mustWork = TRUE)

    # ---- Connect to in-memory DuckDB ------------------------------------
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

    # ---- Load each sheet and write as DuckDB table ----------------------
    sheets <- readxl::excel_sheets(xlsx_path)

    for (s in sheets) {
        df <- readxl::read_excel(xlsx_path, sheet = s)

        # If you want, you can normalise column names here, e.g.:
        # names(df) <- janitor::clean_names(names(df))

        DBI::dbWriteTable(con, s, df, overwrite = TRUE)
    }

    con
}


#' Extract data from the SeeD DuckDB database
#'
#' Sends an SQL query to the DuckDB connection created by
#' [load_seed_database()] and returns the result as a data.frame.
#'
#' @param db A DBI connection object, typically the result of
#'   [load_seed_database()].
#' @param req A character scalar containing the SQL query to run.
#'
#' @return
#' A data.frame containing the query result.
#'
#' @examples
#' \dontrun{
#' db <- load_seed_database("SeeD_master.xlsx")
#'
#' # Get all rows from the 'sites' table
#' sites <- extract_data(db, "SELECT * FROM sites")
#'
#' # Filter at the SQL level
#' modern_lakes <- extract_data(
#'   db,
#'   "SELECT * FROM sites WHERE sampletype = 'modern' AND habitat = 'lake'"
#' )
#' }

#' Extract data from the SeeD DuckDB database
#'
#' Sends an SQL query to the DuckDB connection created by
#' [load_seed_database()] and returns the result as a data.frame.
#'
#' @param db A DBI connection object, typically the result of
#'   [load_seed_database()].
#' @param req A character scalar containing the SQL query to run.
#'
#' @return
#' A data.frame containing the query result.
#'
#' @examples
#' \dontrun{
#' db <- load_seed_database("SeeD_master.xlsx")
#'
#' # Get all rows from the 'sites' table
#' sites <- extract_data(db, "SELECT * FROM sites")
#'
#' # Filter at the SQL level
#' modern_lakes <- extract_data(
#'   db,
#'   "SELECT * FROM sites WHERE sampletype = 'modern' AND habitat = 'lake'"
#' )
#' }
extract_data <- function(db, req) {
    # Basic checks
    if (!DBI::dbIsValid(db)) {
        stop("The database connection `db` is not valid. Did you call load_seed_database() first?")
    }

    if (!is.character(req) || length(req) != 1L) {
        stop("`req` must be a single character string containing an SQL query.")
    }

    DBI::dbGetQuery(db, req)
}




#' Extract metadata from the SEED database
#'
#' This function queries the \code{sites} and/or \code{records} tables from a
#' SEED DuckDB database and returns a tibble. By default, both site-level and
#' record-level information are returned as a joined table (one row per record).
#'
#' The behaviour is controlled by the \code{sites} and \code{records} flags:
#' \itemize{
#'   \item \code{sites = TRUE,  records = TRUE} (default): return the joined
#'         site + record metadata (one row per record).
#'   \item \code{sites = TRUE,  records = FALSE}: return only the site-level
#'         metadata (one row per site).
#'   \item \code{sites = FALSE, records = TRUE}: return only the record-level
#'         metadata (one row per record).
#' }
#'
#' If both \code{sites} and \code{records} are \code{FALSE}, the function
#' throws an error.
#'
#' @param db A database connection object created by \code{\link{load_seed_database}},
#'   i.e. a \code{DBIConnection} to a DuckDB database containing the SEED schema.
#' @param sites Logical. If \code{TRUE}, include site-level metadata in the
#'   result. Defaults to \code{TRUE}.
#' @param records Logical. If \code{TRUE}, include record-level metadata in the
#'   result. Defaults to \code{TRUE}.
#'
#' @return
#' A tibble whose structure depends on \code{sites} and \code{records}:
#' \describe{
#'   \item{\code{sites = TRUE, records = TRUE}}{
#'     One row per record, with columns:
#'     \code{site_id}, \code{site_name}, \code{longitude}, \code{latitude},
#'     \code{country}, \code{record_id}, \code{collection},
#'     \code{collector_analyst}, \code{year}, \code{month}, \code{day},
#'     \code{habitat}, \code{substrate}, \code{sampletype}.
#'   }
#'   \item{\code{sites = TRUE, records = FALSE}}{
#'     One row per site, with columns:
#'     \code{site_id}, \code{site_name}, \code{longitude}, \code{latitude},
#'     \code{country}.
#'   }
#'   \item{\code{sites = FALSE, records = TRUE}}{
#'     One row per record, with columns:
#'     \code{record_id}, \code{site_id}, \code{collection},
#'     \code{collector_analyst}, \code{year}, \code{month}, \code{day},
#'     \code{habitat}, \code{substrate}, \code{sampletype}.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' db <- load_seed_database("SEED_database.xlsx")
#'
#' # Full joined metadata
#' meta <- get_seed_metadata(db)
#'
#' # Site-level table only
#' sites <- get_seed_metadata(db, sites = TRUE, records = FALSE)
#'
#' # Record-level table only
#' recs <- get_seed_metadata(db, sites = FALSE, records = TRUE)
#' }
#'
#' @export
get_seed_metadata <- function(db, sites = TRUE, records = TRUE) {
    if (!sites && !records) {
        stop("At least one of 'sites' or 'records' must be TRUE.")
    }

    if (sites && records) {
        # Joined sites + records (one row per record)
        sql <- "
        SELECT DISTINCT
        s.site_id,
        s.name        AS site_name,
        s.longitude,
        s.latitude,
        s.country,
        r.record_id,
        r.collection,
        r.collector_analyst,
        r.year,
        r.month,
        r.day,
        r.habitat,
        r.substrate,
        r.sampletype,
        sa.data_type
        FROM records r
        JOIN sites s ON r.site_id = s.site_id
        JOIN samples sa on r.record_id=sa.record_id
        "
    } else if (sites && !records) {
        # Sites only (one row per site)
        sql <- "
        SELECT DISTINCT
        s.site_id,
        s.name      AS site_name,
        s.longitude,
        s.latitude,
        s.country
        FROM sites s
        "
    } else { # !sites && records
        # Records only (one row per record)
        sql <- "
        SELECT DISTINCT
        r.record_id,
        r.site_id,
        r.collection,
        r.collector_analyst,
        r.year,
        r.month,
        r.day,
        r.habitat,
        r.substrate,
        r.sampletype,
        sa.data_type
        FROM records r
        JOIN samples sa on r.record_id=sa.record_id
        "
    }

    extract_data(db, sql) %>%
    tibble::as_tibble()
}


#' Extract SEeD data by metadata criteria
#'
#' This function subsets the SEeD database based on fields in the metadata
#' table (sites + records), and then retrieves the corresponding diatom data
#' via [get_data_by_record_id()].
#'
#' Internally, it:
#' - calls [get_seed_metadata()] to obtain the full metadata (sites + records),
#' - applies the requested filters,
#' - extracts the matching `record_id`s,
#' - delegates to [get_data_by_record_id()] to fetch diatoms and taxonomy.
#'
#' All filter arguments default to `NA`, which means "do not filter on this
#' field". Each filter (except coordinates) can be a single value or a vector:
#' any row matching **any** value in the vector is kept. If all filters are
#' left as `NA`, the function returns **all** data in the database.
#'
#' Character fields can be matched exactly or partially (case-insensitive)
#' using `exact`. Numeric filters use membership (`%in%`) for the given values.
#' Latitude and longitude are filtered by ranges via `*_min` / `*_max`.
#'
#' @param db
#'   A DuckDB connection object, as returned by [load_seed_database()].
#'
#' @param site_id,record_id
#'   Integer (or coercible) vectors used to filter the `site_id` and
#'   `record_id` columns in the metadata table. `NA` values are ignored.
#'
#' @param site_name,country,collection,collector_analyst,habitat,substrate,sampletype,data_type
#'   Character vectors used to filter the corresponding metadata columns.
#'   `NA` values are ignored.
#'   If `exact = TRUE`, case-insensitive exact matching is used
#'   (after converting both sides to lower case).
#'   If `exact = FALSE`, case-insensitive partial matching is used:
#'   a row is kept if **any** of the provided patterns appears anywhere in
#'   the field.
#'
#' @param year,month,day
#'   Numeric (or coercible) vectors used to filter the corresponding columns.
#'   `NA` values are ignored. A row is kept if the column value is in the
#'   provided set.
#'
#' @param longitude_min,longitude_max,latitude_min,latitude_max
#'   Numeric bounds for longitude and latitude. Each can be a single value or
#'   a vector; in the latter case, the minimum/maximum of the non-`NA` values
#'   is used. Any of these can be `NA` (open interval).
#'   If both `*_min` and `*_max` are provided and `*_min > *_max`, the values
#'   are swapped and a warning is emitted.
#'
#' @param exact
#'   Logical, default `TRUE`. Controls exact vs partial matching
#'   for all character fields.
#'
#' @param include_envir
#'   Logical, default `FALSE`. Passed through to [get_data_by_record_id()] to
#'   decide whether environmental variables from the `environment` table
#'   should be included in the diatom output.
#'
#' @return
#' A list identical in structure to [get_data_by_record_id()]:
#'
#' - `metadata`: tibble of metadata rows that matched the filters.
#' - `diatoms`: named list (or tibble, depending on your extractor settings)
#'   with diatom data.
#' - `taxonomy`: tibble of taxonomy entries for the selected records.
#'
#' If no rows match the filters, a list with empty components is returned.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'
#'   # All Ecuador sites (any sampletype)
#'   res1 <- get_data_by_metadata(
#'     db,
#'     country = "Ecuador",
#'     exact   = TRUE
#'   )
#'
#'   # All paleo records in Spain, between 35–45°N,
#'   # with "Rio" anywhere in the collection name
#'   res2 <- get_data_by_metadata(
#'     db,
#'     country      = "Spain",
#'     sampletype   = "paleo",
#'     collection   = "Rio",
#'     exact        = FALSE,
#'     latitude_min = 35,
#'     latitude_max = 45
#'   )
#'
#'   # Multiple site_ids and record_ids
#'   res3 <- get_data_by_metadata(
#'     db,
#'     site_id   = c(1, 3, 534),
#'     record_id = c(10, 12)
#'   )
#' }
get_data_by_metadata <- function(
    db,
    site_id           = NA,
    record_id         = NA,
    site_name         = NA,
    country           = NA,
    collection        = NA,
    collector_analyst = NA,
    habitat           = NA,
    substrate         = NA,
    sampletype        = NA,
    data_type         = NA,
    year              = NA,
    month             = NA,
    day               = NA,
    longitude_min     = NA,
    longitude_max     = NA,
    latitude_min      = NA,
    latitude_max      = NA,
    exact             = TRUE,
    include_envir     = FALSE
) {
    # ---------------------------------------------------------------------------
    # 1) Load full metadata (sites + records)
    # ---------------------------------------------------------------------------
    metadata <- get_seed_metadata(db, sites = TRUE, records = TRUE) %>%
    tibble::as_tibble()

    if (nrow(metadata) == 0L) {
        warning("Metadata table is empty.")
        return(list(
            metadata = tibble::tibble(),
            diatoms  = list(),
            taxonomy = tibble::tibble()
        ))
    }

    # ---------------------------------------------------------------------------
    # 2) Helpers for filtering
    # ---------------------------------------------------------------------------

    # Character filters: vector values, exact/partial, case-insensitive
    apply_char_filter <- function(tbl, col_name, values, exact) {
        values <- values[!is.na(values)]
        if (length(values) == 0L) {
            return(tbl)
        }

        col_sym <- rlang::sym(col_name)

        if (exact) {
            # Case-insensitive exact match
            vals_l <- tolower(values)
            tbl %>%
            dplyr::filter(
                !is.na(!!col_sym),
                tolower(!!col_sym) %in% vals_l
            )
        } else {
            # Case-insensitive partial match: any pattern appears in the field
            vals_l <- tolower(values)
            tbl %>%
            dplyr::filter(
                !is.na(!!col_sym),
                purrr::map_lgl(
                    !!col_sym,
                    function(x) {
                        xx <- tolower(x)
                        any(vapply(
                            vals_l,
                            function(v) grepl(v, xx, fixed = TRUE),
                            logical(1)
                        ))
                    }
                )
            )
        }
    }

    # Numeric filters: vector membership
    apply_num_filter <- function(tbl, col_name, values) {
        values <- values[!is.na(values)]
        if (length(values) == 0L) {
            return(tbl)
        }
        col_sym <- rlang::sym(col_name)
        tbl %>%
        dplyr::filter(!!col_sym %in% values)
    }

    # ---------------------------------------------------------------------------
    # 3) Apply numeric filters (site_id, record_id, year, month, day)
    # ---------------------------------------------------------------------------
    metadata <- apply_num_filter(metadata, "site_id",   as.integer(site_id))
    metadata <- apply_num_filter(metadata, "record_id", as.integer(record_id))
    metadata <- apply_num_filter(metadata, "year",      as.integer(year))
    metadata <- apply_num_filter(metadata, "month",     as.integer(month))
    metadata <- apply_num_filter(metadata, "day",       as.integer(day))

    # ---------------------------------------------------------------------------
    # 4) Apply character filters
    # ---------------------------------------------------------------------------
    if (!all(is.na(data_type))) {
        allowed_data_types <- c("count", "percentage", "presence_absence")
        bad_data_types <- setdiff(tolower(data_type[!is.na(data_type)]), allowed_data_types)
        if (length(bad_data_types) > 0L) {
            stop(
                "'data_type' must contain only the following values: ",
                paste(allowed_data_types, collapse = ", ")
            )
        }
    }

    metadata <- apply_char_filter(metadata, "site_name",         site_name,         exact)
    metadata <- apply_char_filter(metadata, "country",           country,           exact)
    metadata <- apply_char_filter(metadata, "collection",        collection,        exact)
    metadata <- apply_char_filter(metadata, "collector_analyst", collector_analyst, exact)
    metadata <- apply_char_filter(metadata, "habitat",           habitat,           exact)
    metadata <- apply_char_filter(metadata, "substrate",         substrate,         exact)
    metadata <- apply_char_filter(metadata, "sampletype",        sampletype,        exact)
    metadata <- apply_char_filter(metadata, "data_type",         data_type,         exact)

    # ---------------------------------------------------------------------------
    # 5) Latitude / longitude ranges
    # ---------------------------------------------------------------------------
    # Coerce bounds to numeric; if vectors, use min/max of non-NA values
    lon_min <- suppressWarnings(min(as.numeric(longitude_min), na.rm = TRUE))
    lon_max <- suppressWarnings(max(as.numeric(longitude_max), na.rm = TRUE))
    lat_min <- suppressWarnings(min(as.numeric(latitude_min), na.rm = TRUE))
    lat_max <- suppressWarnings(max(as.numeric(latitude_max), na.rm = TRUE))

    if (!is.finite(lon_min)) lon_min <- NA_real_
    if (!is.finite(lon_max)) lon_max <- NA_real_
    if (!is.finite(lat_min)) lat_min <- NA_real_
    if (!is.finite(lat_max)) lat_max <- NA_real_

    # Swap if min > max, with warning
    if (!is.na(lon_min) && !is.na(lon_max) && lon_min > lon_max) {
        warning("longitude_min > longitude_max; values were swapped.")
        tmp    <- lon_min
        lon_min <- lon_max
        lon_max <- tmp
    }
    if (!is.na(lat_min) && !is.na(lat_max) && lat_min > lat_max) {
        warning("latitude_min > latitude_max; values were swapped.")
        tmp    <- lat_min
        lat_min <- lat_max
        lat_max <- tmp
    }

    if (!is.na(lon_min)) {
        metadata <- metadata %>%
        dplyr::filter(.data$longitude >= lon_min)
    }
    if (!is.na(lon_max)) {
        metadata <- metadata %>%
        dplyr::filter(.data$longitude <= lon_max)
    }
    if (!is.na(lat_min)) {
        metadata <- metadata %>%
        dplyr::filter(.data$latitude >= lat_min)
    }
    if (!is.na(lat_max)) {
        metadata <- metadata %>%
        dplyr::filter(.data$latitude <= lat_max)
    }

    # ---------------------------------------------------------------------------
    # 6) If nothing left, return empty result
    # ---------------------------------------------------------------------------
    if (nrow(metadata) == 0L) {
        warning("No metadata rows match the requested filters.")
        return(list(
            metadata = tibble::tibble(),
            diatoms  = list(),
            taxonomy = tibble::tibble()
        ))
    }

    # Sort metadata consistently
    metadata <- metadata %>%
    dplyr::arrange(.data$site_id, .data$record_id)

    # ---------------------------------------------------------------------------
    # 7) Extract record_ids and delegate to get_data_by_record_id()
    # ---------------------------------------------------------------------------
    record_ids <- unique(metadata$record_id)

    res <- get_data_by_record_id(
        db            = db,
        record_ids    = record_ids,
        include_envir = include_envir
    )

    # Overwrite metadata with the filtered subset
    res$metadata <- metadata

    res
}

#' Get SEeD data for specific sample types
#'
#' Convenience wrapper around [get_data_by_metadata()] to extract all data
#' for one or more sample types.
#'
#' @param db
#'   A DuckDB connection created by [load_seed_database()].
#'
#' @param sampletype
#'   Character vector of sample types (e.g. `"paleo"`, `"modern"`). `NA`
#'   values are ignored.
#'
#' @param exact
#'   Logical, default `TRUE`. If `TRUE`, case-insensitive exact matching is
#'   used for `sampletype`. If `FALSE`, case-insensitive partial matching is
#'   used (any of the provided patterns may appear anywhere in the field).
#'
#' @param include_envir
#'   Logical; if `TRUE`, include environmental variables (passed on to
#'   [get_data_by_metadata()] / [get_data_by_record_id()]).
#'
#' @return
#' A list as returned by [get_data_by_metadata()].
get_data_by_sampletype <- function(
    db,
    sampletype,
    exact         = TRUE,
    include_envir = FALSE
) {
    if (missing(sampletype) || length(sampletype) == 0L) {
        stop("'sampletype' must be provided and non-empty.")
    }

    get_data_by_metadata(
        db            = db,
        sampletype    = sampletype,
        exact         = exact,
        include_envir = include_envir
    )
}



#' Get SEeD data for specific site IDs
#'
#' @param db A DuckDB connection created by [load_seed_database()].
#' @param site_id Integer vector of site IDs to keep.
#' @param include_envir Logical; if `TRUE`, include environmental variables.
#'
#' @return A list as returned by [get_data_by_metadata()].
get_data_by_site_id <- function(db, site_id, include_envir = FALSE) {
    if (missing(site_id) || length(site_id) == 0L) {
        stop("'site_id' must be provided and non-empty.")
    }

    site_id <- as.integer(site_id)

    get_data_by_metadata(
        db           = db,
        site_id      = site_id,
        include_envir = include_envir
    )
}

#' Get SEeD data by country
#'
#' Convenience wrapper around [get_data_by_metadata()] to extract all data
#' for one or more countries.
#'
#' @param db
#'   A DuckDB connection created by [load_seed_database()].
#'
#' @param country
#'   Character vector of country names to filter on. `NA` values are ignored.
#'
#' @param exact
#'   Logical, default `TRUE`. If `TRUE`, case-insensitive exact matching is
#'   used for `country`. If `FALSE`, case-insensitive partial matching is used
#'   (any of the provided patterns may appear anywhere in the country field).
#'
#' @param include_envir
#'   Logical; if `TRUE`, include environmental variables (passed on to
#'   [get_data_by_metadata()] / [get_data_by_record_id()]).
#'
#' @return
#' A list as returned by [get_data_by_metadata()].
get_data_by_country <- function(
    db,
    country,
    exact         = TRUE,
    include_envir = FALSE
) {
    if (missing(country) || length(country) == 0L) {
        stop("'country' must be provided and non-empty.")
    }

    get_data_by_metadata(
        db            = db,
        country       = country,
        exact         = exact,
        include_envir = include_envir
    )
}


#' Get SEeD data by site name
#'
#' @description
#' Wrapper to select records by partial or exact matching of `site_name`.
#'
#' @param db A DuckDB connection created by [load_seed_database()].
#' @param name Character string or vector of site name patterns.
#' @param exact Logical; if `TRUE` (default), match site names exactly.
#'   If `FALSE`, use case-insensitive partial matching.
#' @param include_envir Logical; if `TRUE`, include environmental variables.
#'
#' @return A list with `metadata`, `diatoms`, and `taxonomy`, as returned by
#'   [get_data_by_record_id()].

get_data_by_name <- function(db, name, exact = TRUE, include_envir = FALSE) {
    if (missing(name) || length(name) == 0L) {
        stop("'name' must be provided and non-empty.")
    }

    name <- as.character(name)

    get_data_by_metadata(
        db           = db,
        site_name    = name,
        exact        = exact,
        include_envir = include_envir
    )
}




#' Extract SEeD record-level metadata and diatom data
#'
#' @description
#' Given one or more `record_id`s, this function:
#'
#' - Returns a **metadata** tibble combining information from the
#'   `sites`, `records`, and `samples` tables.
#' - Returns a **diatoms** list of tibbles, one tibble per `record_id`.
#'   Each tibble is a **wide** matrix of diatom counts (or percentages),
#'   with one row per sample and one column per taxon (`original_name`).
#' - Returns a **taxonomy** tibble with one row per taxon used in those records.
#'
#' If `include_envir = TRUE`, environmental variables are retrieved
#' separately, pivoted long → wide, and **right-joined** onto the diatom
#' wide tables (`right_join(environment, diatoms)`), so that all diatom
#' rows are preserved and matching environmental variables are appended
#' as extra columns.
#'
#' @param db
#'   A DuckDB connection object, as returned by [load_seed_database()].
#'
#' @param record_ids
#'   Integer vector of `record_id` values to extract. Must be coercible to
#'   integers and non-empty.
#'
#' @param include_envir
#'   Logical (default `FALSE`). If `TRUE`, environmental data from the
#'   `environment` table are joined to each per-record diatom tibble
#'   after being pivoted to wide format.
#'
#' @details
#' ### Metadata
#' Metadata are built by joining:
#'
#' - `sites` (`s`)
#' - `records` (`r`)
#' - `samples` (`sa`)
#'
#' and returned as a single tibble with one row per `(record_id, data_type)`
#' combination matching the requested `record_id`s. The tibble is sorted by
#' `site_id` then `record_id`.
#'
#' The `data_type` (e.g. `"modern"`, `"paleo"`) is kept only in the
#' metadata tibble and **not** repeated in the diatom matrices to avoid
#' redundancy.
#'
#' ### Diatoms
#' For each `record_id` present in the metadata:
#'
#' 1. Diatom data are queried by joining `records`, `sites`, `samples`,
#'    `diatoms`, and `taxonomy`.
#' 2. The result is pivoted **long → wide**:
#'    - id columns: `site_id, record_id, sample_id, sample_name, depth, age`
#'    - columns: one per `original_name` (taxon)
#'    - values: `value`, with missing combinations filled with `0`
#'      (`values_fill = list(value = 0)`).
#' 3. For **non-paleo** records (where `sampletype != "paleo"`), the
#'    `depth` and `age` columns are dropped.
#' 4. The resulting tibble is sorted:
#'    - For paleo records (sampletype == "paleo" and `depth` available):
#'      by `depth`, then `sample_id`.
#'    - Otherwise: by `sample_id`.
#'
#' The output `diatoms` element is a named list of tibbles, with names
#' equal to the `record_id`s (as character strings).
#'
#' ### Environment (include_envir = TRUE)
#' When `include_envir = TRUE`, a second query is run per `record_id` to
#' retrieve environmental data by joining `records`, `sites`, `samples`,
#' and `environment`. The result is pivoted long → wide:
#'
#' - id columns: `site_id, record_id, sample_id, sample_name, depth, age`
#' - columns: `env_variable`
#' - values: `env_value`
#'
#' This wide environmental table is then **right-joined onto the diatom
#' table**:
#'
#' ```r
#' df_out <- df_env_wide %>%
#'   dplyr::right_join(
#'     df_diat_wide,
#'     by = c(
#'       "site_id",
#'       "record_id",
#'       "sample_id",
#'       "sample_name",
#'       "depth",
#'       "age"
#'     )
#'   )
#' ```
#'
#' so that:
#'
#' - All **diatom** rows are kept.
#' - Environmental columns appear in addition where available.
#' - Samples without environmental data have `NA` in the env columns.
#'
#' The paleo/non-paleo filtering of `depth` and `age` and the final
#' sorting are applied **after** this join.
#'
#' ### Taxonomy
#' The `taxonomy` tibble is restricted to the taxa that appear in the
#' selected records, and contains:
#'
#' - `original_name`
#' - `accepted_genus`
#' - `accepted_name`
#' - `accepted_abbr`
#'
#' Duplicates are removed with `distinct()` and rows are sorted by
#' `original_name`.
#'
#' @return
#' A list with three components:
#'
#' - `metadata`: a tibble of site/record/sample metadata.
#' - `diatoms`: a named list of tibbles, one per `record_id`,
#'   with diatom counts (and optionally environmental variables) in wide format.
#' - `taxonomy`: a tibble of unique taxonomy entries used in those records.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'   res <- get_data_by_record_id(db, record_ids = c(1, 2), include_envir = TRUE)
#'
#'   res$metadata
#'   names(res$diatoms)
#'   res$diatoms[["1"]]
#'   res$taxonomy
#' }
get_data_by_record_id <- function(db, record_ids, include_envir = FALSE) {
    # ---------------------------------------------------------------------------
    # 0) Argument checks
    # ---------------------------------------------------------------------------
    if (missing(record_ids) || length(record_ids) == 0) {
        stop("'record_ids' must be provided and non-empty.")
    }

    record_ids <- unique(as.integer(record_ids))
    if (any(is.na(record_ids))) {
        stop("'record_ids' must be coercible to integers (no NA allowed).")
    }

    # ---------------------------------------------------------------------------
    # 1) METADATA: sites + records + samples (data_type kept here)
    # ---------------------------------------------------------------------------
    record_list_sql <- paste(record_ids, collapse = ", ")

    metadata_sql <- sprintf(
        "
        SELECT DISTINCT
        s.site_id,
        s.name        AS site_name,
        s.longitude,
        s.latitude,
        s.country,
        r.record_id,
        r.collection,
        r.collector_analyst,
        r.year,
        r.month,
        r.day,
        r.habitat,
        r.substrate,
        r.sampletype,
        sa.data_type   AS data_type
        FROM records r
        JOIN sites    s  ON r.site_id    = s.site_id
        JOIN samples  sa ON sa.record_id = r.record_id
        WHERE r.record_id IN (%s)
        ",
        record_list_sql
    )

    metadata <- extract_data(db, metadata_sql) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(site_id, record_id)

    if (nrow(metadata) == 0) {
        return(list(
            metadata = metadata,
            diatoms  = list(),
            taxonomy = tibble::tibble()
        ))
    }

    # Unique record_ids actually present
    record_ids <- metadata %>%
    dplyr::distinct(record_id) %>%
    dplyr::pull(record_id) %>%
    sort()

    # For deciding paleo vs non-paleo per record_id
    rec_sampletype <- metadata %>%
    dplyr::distinct(record_id, sampletype)

    # ---------------------------------------------------------------------------
    # 2) SQL templates
    # ---------------------------------------------------------------------------
    diatoms_template <- "
    SELECT
    s.site_id,
    r.record_id,
    sa.sample_id,
    sa.sample_name,
    sa.depth,
    sa.age,
    d.diatom_id,
    d.value,
    d.data_type    AS diatom_data_type,
    t.original_name
    FROM records r
    JOIN sites    s  ON r.site_id    = s.site_id
    JOIN samples  sa ON sa.record_id = r.record_id
    JOIN diatoms  d  ON d.sample_id  = sa.sample_id
    JOIN taxonomy t  ON t.diatom_id  = d.diatom_id
    WHERE r.record_id = %d
    "

    if (include_envir) {
        env_template <- "
        SELECT
        s.site_id,
        r.record_id,
        sa.sample_id,
        sa.sample_name,
        sa.depth,
        sa.age,
        e.variable  AS env_variable,
        e.value     AS env_value
        FROM records r
        JOIN sites        s  ON r.site_id    = s.site_id
        JOIN samples      sa ON sa.record_id = r.record_id
        JOIN environment  e  ON e.sample_id  = sa.sample_id
        WHERE r.record_id = %d
        "
    }

    # ---------------------------------------------------------------------------
    # 3) Loop over record_ids, one query (or two) per record, each result -> tibble
    # ---------------------------------------------------------------------------
    diatom_list <- lapply(record_ids, function(rid) {
        # Determine if this record is paleo
        st_vec <- rec_sampletype %>%
        dplyr::filter(record_id == rid) %>%
        dplyr::pull(sampletype)

        is_paleo <- length(st_vec) > 0 &&
        !is.na(st_vec[1]) &&
        tolower(st_vec[1]) == "paleo"

        # --- 3a. DIATOM COUNTS (always) -----------------------------------------
        sql_diat <- sprintf(diatoms_template, rid)

        df_long <- extract_data(db, sql_diat) %>%
        tibble::as_tibble()

        df_diat_wide <- df_long %>%
        dplyr::select(
            site_id,
            record_id,
            sample_id,
            sample_name,
            depth,
            age,
            original_name,
            value
        ) %>%
        tidyr::pivot_wider(
            id_cols     = c(site_id, record_id, sample_id, sample_name, depth, age),
            names_from  = original_name,
            values_from = value,
            values_fill = list(value = 0)
        )

        # --- 3b. ENVIRONMENT (optional, separate, right_join onto diatoms) ------
        if (include_envir) {
            sql_env <- sprintf(env_template, rid)

            df_env_long <- extract_data(db, sql_env) %>%
            tibble::as_tibble()

            if (nrow(df_env_long) > 0) {
                df_env_wide <- df_env_long %>%
                dplyr::select(
                    site_id,
                    record_id,
                    sample_id,
                    sample_name,
                    depth,
                    age,
                    env_variable,
                    env_value
                ) %>%
                tidyr::pivot_wider(
                    id_cols     = c(site_id, record_id, sample_id, sample_name, depth, age),
                    names_from  = env_variable,
                    values_from = env_value
                )

                # RIGHT JOIN: environment (left) onto diatoms (right)
                df_out <- df_env_wide %>%
                dplyr::right_join(
                    df_diat_wide,
                    by = c(
                        "site_id",
                        "record_id",
                        "sample_id",
                        "sample_name",
                        "depth",
                        "age"
                    )
                )
            } else {
                df_out <- df_diat_wide
            }
        } else {
            df_out <- df_diat_wide
        }

        # --- 3c. Drop depth/age if not paleo, and sort --------------------------
        if (!is_paleo) {
            df_out <- df_out %>%
            dplyr::select(-depth, -age)
        }

        if (is_paleo && "depth" %in% names(df_out)) {
            df_out <- df_out %>%
            dplyr::arrange(depth, sample_id)
        } else {
            df_out <- df_out %>%
            dplyr::arrange(sample_id)
        }

        df_out
    })

    names(diatom_list) <- as.character(record_ids)

    # ---------------------------------------------------------------------------
    # 4) TAXONOMY subset for these records
    # ---------------------------------------------------------------------------
    record_list_sql <- paste(record_ids, collapse = ", ")

    taxonomy_sql <- sprintf(
        "
        SELECT DISTINCT
        t.original_name,
        t.accepted_genus,
        t.accepted_name,
        t.accepted_abbr
        FROM taxonomy t
        JOIN diatoms  d  ON d.diatom_id  = t.diatom_id
        JOIN samples  sa ON sa.sample_id = d.sample_id
        JOIN records  r  ON r.record_id  = sa.record_id
        WHERE r.record_id IN (%s)
        ",
        record_list_sql
    )

    taxonomy <- extract_data(db, taxonomy_sql) %>%
    tibble::as_tibble() %>%
    dplyr::distinct(
        original_name,
        accepted_genus,
        accepted_name,
        accepted_abbr,
        .keep_all = TRUE
    ) %>%
    dplyr::arrange(original_name)

    # ---------------------------------------------------------------------------
    # 5) Return everything
    # ---------------------------------------------------------------------------

    # If only one record, simplify the diatoms list.
    if (length(diatom_list) == 1L) {
        diatom_list <- diatom_list[[1]]
    }


    list(
        metadata = metadata,
        diatoms  = diatom_list,
        taxonomy = taxonomy
    )
}







#' Extract SEeD data for one or more sample_ids
#'
#' @description
#' Given one or more `sample_id`s, this function:
#'
#' - Looks up the corresponding `record_id`s in the `samples` table.
#' - Delegates the heavy lifting to [get_data_by_record_id()], which returns
#'   a list with `metadata`, `diatoms` (per-record wide matrices), and
#'   `taxonomy`.
#' - Filters each per-record diatom tibble to keep only the requested
#'   `sample_id`s.
#' - Restricts `metadata` and `taxonomy` to what is actually used by
#'   those samples/records.
#'
#' The output structure is identical to [get_data_by_record_id()].
#'
#' @param db
#'   A DuckDB connection object, as returned by [load_seed_database()].
#'
#' @param sample_ids
#'   Integer (or coercible-to-integer) vector of `sample_id`s to extract.
#'   Must be non-empty and contain no `NA` after coercion.
#'
#' @param include_envir
#'   Logical (default `FALSE`). Passed through to [get_data_by_record_id()].
#'   When `TRUE`, environmental variables (pivoted to wide format) are
#'   right-joined onto each per-record diatom tibble.
#'
#' @return
#' A list with three components:
#'
#' - `metadata`: tibble of site/record/sample metadata restricted to
#'   records actually represented in the filtered diatoms.
#' - `diatoms`: named list of tibbles, one per `record_id`, each containing
#'   only the requested `sample_id`s (in wide format: one column per taxon).
#' - `taxonomy`: tibble of unique taxonomy entries for taxa present in the
#'   filtered diatom tables.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'
#'   res <- get_data_by_sample_id(
#'     db,
#'     sample_ids    = c(2088, 2090, 3001),
#'     include_envir = TRUE
#'   )
#'
#'   res$metadata
#'   names(res$diatoms)
#'   res$diatoms[[1]]
#'   res$taxonomy
#' }
get_data_by_sample_id <- function(db, sample_ids, include_envir = FALSE) {
  # ---------------------------------------------------------------------------
  # 0) Argument checks
  # ---------------------------------------------------------------------------
  if (missing(sample_ids) || length(sample_ids) == 0L) {
    stop("'sample_ids' must be provided and non-empty.")
  }

  sample_ids <- unique(as.integer(sample_ids))
  if (any(is.na(sample_ids))) {
    stop("'sample_ids' must be coercible to integers (no NA allowed).")
  }

  # ---------------------------------------------------------------------------
  # 1) Map sample_ids -> record_ids
  # ---------------------------------------------------------------------------
  sample_list_sql <- paste(sample_ids, collapse = ", ")

  sql_map <- sprintf(
    "
    SELECT DISTINCT
      sa.sample_id,
      sa.record_id
    FROM samples sa
    WHERE sa.sample_id IN (%s)
    ",
    sample_list_sql
  )

  map_tbl <- extract_data(db, sql_map) %>%
    tibble::as_tibble()

  if (nrow(map_tbl) == 0L) {
    warning(
      sprintf(
        "No samples found with sample_id in {%s}.",
        paste(sample_ids, collapse = ", ")
      )
    )
    return(list(
      metadata = tibble::tibble(),
      diatoms  = list(),
      taxonomy = tibble::tibble()
    ))
  }

  record_ids <- sort(unique(map_tbl$record_id))

  # ---------------------------------------------------------------------------
  # 2) Delegate to get_data_by_record_id()
  # ---------------------------------------------------------------------------
  base_res <- get_data_by_record_id(
    db            = db,
    record_ids    = record_ids,
    include_envir = include_envir
  )

  # If get_data_by_record_id returned nothing meaningful, propagate
  if (is.null(base_res$diatoms) ||
      (is.list(base_res$diatoms) && length(base_res$diatoms) == 0L) ||
      (is.data.frame(base_res$diatoms) && nrow(base_res$diatoms) == 0L) ||
      nrow(base_res$metadata) == 0L) {
    return(list(
      metadata = base_res$metadata,
      diatoms  = base_res$diatoms,
      taxonomy = base_res$taxonomy
    ))
  }

  # ---------------------------------------------------------------------------
  # 3) Normalise diatoms to a *list* of tibbles (even for a single record)
  # ---------------------------------------------------------------------------
  if (is.data.frame(base_res$diatoms)) {
    # Single record: wrap into a list and name by its record_id
    diatoms_list <- list(base_res$diatoms)

    # Try to infer the record_id from metadata (unique per call)
    rec_ids_in_meta <- unique(base_res$metadata$record_id)
    if (length(rec_ids_in_meta) == 1L) {
      names(diatoms_list) <- as.character(rec_ids_in_meta)
    } else {
      # Fallback: anonymous name
      names(diatoms_list) <- "record_1"
    }
  } else if (is.list(base_res$diatoms)) {
    diatoms_list <- base_res$diatoms
  } else {
    stop("Unexpected structure in base_res$diatoms; expected list or data.frame.")
  }

  # ---------------------------------------------------------------------------
  # 4) Filter diatoms list to requested sample_ids
  # ---------------------------------------------------------------------------
  requested_ids <- sample_ids

  diatom_list <- lapply(diatoms_list, function(df) {
    if (!"sample_id" %in% names(df)) {
      # Unexpected, but keep shape: empty tibble with same cols
      return(df[0, , drop = FALSE])
    }

    df %>%
      dplyr::filter(.data$sample_id %in% requested_ids)
  })

  # Drop records with no matching samples
  non_empty <- vapply(
    diatom_list,
    function(x) nrow(x) > 0L,
    logical(1)
  )
  diatom_list <- diatom_list[non_empty]

  # If everything got filtered out, return empties
  if (length(diatom_list) == 0L) {
    return(list(
      metadata = tibble::tibble(),
      diatoms  = list(),
      taxonomy = tibble::tibble()
    ))
  }

  # Keep names as record_ids where available
  names(diatom_list) <- names(diatoms_list)[non_empty]

  # ---------------------------------------------------------------------------
  # 5) Subset metadata to the records we actually kept
  # ---------------------------------------------------------------------------
  # Try to parse record_ids from the names; if not possible, fall back to unique in metadata
  suppressWarnings({
    kept_record_ids <- as.integer(names(diatom_list))
  })
  if (any(is.na(kept_record_ids))) {
    kept_record_ids <- unique(base_res$metadata$record_id)
  }

  metadata_sub <- base_res$metadata %>%
    dplyr::filter(.data$record_id %in% kept_record_ids)

  # ---------------------------------------------------------------------------
  # 6) Restrict taxonomy to taxa actually present in the filtered diatoms
  # ---------------------------------------------------------------------------
  # Collect taxon column names from all diatom tibbles
  taxon_cols <- unique(unlist(
    lapply(diatom_list, function(df) {
      setdiff(
        names(df),
        c(
          "site_id", "record_id", "sample_id", "sample_name",
          "depth", "age"
        )
      )
    })
  ))

  taxonomy_sub <- base_res$taxonomy %>%
    dplyr::filter(.data$original_name %in% taxon_cols) %>%
    dplyr::arrange(.data$original_name)

  # ---------------------------------------------------------------------------
  # 7) Return everything
  # ---------------------------------------------------------------------------
  # If only one record, simplify the diatoms list.
  if (length(diatom_list) == 1L) {
      diatom_list <- diatom_list[[1]]
  }

  list(
    metadata = metadata_sub,
    diatoms  = diatom_list,
    taxonomy = taxonomy_sub
  )
}


#' Extract paleo SEeD data by age range (cal yr BP)
#'
#' @description
#' Select **paleo** samples whose age (in cal yr BP) falls within a user-defined
#' age range, and return the corresponding metadata, diatom matrices, and
#' taxonomy.
#'
#' You can specify:
#'
#' - Both `age_young` and `age_old` → closed interval
#'   (`age_young <= age <= age_old`). In cal BP, larger values are older.
#'   If `age_young > age_old`, the two are swapped and a warning is issued.
#' - Only `age_young` (with `age_old` missing or `NA`) → all samples with
#'   `age >= age_young`.
#' - Only `age_old` (with `age_young` missing or `NA`) → all samples with
#'   `age <= age_old`.
#'
#' At least one of the two bounds must be provided and coercible to numeric.
#'
#' Internally, this helper:
#' - Filters to `records.sampletype == "paleo"` (case-insensitive),
#' - Applies the chosen age condition on `samples.age`,
#' - Collects the matching `sample_id`s,
#' - Delegates the heavy lifting to [get_data_by_sample_id()],
#'   which builds the `metadata`, `diatoms`, and `taxonomy` objects.
#'
#' @param db
#'   A DuckDB connection object, as returned by [load_seed_database()].
#'
#' @param age_young
#'   Numeric scalar or `NA`. Younger bound of the age range (in cal yr BP).
#'   If provided and non-`NA`, it defines a lower bound:
#'   - together with `age_old` (if also provided) for a closed interval,
#'   - on its own for an open-ended interval `age >= age_young`.
#'
#' @param age_old
#'   Numeric scalar or `NA`. Older bound of the age range (in cal yr BP).
#'   If provided and non-`NA`, it defines an upper bound:
#'   - together with `age_young` (if also provided) for a closed interval,
#'   - on its own for an open-ended interval `age <= age_old`.
#'
#'   In cal BP, this value should normally be **larger** than `age_young`.
#'   If both are provided and `age_young > age_old`, the function swaps the
#'   two values and emits a warning.
#'
#' @param include_envir
#'   Logical (default `FALSE`). Passed through to [get_data_by_sample_id()].
#'   When `TRUE`, environmental variables (pivoted to wide format) are
#'   right-joined onto each per-record diatom tibble.
#'
#' @return
#' A list with three components (same structure as [get_data_by_sample_id()]):
#'
#' - `metadata`: tibble of site/record/sample metadata for the selected
#'   paleo samples.
#' - `diatoms`: named list of tibbles, one per `record_id`, each containing
#'   only the selected samples, in wide format (one column per taxon).
#' - `taxonomy`: tibble of unique taxonomy entries corresponding to taxa
#'   present in those diatom tables.
#'
#' If no samples match the criteria, the function returns:
#'
#' - `metadata`: empty tibble,
#' - `diatoms`: empty list,
#' - `taxonomy`: empty tibble,
#'
#' and issues a warning.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'
#'   # Closed interval: 0–10 000 cal yr BP
#'   res1 <- get_paleo_data_by_age_bp(
#'     db,
#'     age_young     = 0,
#'     age_old       = 10000,
#'     include_envir = TRUE
#'   )
#'
#'   # All paleo samples younger than 5 000 cal yr BP (age <= 5000)
#'   res2 <- get_paleo_data_by_age_bp(
#'     db,
#'     age_young     = NA,
#'     age_old       = 5000
#'   )
#'
#'   # All paleo samples older than 20 000 cal yr BP (age >= 20000)
#'   res3 <- get_paleo_data_by_age_bp(
#'     db,
#'     age_young     = 20000,
#'     age_old       = NA
#'   )
#' }



get_paleo_data_by_age_bp <- function(db, age_young, age_old, include_envir = FALSE) {
    # ---------------------------------------------------------------------------
    # 0) Argument checks and bound handling
    # ---------------------------------------------------------------------------

    # Flags for "provided and not NA" at the R level
    young_provided <- !(missing(age_young) || (length(age_young) == 1L && is.na(age_young)))
    old_provided   <- !(missing(age_old)   || (length(age_old)   == 1L && is.na(age_old)))

    if (!young_provided && !old_provided) {
        stop("At least one of 'age_young' or 'age_old' must be provided (non-NA).")
    }

    # Coerce to numeric where provided
    if (young_provided) {
        age_young <- as.numeric(age_young)
        if (is.na(age_young)) {
            young_provided <- FALSE
        }
    }

    if (old_provided) {
        age_old <- as.numeric(age_old)
        if (is.na(age_old)) {
            old_provided <- FALSE
        }
    }

    # After coercion, ensure we still have at least one bound
    if (!young_provided && !old_provided) {
        stop("At least one of 'age_young' or 'age_old' must be a valid numeric value.")
    }

    # Both bounds present → enforce BP ordering and swap if needed
    if (young_provided && old_provided && age_young > age_old) {
        warning(
            sprintf(
                "'age_young' (%.3f) was larger than 'age_old' (%.3f) in cal yr BP; values have been swapped.",
                age_young, age_old
            )
        )
        tmp       <- age_young
        age_young <- age_old
        age_old   <- tmp
    }

    # ---------------------------------------------------------------------------
    # 1) Build WHERE clause according to which bounds are present
    # ---------------------------------------------------------------------------
    conds <- c(
        "LOWER(r.sampletype) = 'paleo'",
        "sa.age IS NOT NULL"
    )

    if (young_provided) {
        conds <- c(conds, sprintf("sa.age >= %f", age_young))
    }
    if (old_provided) {
        conds <- c(conds, sprintf("sa.age <= %f", age_old))
    }

    where_clause <- paste(conds, collapse = "\n      AND ")

    sql_samples <- sprintf(
        "
        SELECT DISTINCT
        sa.sample_id
        FROM samples sa
        JOIN records r
        ON sa.record_id = r.record_id
        WHERE %s
        ",
        where_clause
    )

    # ---------------------------------------------------------------------------
    # 2) Fetch matching sample_ids
    # ---------------------------------------------------------------------------
    sample_tbl <- extract_data(db, sql_samples) %>%
    tibble::as_tibble()

    if (nrow(sample_tbl) == 0L) {
        # Build a helpful message depending on which bounds exist
        range_msg <- if (young_provided && old_provided) {
            sprintf("between %.3f and %.3f cal yr BP", age_young, age_old)
        } else if (young_provided) {
            sprintf(">= %.3f cal yr BP", age_young)
        } else {
            sprintf("<= %.3f cal yr BP", age_old)
        }

        warning(
            sprintf(
                "No paleo samples found with age %s.",
                range_msg
            )
        )

        return(list(
            metadata = tibble::tibble(),
            diatoms  = list(),
            taxonomy = tibble::tibble()
        ))
    }

    sample_ids <- sort(unique(sample_tbl$sample_id))

    # ---------------------------------------------------------------------------
    # 3) Delegate to get_data_by_sample_id() for full extraction + structuring
    # ---------------------------------------------------------------------------
    get_data_by_sample_id(
        db            = db,
        sample_ids    = sample_ids,
        include_envir = include_envir
    )
}


#' Extract SEeD data by metadata criteria
#'
#' This function lets you subset the SEeD database based on fields in the
#' metadata table (sites + records), and then retrieves the corresponding
#' diatom data via [get_data_by_record_id()].
#'
#' Internally, it calls [get_seed_metadata()] to obtain the full metadata,
#' applies the requested filters, extracts the matching `record_id`s, and
#' delegates to [get_data_by_record_id()].
#'
#' All filter arguments default to `NA`, which means "do not filter on this
#' field". If all filters are left as `NA`, the function returns **all**
#' data in the database.
#'
#' Character fields can be specified as single values or vectors and can be
#' matched exactly or partially (case-insensitive) using `exact`.
#' Numeric fields can also be single values or vectors. Latitude and longitude
#' are filtered by ranges via `*_min` / `*_max`.
#'
#' Note that this function does **not** subset directly by `site_id` or
#' `record_id`; for that, use the dedicated helpers (e.g. [get_data_by_site()],
#' [get_data_by_record_id()]).
#'
#' @param db A DuckDB connection object, as returned by [load_seed_database()].
#'
#' @param site_name,country,collection,collector_analyst,habitat,substrate,sampletype
#'   Character vectors used to filter the corresponding metadata columns.
#'   If `exact = TRUE`, case-insensitive exact matching is used.
#'   If `exact = FALSE`, case-insensitive partial matching is used
#'   (any of the provided patterns may appear anywhere in the field).
#'
#' @param year,month,day
#'   Numeric (or coercible) vectors used to filter the corresponding columns.
#'
#' @param longitude_min,longitude_max,latitude_min,latitude_max
#'   Numeric bounds for longitude and latitude. Any of these can be `NA`
#'   (open interval). If both `*_min` and `*_max` are provided and
#'   `*_min > *_max`, the values are swapped and a warning is emitted.
#'
#' @param exact Logical, default `TRUE`. Controls exact vs partial matching
#'   for all character fields.
#'
#' @param include_envir Logical, default `FALSE`. Passed through to
#'   [get_data_by_record_id()] to decide whether environmental variables from
#'   the `environment` table should be included in the diatom output.
#'
#' @return
#' A list identical in structure to [get_data_by_record_id()]:
#'
#' - `metadata`: tibble of metadata rows that matched the filters.
#' - `diatoms`: named list of per-record diatom tibbles (or a single tibble,
#'   depending on how you configured the record-based extractor).
#' - `taxonomy`: tibble of taxonomy entries for the selected records.
#'
#' If no rows match the filters, a list with empty components is returned.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'
#'   # All Ecuador sites, any sampletype
#'   res1 <- get_data_by_metadata(
#'     db,
#'     country   = "Ecuador",
#'     exact     = TRUE
#'   )
#'
#'   # All paleo records in Spain, between 35–45°N, with "Rio" in the collection name
#'   res2 <- get_data_by_metadata(
#'     db,
#'     country       = "Spain",
#'     sampletype    = "paleo",
#'     collection    = "Rio",
#'     exact         = FALSE,
#'     latitude_min  = 35,
#'     latitude_max  = 45
#'   )
#' }

#' Combine per-record diatom tables into a single wide table
#'
#' This function takes a SEED-style result list (as returned by
#' `get_seed_site_data()` or `get_seed_sampletype_data()`), where
#' `x$diatoms` is a list of tibbles (one per record_id), and merges
#' all those tibbles into a single wide diatom table.
#'
#' The input list must have elements:
#' - `metadata`: a tibble of site + record (+ sample) metadata.
#' - `diatoms` : a named list of tibbles (possibly empty).
#' - `taxonomy`: a tibble containing at least `original_name`.
#'
#' The output list has the same structure, except:
#' - `diatoms` is now a single tibble.
#'
#' Internally, each per-record diatom table is converted to long format
#' using the taxon names in `taxonomy$original_name`, then all long
#' tables are row-bound, and finally the result is pivoted back to wide
#' with one row per sample and one column per taxon. Missing diatom
#' values are filled with 0.
#'
#' @param x A list with elements `metadata`, `diatoms`, and `taxonomy`.
#'
#' @return A list with the same elements as `x`, but with `diatoms`
#'   replaced by a single wide tibble.
#'
#' @examples
#' \dontrun{
#'   res <- get_seed_site_data(db, site_ids = c(1468, 1475))
#'   res_flat <- combine_seed_diatoms(res)
#'   res_flat$diatoms
#' }
merge_records <- function(x) {
    # Basic sanity checks
    if (!is.list(x)) {
        stop("'x' must be a list with elements 'metadata', 'diatoms', and 'taxonomy'.")
    }
    needed <- c("metadata", "diatoms", "taxonomy")
    if (!all(needed %in% names(x))) {
        stop("Input list must contain elements: ", paste(needed, collapse = ", "), ".")
    }

    diatom_list <- x$diatoms

    # If there is nothing to combine, just replace with empty tibble and return
    if (length(diatom_list) == 0L) {
        x$diatoms <- tibble::tibble()
        return(x)
    }

    # Ensure taxonomy has original_name
    if (!"original_name" %in% names(x$taxonomy)) {
        stop("taxonomy must contain an 'original_name' column.")
    }

    all_taxa <- unique(x$taxonomy$original_name)

    # --------------------------------------------------------------------------
    # 1) Convert each per-record wide diatom table to long format
    # --------------------------------------------------------------------------
    diatoms_long_list <- lapply(diatom_list, function(df) {
        df <- tibble::as_tibble(df)

        # Taxa actually present in this particular tibble
        taxa_cols <- intersect(all_taxa, colnames(df))

        if (length(taxa_cols) == 0L) {
            # No diatom columns here, return empty tibble with same ID cols
            return(tibble::tibble())
        }

        tidyr::pivot_longer(
            df,
            cols      = dplyr::all_of(taxa_cols),
            names_to  = "original_name",
            values_to = "value"
        )
    })

    # Drop completely empty pieces (if any)
    diatoms_long_list <- diatoms_long_list[vapply(
        diatoms_long_list,
        function(z) nrow(z) > 0L,
        logical(1)
    )]

    if (length(diatoms_long_list) == 0L) {
        x$diatoms <- tibble::tibble()
        return(x)
    }

    # --------------------------------------------------------------------------
    # 2) Bind all long tables together
    # --------------------------------------------------------------------------
    diatoms_long <- dplyr::bind_rows(diatoms_long_list)

    # --------------------------------------------------------------------------
    # 3) Pivot back to wide: one row per sample, one column per taxon
    # --------------------------------------------------------------------------
    # All columns except 'original_name' and 'value' are ID / metadata columns.
    id_cols <- setdiff(colnames(diatoms_long), c("original_name", "value"))

    diatoms_wide <- diatoms_long %>%
    tidyr::pivot_wider(
        id_cols    = dplyr::all_of(id_cols),
        names_from = "original_name",
        values_from = "value",
        values_fill = list(value = 0)
    ) %>%
    dplyr::arrange(
        dplyr::across(dplyr::any_of(c("site_id", "record_id", "sample_id")))
    )

    # --------------------------------------------------------------------------
    # 4) Replace diatoms element and return
    # --------------------------------------------------------------------------
    x$diatoms <- diatoms_wide
    x
}


#' Extract SEeD data by diatom taxonomy
#'
#' This function subsets the SEeD database based on diatom names in the
#' `taxonomy` table and returns the corresponding records, diatom data,
#' and taxonomy.
#'
#' You can filter on any combination of:
#' - `original_name`
#' - `accepted_genus`
#' - `accepted_name`
#'
#' Matching can be exact or partial (both case-insensitive). Internally, the
#' function:
#'
#' 1. Loads the relevant columns from `taxonomy`.
#' 2. Applies the requested filters to obtain matching `diatom_id`s.
#' 3. Finds all `record_id`s where those `diatom_id`s occur (via `diatoms`,
#'    `samples`, `records`).
#' 4. Calls [get_data_by_record_id()] on those `record_id`s.
#' 5. Optionally removes samples where all selected taxa have zero counts
#'    (`include_zeros = FALSE`).
#'
#' If all three diatom name arguments are left as `NA`, the function errors
#' and asks for at least one to be supplied.
#'
#' @param db
#'   A DuckDB connection object, as returned by [load_seed_database()].
#'
#' @param original_name
#'   Character vector of values to match against `taxonomy.original_name`.
#'   Default `NA` (not used).
#'
#' @param accepted_genus
#'   Character vector of values to match against `taxonomy.accepted_genus`.
#'   Default `NA` (not used).
#'
#' @param accepted_name
#'   Character vector of values to match against `taxonomy.accepted_name`.
#'   Default `NA` (not used).
#'
#' @param exact
#'   Logical, default `TRUE`. If `TRUE`, matching is case-insensitive
#'   **exact** equality. If `FALSE`, matching is case-insensitive and any
#'   provided pattern may appear anywhere in the field (substring match).
#'
#' @param include_envir
#'   Logical, default `FALSE`. Passed through to [get_data_by_record_id()]
#'   to decide whether environmental variables are included.
#'
#' @param include_zeros
#'   Logical, default `FALSE`. If `FALSE`, samples (rows in the `diatoms`
#'   tibbles) where **all selected taxa** have a count of zero are removed.
#'   If `TRUE`, such samples are kept.
#'
#' @return
#' A list with three components, in the same structure as
#' [get_data_by_record_id()]:
#'
#' - `metadata`: tibble of metadata rows for records where the selected
#'   diatoms occur (possibly reduced if some records end up with no samples).
#' - `diatoms`: a named list of per-record diatom tibbles (optionally
#'   filtered to remove samples with zero counts for all selected taxa).
#' - `taxonomy`: tibble of the taxonomy entries used in those records.
#'
#' If no taxa match the requested filters, or no records contain those taxa,
#' a list with empty components is returned.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'
#'   # Exact match on original_name, drop samples where the taxon is absent
#'   res1 <- get_data_by_taxa(
#'     db            = db,
#'     original_name = "Achnanthidium minutissimum",
#'     exact         = TRUE,
#'     include_zeros = FALSE
#'   )
#'
#'   # Fuzzy match on accepted_genus and accepted_name, keep all samples
#'   res2 <- get_data_by_taxa(
#'     db             = db,
#'     accepted_genus = "Sellaphora",
#'     accepted_name  = "minima",
#'     exact          = FALSE,
#'     include_envir  = TRUE,
#'     include_zeros  = TRUE
#'   )
#' }
get_data_by_taxa <- function(
  db,
  original_name  = NA,
  accepted_genus = NA,
  accepted_name  = NA,
  exact          = TRUE,
  include_envir  = FALSE,
  include_zeros  = FALSE
) {
  # ---------------------------------------------------------------------------
  # 0) Argument checks
  # ---------------------------------------------------------------------------
  orig_vals <- original_name[!is.na(original_name)]
  gen_vals  <- accepted_genus[!is.na(accepted_genus)]
  acc_vals  <- accepted_name[!is.na(accepted_name)]

  if (length(orig_vals) == 0L &&
      length(gen_vals)  == 0L &&
      length(acc_vals)  == 0L) {
    stop(
      "At least one of 'original_name', 'accepted_genus', or 'accepted_name' ",
      "must be provided (non-NA)."
    )
  }

  # ---------------------------------------------------------------------------
  # 1) Load taxonomy (minimal columns)
  # ---------------------------------------------------------------------------
  taxonomy_all <- extract_data(
    db,
    "
    SELECT
      diatom_id,
      original_name,
      accepted_genus,
      accepted_name
    FROM taxonomy
    "
  ) %>%
    tibble::as_tibble()

  if (nrow(taxonomy_all) == 0L) {
    warning("Taxonomy table is empty.")
    return(list(
      metadata = tibble::tibble(),
      diatoms  = list(),
      taxonomy = tibble::tibble()
    ))
  }

  # ---------------------------------------------------------------------------
  # 2) Helper for character filters (exact / partial, case-insensitive)
  # ---------------------------------------------------------------------------
  apply_char_filter <- function(tbl, col_name, values) {
    values <- values[!is.na(values)]
    if (length(values) == 0L) {
      return(tbl)
    }

    col_sym <- rlang::sym(col_name)

    if (exact) {
      # Case-insensitive exact match
      vals_l <- tolower(values)
      tbl %>%
        dplyr::filter(
          !is.na(!!col_sym),
          tolower(!!col_sym) %in% vals_l
        )
    } else {
      # Case-insensitive partial (substring) match
      vals_l <- tolower(values)
      tbl %>%
        dplyr::filter(
          !is.na(!!col_sym),
          purrr::map_lgl(
            !!col_sym,
            function(x) {
              xx <- tolower(x)
              any(vapply(
                vals_l,
                function(v) grepl(v, xx, fixed = TRUE),
                logical(1)
              ))
            }
          )
        )
    }
  }

  # ---------------------------------------------------------------------------
  # 3) Apply taxonomy filters
  # ---------------------------------------------------------------------------
  taxonomy_filt <- taxonomy_all %>%
    apply_char_filter("original_name",  orig_vals) %>%
    apply_char_filter("accepted_genus", gen_vals)  %>%
    apply_char_filter("accepted_name",  acc_vals)

  if (nrow(taxonomy_filt) == 0L) {
    warning("No taxa match the requested name filters.")
    return(list(
      metadata = tibble::tibble(),
      diatoms  = list(),
      taxonomy = tibble::tibble()
    ))
  }

  diatom_ids <- unique(taxonomy_filt$diatom_id)

  # ---------------------------------------------------------------------------
  # 4) Find all record_ids where these diatom_ids occur
  # ---------------------------------------------------------------------------
  diatom_id_list <- paste(diatom_ids, collapse = ", ")

  sql_records <- sprintf(
    "
    SELECT DISTINCT
      r.record_id
    FROM diatoms  d
    JOIN samples  sa ON sa.sample_id = d.sample_id
    JOIN records  r  ON r.record_id  = sa.record_id
    WHERE d.diatom_id IN (%s)
    ",
    diatom_id_list
  )

  rec_tbl <- extract_data(db, sql_records) %>%
    tibble::as_tibble()

  if (nrow(rec_tbl) == 0L) {
    warning("No records contain the selected taxa.")
    return(list(
      metadata = tibble::tibble(),
      diatoms  = list(),
      taxonomy = tibble::tibble()
    ))
  }

  record_ids <- unique(rec_tbl$record_id)

  # ---------------------------------------------------------------------------
  # 5) Delegate to get_data_by_record_id()
  # ---------------------------------------------------------------------------
  res <- get_data_by_record_id(
    db            = db,
    record_ids    = record_ids,
    include_envir = include_envir
  )

  # ---------------------------------------------------------------------------
  # 6) Optionally remove samples with zero counts for all selected taxa
  # ---------------------------------------------------------------------------
  if (!include_zeros && length(res$diatoms) > 0L) {
    # We can only filter on taxa that actually have an original_name
    target_cols <- unique(taxonomy_filt$original_name)
    target_cols <- target_cols[!is.na(target_cols)]

    if (length(target_cols) > 0L) {
      diatom_list <- res$diatoms

      filtered_list <- lapply(diatom_list, function(df) {
        # Columns in this tibble that correspond to selected taxa
        tax_cols <- intersect(colnames(df), target_cols)

        # If none of the selected taxa exist as columns here, keep df as-is
        if (length(tax_cols) == 0L) {
          return(df)
        }

        mat <- df %>%
          dplyr::select(dplyr::all_of(tax_cols)) %>%
          as.matrix()

        # TRUE where at least one selected taxon has a non-zero count
        keep <- rowSums(mat != 0, na.rm = TRUE) > 0

        if (!any(keep)) {
          # No sample with the taxa present in this record:
          # drop this record entirely
          return(NULL)
        }

        df[keep, , drop = FALSE]
      })

      # Drop NULL entries (records with no remaining samples)
      keep_idx <- !vapply(filtered_list, is.null, logical(1L))
      filtered_list <- filtered_list[keep_idx]

      res$diatoms <- filtered_list

      # If we dropped some records entirely, sync metadata to remaining record_ids
      if (length(filtered_list) == 0L) {
        res$metadata <- tibble::tibble()
      } else if (!is.null(res$metadata) && nrow(res$metadata) > 0L) {
        kept_record_ids <- as.integer(names(filtered_list))
        res$metadata <- res$metadata %>%
          dplyr::filter(.data$record_id %in% kept_record_ids)
      }
    }
  }

  res
}


#' Get SEeD data by geographic coordinates
#'
#' @description
#' Convenience wrapper around [get_data_by_metadata()] to subset the SEeD
#' database based on latitude and longitude. You provide a bounding box via
#' minimum/maximum longitude and latitude, and the function returns all
#' records whose site coordinates fall inside that box.
#'
#' Internally, this simply forwards the coordinate bounds to
#' [get_data_by_metadata()] and returns its usual output list:
#' `metadata`, `diatoms`, and `taxonomy`.
#'
#' Any of the bounds can be `NA`, in which case that side of the interval is
#' left open (e.g. `longitude_min = NA` means "no minimum longitude").
#'
#' @param db
#'   A DuckDB connection created by [load_seed_database()].
#'
#' @param longitude_min,longitude_max
#'   Numeric values giving the minimum and maximum longitude of the region of
#'   interest. Either (or both) may be `NA` to leave the interval open.
#'
#' @param latitude_min,latitude_max
#'   Numeric values giving the minimum and maximum latitude of the region of
#'   interest. Either (or both) may be `NA` to leave the interval open.
#'
#' @param include_envir
#'   Logical; if `TRUE`, include environmental variables in the diatom tables
#'   (passed through to [get_data_by_metadata()] / [get_data_by_record_id()]).
#'
#' @return
#' A list as returned by [get_data_by_metadata()], with components:
#'
#' - `metadata`: tibble of metadata rows whose site coordinates fall inside
#'   the requested bounding box.
#' - `diatoms`: diatom data for the matching records.
#' - `taxonomy`: taxonomy table for the taxa present in those records.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'
#'   # South American window
#'   res <- get_data_by_coordinates(
#'     db,
#'     longitude_min = -85,
#'     longitude_max = -30,
#'     latitude_min  = -25,
#'     latitude_max  =  15
#'   )
#' }
get_data_by_coordinates <- function(
  db,
  longitude_min = -85,
  longitude_max = -30,
  latitude_min  = -25,
  latitude_max  =  15,
  include_envir = FALSE
) {
  # Optional: if all bounds are NA, this is equivalent to "no filter"
  if (all(is.na(c(longitude_min, longitude_max, latitude_min, latitude_max)))) {
    warning(
      "All coordinate bounds are NA; no spatial filtering will be applied. ",
      "Consider calling get_data_by_metadata() directly if this is intended."
    )
  }

  get_data_by_metadata(
    db             = db,
    longitude_min  = longitude_min,
    longitude_max  = longitude_max,
    latitude_min   = latitude_min,
    latitude_max   = latitude_max,
    include_envir  = include_envir
  )
}


#' Rename diatom columns using taxonomy
#'
#' @description
#' Given a standard SEeD data object (a list with `metadata`, `diatoms`,
#' and `taxonomy`), this function renames the **diatom columns** in
#' `diatoms` using one of the name fields from the `taxonomy` table
#' (e.g. `accepted_name` or `accepted_abbr`).
#'
#' Internally, the function:
#' - Assumes that diatom columns in `diatoms` are currently labelled by
#'   `original_name`.
#' - Uses `taxonomy$original_name` as the key and the chosen
#'   `taxonomy_col` as the new label.
#' - For cases where multiple `original_name`s map to the same new name
#'   (e.g. synonyms sharing the same `accepted_name`), it **merges those
#'   columns by summing row-wise** and returns a single column with the
#'   new name.
#' - Leaves taxon columns that do not have a valid entry in the chosen
#'   `taxonomy_col` unchanged.
#'
#' This function is designed to operate on objects returned by the SEeD
#' extractor helpers such as [get_data_by_record_id()],
#' [get_data_by_metadata()], etc. It assumes that:
#' - `diatoms` is either:
#'   - a *named list* of wide tibbles (one per `record_id`), or
#'   - a *single* wide tibble;
#' - `taxonomy` contains at least the columns `original_name` and the
#'   chosen `taxonomy_col`.
#'
#' **Important limitation**:
#' Once you have renamed/merged columns using, e.g., `accepted_name`,
#' the mapping is no longer invertible if different `original_name`s
#' converged to the same `accepted_name`. You should therefore apply
#' this function only once per extracted object, and always on data
#' directly obtained from the database (i.e. with taxon columns still
#' labelled by `original_name`).
#'
#' @param seed_obj
#'   A list with components `metadata`, `diatoms`, and `taxonomy` as
#'   returned by SEeD extraction helpers.
#'
#' @param taxonomy_col
#'   Character scalar giving the name of the taxonomy column to use for
#'   renaming (e.g. `"accepted_name"`, `"accepted_abbr"`). Must exist in
#'   `seed_obj$taxonomy`.
#'
#' @return
#' A modified copy of `seed_obj` where:
#' - `metadata` and `taxonomy` are unchanged;
#' - `diatoms` has the same structure (list of tibbles or single tibble),
#'   but diatom columns have been renamed/merged according to
#'   `taxonomy_col`.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'   res <- get_data_by_record_id(db, record_ids = c(1, 2))
#'
#'   # Rename diatom columns to accepted full names
#'   res2 <- rename_diatom_columns(res, taxonomy_col = "accepted_name")
#'
#'   # Or rename to abbreviations
#'   res3 <- rename_diatom_columns(res, taxonomy_col = "accepted_abbr")
#' }
rename_diatom_columns <- function(seed_obj, taxonomy_col = "accepted_name") {
  # ---------------------------------------------------------------------------
  # 0) Basic checks
  # ---------------------------------------------------------------------------
  if (!is.list(seed_obj)) {
    stop("'seed_obj' must be a list with components 'metadata', 'diatoms', and 'taxonomy'.")
  }
  if (!all(c("metadata", "diatoms", "taxonomy") %in% names(seed_obj))) {
    stop("'seed_obj' must contain 'metadata', 'diatoms', and 'taxonomy' components.")
  }

  taxonomy <- seed_obj$taxonomy

  if (!is.data.frame(taxonomy)) {
    stop("'seed_obj$taxonomy' must be a data.frame / tibble.")
  }
  if (!("original_name" %in% names(taxonomy))) {
    stop("'seed_obj$taxonomy' must contain column 'original_name'.")
  }
  if (!(taxonomy_col %in% names(taxonomy))) {
    stop(
      sprintf(
        "'taxonomy_col' = '%s' not found in taxonomy columns: %s",
        taxonomy_col,
        paste(names(taxonomy), collapse = ", ")
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 1) Build mapping original_name -> new_name (from taxonomy_col)
  # ---------------------------------------------------------------------------
  tax_map <- taxonomy %>%
    dplyr::select(
      original_name,
      new_name = !!rlang::sym(taxonomy_col)
    ) %>%
    dplyr::filter(!is.na(.data$new_name), .data$new_name != "") %>%
    dplyr::distinct()

  if (nrow(tax_map) == 0L) {
    warning(
      "No non-NA / non-empty values found in taxonomy column '",
      taxonomy_col,
      "'. Returning 'seed_obj' unchanged."
    )
    return(seed_obj)
  }

  # ---------------------------------------------------------------------------
  # 2) Helper to process a single diatom tibble
  # ---------------------------------------------------------------------------
  rename_one_diat <- function(df) {
    if (!is.data.frame(df)) {
      return(df)
    }

    # ID columns that should never be treated as taxa
    id_candidates <- c("site_id", "record_id", "sample_id", "sample_name", "depth", "age")
    id_cols <- intersect(id_candidates, names(df))
    taxon_cols <- setdiff(names(df), id_cols)

    if (length(taxon_cols) == 0L) {
      return(df)
    }

    # Restrict mapping to taxon columns present in this table
    tax_map_sub <- tax_map %>%
      dplyr::filter(.data$original_name %in% taxon_cols)

    if (nrow(tax_map_sub) == 0L) {
      # Nothing to rename here
      return(df)
    }

    df_out <- df

    # Group original_name by new_name
    groups <- split(tax_map_sub$original_name, tax_map_sub$new_name)

    for (nm in names(groups)) {
      olds <- groups[[nm]]
      # Keep only columns that actually exist in the current table
      olds <- intersect(olds, names(df_out))
      if (length(olds) == 0L) {
        next
      }

      # Existing column with the target new name?
      has_existing_new <- nm %in% names(df_out)

      # Base for the new column
      if (has_existing_new && !(nm %in% olds)) {
        base_vec <- df_out[[nm]]
      } else {
        base_vec <- 0
      }

      # Sum over all "old" columns
      sum_mat <- as.data.frame(df_out[, olds, drop = FALSE])
      summed  <- rowSums(sum_mat, na.rm = TRUE)

      new_col <- base_vec + summed

      # Assign / replace the new_name column
      df_out[[nm]] <- new_col

      # Drop the old columns, except if one of them is the new name itself
      drop_olds <- setdiff(olds, nm)
      if (length(drop_olds) > 0L) {
        df_out <- df_out[, setdiff(names(df_out), drop_olds), drop = FALSE]
      }
    }

    df_out
  }

  # ---------------------------------------------------------------------------
  # 3) Apply to diatoms (list or single tibble)
  # ---------------------------------------------------------------------------
  diatoms <- seed_obj$diatoms

  if (is.data.frame(diatoms)) {
    # Single tibble
    seed_obj$diatoms <- rename_one_diat(diatoms)
  } else if (is.list(diatoms)) {
    # List of tibbles
    seed_obj$diatoms <- lapply(diatoms, rename_one_diat)
  } else {
    warning("'seed_obj$diatoms' is neither a data.frame nor a list; leaving it unchanged.")
  }

  seed_obj
}



#' Export SEeD data object to an Excel workbook
#'
#' @description
#' Export a standard SEeD data object (a list with `metadata`, `diatoms`,
#' and `taxonomy`) to an `.xlsx` file.
#'
#' The workbook contains:
#' - One sheet named `"metadata"` with the metadata tibble.
#' - One sheet named `"taxonomy"` with the taxonomy tibble.
#' - One sheet per diatom table:
#'   - If `diatoms` is a single tibble/data.frame, it is written as a sheet
#'     named `"diatoms"`.
#'   - If `diatoms` is a list of tibbles, each element is written as its own
#'     sheet. The sheet names are taken from the list names. If some elements
#'     are unnamed, synthetic names of the form `"record_1"`, `"record_2"`, …
#'     are used. Sheet names are sanitised to be Excel-compatible and truncated
#'     to 31 characters if needed.
#'
#' This function uses [writexl::write_xlsx()] under the hood.
#'
#' @param seed_obj
#'   A list with components `metadata`, `diatoms`, and `taxonomy`, as returned
#'   by the SEeD extractor helpers.
#'
#' @param path
#'   Character string or [base::connection]-coercible path to the output
#'   `.xlsx` file. Defaults to `"seed_export.xlsx"` in the current working
#'   directory.
#'
#' @return
#' Invisibly returns the path to the created workbook.
#'
#' @examples
#' \dontrun{
#'   db   <- load_seed_database("SEeD_database.duckdb")
#'   res  <- get_data_by_record_id(db, record_ids = c(1, 2))
#'
#'   export_seed_to_xlsx(res, path = "seed_records_1_2.xlsx")
#' }
export_seed_to_xlsx <- function(seed_obj, path = "seed_export.xlsx") {
  # ---------------------------------------------------------------------------
  # 0) Basic checks
  # ---------------------------------------------------------------------------
  if (!is.list(seed_obj)) {
    stop("'seed_obj' must be a list with components 'metadata', 'diatoms', and 'taxonomy'.")
  }
  if (!all(c("metadata", "diatoms", "taxonomy") %in% names(seed_obj))) {
    stop("'seed_obj' must contain 'metadata', 'diatoms', and 'taxonomy' components.")
  }

  # Check that writexl is available
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required for 'export_seed_to_xlsx()'. Please install it.")
  }

  metadata <- seed_obj$metadata
  diatoms  <- seed_obj$diatoms
  taxonomy <- seed_obj$taxonomy

  if (!is.data.frame(metadata)) {
    stop("'seed_obj$metadata' must be a data.frame / tibble.")
  }
  if (!is.data.frame(taxonomy)) {
    stop("'seed_obj$taxonomy' must be a data.frame / tibble.")
  }

  # ---------------------------------------------------------------------------
  # 1) Build the list of sheets
  # ---------------------------------------------------------------------------
  sheets <- list(
    metadata = metadata,
    taxonomy = taxonomy
  )

  # Helper to sanitise Excel sheet names:
  # - replace invalid characters \ / ? * [ ] : with "_"
  # - trim to 31 characters
  # - avoid empty names
  sanitize_sheet_name <- function(x) {
    x <- as.character(x)
    x <- gsub("[\\\\/\\?\\*\\[\\]:]", "_", x)
    x <- trimws(x)
    x[nchar(x) == 0L] <- "sheet"
    substr(x, 1L, 31L)
  }

  if (is.data.frame(diatoms)) {
    # Single tibble
    sheets[["diatoms"]] <- diatoms
  } else if (is.list(diatoms)) {
    diatom_list <- diatoms

    # Ensure names exist
    if (is.null(names(diatom_list)) || any(names(diatom_list) == "")) {
      default_names <- paste0("record_", seq_along(diatom_list))
      if (is.null(names(diatom_list))) {
        names(diatom_list) <- default_names
      } else {
        idx_empty <- which(names(diatom_list) == "")
        names(diatom_list)[idx_empty] <- default_names[idx_empty]
      }
    }

    # Sanitise sheet names for diatoms
    diatom_sheet_names <- sanitize_sheet_name(names(diatom_list))
    # Ensure uniqueness (Excel requires unique sheet names)
    diatom_sheet_names <- make.unique(diatom_sheet_names)

    for (i in seq_along(diatom_list)) {
      nm <- diatom_sheet_names[i]
      df <- diatom_list[[i]]
      if (!is.data.frame(df)) {
        next
      }
      sheets[[nm]] <- df
    }
  } else {
    warning("'seed_obj$diatoms' is neither a data.frame nor a list; no diatom sheets will be written.")
  }

  # Final sanitisation + uniqueness for all sheet names
  all_names <- names(sheets)
  all_names <- sanitize_sheet_name(all_names)
  all_names <- make.unique(all_names)
  names(sheets) <- all_names

  # ---------------------------------------------------------------------------
  # 2) Write workbook
  # ---------------------------------------------------------------------------
  writexl::write_xlsx(sheets, path = path)

  invisible(path)
}


#' Get the spatial distribution of one or more taxa
#'
#' @description
#' This function returns the geographic coordinates of sites where one or more
#' selected taxa have been observed in the SEeD database.
#'
#' Taxa can be searched using any combination of the three naming conventions:
#'
#' - `original_name`
#' - `accepted_genus`
#' - `accepted_name`
#'
#' Matching can be exact or partial (case-insensitive), following the same
#' logic as [get_data_by_taxa()].
#'
#' By default, only non-paleo observations are returned. If
#' `include_paleo = TRUE`, paleo observations are also included.
#'
#' The returned object is a tibble with two columns:
#'
#' - `Lon`
#' - `lat`
#'
#' Each row corresponds to one unique coordinate pair where the selected taxon
#' (or taxa) was found.
#'
#' @param db
#'   A DuckDB connection object, as returned by [load_seed_database()].
#'
#' @param original_name
#'   Character vector of values to match against `taxonomy.original_name`.
#'   Default `NA` (not used).
#'
#' @param accepted_genus
#'   Character vector of values to match against `taxonomy.accepted_genus`.
#'   Default `NA` (not used).
#'
#' @param accepted_name
#'   Character vector of values to match against `taxonomy.accepted_name`.
#'   Default `NA` (not used).
#'
#' @param exact
#'   Logical, default `TRUE`. If `TRUE`, matching is case-insensitive exact
#'   equality. If `FALSE`, matching is case-insensitive and any provided
#'   pattern may appear anywhere in the field (substring match).
#'
#' @param include_paleo
#'   Logical, default `FALSE`. If `FALSE`, records with
#'   `sampletype == "paleo"` are excluded. If `TRUE`, paleo records are also
#'   included.
#'
#' @return
#' A tibble with two columns:
#'
#' - `Lon`
#' - `lat`
#'
#' containing the unique coordinates where the selected taxon (or taxa) was
#' found.
#'
#' If no taxa match the requested filters, or if no matching observations are
#' found, an empty tibble with columns `Lon` and `lat` is returned.
#'
#' @examples
#' \dontrun{
#'   db <- load_seed_database("SEeD_database.xlsx")
#'
#'   # Exact match on accepted_name, excluding paleo sites
#'   get_distribution(
#'     db,
#'     accepted_name = "Achnanthidium minutissimum"
#'   )
#'
#'   # Fuzzy match on genus, including paleo observations
#'   get_distribution(
#'     db,
#'     accepted_genus = "Sellaphora",
#'     exact = FALSE,
#'     include_paleo = TRUE
#'   )
#' }
get_distribution <- function(
  db,
  original_name  = NA,
  accepted_genus = NA,
  accepted_name  = NA,
  exact          = TRUE,
  include_paleo  = FALSE
) {
  # ---------------------------------------------------------------------------
  # 0) Argument checks
  # ---------------------------------------------------------------------------
  orig_vals <- original_name[!is.na(original_name)]
  gen_vals  <- accepted_genus[!is.na(accepted_genus)]
  acc_vals  <- accepted_name[!is.na(accepted_name)]

  if (length(orig_vals) == 0L &&
      length(gen_vals)  == 0L &&
      length(acc_vals)  == 0L) {
    stop(
      "At least one of 'original_name', 'accepted_genus', or 'accepted_name' ",
      "must be provided (non-NA)."
    )
  }

  # ---------------------------------------------------------------------------
  # 1) Load taxonomy (minimal columns)
  # ---------------------------------------------------------------------------
  taxonomy_all <- extract_data(
    db,
    "
    SELECT
      diatom_id,
      original_name,
      accepted_genus,
      accepted_name
    FROM taxonomy
    "
  ) %>%
    tibble::as_tibble()

  if (nrow(taxonomy_all) == 0L) {
    warning("Taxonomy table is empty.")
    return(tibble::tibble(Lon = numeric(), lat = numeric()))
  }

  # ---------------------------------------------------------------------------
  # 2) Helper for character filters (exact / partial, case-insensitive)
  # ---------------------------------------------------------------------------
  apply_char_filter <- function(tbl, col_name, values) {
    values <- values[!is.na(values)]
    if (length(values) == 0L) {
      return(tbl)
    }

    col_sym <- rlang::sym(col_name)

    if (exact) {
      vals_l <- tolower(values)
      tbl %>%
        dplyr::filter(
          !is.na(!!col_sym),
          tolower(!!col_sym) %in% vals_l
        )
    } else {
      vals_l <- tolower(values)
      tbl %>%
        dplyr::filter(
          !is.na(!!col_sym),
          purrr::map_lgl(
            !!col_sym,
            function(x) {
              xx <- tolower(x)
              any(vapply(
                vals_l,
                function(v) grepl(v, xx, fixed = TRUE),
                logical(1)
              ))
            }
          )
        )
    }
  }

  # ---------------------------------------------------------------------------
  # 3) Apply taxonomy filters
  # ---------------------------------------------------------------------------
  taxonomy_filt <- taxonomy_all %>%
    apply_char_filter("original_name",  orig_vals) %>%
    apply_char_filter("accepted_genus", gen_vals)  %>%
    apply_char_filter("accepted_name",  acc_vals)

  if (nrow(taxonomy_filt) == 0L) {
    warning("No taxa match the requested name filters.")
    return(tibble::tibble(Lon = numeric(), lat = numeric()))
  }

  diatom_ids <- unique(taxonomy_filt$diatom_id)
  diatom_id_list <- paste(diatom_ids, collapse = ", ")

  # ---------------------------------------------------------------------------
  # 4) Query unique coordinates where the taxon is present
  # ---------------------------------------------------------------------------
  paleo_clause <- if (include_paleo) {
    ""
  } else {
    "AND (r.sampletype IS NULL OR LOWER(r.sampletype) != 'paleo')"
  }

  sql_dist <- sprintf(
    "
    SELECT DISTINCT
      s.longitude AS long,
      s.latitude  AS lat,
      s.name AS name,
      s.country AS country,
      r.sampletype AS sampletype
    FROM diatoms d
    JOIN samples sa
      ON sa.sample_id = d.sample_id
    JOIN records r
      ON r.record_id = sa.record_id
    JOIN sites s
      ON s.site_id = r.site_id
    WHERE d.diatom_id IN (%s)
      AND d.value > 0
      %s
      AND s.longitude IS NOT NULL
      AND s.latitude  IS NOT NULL
    ORDER BY long, lat
    ",
    diatom_id_list,
    paleo_clause
  )

  dist_tbl <- extract_data(db, sql_dist) %>%
    tibble::as_tibble()

  if (nrow(dist_tbl) == 0L) {
    warning("No observations found for the selected taxa.")
    return(tibble::tibble(Lon = numeric(), lat = numeric()))
  }

  dist_tbl
}
