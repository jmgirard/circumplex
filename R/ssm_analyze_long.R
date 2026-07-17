#' Perform SSM analyses on long-format repeated-measures data
#'
#' A convenience wrapper around the `occasions` interface of [ssm_analyze()]
#' for data stored in long format (one row per person per occasion). It
#' reshapes the data into the wide, one-row-per-person layout that
#' [ssm_analyze()] consumes and then delegates to it; all estimation is
#' performed by [ssm_analyze()] unchanged. See the `occasions` argument of
#' [ssm_analyze()] for the analysis semantics -- per-occasion profiles, paired
#' within-person contrasts, and the listwise-only handling of missing waves
#' (a person missing any occasion is dropped from all occasions).
#'
#' @param data Required. A data frame (or matrix) in long format containing an
#'   identifier column, an occasion column, and the circumplex scale scores
#'   (one set of score columns, repeated across occasions in different rows).
#' @param scales Required. A character vector of column names, or a numeric
#'   vector of column indexes, giving the circumplex scale scores in `data`
#'   (the same scales measured at every occasion).
#' @param angles Optional. A numeric vector containing the angular displacement
#'   of each circumplex scale included in `scales` (in degrees) (default =
#'   `octants()`).
#' @param id Required. A single column name or index identifying the person
#'   that each row belongs to.
#' @param occasion Required. A single column name or index identifying the
#'   occasion (wave) that each row belongs to. Occasion order -- which governs
#'   the second-minus-first direction of a paired contrast -- is taken from the
#'   factor levels of this column when it is a factor, and otherwise from the
#'   order in which the occasions first appear in `data`. It is never sorted
#'   alphabetically, so a `T10`/`T2` pair keeps its supplied order.
#' @param grouping Optional. A single column name or index giving a
#'   time-invariant grouping variable (one value per person; an error is raised
#'   if a person's grouping value varies across occasions).
#' @param contrast Optional. A logical value; if `TRUE` (and the data contain
#'   exactly two occasions in a single group), the paired within-person
#'   contrast (second occasion minus first) is calculated (default = FALSE).
#' @param boots,interval,parallel,ncpus,method Optional. Passed through to
#'   [ssm_analyze()]; see its documentation.
#'
#' @return A list containing the results and description of the analysis, as
#'   returned by [ssm_analyze()] (with an `Occasion` column). See
#'   [ssm_analyze()].
#'
#' @family ssm functions
#' @family analysis functions
#' @seealso [ssm_analyze()] for the wide-format interface and the analysis
#'   semantics this wrapper delegates to.
#' @export
#' @examples
#' # Reshape the built-in wide example to long, then analyze it directly.
#' data("aw2009")
#' wide <- aw2009
#' wide$id <- seq_len(nrow(wide))
#' long <- stats::reshape(
#'   wide,
#'   direction = "long",
#'   varying = list(names(aw2009)),
#'   v.names = "score",
#'   times = names(aw2009),
#'   timevar = "scale",
#'   idvar = "id"
#' )
#' # (In practice `data` already stores repeated occasions in long form.)
ssm_analyze_long <- function(data, scales, angles = octants(),
                             id, occasion, grouping = NULL, contrast = FALSE,
                             boots = 2000, interval = 0.95,
                             parallel = "no", ncpus = 1,
                             method = "bootstrap") {

  # Validate arguments
  stopifnot(is.data.frame(data) || is.matrix(data))
  if (is.matrix(data)) data <- as.data.frame(data)
  stopifnot(is.numeric(angles))
  stopifnot(is_var(scales))
  stopifnot(length(scales) == length(angles))
  stopifnot(is_var(id, n = 1))
  stopifnot(is_var(occasion, n = 1))
  stopifnot(is_null_or_var(grouping, n = 1))
  stopifnot(is_flag(contrast))

  # Resolve column references to names for the wide layout, but keep the scale
  # *positions* for extracting scores (never resolve numeric indices to names
  # before subsetting: `[.data.frame` collapses duplicate names onto the first).
  nm <- names(data)
  resolve <- function(v) if (is.numeric(v)) nm[v] else v
  scale_names <- resolve(scales)
  scale_pos <- if (is.numeric(scales)) as.integer(scales) else match(scale_names, nm)
  id_name <- resolve(id)
  occ_name <- resolve(occasion)
  grp_name <- if (is.null(grouping)) NULL else resolve(grouping)
  refs <- c(scale_names, id_name, occ_name, grp_name)
  if (anyNA(refs) || !all(refs %in% nm)) {
    stop(
      "`scales`, `id`, `occasion`, and `grouping` must all name columns in ",
      "`data`.",
      call. = FALSE
    )
  }

  # Drop rows with missing id or occasion (they cannot be placed in the grid).
  drop_row <- is.na(data[[id_name]]) | is.na(data[[occ_name]])
  if (any(drop_row)) {
    message(
      sum(drop_row),
      " row(s) removed due to missing `id` or `occasion` values."
    )
    data <- data[!drop_row, , drop = FALSE]
  }
  if (nrow(data) == 0) {
    stop("No rows remain after removing missing `id`/`occasion` values.",
         call. = FALSE)
  }

  # Occasion order: factor levels (if a factor) or first appearance, never
  # sorted -- so a T10/T2 pair keeps the user's temporal order (spec sec. 1.2).
  occ_raw <- data[[occ_name]]
  occ_chr <- as.character(occ_raw)
  if (is.factor(occ_raw)) {
    occ_levels <- levels(droplevels(occ_raw))
  } else {
    occ_levels <- unique(occ_chr)
  }
  if (length(occ_levels) < 2) {
    stop("`occasion` must distinguish at least two occasions.", call. = FALSE)
  }

  ids <- unique(data[[id_name]])
  id_index <- match(data[[id_name]], ids)
  occ_index <- match(occ_chr, occ_levels)

  # Duplicate (id, occasion) rows would make the wide layout ambiguous.
  key <- paste(id_index, occ_index, sep = "\r")
  if (anyDuplicated(key)) {
    d <- which(duplicated(key))[1]
    stop(
      "Duplicate rows for the same person and occasion (id `", ids[id_index[d]],
      "` at occasion `", occ_levels[occ_index[d]], "`): each person must ",
      "appear at most once per occasion.",
      call. = FALSE
    )
  }

  # Grouping must be time-invariant (one value per person).
  if (!is.null(grp_name)) {
    g_split <- split(data[[grp_name]], id_index)
    varying <- vapply(
      g_split,
      function(g) length(unique(g[!is.na(g)])) > 1,
      logical(1)
    )
    if (any(varying)) {
      bad_ids <- ids[as.integer(names(varying)[varying])]
      stop(
        "`grouping` must be time-invariant: person(s) `",
        paste(bad_ids, collapse = "`, `"),
        "` have more than one grouping value across occasions.",
        call. = FALSE
      )
    }
  }

  # Reshape to wide: one row per person, one occasion-suffixed score block per
  # occasion, so ssm_analyze()'s `occasions` list can select them by name.
  scores <- as.matrix(data[, scale_pos, drop = FALSE])
  occasions <- vector("list", length(occ_levels))
  names(occasions) <- occ_levels
  wide <- data.frame(row.names = seq_along(ids))
  for (k in seq_along(occ_levels)) {
    block_cols <- paste0(scale_names, "__", occ_levels[k])
    rows_k <- which(occ_index == k)
    block <- matrix(
      NA_real_, nrow = length(ids), ncol = length(scale_names),
      dimnames = list(NULL, block_cols)
    )
    block[id_index[rows_k], ] <- scores[rows_k, , drop = FALSE]
    wide[block_cols] <- as.data.frame(block)
    occasions[[k]] <- block_cols
  }
  if (!is.null(grp_name)) {
    wide[[grp_name]] <- data[[grp_name]][match(ids, data[[id_name]])]
  }

  # Delegate to the validated wide-format occasions path. Estimation, missing-
  # wave (listwise) handling, contrasts, and CIs are all ssm_analyze()'s.
  ssm_analyze(
    data = wide, occasions = occasions, angles = angles,
    grouping = grp_name, contrast = contrast,
    boots = boots, interval = interval,
    parallel = parallel, ncpus = ncpus, method = method
  )
}
