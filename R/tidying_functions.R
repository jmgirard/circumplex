#' Ipsatize circumplex items using deviation scoring across variables
#'
#' Rescore each circumplex item using deviation scoring across variables. In
#' other words, subtract each observation's mean response from each response.
#' This effectively removes the presence of a general factor, which can make
#' certain circumplex fit analyses more powerful.
#'
#' @param data Required. A data frame or matrix containing at least circumplex
#'   scales.
#' @param items Required. A character vector containing the column names, or a
#'   numeric vector containing column indexes, of item variables in `data` to be
#'   ipsatized.
#' @param na.rm Optional. A logical that determines whether missing values
#'   should be ignored during the calculation of the mean during ipsatization
#'   (default = TRUE).
#' @param prefix Optional. A string that will be added to the start of each
#'   `items` name in the output (default = "").
#' @param suffix Optional. A string that will be added to the end of each
#'   `items` name in the output (default = "_i").
#' @param append Optional. A logical that determines whether to append the
#'   ipsatized scores to `data` in the output or just return the ipsatized
#'   scores alone (default = TRUE).
#' @return A data frame that matches `data` except that the variables specified
#'   in `items` have been rescored using ipsatization.
#' @family tidying functions
#' @export
#' @examples
#' data("raw_iipsc")
#' ipsatize(raw_iipsc, items = 1:32)
#' ipsatize(raw_iipsc, items = sprintf("IIP%02d", 1:32))
ipsatize <- function(data, items, na.rm = TRUE, 
                     prefix = "", suffix = "_i", append = TRUE) {
  
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(items))
  stopifnot(is_flag(na.rm))
  stopifnot(is_char(prefix, n = 1))
  stopifnot(is_char(suffix, n = 1))
  stopifnot(is_flag(append))

  if (is.matrix(data)) data <- as.data.frame(data)
  item_data <- data[items]
  item_names <- colnames(item_data)
  rmean <- rowMeans(item_data, na.rm = na.rm)
  scores <- sapply(item_data, function(x) x - rmean)
  colnames(scores) <- paste0(prefix, item_names, suffix)
  
  if (append) {
    cbind(data, scores)
  } else {
    as.data.frame(scores)
  }
}

#' Score circumplex scales from item responses
#'
#' Calculate mean scores on circumplex scales from item responses by using a set
#' of scoring instructions, which may be loaded from the package or created as a
#' custom data frame.
#'
#' @param data Required. A data frame or matrix containing at least
#'   circumplex scales.
#' @param items Required. The variable names or column numbers for the variables
#'   in \code{.data} that contain all the circumplex items from a single
#'   circumplex measure, in ascending order from item 1 to item N.
#' @param instrument Required. An instrument object from the package. To see the
#'   available circumplex instruments, use \code{instruments()}.
#' @param na.rm Optional. A logical that determines if missing values should be
#'   omitted from the calculation of scores (default = TRUE). When set to TRUE,
#'   scales with missing data are essentially calculated with mean imputation.
#' @param prefix Optional. A string to include at the beginning of the newly
#'   calculated scale variables' names, before \code{Abbrev} from \code{key} and
#'   \code{suffix} (default = "").
#' @param suffix Optional. A string to include at the end of the newly
#'   calculated scale variables' names, after \code{Abbrev} from \code{key} and
#'   \code{prefix} (default = "").
#' @param append Optional. A logical that determines whether the calculated
#'   score variables will be appended to `data` or returned on their own
#'   (default = TRUE).
#' @return A data frame that matches \code{.data} except that new variables are
#'   appended that contain mean scores on each variable included in \code{key}.
#' @family tidying functions
#' @export
#' @examples
#' data("raw_iipsc")
#' score(raw_iipsc, items = 1:32, instrument = iipsc, prefix = "IIPSC_")
score <- function(data, items, instrument, na.rm = TRUE, 
                  prefix = "", suffix = "", append = TRUE) {
  
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(items))
  stopifnot(inherits(instrument, "circumplex_instrument"))
  stopifnot(is_flag(na.rm))
  stopifnot(is_char(prefix))
  stopifnot(is_char(suffix))
  stopifnot(is_flag(append))

  if (is.matrix(data)) data <- as.data.frame(data)
  item_data <- data[items]
  n_items <- length(items)
  key <- instrument$Scales

  scores <- matrix(NA, nrow = nrow(item_data), ncol = nrow(key))
  colnames(scores) <- paste0(prefix, key$Abbrev, suffix)
  
  for (i in 1:nrow(key)) {
    item_nums <- as.numeric(strsplit(key$Items[[i]], ",")[[1]])
    
    if (max(item_nums) > n_items) {
      stop("Key is asking for more items than were provided to function.")
    }
    
    scores[, i] <- rowMeans(item_data[item_nums], na.rm)
  }
  scores[is.nan(scores)] <- NA_real_
  
  if (append) {
    cbind(data, scores)
  } else {
    as.data.frame(scores)
  }
}

# Would norm_standardize() accept this sample of this instrument? Shares the
# anchor-range predicate with the refusal below rather than restating it, so
# the disclosure's count of usable alternatives cannot drift from what the
# refusal actually does.
norm_sample_usable <- function(instrument, sample) {
  key <- instrument$Norms[[1]]
  key <- key[which(key$Sample == sample), ]
  if (nrow(key) == 0) return(FALSE)
  anchors <- instrument$Anchors
  if (is.null(anchors)) return(TRUE)
  all(
    key$M >= min(anchors$Value) & key$M <= max(anchors$Value),
    na.rm = TRUE
  )
}

#' Standardize circumplex scales using normative data
#'
#' Take in a data frame containing circumplex scales, angle definitions for each
#' scale, and an instrument whose normative data will be used, and return that
#' same data frame with each specified circumplex scale transformed into
#' standard scores (i.e., z-scores) based on comparison to that instrument's
#' normative sample.
#'
#' The sample the scores are compared against is a result-determining choice,
#' not a technicality: different samples of the same instrument can move a
#' respondent's z-scores by more than half a standard deviation. So unless
#' `quiet = TRUE`, every successful call reports which sample it used, how
#' large that sample was, and how it is described, and every call attaches the
#' same facts to the result (see the Value section below). Use `norms()` to see
#' the samples an instrument carries before choosing one.
#'
#' @param data Required. A data frame or matrix containing at least circumplex
#'   scales.
#' @param scales Required. A character vector containing the column names, or a
#'   numeric vector containing the column indexes, for the variables (scale
#'   scores) to be standardized.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in `scales` (in degrees). Can use the
#'   `octants()`, `poles()`, or `quadrants()` convenience functions. Each angle
#'   is matched to the instrument's normative data by angular position, so 0
#'   and 360 degrees are treated as the same angle; an angle with no matching
#'   normative row (or with more than one) produces an informative error.
#' @param instrument Required. An instrument object from the package. To see the
#'   available circumplex instruments, see `instruments()`.
#' @param sample Required. An integer corresponding to the normative sample to
#'   use in standardizing the scale scores (default = 1). See `?norms` to
#'   see the normative samples available for an instrument. Two conditions are
#'   refused with an error rather than used: a `sample` the instrument does not
#'   carry (the error lists the sample numbers it does), and a sample whose
#'   mean scores fall outside the instrument's own response range, which cannot
#'   be on the same metric as the scores being standardized. `norms()` lists
#'   the alternatives in both cases.
#' @param prefix Optional. A string to include at the beginning of the newly
#'   calculated scale variables' names, before the scale name and `suffix`
#'   (default = "").
#' @param suffix Optional. A string to include at the end of the newly
#'   calculated scale variables' names, after the scale name and `prefix`
#'   (default = "_z").
#' @param append Optional. A logical that determines whether the calculated
#'   standardized scores should be added as columns to `data` in the output or
#'   the standardized scores alone should be output (default = TRUE).
#' @param quiet Optional. A logical that suppresses the message naming the
#'   normative sample used (default = FALSE). Set to `TRUE` in loops, knitted
#'   documents, and anywhere else the message is noise; the returned attribute
#'   below records the same facts either way.
#' @return A data frame that contains the norm-standardized versions of
#'   `scales`. It carries a `"norm_sample"` attribute -- a list with elements
#'   `Instrument`, `Sample`, `Size`, `Population` and `Kind` -- recording which
#'   normative sample produced the scores and what kind of reference
#'   distribution it is (see [norms()] for the three kinds), so a script that
#'   never sees the console can still report what its z-scores are relative to.
#'   Retrieve it with `attr(x, "norm_sample")`.
#' @export
#' @family tidying functions
#' @examples
#' data("jz2017")
#' norm_standardize(jz2017, scales = 2:9, instrument = iipsc, sample = 1)
#'
#' # The IIP-SC carries more than one normative sample. Omitting `sample` takes
#' # the first, and the message says which one that was.
#' z <- norm_standardize(jz2017, scales = 2:9, instrument = iipsc)
#' attr(z, "norm_sample")
norm_standardize <- function(data, scales, angles = octants(), instrument,
                       sample = 1, prefix = "", suffix = "_z", append = TRUE,
                       quiet = FALSE) {

  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))
  stopifnot(inherits(instrument, "circumplex_instrument"))
  stopifnot(is_num(sample, n = 1))
  stopifnot(is_char(prefix, n = 1))
  stopifnot(is_char(suffix, n = 1))
  stopifnot(is_flag(append))
  stopifnot(is_flag(quiet))

  if (is.matrix(data)) data <- as.data.frame(data)
  key <- instrument$Norms[[1]]
  # which() rather than a bare logical index: `key$Sample == NA` is NA rather
  # than FALSE, and indexing a data frame by NA returns a row of NAs instead of
  # no rows -- so an NA `sample` (or an NA in the column) would survive the
  # zero-row guard below and fail later, somewhere that names neither the
  # argument nor the mistake. which() drops NAs from either side.
  key <- key[which(key$Sample == sample), ]

  # An unmatched `sample` used to fall through to the arity check below, whose
  # message names neither the argument at fault nor what would have been
  # valid -- so the one mistake the argument invites was reported as a
  # mismatch between `scales` and the norms.
  if (nrow(key) == 0) {
    available <- sort(unique(instrument$Norms[[1]]$Sample))
    stop(
      "No normative data for sample ", sample, ". The ",
      instrument$Details$Abbrev, " carries sample",
      if (length(available) > 1) "s " else " ",
      paste(available, collapse = ", "),
      "; see norms() for what each one is.",
      call. = FALSE
    )
  }

  stopifnot(length(scales) == nrow(key))

  # A normative sample whose means fall outside the instrument's own response
  # range cannot be on the same metric as the scores being standardized, so the
  # z-scores it produces would carry an undefined unit. There is no reading
  # under which those numbers are right, so refuse rather than return them.
  anchors <- instrument$Anchors
  if (!is.null(anchors) && nrow(key) > 0) {
    lo <- min(anchors$Value)
    hi <- max(anchors$Value)
    outside <- which(key$M < lo | key$M > hi)
    if (length(outside) > 0) {
      # The norms label their scale column `Scale` on some instruments and
      # `Abbrev` on others; reading `Scale` unconditionally printed a bare NA
      # for the seven that use the other name.
      labels <- if ("Scale" %in% names(key)) key$Scale else key$Abbrev
      stop(
        "The ", instrument$Details$Abbrev, " normative sample ", sample,
        " cannot be used for standardization. Its mean score for ",
        paste(labels[outside], collapse = ", "),
        " falls outside the instrument's ", lo, " to ", hi,
        " response range, so this sample is not on the same metric as the ",
        "scores being standardized. Use norms() to see the other samples ",
        "available for this instrument.",
        call. = FALSE
      )
    }
  }
  
  scale_data <- data[scales]
  scale_names <- colnames(scale_data)
  
  scores <- matrix(NA, nrow = nrow(scale_data), ncol = length(scales))
  colnames(scores) <- paste0(prefix, scale_names, suffix)

  angles <- as.numeric(angles)
  for (i in seq_along(scales)) {
    # Match each scale to its norm row by circular angular distance, so that
    # 0 and 360 degrees (the same angle) match and float noise is tolerated
    dist_i <- abs(angles[[i]] - key$Angle) %% 360
    dist_i <- pmin(dist_i, 360 - dist_i)
    index_i <- which(dist_i < 1e-6)
    if (length(index_i) == 0) {
      stop(
        "No normative data for a scale at ", angles[[i]], " degrees. ",
        "Available angles: ", paste(sort(unique(key$Angle)), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    if (length(index_i) > 1) {
      stop(
        "Multiple normative rows match ", angles[[i]], " degrees; the ",
        "instrument's normative data has duplicate angles.",
        call. = FALSE
      )
    }
    m_i <- key$M[index_i]
    s_i <- key$SD[index_i]
    scores[, i] <- (scale_data[[i]] - m_i) / s_i
  }
  scores[is.nan(scores)] <- NA_real_

  # Which sample the scores are relative to is a result-determining input --
  # the choice moves scores far more than the sampling error of any one
  # sample's moments -- so it is disclosed at the call site rather than left
  # in an argument the caller may have defaulted. The read is keyed on Sample
  # rather than taken by row position: nothing requires Norms[[2]] to be
  # stored in Sample order, and a positional read would silently report a
  # different sample's size and description than the one used.
  info <- instrument$Norms[[2]]
  info <- info[info$Sample == sample, ]
  disclosure <- list(
    Instrument = instrument$Details$Abbrev,
    Sample = sample,
    Size = info$Size[[1]],
    Population = info$Population[[1]],
    Kind = info$Kind[[1]]
  )

  if (!quiet) {
    # The stored label names the group the sample was drawn from, so it is
    # printed as a plain description; framing it as a population would make
    # the package assert a representativeness none of these samples claims.
    #
    # The count offers the reader an alternative they can act on, so it counts
    # only samples this function would accept -- not rows of Norms[[2]]. A
    # sample whose means leave the response range is refused, and advertising
    # it would point at a call that errors.
    n_other <- sum(vapply(
      setdiff(instrument$Norms[[2]]$Sample, sample),
      function(other) norm_sample_usable(instrument, other),
      logical(1)
    ))
    # The kind travels with the description rather than replacing it: the
    # description says who these people were, the kind says what their
    # statistics are good for. `?norms` defines the three; the message names
    # one, because the words that would explain the difference here are the
    # ones this message may not use.
    msg <- paste0(
      "Standardized against ", disclosure$Instrument, " normative sample ",
      sample, ": N = ", disclosure$Size, ", ", disclosure$Population,
      ". Reference kind: ", norm_kind_phrase(disclosure$Kind), "."
    )
    if (n_other > 0) {
      msg <- paste0(
        msg, " ", n_other, " other sample", if (n_other > 1L) "s are" else " is",
        " available; see norms()."
      )
    }
    message(msg)
  }

  out <- if (append) {
    cbind(data, scores)
  } else {
    as.data.frame(scores)
  }
  attr(out, "norm_sample") <- disclosure
  out
}

#' Standardize circumplex scales using sample data
#'
#' Take in a data frame containing circumplex scales (or items) and return that
#' same data frame with each specified variable transformed into standard scores
#' (i.e., z-scores) based on observed means and SDs.
#'
#' @param data Required. A data frame or matrix containing at least circumplex
#'   scales.
#' @param scales Required. A character vector containing the column names, or a
#'   numeric vector containing the column indexes, for the variables (scale
#'   scores) to be standardized.
#' @param na.rm Optional. A logical that determines whether to remove missing
#'   values from scales when calculating the means and SDs used for
#'   standardization (default = TRUE).
#' @param prefix Optional. A string to include at the beginning of the newly
#'   calculated scale variables' names, before the scale name and `suffix`
#'   (default = "").
#' @param suffix Optional. A string to include at the end of the newly
#'   calculated scale variables' names, after the scale name and `prefix`
#'   (default = "_z").
#' @param append Optional. A logical that determines whether the calculated
#'   standardized scores should be added as columns to `data` in the output or
#'   the standardized scores alone should be output (default = TRUE).
#' @return A data frame that contains the self-standardized versions of
#'   `scales`.
#' @export
#' @family tidying functions
#' @examples
#' self_standardize(aw2009, scales = 1:8)
self_standardize <- function(data, scales, na.rm = TRUE,
                             prefix = "", suffix = "_z", append = TRUE) {
  
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(scales))
  stopifnot(is_flag(na.rm))
  stopifnot(is_char(prefix, n = 1))
  stopifnot(is_char(suffix, n = 1))
  stopifnot(is_flag(append))

  if (is.matrix(data)) data <- as.data.frame(data)
  scale_data <- data[scales]
  scale_names <- colnames(scale_data)

  zscore <- function(x, na.rm = na.rm) {
    (x - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm = na.rm)
  }
  
  scores <- sapply(scale_data, FUN = zscore, na.rm = na.rm)
  colnames(scores) <- paste0(prefix, scale_names, suffix)
  scores[is.nan(scores)] <- NA_real_
  
  if (append) {
    cbind(data, scores)
  } else {
    as.data.frame(scores) 
  }
}
