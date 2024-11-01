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
#' @param data Required. A data frame containing at least circumplex scales.
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
#' instrument("iipsc")
#' score(raw_iipsc, items = 1:32, instrument = iipsc, prefix = "IIPSC_")
score <- function(data, items, instrument, na.rm = TRUE, 
                  prefix = "", suffix = "", append = TRUE) {
  
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(items))
  stopifnot(class(instrument) == "circumplex_instrument")
  stopifnot(is_flag(na.rm))
  stopifnot(is_char(prefix))
  stopifnot(is_char(suffix))
  stopifnot(is_flag(append))
  
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

#' Standardize circumplex scales using normative data
#'
#' Take in a data frame containing circumplex scales, angle definitions for each
#' scale, and normative data (from the package or custom) and return that same
#' data frame with each specified circumplex scale transformed into standard
#' scores (i.e., z-scores) based on comparison to the normative data.
#'
#' @param data Required. A data frame or matrix containing at least circumplex
#'   scales.
#' @param scales Required. A character vector containing the column names, or a
#'   numeric vector containing the column indexes, for the variables (scale
#'   scores) to be standardized.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in `scales` (in degrees). Can use the
#'   `octants()`, `poles()`, or `quadrants()` convenience functions.
#' @param instrument Required. An instrument object from the package. To see the
#'   available circumplex instruments, see `instruments()`.
#' @param sample Required. An integer corresponding to the normative sample to
#'   use in standardizing the scale scores (default = 1). See `?norms` to
#'   see the normative samples available for an instrument.
#' @param prefix Optional. A string to include at the beginning of the newly
#'   calculated scale variables' names, before the scale name and `suffix`
#'   (default = "").
#' @param suffix Optional. A string to include at the end of the newly
#'   calculated scale variables' names, after the scale name and `prefix`
#'   (default = "_z").
#' @param append Optional. A logical that determines whether the calculated
#'   standardized scores should be added as columns to `data` in the output or
#'   the standardized scores alone should be output (default = TRUE).
#' @return A data frame that contains the norm-standardized versions of `scales`.
#' @export
#' @family tidying functions
#' @examples
#' data("jz2017")
#' instrument("iipsc")
#' norm_standardize(jz2017, scales = 2:9, instrument = iipsc, sample = 1)
norm_standardize <- function(data, scales, angles = octants(), instrument, 
                       sample = 1, prefix = "", suffix = "_z", append = TRUE) {
  
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))
  stopifnot(class(instrument) == "circumplex_instrument")
  stopifnot(is_num(sample, n = 1))
  stopifnot(is_char(prefix, n = 1))
  stopifnot(is_char(suffix, n = 1))
  stopifnot(is_flag(append))
  
  
  key <- instrument$Norms[[1]]
  key <- key[key$Sample == sample, ]
  
  stopifnot(length(scales) == nrow(key))
  
  scale_data <- data[scales]
  scale_names <- colnames(scale_data)
  
  scores <- matrix(NA, nrow = nrow(scale_data), ncol = length(scales))
  colnames(scores) <- paste0(prefix, scale_names, suffix)
  
  for (i in 1:length(scales)) {
    index_i <- key$Angle == angles[[i]]
    m_i <- key$M[index_i]
    s_i <- key$SD[index_i]
    scores[, i] <- (scale_data[[i]] - m_i) / s_i
  }
  scores[is.nan(scores)] <- NA_real_
  
  if (append) {
    cbind(data, scores)
  } else {
    as.data.frame(scores) 
  }
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
  
  scale_data <- data[scales]
  scale_names <- colnames(scale_data)
  
  scores <- matrix(NA, nrow = nrow(scale_data), ncol = length(scales))
  colnames(scores) <- paste0(prefix, scale_names, suffix)
  
  for (i in 1:length(scales)) {
    m_i <- mean(scale_data[[i]], na.rm = na.rm)
    s_i <- sd(scale_data[[i]], na.rm = na.rm)
    scores[, i] <- (scale_data[[i]] - m_i) / s_i
  }
  scores[is.nan(scores)] <- NA_real_
  
  if (append) {
    cbind(data, scores)
  } else {
    as.data.frame(scores) 
  }
}
