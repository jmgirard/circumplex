#' Ipsatize circumplex items using deviation scoring across variables
#'
#' Rescore each circumplex item using deviation scoring across variables. In
#' other words, subtract each observation's mean response from each response.
#' This effectively removes the presence of a general factor, which can make
#' certain circumplex fit analyses more powerful.
#'
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param items Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex items to be ipsatized.
#' @return A data frame that matches \code{.data} except that new variables are
#'   appended that contain ipsatized versions of \code{items}. These new
#'   variables will have the same name as \code{items} but with a "_i" suffix.
#' @family tidying functions
#' @export
#' @examples
#' data("jz2017")
#' ipsatize(jz2017, PA:NO)
ipsatize <- function(.data, items) {
  items_en <- rlang::enquo(items)
  new <- .data %>%
    dplyr::select(!!items_en) %>%
    dplyr::mutate(.im = rowMeans(.)) %>%
    dplyr::transmute_at(
      .vars = dplyr::vars(!!items_en),
      .funs = dplyr::funs(i = . - .im)
    )
  .data <- dplyr::bind_cols(.data, new)
  .data
}

#' Score circumplex scales from item responses
#'
#' Calculate mean scores on circumplex scales from item responses by using a
#' set of scoring instructions, which may be loaded from the package or created
#' as a custom data frame.
#'
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param items Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain all the circumplex items from a
#'   single circumplex measure, in ascending order from item 1 to item N.
#' @param key Required. A data frame containing information about each
#'   circumplex scale to be calculated from \code{items}. The following
#'   variables are required: \code{Abbrev} and \code{Items}. See \code{?iipsc}.
#' @param na.rm Optional. A logical that determines if missing values should be
#'   omitted from the calculation of scores (default = TRUE). When set to TRUE,
#'   scales with missing data are essentially calculated with mean imputation.
#' @param prefix Optional. A string to include at the beginning of the newly
#'   calcualted scale variables' names, before \code{Abbrev} from \code{key}
#'   and \code{suffix} (default = "").
#' @param suffix Optional. A string to include at the end of the newly
#'   calculated scale variables' names, after \code{Abbrev} from \code{key}
#'   and \code{prefix} (default = "").
#' @return A data frame that matches \code{.data} except that new variables are
#'   appended that contain mean scores on each variable included in \code{key}.
#' @family tidying functions
#' @export
#' @examples
#' #data("aw2012")
#' data("iipsc")
#' #score(aw2012, IIP01:IIP32, iipsc)
score <- function(.data, items, key, na.rm = TRUE, prefix = "", suffix = "") {
  items_en <- rlang::enquo(items)
  assert_that(is_provided(.data), is_enquo(!!items_en), is_provided(key))
  assert_that(is.flag(na.rm), is.string(prefix))

  item_data <- .data %>% dplyr::select(!!items_en)
  n_items <- ncol(item_data)
  for (i in 1:nrow(key)) {
    new_name <- rlang::sym(paste0(prefix, key$Abbrev[[i]], suffix))
    item_nums <- as.numeric(strsplit(key$Items[[i]], ",")[[1]])
    if (max(item_nums) > n_items) {
      stop("Key is asking for more items than were provided to function.")
    }
    scores <- item_data %>%
      dplyr::transmute(!!new_name := rowMeans(item_data[, item_nums], na.rm))
    .data <- dplyr::bind_cols(.data, scores)
  }
  .data
}

#' Standardize circumplex scales using normative data
#'
#' Take in a data frame containing circumplex scales, angle definitions for each
#' scale, and normative data (from the package or custom) and return that same
#' data frame with each specified circumplex scale transformed into standard
#' scores (i.e., z-scores) based on comparison to the normative data.
#'
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be
#'   standardized.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scales} (in degrees).
#' @param norms Required. A data frame containing normative data for each
#'   circumplex scale included in \code{scales}. The following variables are
#'   required: \code{Angle}, \code{M}, and \code{SD}. See \code{?iipsc}.
#' @return A data frame that matches \code{.data} except that new variables are
#'   appended that contain standardized versions of \code{scales}. These new
#'   variables will have the same name as \code{scales} but with a "_z" suffix.
#' @export
#' @family tidying functions
#' @examples
#' data("jz2017")
#' data("iipsc")
#' standardize(jz2017, scales = PA:NO, angles = octants(), norms = iipsc)
standardize <- function(.data, scales, angles, norms) {
  scales_en <- rlang::enquo(scales)
  scale_names <- names(dplyr::select(.data, !!scales_en))
  assert_that(is.numeric(angles))
  assert_that(length(scale_names) == length(angles))
  assert_that(length(scale_names) <= nrow(norms))
  for (i in 1:length(angles)) {
    scale_i <- scale_names[[i]]
    new_var <- rlang::sym(paste0(scale_i, "_z"))
    index_i <- norms$Angle == angles[[i]]
    m_i <- norms$M[index_i]
    s_i <- norms$SD[index_i]
    .data <- .data %>%
      dplyr::mutate_at(
        .vars = scale_i,
        .funs = dplyr::funs(!!new_var := (. - m_i) / s_i)
      )
  }
  .data
}
