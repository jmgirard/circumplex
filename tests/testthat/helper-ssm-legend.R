# Legend-key introspection for plot tests (M36).
#
# A ggplot2 legend draws one key per scale break, but it only draws a key's
# GLYPH for values present in some layer's data: a break kept alive by
# `scale_*_manual(limits =, drop = FALSE)` with no matching row renders as a
# label with a zeroGrob where its symbol belongs. That is invisible to every
# data-level assertion and to a vdiffr baseline regenerated after the fact, so
# the guard has to read the rendered key grobs.
#
# In the built gtable each key is a gTree holding a `legend.key.rect` plus its
# glyph (a `points` grob when drawn, a `zeroGrob` when not); the guide's title
# is a text grob in the same legend gtable. legend_key_glyphs() finds the
# legend carrying `title` and returns one element per key: the pch vector of a
# drawn glyph, or NA for a key whose glyph is missing.

# Every grob in a subtree, depth-first (gTrees expose `children`, gtables `grobs`).
grob_descendants <- function(x) {
  kids <- c(as.list(x$children), as.list(x$grobs))
  if (length(kids) == 0L) return(list(x))
  c(list(x), unlist(lapply(kids, grob_descendants), recursive = FALSE))
}

grob_labels <- function(x) {
  labs <- vapply(
    grob_descendants(x),
    function(g) if (inherits(g, "text")) paste(g$label, collapse = "|") else "",
    character(1)
  )
  labs[nzchar(labs)]
}

# The keys of the legend titled `title`: each a gTree holding a
# `legend.key.rect` and the glyph grobs the layers drew into it.
legend_keys <- function(plot, title) {
  gt <- ggplot2::ggplotGrob(plot)
  boxes <- gt$grobs[grepl("^guide-box", gt$layout$name)]
  if (length(boxes) == 0L) return(list())

  # The legend carrying `title` is the smallest gtable whose subtree contains a
  # text grob with that exact label -- "smallest" so a guide box holding several
  # legends resolves to the one legend, not the box around all of them.
  candidates <- Filter(
    function(g) inherits(g, "gtable") && title %in% grob_labels(g),
    unlist(lapply(boxes, grob_descendants), recursive = FALSE)
  )
  if (length(candidates) == 0L) return(list())
  sizes <- vapply(candidates, function(g) length(grob_descendants(g)), integer(1))
  legend <- candidates[[which.min(sizes)]]

  # A key is a gTree holding a legend.key.rect; its sibling is the glyph.
  keys <- Filter(
    function(g) {
      inherits(g, "gTree") &&
        any(grepl("legend\\.key\\.rect", vapply(
          as.list(g$children),
          function(ch) if (is.null(ch$name)) "" else ch$name,
          character(1)
        )))
    },
    grob_descendants(legend)
  )
  keys
}

legend_key_glyphs <- function(plot, title) {
  keys <- legend_keys(plot, title)
  # Every points grob in the key, not just the first: two layers both claiming a
  # key overdraw identical glyphs, which is invisible by eye and in a baseline
  # but means the legend is being assembled twice.
  lapply(keys, function(k) {
    pts <- Filter(function(ch) inherits(ch, "points"), as.list(k$children))
    if (length(pts) == 0L) {
      NA_real_
    } else {
      as.numeric(unlist(lapply(pts, function(p) p$pch)))
    }
  })
}

# The line grobs drawn into each key of the legend titled `title` (M154): one
# element per key, each a list of `list(lty =, col =)` per line grob found, an
# empty list for a key with no line. A path key is a segments grob under
# draw_key_path(); a polyline is accepted too so the reader survives a geom
# whose key draws that instead. `lty` is normalized to its name where ggplot2
# handed grid a name, and to a character digit otherwise, so the assertions
# can compare against "solid" / "dashed".
legend_key_lines <- function(plot, title) {
  keys <- legend_keys(plot, title)
  lapply(keys, function(k) {
    lines <- Filter(
      function(ch) inherits(ch, c("segments", "polyline", "lines")),
      as.list(k$children)
    )
    lapply(lines, function(l) {
      col <- l$gp$col
      # grid hands the colour over as "#000000FF" or as a name; normalize to
      # the six-digit hex so "black" and its alpha-suffixed form compare equal.
      col <- if (is.null(col) || is.na(col)) {
        NA_character_
      } else {
        do.call(grDevices::rgb, as.list(grDevices::col2rgb(col)[, 1] / 255))
      }
      list(lty = as.character(l$gp$lty %||% "solid"), col = col)
    })
  })
}
