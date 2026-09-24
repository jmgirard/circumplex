# Segment introspection for the trajectory plot's displacement panel (M154).
#
# The displacement panel draws one segment per pair of consecutive time points
# so that a segment touching an uncertified point can be dashed on its own.
# Read the built segment data rather than the input frame: a gap test that only
# looks at `p$data` cannot see a segment that bridges the gap (M154 plan
# audit), and the line type is decided at build time by the linetype scale.

# The built data of the GeomSegment layer(s), one row per drawn segment,
# ordered by group then by x. `linetype` is the scale's output ("solid" /
# "dashed") where a linetype aesthetic is mapped, and the geom default
# otherwise.
traj_segments <- function(p) {
  built <- ggplot2::ggplot_build(p)
  idx <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)
  ))
  out <- do.call(rbind, built$data[idx])
  # A layer with nothing to draw is built as a zero-row frame with no
  # positional columns, so there is nothing to order.
  if (nrow(out) == 0L) return(out)
  out[order(out$group, out$x), , drop = FALSE]
}

# The segment line types as character, in x order, for one group's series.
traj_segment_lty <- function(p, group = NULL) {
  s <- traj_segments(p)
  if (!is.null(group)) s <- s[s$group == group, , drop = FALSE]
  as.character(s$linetype)
}
