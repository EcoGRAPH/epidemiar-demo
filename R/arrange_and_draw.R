
#' Draw a multipanel plot
#'
#' Arrange multiple ggplots or grobs into a single grob, move to a new page, and
#' produce graphical output (i.e. plot it). This allows you to combine multiple
#' ggplots into a single plot.
#'
#' @param x A list of ggplot objects to be included in the plot.
#' @param title A title for the multipanel plot.
#' @param fontsize Font size to use for the title. If `fontsize` is not
#'   specified, it is set to 1.4 times the font size of the title in the first
#'   object supplied to `...`. If the first object has no title, it is set to
#'   1.4 times the default title fontsize in [`ggplot2::theme_gray`].
#' @param hjust Horizontal justification. Valid options are `"left"`,
#'   `"center"`, and `"right"`.
#' @param padding Margin around the title. Must be a [grid::unit()] object of
#'   length one. The default is `unit(1.5, "line")`.
#' @param ... Arguments to be passed on to [gridExtra::arrangeGrob()].
#'
#' @details The method implemented here is basically the
#'   [gridExtra::arrangeGrob()] > [grid::grid.newpage()] > [grid::grid.draw()]
#'   workflow espoused by the **gridExtra** package. For additional details,
#'   including help with complex layouts, see the gridExtra vignette
#'   [Arranging multiple grobs on a page](https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html).
#'
#'   If argument `title` is not null and argument `fontsize` is NULL, a fontsize
#'   fpr the title is calculated from the *first* element of `x`. If you want it
#'   calculated from a different element, change the order of the elements
#'   before you call the function and use the `layout_matrix` argument to
#'   rearrange subplots.
#'
#' @inherit grid::grid.draw return
#'
#' @export
#'
#' @seealso [ggplot_grob_title_gp()]
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p1 <- ggplot(economics, aes(date, unemploy)) +
#'   geom_line() + ggtitle("Economic Data")
#' p2 <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'   ggtitle("Good 'ol mtcars")
#' arrange_and_draw(list(p1, p2))
#' arrange_and_draw(list(p1, p2), title = "Two example plots")
#' arrange_and_draw(list(p1, p2), nrow = 1, title = "Side by side plots")
#' arrange_and_draw(list(p1, p2), layout_matrix = rbind(c(1,1), c(2,NA)),
#'   title = "More complicated layouts are possible")
#' arrange_and_draw(list(p1, p2), nrow = 1, title = "A Really Big Title!",
#'   fontsize = 36, hjust = "center", padding = unit(5, "line"))
#' }
arrange_and_draw <- function(x,
                             title = NULL,
                             fontsize = NULL,
                             hjust = "left",
                             padding = unit(1.5, "line"),
                             ...) {

  # make sure all arguments in ... are ggplots or arguments to arrangGrob
  inherit.ggplot <-
    unlist(lapply(x, inherits, what = "ggplot"))

  if(!all(inherit.ggplot))
    stop("At least on of the objects is not a ggplot.")

  # check the other arguments
  if(!missing(title)) {

    # make sure title is a character of length 1
    stopifnot(inherits(title, "character"))
    stopifnot(length(title) == 1)

    # make sure fontsize is a numeric of length 1
    if(!missing(fontsize)) {
      stopifnot(inherits(fontsize, "numeric"))
      stopifnot(length(fontsize) == 1)
    }

    # make sure hjust is valid
    stopifnot(hjust %in% c("left", "center", "right"))

    # make sure padding is a unit
    if (missing(padding)) stopifnot(inherits(padding, "unit"))

  }

  # combine ... into a single grob consisting of multiple grobs, one for each
  # object passed in via ...
  p <- gridExtra::arrangeGrob(grobs = x, padding = padding, ...)

  # if *no* title argument was provided, just draw the existing multi-grob plot
  if (missing(title)) {

    # just return the grob as is, with no title
    p

  } else {

    # but if a title *was* provided, create a text grob and add it on top
    # first determine the font size to use for the text grob

    # if no fontsize provided, calculate one
    if (missing(fontsize)) {

      # there are two ways to calcualte a title font size:
      # 1. get the actual font size of the first grob's title and bigger it
      # 2. get the default title size of a ggplot and bigger it

      # figure out which method to use and implement it
      grob1 <- p$grobs[[1]]                       # get the first sub-grob
      title_gp <- ggplot_grob_title_gp(grob1)     # get the title parameters

      if (!is.null(title_gp)) {

        # if the first grob *has* a title, implement method 1
        fontsize = title_gp$fontsize * 1.4

      } else {

        # if first grob *lacks* a title, implement method 2
        theme_gray_text_size <- ggplot2::theme_gray()$text$size
        theme_gray_plot_title_size <- ggplot2::theme_gray()$plot.title$size
        stopifnot(inherits(theme_gray_plot_title_size, "rel"))
        fontsize = theme_gray_text_size * theme_gray_plot_title_size[1] * 1.4

      }                                     # finish implementing method 1 or 2

    }                                       # finish calculating font size

    # fontsize should now be defined either by the user or by calculation
    stopifnot(is.numeric(fontsize))

    x <- switch(hjust, "left" = 0, "center" = 0.5, "right" = 1)

    # creat a text grob for the title
    title <- grid::textGrob(
      title,
      just = hjust,
      x = unit(x, "npc"),
      gp = grid::gpar(fontsize = fontsize)      # use the fontsize argument
    )

    # add the title grob to the multipanel grob
    h <- grid::grobHeight(title) + padding      # use the padding argument
    p <- gtable::gtable_add_rows(p, h, pos = 0) # 0 adds on the top
    p <- gtable::gtable_add_grob(
      p,
      title,
      t = 1,
      l = 1,
      r = ncol(p),
      z = Inf,
      clip = "off"
    )

  }                                         # finish adding a title grob

  grid::grid.newpage()                      # clear current graphical device
  grid::grid.draw(p)                        # draw the grob

}
