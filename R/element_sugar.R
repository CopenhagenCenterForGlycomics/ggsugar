require(ggplot2)
require(grid)

#'@importFrom stats setNames
#'@importFrom utils read.delim
NULL

#' Draw a sugar as an element of the plot - drop-in replacement for axis labels
#' 
#' @export
element_sugar = function(align="bottom",size=2) {
    structure(
      list(align=align,size=size),
      class = c("element_sugar_custom","element_blank", "element") # inheritance test workaround
    )
  }

draw_element_sugar = function(template_sugar,x,y,align="bottom",size=2) {

  sugar_viewport = grid::viewport(
    x=x,
    y=y,
    width=grid::unit(size,"line"),
    height=grid::unit(size,"line"),
    just=c("centre",align)
  )
  
  sugar_grob = grid::gTree(vp=sugar_viewport,children = grid::gList(template_sugar))
  sugar_grob$sugar_height = grid::unit(size,"line")
  sugar_grob
}

# returns a gTree with two children: the text label, and a rasterGrob below
#' @importFrom ggplot2 element_grob
#' @export
element_grob.element_sugar_custom <- function(element,label,x, y, ...)  {
  if (class(x) != 'unit' && class(label) == 'unit') {
    temp_labels = x
    x = label
    label = temp_labels
  } 
  if ( all(is.na(x)) ) {
    return(grid::gTree())
  }
  if (missing(y)) {
    y = rep(grid::unit(0.5,"npc"),length(x))
  }
  sugars = get_template_sugar(label)
  grid::gTree(children=do.call(grid::gList, sapply(1:length(x),function(idx) draw_element_sugar(sugars[[idx]],x[idx],y[idx],align=element$align,size=element$size),simplify=F) ), cl = "custom_sugar")
}

# gTrees don't know their size and ggplot would squash it, so give it room
#' @importFrom grid grobHeight
#' @importFrom grid heightDetails
#' @export grobHeight.custom_sugar
#' @export heightDetails.custom_sugar
grobHeight.custom_sugar = heightDetails.custom_sugar = function(x, ...) {
  do.call(max,sapply(x$children,function(kid) kid$sugar_height ,simplify=F))
}