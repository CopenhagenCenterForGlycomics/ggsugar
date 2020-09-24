require(ggplot2)
require(grid)

#' Draw a sugar as an element of the plot - drop-in replacement for axis labels
#' 
#' @export
element_sugar = function() {
    structure(
      list(),
      class = c("element_sugar_custom","element_blank", "element") # inheritance test workaround
    )
  }

draw_element_sugar = function(sugar,x,align="bottom") {
  require(grImport2)

  template_sugar = get_template_sugar(sugar)
      
  sugar_viewport = grid::viewport(
    x=x,
    width=grid::unit(2,"line"),
    height=grid::unit(2,"line"),
    just=c("centre",align)
  )
  
  sugar_grob = grid::gTree(vp=sugar_viewport,children = grid::gList(template_sugar))
  
  sugar_grob
}

# returns a gTree with two children: the text label, and a rasterGrob below
element_grob.element_sugar_custom <- function(element, x, label,...)  {
  gTree(children=do.call(gList, sapply(1:length(x),function(idx) draw_element_sugar(label[idx],x[idx]),simplify=F) ), cl = "custom_sugar")
}

# gTrees don't know their size and ggplot would squash it, so give it room
grobHeight.custom_sugar = heightDetails.custom_sugar = function(x, ...)
  unit(4, "lines")