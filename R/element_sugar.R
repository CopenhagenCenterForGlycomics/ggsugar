require(ggplot2)
require(grid)

#' Draw a sugar as an element of the plot - drop-in replacement for axis labels
#' 
#' @export
element_sugar = function(align="bottom") {
    structure(
      list(align=align),
      class = c("element_sugar_custom","element_blank", "element") # inheritance test workaround
    )
  }

draw_element_sugar = function(sugar,x,y,align="bottom") {
  require(grImport2)
  template_sugar = get_template_sugar(sugar)
  sugar_viewport = grid::viewport(
    x=x,
    y=y,
    width=grid::unit(2,"line"),
    height=grid::unit(2,"line"),
    just=c("centre",align)
  )
  
  sugar_grob = grid::gTree(vp=sugar_viewport,children = grid::gList(template_sugar))
  sugar_grob
}

# returns a gTree with two children: the text label, and a rasterGrob below
element_grob.element_sugar_custom <- function(element,label,x, y, ...)  {
  if (class(x) != 'unit' && class(label) == 'unit') {
    temp_labels = x
    x = label
    label = temp_labels
  } 
  if ( all(is.na(x)) ) {
    return(gTree())
  }
  if (missing(y)) {
    y = rep(unit(0.5,"npc"),length(x))
  }
  gTree(children=do.call(gList, sapply(1:length(x),function(idx) draw_element_sugar(label[idx],x[idx],y[idx],align=element$align),simplify=F) ), cl = "custom_sugar")
}

# gTrees don't know their size and ggplot would squash it, so give it room
grobHeight.custom_sugar = heightDetails.custom_sugar = function(x, ...)
  unit(4, "lines")