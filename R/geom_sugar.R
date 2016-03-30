#' Draw sugars
#' Required aes x,y,class
#' @export
geom_sugar <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity",
                          show.legend = NA, inherit.aes = TRUE,na.rm=T,offset=0,fill=NULL,...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSugar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm=na.rm,
      offset=offset,
      fill=fill,
      ...
    )
  )
}

draw_sugar = function(x,y,sugar,offset,fill=NULL) {
  args = list(x=x,y=y,offset=offset)
  if (! is.na(fill)) {
    args[['fill']] = fill
  }
  func = NULL

  if (sugar == "galnac") {
    func = draw_galnac
  }
  if (sugar == "man") {
    func = draw_man
  }
  if (sugar == "gal(b1-3)galnac") {
    func = draw_galb13galnac
  }

  if ( ! is.null(func) ) {
    return(do.call(func,args))
  }
}

draw_man = function(x,y,offset=0,fill='green') {
  grid::circleGrob(
    x, grid::unit(y,"native") + grid::unit(offset * .pt,"mm"),
    r = grid::unit(0.5 * .pt ,"mm"),
    default.units = "native",
    gp = grid::gpar(
      col = ggplot2::alpha("black", 1),
      fill = ggplot2::alpha(fill, 1),
      lwd = 0.5 * .pt,
      lty = 1,
      lineend = "butt"
    )
  )
}

draw_galnac = function(x,y,offset=0,fill='yellow') {
  grid::rectGrob(
    x, grid::unit(y,"native") + grid::unit(offset * .pt,"mm"),
    width = grid::unit(1 * .pt ,"mm"),
    height = grid::unit(1 * .pt,"mm"),
    default.units = "native",
    just = c("centre", "centre"),
    gp = grid::gpar(
      col = ggplot2::alpha("black", 1),
      fill = ggplot2::alpha(fill, 1),
      lwd = 0.5 * .pt,
      lty = 1,
      lineend = "butt"
    )
  )
}

draw_galb13galnac = function(x,y,offset=0,fill='yellow') {
  galnac = draw_galnac(x,y,offset,fill)
  gal = grid::circleGrob(
    x, grid::unit(y,"native") + grid::unit((offset+1) * .pt,"mm"),
    r = grid::unit(0.5 * .pt ,"mm"),
    default.units = "native",
    gp = grid::gpar(
      col = ggplot2::alpha("black", 1),
      fill = ggplot2::alpha(fill, 1),
      lwd = 0.5 * .pt,
      lty = 1,
      lineend = "butt"
    )
  )
  grid::gList(gal,galnac)
}

.pt <- 72.27 / 25.4

#' @export
GeomSugar <- ggplot2::ggproto("GeomSugar", ggplot2::Geom,
                        required_aes=c('x','y','class'),
                        draw_panel = function(data, panel_scales, coord,offset=0,fill=NULL) {
                          coords <- coord$transform(data, panel_scales)
                          draw_sugar_vec = Vectorize(draw_sugar,SIMPLIFY=F)
                          results = draw_sugar_vec(coords$x,coords$y,coords$class,offset,ifelse(is.null(fill),NA,fill))
                          do.call(grid::gList,results)
                        }
)
