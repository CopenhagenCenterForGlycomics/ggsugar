#' Draw sugars
#' Required aes x,y,class
#' @export
geom_sugar <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity",
                          show.legend = NA, inherit.aes = TRUE,na.rm=T,offset=0,fill=NULL,...) {
  if ( ! is.null(data) && inherit.aes ) {
    message("geom_sugar will not inherit aesthetics from parent when data is provided")
    inherit.aes = FALSE
  }
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

generate_package_data = function() {
  svgs = list.files(path='sugar_svgs')
  template_sugars = setNames(lapply(svgs, function(svg) {
    grConvert::convertPicture(file.path('sugar_svgs',svg),'cairo.svg')
    grImport2::pictureGrob(grImport2::readPicture('cairo.svg'),
      0,
      0.5,
      width = 1,
      height = 1,
      just=c("left","center"),
      default.units = "native"
    )
  }),tolower(stringr::str_replace(svgs,'.svg','')))
  usethis::use_data(template_sugars,internal=T)  
}

draw_sugar = function(x,y,sugar,offset=0,size=1) {
  require(grImport2)
  template_sugar = ggsugar:::template_sugars[[sugar]]

#      align_grid = grid::rectGrob(
#        x, grid::unit(y,"native") + grid::unit(offset * .pt,"mm"),
#        width = grid::unit(0.5*size * .pt ,"mm"),
#        height = grid::unit(0.5*size * .pt,"mm"),
#        default.units = "native",
#        just = c("centre", "bottom"),
#        gp = grid::gpar(
#          col = ggplot2::alpha("black", 0.1),
#          fill = ggplot2::alpha("red", 0.1),
#          lwd = 0.5 * .pt,
#          lty = 1,
#          lineend = "butt"
#        )
#      )
      
      sugar_viewport = grid::viewport(
        x=x,
        y=grid::unit(y,"native")+ grid::unit(offset * .pt,"mm"),
        width=grid::unit(0.5*size * .pt ,"mm"),
        height=grid::unit(0.5*size * .pt,"mm"),
        just=c("centre","bottom")
      )
      
      sugar_grob = grid::gTree(vp=sugar_viewport,children = grid::gList(template_sugar))
      
      # If you want to add an alignment grid
      # uncomment the above align_grid and return the statement below
      # grid::gList(align_grid,sugar_grob)
      
      sugar_grob
}

.pt <- 72.27 / 25.4

#' @export
GeomSugar <- ggplot2::ggproto("GeomSugar", ggplot2::Geom,
                        required_aes=c('x','y','class'),
                        draw_panel = function(data, panel_scales, coord,offset=0,size=1) {
                          coords <- coord$transform(data, panel_scales)
                          draw_sugar_vec = Vectorize(draw_sugar,SIMPLIFY=F)
                          results = draw_sugar_vec(coords$x,coords$y,coords$class,offset,size)
                          do.call(grid::gList,results)
                        }
)
