#' Draw sugars
#' Required aes x,y,sugar
#' @export
geom_sugar <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity",
                          show.legend = NA, inherit.aes = TRUE,na.rm=T,offset=0,align="bottom",...) {
  if ( ! is.null(data) && inherit.aes ) {
    message("geom_sugar will not inherit aesthetics from parent when data is provided")
    inherit.aes = FALSE
  }
  if (! (align %in% c("center","centre","bottom"))) {
    stop("Invalid align parameter to geom_sugar, must be one of 'center' or 'bottom'")
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
      align=align,
      ...
    )
  )
}

#' List available sugars
#' @export
supported_sugars <- function() {
  ggsugar::glycans$nickname
}

get_template_sugar_pre_gen = function(sugar) {
  nicnknames = ggsugar::glycans
  nicnknames$nickname = tolower(nicnknames$nickname)

  if (length(intersect(tolower(sugar),nicnknames$nickname)) > 0) {
    sugar = with(nicnknames, setNames(tolower(sequence),nickname))[tolower(sugar)]
  }

  r_version = paste('R',R.version$major,sep='.')
  template_sugars = template_sugars_for_r[[r_version]]

  lower_names = setNames(template_sugars, tolower(names(template_sugars)))
  template_sugar = lower_names[tolower(sugar)]

  return(template_sugar)
}


get_template_sugar_dynamic <- function(sugar) {

  nicnknames = ggsugar::glycans
  nicnknames$nickname = tolower(nicnknames$nickname)

  if (length(intersect(tolower(sugar),nicnknames$nickname)) > 0) {
    sugar = with(nicnknames, setNames(tolower(sequence),nickname))[tolower(sugar)]
  }

  svg = seq_to_svg(sugar)
  as.vector(lapply(svg, function(svgdata) {
    input_svg = tempfile("input",fileext=".svg")
    cat(svgdata,file=input_svg,sep="\n")
    cairo_svg = tempfile("cairo",fileext=".svg")
    suppressMessages(grConvert::convertPicture(input_svg,cairo_svg))
    picgrob = grImport2::pictureGrob(grImport2::readPicture(cairo_svg),
      0,
      0.5,
      width = 1,
      height = 1,
      just=c("left","center"),
      default.units = "native"
    )
    picgrob
  }))
}

v8_ctx = NULL

get_v8 = function() {
  if (is.null(v8_ctx)) {
    v8_ctx <- V8::v8()
    v8_ctx$source(system.file("sviewer-headless.js", package = "ggsugar", mustWork = TRUE));    
  }
  v8_ctx
}

seq_to_svg = function(seq) {
  v8_ctx = get_v8()
  v8_ctx$assign("seq",seq)
  v8_ctx$eval(paste("render_iupac_sugar(seq).then( res => console.r.assign('svg_temp',res) )",sep=""))
  if (exists('svg_temp',envir=.GlobalEnv)) {
    message("Generated sugar for ",seq)
    retval=get('svg_temp',envir=.GlobalEnv)
    rm('svg_temp',envir=.GlobalEnv)
    retval
  } else {
    return()
  }
}


#' @importFrom grImport2 grid.picture
#' @importFrom grImport2 grid.symbols
get_template_sugar <- function(sugar) {
  # There's a bug with the hirenj/grImport2 and the sjp/grImport2 with viewports, so we should use the 0.2.0 just for
  # generating template sugars until the other bug in grImport2 can be patched in the 0.2.0 release line
  if (requireNamespace('V8',quietly = TRUE) && requireNamespace('grConvert',quietly = TRUE) ) {
    get_template_sugar_dynamic(sugar)
  } else {
    get_template_sugar_pre_gen(sugar)
  }
}


generate_package_data = function() {
  if (!(requireNamespace('V8',quietly = TRUE) && requireNamespace('grConvert',quietly = TRUE) && requireNamespace('grImport2',quietly=T)) ) {
    stop('V8 and grConvert packages need to be installed to generate package data')
  }

  glycans = read.delim('data/nicknames.tsv')
  template_sugars = unlist(sapply(glycans$sequence, function(seq) get_template_sugar(seq), simplify=F ),recursive=F)
  r_version = paste('R',R.version$major,sep='.')

  template_sugars_for_r[[ r_version ]] = template_sugars
  usethis::use_data(template_sugars_for_r,internal=T,overwrite=T) 
  usethis::use_data(glycans,overwrite=T)
}

draw_sugar = function(x,y,sugar,offset=0,size=1,align="bottom") {

  template_sugar = get_template_sugar(sugar)
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
  rendered = lapply(1:length(template_sugar), function(idx) {
    sugar_viewport = grid::viewport(
      x=x[idx],
      y=grid::unit(y[idx],"native")+ grid::unit(offset * .pt,"mm"),
      width=grid::unit(0.5*size * .pt ,"mm"),
      height=grid::unit(0.5*size * .pt,"mm"),
      just=c("centre",align)
    )
    sugar_grob = grid::gTree(vp=sugar_viewport,children = do.call(grid::gList,template_sugar[idx]))
  })
  
  # If you want to add an alignment grid
  # uncomment the above align_grid and return the statement below
  # grid::gList(align_grid,sugar_grob)
  
  do.call(grid::gList,rendered)
}

.pt <- 72.27 / 25.4

#' @export
GeomSugar <- ggplot2::ggproto("GeomSugar", ggplot2::Geom,
                        required_aes=c('x','y','sugar'),
                        draw_panel = function(data, panel_scales, coord,offset=0,size=1,align="bottom") {
                          coords <- coord$transform(data, panel_scales)
                          #draw_sugar_vec = Vectorize(draw_sugar,SIMPLIFY=F)
                          results = draw_sugar(coords$x,coords$y,coords$sugar,offset,size,align)
                          do.call(grid::gList,results)
                        }
)
