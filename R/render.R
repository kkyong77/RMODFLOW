#' Generic function to render RMODFLOW arrays in 3D
#' 
#' @rdname rmf_render
#' @export
rmf_render <- function(...) {
  UseMethod('rmf_render')
}

rmf_render.rmf_2d_array <- function(array, dis, height, height_exaggeration = 25,
                                    add = FALSE, colour_palette = rmfi_rev_rainbow,
                                    nlevels = 10,
                                    zlim = range(height),
                                    bas,
                                    mask = rmfi_ifelse0(is.null(bas), array*0+1, {warning('Using first ibound layer as mask.', call. = FALSE); rmfi_ifelse0(bas$xsection, aperm(bas$ibound, c(3,2,1))[,,1], bas$ibound[,,1])})) {
  
  # TODO update coordinates to render faces instead of nodes, this works for
  # surface3d, just use vertical jumps between cells, so add one row and one
  # column per row and column? also for boundary cells, we can include cell
  # sides for bottom, add another surface3d call?
  # but leave option for connecting nodes maybe?
  
  zlim <- zlim * height_exaggeration
  
  xyz <- rmf_cell_coordinates(dis)
  x <- xyz$x[,,1]
  y <- xyz$y[,,1]
  
  # TODO support for custom crs
  
  # if(!is.null(prj)) {
  #   xyz <- rmf_convert_grid_to_xyz(x=c(x),y=c(y),prj=prj)
  #   x[,] <- xyz$x
  #   y[,] <- xyz$y
  # }
  
  z <- t(height)*height_exaggeration

  nrows <- nrow(z)
  ncols <- ncol(z)
  z[as.numeric(t(mask)) == 0] <- NA
  z <- matrix(z, nrow = nrows, ncol = ncols)
  
  x <- x[rep(1:nrow(x), each = 2),] 
  x <- x[, rep(1:ncol(x), each = 2)]
  x <- x + matrix(rep(dis$delr/2, each = 2) * c(-1, 1), byrow = TRUE, nrow = nrow(x), ncol = ncol(x))
  y <- y[rep(1:nrow(y), each = 2),] 
  y <- y[, rep(1:ncol(y), each = 2)]
  y <- y + matrix(rep(dis$delc/2, each = 2) * c(1, -1), byrow = FALSE, nrow = nrow(y), ncol = ncol(y))
  z <- z[rep(1:nrow(z), each = 2),]
  z <- z[, rep(1:ncol(z), each = 2)]

  # TODO update color so that the higher cell color is used for the vertical
  # faces
  
  colorlut <- colorRampPalette(colour_palette(nlevels))(25) # height color lookup table
  col <- colorlut[ round(approx(seq(zlim[1],zlim[2],length=25+1),seq(0.5,25+0.5,length=25+1),xout=c(z),rule=2)$y) ] # assign colors to heights for each point
  alpha <- rep(1,length(col))
  # alpha[which(c(t(mask))==0)] <- 0
  if(!add) rgl::open3d()
  # if(type=='fill')
    rgl::surface3d(t(x),t(y),z,color=col,alpha=alpha,
                                  back='filled', front = "filled",smooth=FALSE,
                                  lit = FALSE, polygon_offset = 1) 
  # if(type=='grid')
    rgl::surface3d(t(x),t(y),z,front='lines',alpha=alpha,
                                  back='lines',smooth=FALSE, lit = FALSE,
                   color = "black") 
}


if (FALSE) {
 
  # test with volcano
  data(volcano)
  z <- 2 * volcano        # Exaggerate the relief
  x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
  y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
  
  x <- rep(x, each = 2) + c(-5, 5)
  y <- rep(y, each = 2) + c(-5, 5)
  z <- z[rep(1:nrow(z), each = 2),]
  z <- z[, rep(1:ncol(z), each = 2)]
  z[z < 200] <- NA
  
  zlim <- range(z, na.rm = TRUE)
  zlen <- zlim[2] - zlim[1] + 1
  colorlut <- rainbow(zlen) # height color lookup table
  col <- colorlut[ round((z - zlim[1])*2,0) + 1 ] # assign colors to heights for each point
  open3d()
  surface3d(x, y, z, color = col, front = "filled", back = "filled", lit = FALSE, polygon_offset = 1)
  surface3d(x, y, z, color = "black", front = "lines", back = "lines", lit = FALSE)

  
  
   
}
