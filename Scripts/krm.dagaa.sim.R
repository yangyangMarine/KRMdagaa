krm.dagaa.sim <- function (frequency = 120 * 1000, c.w = 1490, rho.w = 1030, theta = 90, 
                           c.fb = 1570, c.sb = 345, rho.sb = 1.24, rho.fb = 1070, L = NULL, 
                           x_fb = NULL, x_sb = NULL, w_fb = NULL, w_sb = NULL, z_fbU = NULL, 
                           z_fbL = NULL, z_sbU = NULL, z_sbL = NULL, x_sb2 = NULL, w_sb2 = NULL, 
                           z_sbU2 = NULL, z_sbL2 = NULL, fbshape = NULL, sbshape = NULL, 
                           misc = NULL) 
{
  if (is.null(c.w)) {
    c.w = 1490
  }
  if (is.null(rho.w)) {
    rho.w = 1030
  }
  if (is.null(theta)) {
    theta = 90
  }
  if (is.null(c.fb)) {
    c.fb = c.w
  }
  if (is.null(c.sb)) {
    c.sb = c.fb
  }
  if (is.null(rho.fb)) {
    rho.fb = rho.w
  }
  if (is.null(rho.sb)) {
    rho.sb = rho.fb
  }
  if (is.null(L)) {
    if (length(x_fb > 1)) {
      xv = x_fb
    }
    else {
      xv = x_sb
    }
    L = max(xv) - min(xv)
  }
  vars = expand.grid(frequency, c.w, rho.w, theta, c.fb, c.sb, 
                     rho.sb, rho.fb, L)
  TS = data.frame(t(mapply(function(frequency, c.w, rho.w, 
                                    theta, c.fb, c.sb, rho.sb, rho.fb, L) {
    krm.dagaa(frequency, c.w, rho.w, theta, c.fb, c.sb, rho.sb, 
              rho.fb, L, x_fb = x_fb, x_sb = x_sb, w_fb = w_fb, 
              w_sb = w_sb, z_fbU = z_fbU, z_fbL = z_fbL, z_sbU = z_sbU, 
              z_sbL = z_sbL, x_sb2 = x_sb2, w_sb2 = w_sb2, z_sbU2 = z_sbU2, 
              z_sbL2 = z_sbL2)
  }, vars[, 1], vars[, 2], vars[, 3], vars[, 4], vars[, 5], 
  vars[, 6], vars[, 7], vars[, 8], vars[, 9])))
  TS[] <- sapply(TS, as.numeric)
  return(TS)
}