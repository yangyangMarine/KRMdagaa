krm.dagaa <- function (frequency = 120 * 1000, c.w = 1490, rho.w = 1030, theta = 90, 
                       c.fb = 1570, c.sb = 345, rho.sb = 1.24, rho.fb = 1070, L = NULL, 
                       x_fb = NULL, x_sb = NULL, w_fb = NULL, w_sb = NULL, z_fbU = NULL, 
                       z_fbL = NULL, z_sbU = NULL, z_sbL = NULL, x_sb2 = NULL, w_sb2 = NULL, 
                       z_sbU2 = NULL, z_sbL2 = NULL, fbshape = NULL, sbshape = NULL, 
                       misc = NULL) 
{
  if (theta < 65 | theta > 115) {
    warning("!!!! Theta not within 65 and 115 degrees !!!!")
  }
  TSout = data.frame()
  if (is.null(fbshape) == FALSE) {
    if (length(fbshape > 0)) {
      if ("x_fb" %in% names(fbshape)) {
        x_fb = as.numeric(unlist(fbshape["x_fb"]))
      }
      if ("w_fb" %in% names(fbshape)) {
        w_fb = as.numeric(unlist(fbshape["w_fb"]))
      }
      if ("z_fbU" %in% names(fbshape)) {
        z_fbU = as.numeric(unlist(fbshape["z_fbU"]))
      }
      if ("z_fbL" %in% names(fbshape)) {
        z_fbL = as.numeric(unlist(fbshape["z_fbL"]))
      }
    }
  }
  if (is.null(sbshape) == FALSE) {
    if (length(sbshape > 0)) {
      if ("x_sb" %in% names(sbshape)) {
        x_sb = as.numeric(unlist(sbshape["x_sb"]))
      }
      if ("w_sb" %in% names(sbshape)) {
        w_sb = as.numeric(unlist(sbshape["w_sb"]))
      }
      if ("z_sbU" %in% names(sbshape)) {
        z_sbU = as.numeric(unlist(sbshape["z_sbU"]))
      }
      if ("z_sbL" %in% names(sbshape)) {
        z_sbL = as.numeric(unlist(sbshape["z_sbL"]))
      }
    }
  }
  if (is.null(misc) == FALSE) {
    if ("frequency" %in% names(misc)) {
      frequency = misc["frequency"]
    }
    if ("c.w" %in% names(misc)) {
      c.w = misc["c.w."]
    }
    if ("rho.w" %in% names(misc)) {
      rho.w = misc["rho.w"]
    }
    if ("theta" %in% names(misc)) {
      theta = misc["theta"]
    }
    if ("c.fb" %in% names(misc)) {
      c.fb = misc["c.fb"]
    }
    if ("c.sb" %in% names(misc)) {
      c.sb = misc["c.sb"]
    }
    if ("rho.sb" %in% names(misc)) {
      rho.sb = misc["rho.sb"]
    }
    if ("rho.fb" %in% names(misc)) {
      rho.fb = misc["rho.fb"]
    }
    if ("L" %in% names(misc)) {
      L = misc["L"]
    }
  }
  if (is.null(L)) {
    L = max(x_fb) - min(x_fb)
  }
  if (length(x_fb) > 1) {
    xv = x_fb
  }
  else {
    xv = x_sb
  }
  if (L != max(xv) - min(xv)) {
    scale = L/(max(xv) - min(xv))
    x_fb = x_fb * scale
    x_sb = x_sb * scale
    x_sb2 = x_sb2 * scale
    w_fb = w_fb * scale
    w_sb = w_sb * scale
    w_sb2 = w_sb2 * scale
    z_fbU = z_fbU * scale
    z_fbL = z_fbL * scale
    z_sbU = z_sbU * scale
    z_sbL = z_sbL * scale
    z_sbU2 = z_sbU2 * scale
    z_sbL2 = z_sbL2 * scale
  }
  if (length(x_fb) < 2) {
    rho.fb = rho.w
    c.fb = c.w
  }
  if (length(x_sb) < 2) {
    rho.sb = rho.w
    c.sb = c.w
  }
  k <- kcalc(c.w, frequency)
  k.fb <- kcalc(c.fb, frequency)
  Rbc = (rho.sb * c.sb - rho.fb * c.fb)/(rho.sb * c.sb + rho.fb * 
                                           c.fb)
  Rwb = (rho.fb * c.fb - rho.w * c.w)/(rho.fb * c.fb + rho.w * 
                                         c.w)
  T.T = 1 - Rwb^2
  
  if (length(x_sb) > 1) {
    a.sb = zoo::rollsum(w_sb, 2)/4
    Asb = k * a.sb/(k * a.sb + 0.083)
    psi.p = k * a.sb/(40 + k * a.sb) - 1.05
    du.sb = diff(x_sb) * sin(theta * pi/180)
    v.sb = (zoo::rollsum(x_sb, 2) * cos(theta * pi/180) + 
              zoo::rollsum(z_sbU, 2) * sin(theta * pi/180))/2
    f.soft = sum(-(0+1i) * Rbc * T.T/(2 * sqrt(pi)) * Asb * 
                   sqrt((k * a.sb + 1) * sin(theta * pi/180)) * exp(-(0+1i) * 
                                                                      (2 * k * v.sb + psi.p)) * du.sb)
  }
  else {
    f.soft = 0+0i
  }
  if (length(x_sb2) > 1) {
    a.sb = zoo::rollsum(w_sb2, 2)/4
    Asb = k * a.sb/(k * a.sb + 0.083)
    psi.p = k * a.sb/(40 + k * a.sb) - 1.05
    du.sb = diff(x_sb2) * sin((theta) * pi/180)
    v.sb = (zoo::rollsum(x_sb2, 2) * cos((theta) * pi/180) + 
              zoo::rollsum(z_sbU2, 2) * sin((theta) * pi/180))/2
    f.soft2 = sum(-(0+1i) * Rbc * T.T/(2 * sqrt(pi)) * Asb * 
                    sqrt((k * a.sb + 1) * sin((theta) * pi/180)) * exp(-(0+1i) * 
                                                                            (2 * k * v.sb + psi.p)) * du.sb)
  }
  else {
    f.soft2 = 0+0i
  }
  if (length(x_fb) > 1) {
    a.fb = zoo::rollsum(w_fb, 2)/4
    du.fb = diff(x_fb) * sin(theta * pi/180)
    v.fbU = (zoo::rollsum(x_fb, 2) * cos(theta * pi/180) + 
               zoo::rollsum(z_fbU, 2) * sin(theta * pi/180))/2
    v.fbL = (zoo::rollsum(x_fb, 2) * cos(theta * pi/180) + 
               zoo::rollsum(z_fbL, 2) * sin(theta * pi/180))/2
    psi.fb = -pi * k.fb * v.fbU/(2 * (k.fb * v.fbU + 0.4))
    f.fluid = -(0+1i) * (Rwb/(2 * sqrt(pi))) * sum(sqrt(k * 
                                                          a.fb) * (exp(-(0+2i) * k * v.fbU) - T.T * exp((0+1i) * 
                                                                                                          (-2 * k * v.fbU + 2 * k.fb * (v.fbU - v.fbL) + psi.fb))) * 
                                                     du.fb)
  }
  else {
    f.fluid = 0+0i
  }
  sigma <- abs(f.fluid + f.soft + f.soft2)
  TS <- 20 * log10(sigma)
  TSout = rbind(TSout, data.frame(frequency = frequency, TS = TS, 
                                  c_w = c.w, rho_w = rho.w, theta = theta, c_fb = c.fb, 
                                  c_sb = c.sb, rho_sb = rho.sb, rho_fb = rho.fb, L = L))
  return(TSout)
}

