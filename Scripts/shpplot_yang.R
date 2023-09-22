#function----
shplot_yang <- function (x_fb = NULL, x_sb = NULL, w_fb = NULL, w_sb = NULL, 
                         z_fbU = NULL, z_fbL = NULL, z_sbU = NULL, z_sbL = NULL, x_sb2 = NULL, 
                         w_sb2 = NULL, z_sbU2 = NULL, z_sbL2 = NULL, pa = FALSE) 
{
  if (pa) {
    par(mfrow = c(1, 2))
  }
  if (length(x_fb) > 1 & length(x_sb) > 1) {
    plot(x_fb, z_fbU, type = "l", ylim = c(min(z_fbL, 
                                               na.rm = T), max(z_fbU, na.rm = T)), asp = 1, main = "Lateral", 
         xlab = "x (cm)", ylab = "z (cm)")
    lines(x_fb, z_fbL, type = "l")
    lines(x_sb, z_sbU, type = "l")
    lines(x_sb, z_sbL, type = "l")
    if (length(x_sb2) > 1) {
      lines(x_sb2, z_sbU2, type = "l")
      lines(x_sb2, z_sbL2, type = "l")
    }
    plot(x_fb, w_fb/2, type = "l", ylim = c(min(z_fbL, 
                                                na.rm = T), max(z_fbU, na.rm = T)), asp = 1, main = "Dorsal", 
         xlab = "x (cm)", ylab = "w (cm)")
    lines(x_fb, -w_fb/2, type = "l")
    lines(x_sb, w_sb/2, type = "l")
    lines(x_sb, -w_sb/2, type = "l")
    if (length(x_sb2) > 1) {
      lines(x_sb2, w_sb2/2, type = "l")
      lines(x_sb2, -w_sb2/2, type = "l")
    }
  }
  else if (length(x_sb) > 1) {
    plot(x_sb, z_sbU, type = "l", ylim = c(min(z_sbL, 
                                               na.rm = T), max(z_sbU, na.rm = T)), asp = 1, main = "Lateral", 
         xlab = "x (cm)", ylab = "z (cm)")
    lines(x_sb, z_sbL, type = "l")
    if (length(x_sb2) > 1) {
      lines(x_sb2, z_sbU2, asp = 1, main = "Lateral", 
            xlab = "x (cm)", ylab = "z (cm)")
      lines(x_sb2, z_sbL2, type = "l")
    }
    plot(x_sb, w_sb/2, type = "l", ylim = c(min(z_sbL, 
                                                na.rm = T), max(z_sbU, na.rm = T)), asp = 1, main = "Dorsal", 
         xlab = "x (cm)", ylab = "w (cm)")
    lines(x_sb, -w_sb/2, type = "l")
    if (length(x_sb2) > 1) {
      lines(x_sb2, w_sb2/2, asp = 1, main = "Dorsal", 
            xlab = "x (cm)", ylab = "w (cm)")
      lines(x_sb2, -w_sb2/2, type = "l")
    }
  }
  else if (length(x_fb) > 1) {
    plot(x_fb, z_fbU, type = "l", ylim = c(min(z_fbL, 
                                               na.rm = T), max(z_fbU, na.rm = T)), asp = 1, main = "Dorsal", 
         xlab = "x (cm)", ylab = "z (cm)")
    lines(x_fb, z_fbL, type = "l")
    plot(x_fb, w_fb/2, type = "l", ylim = c(min(z_fbL, 
                                                na.rm = T), max(z_fbU, na.rm = T)), asp = 1, main = "Dorsal", 
         xlab = "x (cm)", ylab = "w (cm)")
    lines(x_fb, -w_fb/2, type = "l")
  }
}
