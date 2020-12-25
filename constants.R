# Paper
a4        <- list(x = 210, y = 297) # mm
boundary_band <- 2 # mm
boundary_band_index <- (-boundary_band):boundary_band

# position
home   <- list(x = 50, y = a4$y / 2)

# delay
delay_draw <- .02 # s
delay_move <- .01 # s
delay_head <- .4 # s
delay_dry_speedup <- 10

# Drawer
base_angle        <- pi / 3
base_position     <- c(0, a4$y / 2) # (x, y) position of servo_0
arm0_length       <- 86.7 # mm
arm1_length       <- 84.5 # mm
FORBIDDEN         <- c(Inf, Inf) # Return value if (x,y) can't be reached

# Files
daemon                   <- "/dev/pca9685servo"
valid_plotting_area_file <- "available-positions-on-a4.RData"

# Servos
min_pulse_width <-  500 # us
max_pulse_width <- 2500 # us
min_pulse_step  <-    5 # ms
s0              <- 13   # channel
s1              <- 14   # channel
s2              <- 15   # channel
pen_down_us     <- 1850 # us - pencil down
pen_up_us       <- 1500 # us - pencil up

# Column names of matrices
COLNMS <- c("x0", "y0", "x1", "y1")