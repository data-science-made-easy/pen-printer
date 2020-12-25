status <- list(x = NA, y = NA, pen_down = NA, dry_run = FALSE) # Global variable so we know where pen is even in case of crash
source("constants.R")
source("pc-functions.R")      # Functions we need mostly on PC
source("pi-functions.R")      # Functions we need mostly on RPi
source("servo-calibration.R") # First time slow. Determines also available positions on A4

mat   <- img_to_lines("img/girl.jpg") 
mat   <- fit_img_to_paper(mat)
mat   <- to_all_straight_lines_needed(mat)
tasks <- interpolate_all_lines_to_enable_smooth_plotting(mat)

pc_plot(tasks) # simulation of final result
# Red dot is position of first servo. Blue dot is home position of pen. Green is allowed plotting area.

dput(tasks, file = "tasks.RData") # scp this file to your pi and start source('pi-plot-girl.R') there