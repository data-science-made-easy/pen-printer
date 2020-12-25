status <- list(x = NA, y = NA, pen_down = NA, dry_run = FALSE) # Global variable so we know where pen is even in case of crash

# Start deamon
system("sudo ./pca9685servodaemon/pca9685servod --noflicker")

# Get constants and functions 
source("constants.R")
source("pc-functions.R")      # Functions we need mostly on PC
source("pi-functions.R")      # Functions we need mostly on RPi
source("servo-calibration.R") # First time slow. Determines also available positions on A4

# Get tasks to plot
tasks <- dget("tasks.RData")

go_off_paper() # So you can tape your A4

# Plot line figure
do_tasks(tasks)
