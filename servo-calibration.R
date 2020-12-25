# Calibration
add <- function(tab, servo, angle, pulse_width) {
  tab[1 + nrow(tab), ] <- c(servo, angle, pulse_width)
  tab
}
cal <- data.frame(matrix(ncol = 3, nrow = 0, dimnames = list(NULL, c("servo", "rad", "us"))))
cal <- add(cal, servo = s0, angle = pi * 2.5/180,     pulse_width =  500) # ~0
cal <- add(cal, servo = s0, angle = pi / 2,           pulse_width = 1530) # ~pi/2
cal <- add(cal, servo = s0, angle = pi - pi * 11/180, pulse_width = 2500) # ~pi
cal <- add(cal, servo = s1, angle = 16/180 * pi,      pulse_width = 2500) # ~0
cal <- add(cal, servo = s1, angle = pi / 2,           pulse_width = 1570) # ~pi/2
cal <- add(cal, servo = s1, angle = pi,               pulse_width =  500) # ~pi
cal <- add(cal, servo = s2, angle = 0,                pulse_width = 2500) # ~0
cal <- add(cal, servo = s2, angle = pi / 2,           pulse_width = 1500) # ~pi/2
cal <- add(cal, servo = s2, angle = pi,               pulse_width =  500) # ~pi

# Given min_pulse_step, determine min_angle_step
min_n_pulse_steps <- (max_pulse_width - min_pulse_width) / min_pulse_step
min_angle_step    <- list(s0 = abs(cal$rad[3] - cal$rad[1]) / min_n_pulse_steps, s1 = abs(cal$rad[6] - cal$rad[4]) / min_n_pulse_steps, s2 = abs(cal$rad[9] - cal$rad[7]) / min_n_pulse_steps)

# Determine available positions in convex shape to make fitting of lines easy
av_pos <- get_valid_convex_a4_plotting_area("available-positions-on-a4.RData") # first time slow