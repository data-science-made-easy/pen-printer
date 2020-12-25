turn <- function(servo, us) { # can turn multiple servos simultaneously
  # print(paste0("    Pulse widths: ", paste0(round(us, 1), collapse = ", "), " µs"))
  if (!status$dry_run) cat(paste0(servo, "=", us, "us", collapse = "\n"), file = daemon, fill = T)
}

sleep <- function(sec) {
  if (!status$dry_run) Sys.sleep(sec)
}

valid_angle <- function(servo, angle) {
  cal <- cal[which(servo == cal$servo), ]
  
  angle <= cal$rad[3] & cal$rad[1] <= angle
}

angle_to_pos <- function(angle_0, angle_1) {
  b     <- sqrt(arm0_length^2 + arm1_length^2 - cos(angle_1) * 2 * arm0_length * arm1_length)
  gamma <- acos((arm0_length^2 + b^2 - arm1_length^2) / 2 / arm0_length / b)
  delta <- base_angle + angle_0 - gamma
  x     <- b * sin(delta)
  y_dot <- b * cos(delta)
  y     <- a4$y / 2 - y_dot

  c(x, y)
}

angle_to_us <- function(servo, angle) {
  if (1 < length(angle)) { # handle vectors
    if (1 == length(servo)) servo <- rep(servo, length(angle))
    us_vec <- NULL
    for (i in 1:length(angle)) us_vec[i] <- angle_to_us(servo[i], angle[i])
    return(us_vec)
  }      
  
  if (!valid_angle(servo, angle)) stop(paste("Angle", angle, "(rad) does not fall in the calibration range of servo", servo))
  
  cal      <- cal[which(servo == cal$servo), ]
  index    <- if (angle < cal$rad[2]) 1:2 else 2:3
  cal      <- cal[index, ]
  fraction <- (angle - cal$rad[1]) / diff(cal$rad)
  
  cal$us[1] + fraction * diff(cal$us)
}

pos_to_angle <- function(x, y) {
  y_original <- y
  y <- y - a4$y / 2
  z <- sqrt(x^2 + y^2)
  
  gamma   <- atan(x/y)
  cos_delta <- (arm0_length^2 + z^2 - arm1_length^2) / 2 / arm0_length / z
  if (cos_delta < -1 | 1 < cos_delta) if (status$dry_run) return(FORBIDDEN) else stop("Arms too short :-O")
  delta   <- acos(cos_delta)
  theta   <- delta - gamma
  angle_0 <- pi - base_angle + theta
  # Enable to move below upper half of a4
  if (pi < angle_0) angle_0 <- angle_0 - pi
  
  cos_angle_1 <- (arm0_length^2 + arm1_length^2 - z^2) / 2 / arm0_length / arm1_length
  if (cos_angle_1 < -1 | 1 < cos_angle_1) if (status$dry_run) return(FORBIDDEN) else stop("Arms too short :-O")
  angle_1 <- acos(cos_angle_1)
  
  if (!valid_angle(s0, angle_0)) if (status$dry_run) return(FORBIDDEN) else stop(paste("Servo 0 can't take angle", angle_0))
  if (!valid_angle(s1, angle_1)) if (status$dry_run) return(FORBIDDEN) else stop(paste("Servo 1 can't take angle", angle_1))
  
  # Check round trip (pos -> angle -> pos)
  pos <- angle_to_pos(angle_0, angle_1)
  if (round(x, 1) != round(pos[1], 1) | round(y_original, 1) != round(pos[2], 1)) if (status$dry_run) return(FORBIDDEN) else stop(paste0("Position (", x, ", ", y_original, ") not in reach."))
  
  c(angle_0, angle_1)
}

position_known <- function() !is.na(status$x) & !is.na(status$y)

is_home <- function() if (position_known()) return(home$x == status$x & home$y == status$y) else return(F)

go_home <- function(force = FALSE) {
  pen_up(force)
  cat("Going home...\n")  
  turn(c(s0, s1), angle_to_us(c(s0, s1), pos_to_angle(home$x, home$y)))
  status$x <<- home$x
  status$y <<- home$y
  cat(paste0("Home! (", home$x, ", ", home$y, ") ➔"), "\n")
}

pen_up <- function(force = FALSE) {
  if (is.na(status$pen_down) | force) status$pen_down <<- T
  if (status$pen_down) {
    cat("Pen ➔\n")
    turn(s2, pen_up_us)
    status$pen_down <<- F
    sleep(delay_head)
  }
}

pen_down <- function(force = FALSE) {
  if (is.na(status$pen_down) | force) status$pen_down <<- F
  if (!status$pen_down) {
    cat("Pen ↓\n")
    turn(s2, pen_down_us)
    status$pen_down <<- T
    sleep(delay_head)
  }
}

set_pen <- function(pen_down = FALSE, force = FALSE) {
  if (pen_down) pen_down(force) else pen_up(force)
}

do_tasks <- function(tasks) {
  # pen_up()
  go_home()
  cat("Home!\n")
  on.exit({pen_up(force = TRUE); go_off_paper(); cat("Done.\n")}) # i.e. go home if user cancels current tasks
  for (i in 1:nrow(tasks)) {
    task <- tasks[i, ]
    set_pen(task$pen_down)
    turn(c(s0, s1), c(task$us0, task$us1))
    status$x <<- task$x
    status$y <<- task$y
    cat(paste0(if (status$pen_down) "↓" else "➔", stringr::str_pad(paste0(" (x, y) = (", round(status$x, 1), ", ", round(status$y, 1), ");"), 30, "right"), "deviation: ", stringr::str_pad(round(task$error, 2), 4, "right"), " mm"), "\n")
    sleep(if (task$pen_down) delay_draw else delay_move)
  }
}

distance_to_ideal_line <- function(intercept, slope, x, y) {
  b <- -1
  abs(slope * x + b * y + intercept) / sqrt(slope^2 + b^2)
}

go_to <- function(x, y) {
  if (!position_known()) go_home_quick()
    
  # angle_source <- pos_to_angle(status$x, status$y)
  angle_target <- pos_to_angle(x, y)
  
  # us_source <- angle_to_us(c(s1,s2), angle_source)
  us_target <- angle_to_us(c(s0,s1), angle_target)
  
  turn(c(s0,s1), us_target)
  status$x <<- x
  status$y <<- y
  sleep(.5)
}

go_to_corner <- function() go_to(5, 290)

go_off_paper <- function() {
  pen_up()
  cat("Going off paper...\n")
  turn(c(s0, s1), c(angle_to_us(s0,2.8), angle_to_us(s1, 2.5)))
}

set_dot <- function(x, y) {
  pen_up(force = T)
  go_to(x, y)
  pen_down()
  pen_up()
}





































