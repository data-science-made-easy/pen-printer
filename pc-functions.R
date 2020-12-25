pc_plot <- function(tasks) {
  lim <- c(-10, a4$y + 10)
  image(0:nrow(av_pos), 0:ncol(av_pos), av_pos, xlim = lim, ylim = lim, col = c("white", "lightgreen"), axes = F, xlab = "horizontal (mm)", ylab = "vertical (mm)", main = "A4")
  axis(1)
  axis(2, las = 2)
  rect(0, 0, a4$x, a4$y)
  points(0, a4$y/2, pch = 19, col = "red")
  points(home$x, home$y, pch = 19, col = "blue")
  
  xs <- tasks$x
  ys <- tasks$y
  index <- which(!tasks$pen_down)
  xs[index] <- NA
  ys[index] <- NA
  lines(xs, ys, lwd = 2)
}

valid_points <- function(p) {
  p <- round(p)
  p <- 1 + p 
  if (any(p < 1) | any(nrow(av_pos) < p$x) | any(ncol(av_pos) < p$y)) {
    return(FALSE)
  } else {
    return(!any(!apply(p, 1, function(xy) av_pos[xy[1], xy[2]])))
  }
}

smart_order_from_home <- function(mat) {
  smat <- NULL
  
  x <- home$x
  y <- home$y
  
  while (0 < nrow(mat)) {
    d0 <- sqrt((mat$x0 - x)^2 + (mat$y0 - y)^2)
    d1 <- sqrt((mat$x1 - x)^2 + (mat$y1 - y)^2)
    index0 <- which.min(d0)
    index1 <- which.min(d1)
    min0   <- d0[index0]
    min1   <- d1[index1]
    if (min0 < min1) { # closest point is in {x0,y0}
      smat <- rbind(smat, as.numeric(mat[index0, ])) # put (x0,y0) -> (x1,y1) as next
      x    <- mat$x1[index0] # end point of this line is start point of following line
      y    <- mat$y1[index0]
      mat  <- mat[-index0, ]
    } else { # closest point is in {x1,y1}
      smat <- rbind(smat, as.numeric(mat[index1, c(3,4,1,2)]))
      x    <- mat$x0[index1]
      y    <- mat$y0[index1]
      mat  <- mat[-index1, ]
    }
  }
  colnames(smat) <- c("x0", "y0", "x1", "y1")
  data.frame(smat)
}

insert_move_between_unconnected_lines <- function(mat) {
  m <- mat[1, ]
    
  if (1 < nrow(mat)) for (i in 2:nrow(mat)) {
    if (round(mat$x1[i - 1], 1) == round(mat$x0[i], 1) & round(mat$y1[i - 1], 1) == round(mat$y0[i], 1)) { # line i - 1 connects line i
      m <- rbind(m, mat[i, ])
      stopifnot(mat$pen_down[i])
    } else { # we need to move the pen to following line (pen up)
      m <- rbind(m, c(mat$x1[i - 1], mat$y1[i - 1], mat$x0[i], mat$y0[i], FALSE))
      m <- rbind(m, mat[i, ]) # then plot line
    }
  }
  
  class(m$pen_down) = "logical"
  m
}

interpolate_line <- function(lst) {
  x0 <- lst$x0
  y0 <- lst$y0
  x1 <- lst$x1
  y1 <- lst$y1
  
  # straight line from (x0,y0) -> (x1,y1)
  coef <- if (x0 == x1) c(0, Inf) else lsfit(c(x0,x1), c(y0,y1))$coef # describe line y = coef[1] + coef[2] * x
  dst  <- function(xy) if (all(c(0, Inf) == coef)) abs(x0 - xy[1]) else distance_to_ideal_line(coef[1], coef[2], xy[1], xy[2])

  # collect angles to take subsequentially
  a0_seq <- a0 <- pos_to_angle(x0, y0)[1]
  a1_seq <- a1 <- pos_to_angle(x0, y0)[2]
  x_seq  <- x0
  y_seq  <- y0
  error_seq <- 0
  
  improving <- TRUE
  while (improving) {
    a0_neg  <- a0 - min_angle_step$s0
    a0_neg2 <- a0 - 2 * min_angle_step$s0
    a0_pos  <- a0 + min_angle_step$s0
    a0_pos2 <- a0 + 2 * min_angle_step$s0
    a1_neg  <- a1 - min_angle_step$s1
    a1_neg2 <- a1 - 2 * min_angle_step$s1
    a1_pos  <- a1 + min_angle_step$s1
    a1_pos2 <- a1 + 2 * min_angle_step$s1
  
    # Now check what is AND closest to line AND closer to (x1,y1)
    angle0_vec       <- NULL
    angle1_vec       <- NULL
    this_dst_to_line <- NULL
    this_dst_to_end  <- NULL
    xy_list          <- list()
    for (angle0 in c(a0_neg2, a0_neg, a0, a0_pos, a0_pos2)) for (angle1 in c(a1_neg2, a1_neg, a1, a1_pos, a1_pos2)) {
      angle0_vec       <- c(angle0_vec, angle0)
      angle1_vec       <- c(angle1_vec, angle1)
      xy               <- angle_to_pos(angle0, angle1)
      xy_list[[1 + length(xy_list)]] <- xy
      this_dst_to_line <- c(this_dst_to_line, dst(xy))
      this_dst_to_end  <- c(this_dst_to_end, sqrt((xy[1] - x1)^2 + (xy[2] - y1)^2))
    }

    middle <- (1 + length(angle0_vec)) / 2
    current_dst_to_line <- this_dst_to_line[middle]
    current_dst_to_end  <- this_dst_to_end[middle]
  
    index <- which(this_dst_to_end < current_dst_to_end) # closer to target

    if (length(index)) { # determine which is nearest to optimal line
      index_best <- index[which.min(this_dst_to_line[index])]
      a0 <- angle0_vec[index_best]
      a1 <- angle1_vec[index_best]
      a0_seq <- c(a0_seq, a0)
      a1_seq <- c(a1_seq, a1)
      x_seq  <- c(x_seq, xy_list[[index_best]][1])
      y_seq  <- c(y_seq, xy_list[[index_best]][2])
      error_seq <- c(error_seq, this_dst_to_line[index_best])
    } else {
      improving <- FALSE
    }
  }

  # Add us
  us0 <- angle_to_us(s0, a0_seq)
  us1 <- angle_to_us(s1, a1_seq)
  
  mat <- data.frame(cbind(x = x_seq, y = y_seq, angle0 = a0_seq, angle1 = a1_seq, us0 = us0, us1 = us1, pen_down = lst$pen_down, error = error_seq))
  mat[, "pen_down"] <- as.logical(mat[, "pen_down"])
  mat
}

interpolate_all_lines_to_enable_smooth_plotting <- function(mat) {
  path_mat <- NULL
  for (i in 1:nrow(mat)) path_mat <- rbind(path_mat, interpolate_line(mat[i, ]))
  path_mat
}

prepend_home <- function(mat) {
  mat <- rbind(
    c(home$x, home$y, mat$x0[1], mat$y0[1], FALSE),
    mat
  )
  class(mat$pen_down) = "logical"
  mat
}

add_pen_down <- function(mat) {
  mat$pen_down <- TRUE
  mat
}

to_all_straight_lines_needed <-function(mat) {
  mat <- smart_order_from_home(mat)
  mat <- add_pen_down(mat)
  mat <- prepend_home(mat)
  mat <- insert_move_between_unconnected_lines(mat)

  mat
}

lines_to_points <- function(mat) {
  mat <- as.matrix(mat)
  mat <- rbind(mat[, 1:2], mat[, 3:4])
  colnames(mat) <- c("x", "y")
  data.frame(mat)
}

as_line_set <- function(mat) {
  colnames(mat)[1:4] <- COLNMS
  as.data.frame(mat)
}

to_gray <- function(img) 0.2126 * img[,,1] + 0.7152 * img[,,2] + 0.0722 * img[,,3]
img_to_lines <- function(path = "img/pieter.png") {
  img <- if ("png" == stringr::str_sub(path, start=-3)) png::readPNG(path, native = F) else jpeg::readJPEG(path, native = F)  
  gr <- to_gray(img)
  gr <- gr * 255
  edge <- image.LineSegmentDetector::image_line_segment_detector(gr)
  plot(edge)
  e <- data.frame(edge[[1]][, 1:4])
  colnames(e) <- COLNMS
  e
}

fit_img_to_paper <- function(mat) {
  p <- lines_to_points(mat)

  # Rectangle proportional to a4
  a4_rect_x <- c(which(av_pos[, 150])[1]-1, 140) # mm
  a4_rect_y <- c(a4$y / 2 - diff(a4_rect_x) / a4$x * a4$y / 2, a4$y / 2 + diff(a4_rect_x) / a4$x * a4$y / 2) # mm

  # scale xy proportionally to a4 and shift to lower left corner
  # shift first
  x_shift <- a4_rect_x[1]
  y_shift <- a4_rect_y[1]
  # scale next
  x_range <- range(p$x)
  y_range <- range(p$y)
  dx <- diff(x_range)
  dy <- diff(y_range)
  fact <- if (a4$x / a4$y < dx / dy) diff(a4_rect_x) / dx else diff(a4_rect_y) / dy

  morph <- function(p, x_shift, y_shift, fact) {
    p <- data.frame(p)
    colnames(p) <- c("x", "y")
    p$x <- p$x - min(p$x)
    p$y <- p$y - min(p$y)
    p$x <- p$x * fact
    p$y <- p$y * fact
    p$x <- p$x + x_shift
    p$y <- p$y + y_shift
    p
  }

  succes <- valid_points(morph(p, x_shift, y_shift, fact))
  scaling_speed <- 1.001
  while (succes) {
    if (valid_points(morph(p, x_shift, y_shift, scaling_speed * fact))) { # scale up
      fact <- scaling_speed * fact
    } else if (valid_points(morph(p, x_shift - 1, y_shift, scaling_speed * fact))) { # shift left and scale up
      fact <- scaling_speed * fact
      x_shift <- x_shift - 1
    } else if (valid_points(morph(p, x_shift + 1, y_shift, scaling_speed * fact))) { # shift right and scale up
      fact <- scaling_speed * fact
      x_shift <- x_shift + 1
    } else if (valid_points(morph(p, x_shift, y_shift - 1, scaling_speed * fact))) { # shift down and scale up
      fact <- scaling_speed * fact
      y_shift <- y_shift - 1
    } else if (valid_points(morph(p, x_shift, y_shift + 1, scaling_speed * fact))) { # shift down and scale up
      fact <- scaling_speed * fact
      y_shift <- y_shift + 1
    } else {
      succes <- FALSE
    }  
  }

  mat <- as_line_set(cbind(morph(mat[, 1:2], x_shift, y_shift, fact), morph(mat[, 3:4], x_shift, y_shift, fact)))
  
  mat
}

# Determine valid area and make shape convex to facilitate plotting
get_valid_convex_a4_plotting_area <- function(file_name = valid_plotting_area_file) {
  if (!file.exists(file_name)) {
    current_status_dry_run <- status$dry_run
    status$dry_run <<- TRUE

    rectangle <- matrix(NA, nrow = 1 + a4$x, ncol = 1 + a4$y)
    for (i in 1:nrow(rectangle)) for (j in 1:ncol(rectangle)) {
      x <- i - 1
      y <- j - 1
      rectangle[i, j] <- all(FORBIDDEN != pos_to_angle(x, y))
    }

    # Add band preventing boundary cases
    rectangle_band <- rectangle
    for (i in 1:nrow(rectangle)) for (j in 1:ncol(rectangle)) {
      if (i < 1 + boundary_band | j < 1 + boundary_band | nrow(rectangle) - boundary_band < i | ncol(rectangle) - boundary_band < j) {
        rectangle_band[i, j] <- FALSE
      } else {
        if (any(!rectangle[boundary_band_index + i, boundary_band_index + j])) {
          rectangle_band[i, j] <- FALSE
        }
      }
    }
    rectangle <- rectangle_band

    # Make shape convex
    # This makes plotting way easier
    rectangle[1:50, ] <- FALSE

    status$dry_run <<- current_status_dry_run
    
    dput(rectangle, file_name)
  } else {
    rectangle <- dget(file_name)
  }
  
  rectangle
}






















