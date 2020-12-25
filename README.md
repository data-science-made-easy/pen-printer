# Build a cheap pen-printer with R

# Buy

1. Raspberry Pi
2. PCA9685 controller with three SG90 servos
3. Power supply for both Rpi and PCA9685
4. Popsicle sticks
5. Glue
6. Clip
7. Pen

Ad 1. I have `cat /proc/device-tree/model` Raspberry Pi Model B Plus Rev 1.2 with 'Linux rpi 5.4.51+ #1333' as operating system.

## Prerequisites
On your PC or Mac install R and required libraries. You can go through the code to find out which libraries you need to install. Alternatively, you can just run the code and find out by trial and error.

On your RPi install:

- R; I have R version 3.5.2 (2018-12-20) -- "Eggshell Igloo"
- Install [pca9685servodaemon](https://github.com/data-science-made-easy/pca9685servodaemon) in the same folder as this repository; kudos to [Tim Rowledge](https://github.com/timrowledge)
- download this repository to both PC and RPi

### Hardware
The image shows the first servo is under an angle of `base_angle` = 60° = pi / 3 (small angle with paper).

![Photo](img/pen-printer.png)

## Start R on your PC
Run `pc-plot-girl.R`. This will

- load constants
  - update the constants where needed
  - e.g., the servos are now on channels (s0, first arm) 13, (s1, second arm) 14 and (s2, head with pen) 15; please update these values if you put the servos on different channels on the pca controller
- load calibration details of your servos
  - for the most accurate results, you should calibrate your servos by hand and update `servo-calibration.R`
- determine the positions within reach of the pen
- load `img/girl.jpg` and convert to lines
- maximally upscale these lines to just fit within reachable area
- plot a4 with reachable area (green) and lines
- save lines as `tasks.RData`

Now scp `tasks.RData` to this repositories folder on your RPi.

The tasks data.frame looks like this:
````
> head(tasks)
       x        y   angle0    angle1      us0      us1 pen_down      error
50.00000 148.5000 1.756192 0.5922108 1660.427 2274.649    FALSE 0.00000000
50.58450 148.2865 1.748927 0.5993667 1655.316 2269.496    FALSE 0.10211238
51.75221 148.2227 1.741662 0.6136783 1650.205 2259.191    FALSE 0.05027103
52.33360 148.0006 1.734397 0.6208342 1645.094 2254.038    FALSE 0.06092845
53.49704 147.9321 1.727132 0.6351459 1639.983 2243.732    FALSE 0.08616331
54.07519 147.7014 1.719867 0.6423017 1634.872 2238.580    FALSE 0.03410046
````

The tasks data.frame consists of interpollated minor lines that describe the major lines resulting from the edge detection. First two columsn (`x`, `y`) are the coordinates (mm), followed by angles of arms (rad) and corresponding pulsewidth of first two servos. `pen_down` indicates whether the minor line should be plotted. `error` is a pre-calculated deviation from a straight line between start point and begin point of major line. Practically this error is larger.

## Start R on your RPi
`source pi-plot-girl.R` should plot the image that is shown above.