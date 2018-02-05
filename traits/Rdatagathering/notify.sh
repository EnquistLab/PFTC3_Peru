#!/bin/sh
cd ~/peru/PFTC3_Peru/traits/Rdatagathering/

inotifywait -m -e create /home/pi/Desktop/ | Rscript run_check_image.R
  