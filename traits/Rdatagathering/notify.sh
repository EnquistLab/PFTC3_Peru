#!/bin/sh
cd ~/peru/PFT3C_Peru/traits/Rdatagathering/

inotifywait -e create /home/pi/Desktop/ | Rscript run_check_image.R
  