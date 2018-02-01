#!/bin/sh
cd /home/pi/peru/PFT3_Peru/traits/Rdatagathering
while inotifywait -e create /home/pi/Desktop/ > new.txt; do
  Rscript check_image.R
  fi
