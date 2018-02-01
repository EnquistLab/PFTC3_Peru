#!/bin/sh
cd /home/pi/peru
while inotifywait -e create /home/pi/Desktop/ > new.txt; do
  Rscript traits/Rdatagathering/check_image.R
  fi
