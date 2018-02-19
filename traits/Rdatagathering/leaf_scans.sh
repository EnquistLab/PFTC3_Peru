#!/bin/sh
cd ~/peru/PFTC3_Peru/traits/Rdatagathering/

xsane

while true
do
  inotifywait -e create /home/pi/Desktop/Peru_leaves |  Rscript run_check_image.R
done 
