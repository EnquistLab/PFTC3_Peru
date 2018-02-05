#!/bin/sh
cd ~/peru/PFTC3_Peru/traits/Rdatagathering/

while inotifywait -e create /home/pi/Desktop/ do
Rscript run_check_image.R
done  