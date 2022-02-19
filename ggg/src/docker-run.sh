#!/usr/bin/env bash

# Construct the output directory structure
#python3 ./src/build_paths.py

python3 ./part1.1.py
java -cp test.jar GeneratedCalculator > output/mi.json
python3 ./part1.py



python3 ./logistic_map.py
python3 ./part4.py

#matlab -batch "MIcalc"
#matlab -batch "TEcalc"
#matlab -batch "plotCI"
