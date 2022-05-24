#!/bin/bash

nohup Rscript bin/main.R --cross_val --config config/cross_val.json &
# Add compute analytics after refactor
