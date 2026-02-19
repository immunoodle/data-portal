#!/bin/bash

# run_app.sh - Wrapper to run the MADI Data Portal locally
# Ensures Rscript is found and provides instructions

echo "========================================"
echo "   MADI Data Portal - Local Launcher    "
echo "========================================"

# Check if Rscript is available
if ! command -v Rscript &> /dev/null; then
    echo "❌ Error: Rscript could not be found."
    echo "Please ensure R is installed and Rscript is in your PATH."
    exit 1
fi

echo "Starting R Shiny App..."
echo "Logs will appear below."
echo "----------------------------------------"

# Run the R script
Rscript run_locally.R
