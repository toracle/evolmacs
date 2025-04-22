#!/bin/bash

# Run tests with coverage
eldev test

# If we have the coverage file
if [ -f "coverage-final.json" ]; then
    # If we have genhtml command (from lcov package)
    if command -v genhtml >/dev/null 2>&1; then
        # Create html directory if it doesn't exist
        mkdir -p coverage/html
        
        # Convert to lcov format using json2lcov
        if command -v json2lcov >/dev/null 2>&1; then
            json2lcov coverage-final.json > coverage/coverage.info
            genhtml coverage/coverage.info --output-directory=coverage/html
            echo "Coverage report generated in coverage/html directory"
            echo "Open coverage/html/index.html in your browser to view the report"
        else
            echo "json2lcov not found. Install it with: npm install -g json2lcov"
        fi
    else
        echo "genhtml not found. Install it with your package manager (e.g., apt install lcov)"
        echo "For just the raw JSON data, see: ./coverage-final.json"
    fi
else
    echo "No coverage data found. Make sure tests ran successfully."
fi