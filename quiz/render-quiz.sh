#!/bin/bash

# render-quiz.sh
# Renders a Quarto document and automatically appends suffix based on params.solution:
#   - params.solution: true  → _solution.pdf
#   - params.solution: false → _question.pdf
# Creates a symlink with the original name for Quarto Preview compatibility
# Usage: ./render-quiz.sh quiz_05_logistic_regression.qmd

if [ $# -eq 0 ]; then
    echo "Usage: $0 <qmd_file>"
    exit 1
fi

QMD_FILE="$1"
BASE_NAME="${QMD_FILE%.qmd}"

# Render the document
echo "Rendering $QMD_FILE..."
quarto render "$QMD_FILE" --no-execute-daemon

# Check if params.solution is true (and not commented out)
if grep -q "^\s*solution:\s*true" "$QMD_FILE" && ! grep -q "^\s*#.*solution:\s*true" "$QMD_FILE"; then
    # Solution mode - rename to _solution.pdf
    if [ -f "${BASE_NAME}.pdf" ]; then
        mv "${BASE_NAME}.pdf" "${BASE_NAME}_solution.pdf"
        echo "✓ Renamed to: ${BASE_NAME}_solution.pdf"
        # Create symlink for preview (remove old symlink first if exists)
        rm -f "${BASE_NAME}.pdf"
        ln -sf "${BASE_NAME}_solution.pdf" "${BASE_NAME}.pdf"
        echo "✓ Created preview symlink: ${BASE_NAME}.pdf → ${BASE_NAME}_solution.pdf"
    fi
else
    # Question mode - rename to _question.pdf
    if [ -f "${BASE_NAME}.pdf" ]; then
        mv "${BASE_NAME}.pdf" "${BASE_NAME}_question.pdf"
        echo "✓ Renamed to: ${BASE_NAME}_question.pdf"
        # Create symlink for preview (remove old symlink first if exists)
        rm -f "${BASE_NAME}.pdf"
        ln -sf "${BASE_NAME}_question.pdf" "${BASE_NAME}.pdf"
        echo "✓ Created preview symlink: ${BASE_NAME}.pdf → ${BASE_NAME}_question.pdf"
    fi
fi
