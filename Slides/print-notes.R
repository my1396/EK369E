#!/usr/bin/env Rscript

# print-notes.R
# Prints a xaringan deck to PDF *with* the presenter notes (the `???` blocks):
# one A4 portrait page per slide, slide on top, notes underneath.
#
# It does not touch the deck itself. It writes a throw-away copy of the
# rendered .html with theme/print-notes.css inlined as a print-only stylesheet,
# lets headless Chrome print that copy, then deletes it.
#
# Usage:
#   ./print-notes.R 2_portfolio_theory.Rmd            # uses the existing .html
#   ./print-notes.R 2_portfolio_theory.Rmd --render   # re-knit the .Rmd first
#   ./print-notes.R 2_portfolio_theory.html -o notes.pdf --zoom 0.5
#
# Options:
#   --render        knit the .Rmd before printing
#   -o FILE         output pdf (default: <deck>_notes.pdf)
#   --zoom NUM      slide size on the page, 0-1 (default 0.58; smaller = more
#                   room for long notes)
#   --wait NUM      seconds to let MathJax finish before printing (default 10)

args <- commandArgs(trailingOnly = TRUE)

get_opt <- function(flag, default = NULL) {
   i <- match(flag, args)
   if (is.na(i)) return(default)
   if (i == length(args)) stop(flag, " needs a value", call. = FALSE)
   args[i + 1]
}
has_flag <- function(flag) flag %in% args

input <- args[!startsWith(args, "-")]
input <- setdiff(input, c(get_opt("-o"), get_opt("--zoom"), get_opt("--wait")))
if (length(input) != 1) {
   stop("Usage: print-notes.R <deck.Rmd|deck.html> [--render] [-o out.pdf] [--zoom 0.58] [--wait 10]",
        call. = FALSE)
}

input <- normalizePath(input, mustWork = TRUE)
base  <- sub("\\.(Rmd|rmd|html)$", "", input)
html  <- paste0(base, ".html")

if (has_flag("--render") || !file.exists(html)) {
   rmd <- paste0(base, ".Rmd")
   if (!file.exists(rmd)) stop("No .html and no .Rmd to render: ", html, call. = FALSE)
   message("Rendering ", basename(rmd), " ...")
   rmarkdown::render(rmd, quiet = TRUE)
}

zoom   <- as.numeric(get_opt("--zoom", "0.58"))
wait   <- as.numeric(get_opt("--wait", "10"))
output <- get_opt("-o", paste0(base, "_notes.pdf"))

# theme/print-notes.css sits next to this script.
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
css_file   <- file.path(script_dir, "theme", "print-notes.css")
if (!file.exists(css_file)) stop("Missing stylesheet: ", css_file, call. = FALSE)

css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")
css <- sub("zoom: 0.58;", sprintf("zoom: %s;", zoom), css, fixed = TRUE)

# Chrome resolves libs/, images/ etc. relative to the file, so the copy has to
# live in the same directory as the deck.
tmp_html <- file.path(dirname(html), paste0(".", basename(base), "-notes-tmp.html"))

doc <- paste(readLines(html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (!grepl("</body>", doc, fixed = TRUE)) stop("No </body> in ", html, call. = FALSE)
doc <- sub("</body>",
           paste0('<style type="text/css" media="print">\n', css, '\n</style>\n</body>'),
           doc, fixed = TRUE)
writeLines(doc, tmp_html, useBytes = TRUE)

message("Printing ", basename(html), " with presenter notes ...")
# `on.exit` is useless in a top-level script, so clean up by hand either way.
ok <- tryCatch({
   pagedown::chrome_print(tmp_html, output = output, wait = wait)
   TRUE
}, error = function(e) {
   unlink(tmp_html)
   stop(conditionMessage(e), call. = FALSE)
})
unlink(tmp_html)
message("✓ ", output)
