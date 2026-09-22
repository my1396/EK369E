#!/usr/bin/env Rscript

# add-bookmarks.R
# Adds a clickable PDF outline (bookmarks) to a xaringan deck printed to PDF.
#
# Chrome's print-to-PDF -- what pagedown::chrome_print and renderthis::to_pdf
# use -- never writes an outline, so it has to be bolted on afterwards. The
# slide titles come from the remark source embedded in the rendered .html, and
# pdftk's `update_info_utf8` writes them into the existing .pdf without
# re-encoding a single page (no quality loss, no bloat).
#
# `#` headings become level-1 bookmarks, `##` level-2, `###` level-3, so the
# deck's section slides turn into collapsible sections in the PDF sidebar.
#
# Usage:
#   ./add-bookmarks.R 2_portfolio_theory.pdf               # bookmark in place
#   ./add-bookmarks.R 2_portfolio_theory.Rmd --render      # knit, print, bookmark
#   ./add-bookmarks.R 2_portfolio_theory.pdf --list        # preview the outline
#   ./add-bookmarks.R deck_notes.pdf -o deck_marked.pdf    # write a copy
#
# Options:
#   --list          print the outline it would write, touch nothing
#   --render        knit the .Rmd and print it to PDF first
#   --html FILE     deck html to read titles from (default: <deck>.html)
#   -o FILE         output pdf (default: overwrite the input pdf)

## ========================================================================== ##
## 1. Command line ------------------------------------------------------------
## ========================================================================== ##

args <- commandArgs(trailingOnly = TRUE)

get_opt <- function(flag, default = NULL) {
   i <- match(flag, args)
   if (is.na(i)) return(default)
   if (i == length(args)) stop(flag, " needs a value", call. = FALSE)
   args[i + 1]
}
has_flag <- function(flag) flag %in% args

input <- args[!startsWith(args, "-")]
input <- setdiff(input, c(get_opt("-o"), get_opt("--html")))
if (length(input) != 1) {
   stop("Usage: add-bookmarks.R <deck.pdf|deck.Rmd> [--render] [--list] ",
        "[--html deck.html] [-o out.pdf]", call. = FALSE)
}

input <- normalizePath(input, mustWork = TRUE)
base  <- sub("\\.(Rmd|rmd|html|pdf)$", "", input)
pdf   <- paste0(base, ".pdf")

# `<deck>_notes.pdf` holds the same slides as `<deck>.html`, so fall back to the
# un-suffixed deck rather than demanding --html for the notes handout.
html <- get_opt("--html", paste0(base, ".html"))
if (!file.exists(html)) html <- paste0(sub("_notes$", "", base), ".html")

output <- get_opt("-o", pdf)

if (has_flag("--render")) {
   rmd <- paste0(base, ".Rmd")
   if (!file.exists(rmd)) stop("Nothing to render: ", rmd, call. = FALSE)
   message("Rendering ", basename(rmd), " ...")
   rmarkdown::render(rmd, quiet = TRUE)
   message("Printing ", basename(html), " ...")
   if (requireNamespace("renderthis", quietly = TRUE)) {
      renderthis::to_pdf(html, to = pdf)
   } else {
      pagedown::chrome_print(html, output = pdf)
   }
}

if (!file.exists(html)) stop("No deck html to read titles from: ", html, call. = FALSE)
if (!file.exists(pdf))  stop("No pdf to bookmark: ", pdf, call. = FALSE)
if (!nzchar(Sys.which("pdftk"))) {
   stop("pdftk not found on PATH (brew install pdftk-java)", call. = FALSE)
}

## ========================================================================== ##
## 2. Pull the remark source back out of the rendered html --------------------
## ========================================================================== ##

# xaringan ships the whole deck as markdown inside <textarea id="source">, which
# is the same text remark.js splits into slides -- so slide n there is page n
# in the print-out.
read_slides <- function(html) {
   doc <- paste(readLines(html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
   src <- regmatches(doc, regexpr('(?s)<textarea id="source">.*?</textarea>',
                                  doc, perl = TRUE))
   if (!length(src)) stop("No remark source in ", basename(html), call. = FALSE)
   src <- sub('(?s)^<textarea id="source">(.*)</textarea>$', "\\1", src, perl = TRUE)
   entities <- c("&lt;" = "<", "&gt;" = ">", "&quot;" = '"', "&amp;" = "&")
   for (i in seq_along(entities)) {
      src <- gsub(names(entities)[i], entities[i], src, fixed = TRUE)
   }
   strsplit(paste0("\n", trimws(src), "\n"), "\n---[ \t]*\n")[[1]]
}

# Slide properties are the `key: value` lines at the very top of a slide.
slide_props <- function(txt) {
   lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
   lines <- lines[cumsum(nzchar(trimws(lines))) > 0]
   props <- character()
   for (ln in lines) {
      if (!grepl("^[a-zA-Z][a-zA-Z0-9_-]*[ \t]*:", ln)) break
      props[tolower(trimws(sub(":.*$", "", ln)))] <- trimws(sub("^[^:]*:", "", ln))
   }
   props
}

is_true <- function(props, key) isTRUE(tolower(props[key]) == "true")

# Headings arrive as raw markdown, complete with .class[] spans, $math$ and the
# odd <br/>; none of that belongs in a sidebar entry.
clean_title <- function(x) {
   x <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", x)      # [text](link) -> text
   x <- gsub("<[^>]*>", "", x)                          # html tags
   x <- gsub("\\.[A-Za-z0-9_-]+\\[", "", x)             # remark .class[ spans
   x <- gsub("\\\\[A-Za-z]+", "", x)                    # latex macros
   x <- gsub("[][$`*_]", "", x)                         # leftover markup
   x <- gsub("&nbsp;", " ", x, fixed = TRUE)
   trimws(gsub("[ \t]+", " ", x))
}

# First markdown heading of a slide, ignoring presenter notes and anything
# inside a fenced code block (R comments start with `#` too).
slide_heading <- function(txt) {
   body  <- strsplit(txt, "\n???", fixed = TRUE)[[1]][1]
   lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
   fenced <- FALSE
   for (ln in lines) {
      if (grepl("^[ \t]*```", ln)) {
         fenced <- !fenced
         next
      }
      if (fenced) next
      if (grepl("^[ \t]*#{1,4}[ \t]+\\S", ln)) {
         return(list(level = nchar(gsub("^[ \t]*(#+).*$", "\\1", ln)),
                     title = clean_title(sub("^[ \t]*#+[ \t]+", "", ln))))
      }
   }
   NULL
}

## ========================================================================== ##
## 3. Walk the deck, matching slides to printed pages -------------------------
## ========================================================================== ##

slides <- read_slides(html)
marks  <- list()
page   <- 0L

for (slide in slides) {
   props <- slide_props(slide)
   # `layout: true` slides are templates and `exclude: true` slides are dropped;
   # neither reaches the printer, so neither consumes a page.
   if (is_true(props, "layout") || is_true(props, "exclude")) next

   # Incremental `--` steps do not each get a page: xaringan prints only the
   # final state of a slide, hiding the rest with `.has-continuation`. So one
   # printed page per `---` slide.
   page <- page + 1L

   heading <- slide_heading(slide)
   if (!is.null(heading) && nzchar(heading$title)) {
      marks[[length(marks) + 1L]] <- c(heading, list(page = page))
   }
}

if (!length(marks)) stop("No headings found -- nothing to bookmark.", call. = FALSE)

# pdftk rejects an outline that skips a level, so a `###` with no `##` above it
# gets promoted to sit directly under its parent.
depth <- 0L
for (i in seq_along(marks)) {
   marks[[i]]$level <- min(marks[[i]]$level, depth + 1L)
   depth <- marks[[i]]$level
}

n_pdf <- as.integer(sub("^NumberOfPages: ", "",
                        grep("^NumberOfPages:", system2("pdftk", c(shQuote(pdf), "dump_data"),
                                                        stdout = TRUE), value = TRUE)[1]))
# A pdf printed before theme/custom.css grew its `page-break-after: avoid`
# rule still carries remark's blank final page. Trailing extras leave every
# bookmark where it belongs; a short pdf does not.
if (!is.na(n_pdf) && n_pdf > page) {
   message(sprintf("note: %s has %d pages for %d slides -- %d trailing page(s), ",
                   basename(pdf), n_pdf, page, n_pdf - page),
           "probably blank; bookmarks are unaffected")
} else if (!is.na(n_pdf) && n_pdf < page) {
   warning(sprintf("%s has %d pages but the deck has %d slides -- the pdf is stale, ",
                   basename(pdf), n_pdf, page),
           "re-print it before bookmarking", call. = FALSE, immediate. = TRUE)
}

if (has_flag("--list")) {
   for (m in marks) {
      cat(sprintf("%s%-3d %s\n", strrep("  ", m$level - 1L), m$page, m$title))
   }
   quit(save = "no")
}

## ========================================================================== ##
## 4. Write the outline into the pdf ------------------------------------------
## ========================================================================== ##

# Keep the file's existing metadata: dump it, strip whatever outline is already
# there, and hand the whole thing back with the new bookmarks appended.
dump <- system2("pdftk", c(shQuote(pdf), "dump_data_utf8"), stdout = TRUE)
info <- c(dump[!grepl("^Bookmark", dump)],
          unlist(lapply(marks, function(m) {
             c("BookmarkBegin",
               paste0("BookmarkTitle: ", m$title),
               paste0("BookmarkLevel: ", m$level),
               paste0("BookmarkPageNumber: ", m$page))
          })))

info_file <- tempfile(fileext = ".txt")
tmp_pdf   <- tempfile(fileext = ".pdf")
writeLines(info, info_file, useBytes = TRUE)

status <- system2("pdftk", c(shQuote(pdf), "update_info_utf8", shQuote(info_file),
                             "output", shQuote(tmp_pdf)))
unlink(info_file)
if (status != 0 || !file.exists(tmp_pdf)) stop("pdftk failed", call. = FALSE)

if (!file.copy(tmp_pdf, output, overwrite = TRUE)) {
   unlink(tmp_pdf)
   stop("Could not write ", output, call. = FALSE)
}
unlink(tmp_pdf)

message(sprintf("✓ %s -- %d bookmarks over %d pages",
                basename(output), length(marks), page))
