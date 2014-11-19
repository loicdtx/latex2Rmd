#' Latex to R markdown converter
#' 
#' @description Does 70 percent of the job, the rest is manual edits.
#' 
#' @param input Character Filename of input file
#' @param output character Filename of output file
#' 
#' @details I recommend to first transfer the content of the latex file to a RStudio Rmd template (removing headers, packages importts, etc) and then run the function.
#' 
#' What it doesn't do:
#' \itemize{
#'  \item Insert figures from external sources.
#'  \item Differentiate ordered lists from unordered lists
#'  \item Fix references to figures in the text
#'  \item Adapt file headers and so on
#' }
#' 
#' @author Loic Dutrieux
#' 
#' @import stringr
#' 
#' @export

latex2Rmd <- function(input, output) {
    text <- readChar(input, file.info(input)$size)
    
    text <- str_replace_all(string = text, pattern = '<<(.*?)>>=(.*?)@', replacement = '```{r, \\1}\\2```')
    
    # Order these in likelihood of nestedness
    text <- str_replace_all(string = text, pattern = '\\\\href\\{(.*?)\\}\\{(.*?)\\}', replacement = '[\\2](\\1)')
    text <- str_replace_all(string = text, pattern = '\\\\url\\{(.*?)\\}', replacement = '[\\1]')
    text <- str_replace_all(string = text, pattern = '\\\\code\\{(.*?)\\}', replacement = '`\\1`')
    text <- str_replace_all(string = text, pattern = '\\\\texttt\\{(.*?)\\}', replacement = '`\\1`')
    text <- str_replace_all(string = text, pattern = '\\\\textbf\\{(.*?)\\}', replacement = '`\\1`')
    text <- str_replace_all(string = text, pattern = '\\\\emph\\{(.*?)\\}', replacement = '*\\1*')
    text <- str_replace_all(string = text, pattern = '\\\\textit\\{(.*?)\\}', replacement = '*\\1*')
    
    text <- str_replace_all(string = text, pattern = '\\\\section\\*?\\{(.*?)\\}', replacement = '# \\1')
    text <- str_replace_all(string = text, pattern = '\\\\subsection\\*?\\{(.*?)\\}', replacement = '## \\1')
    text <- str_replace_all(string = text, pattern = '\\\\subsubsection\\*?\\{(.*?)\\}', replacement = '### \\1')
    
    
    text <- str_replace_all(string = text, pattern = '\\\\begin\\{(.*?)\\}', replacement = '')
    text <- str_replace_all(string = text, pattern = '\\\\end\\{(.*?)\\}', replacement = '')
    text <- str_replace_all(string = text, pattern = '\\\\item', replacement = '*')
    
    text <- str_replace_all(string = text, pattern = '\\\\', replacement = '\\\n')
    
    text <- str_replace_all(string = text, pattern = '\\\r', replacement = '')
    
    
    writeLines(text, output, sep = '')
}