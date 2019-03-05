# Insert line break for titles with multiple words
make_breaks <- function(x) {
    library(stringr)
    spaces <- str_count(x, " ")
    if( spaces < 2 ) {
        return(x)
    } else {
        spaces <- ceiling(spaces/2)
        all_spaces <- stringr::str_locate_all(x, " ")
        idx <- all_spaces[[1]][spaces]  # this is where to insert line break
        substr(x, idx, idx) <- "\n"
        x
        
    }
    
}