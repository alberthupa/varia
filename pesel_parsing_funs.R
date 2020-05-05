# Functions extracting year of birth and gender from PESEL number
# Working on vectors
# Pure base R
# Following rules on https://dzidziusiowo.pl/niemowlak/pielegnacja-i-rozwoj/99-numer-pesel


# ANOTHER MAIN FUNCTION: GET DATE OF BIRTH
get_date_of_birth_from_pesel <- function(pesel_to_parse) {
  #' INNER FUNCTION TO EXTRACT DATA
  extract_date_from_pesel <- function(pesel_chr_to_parse) {
    #' filter out exclusions
    if (is.na(pesel_chr_to_parse) |
        is.null(pesel_chr_to_parse) |
        nchar(pesel_chr_to_parse) != 11 |
        grepl("[a-z]", pesel_chr_to_parse) | 
        pesel_chr_to_parse == "00000000000") {
      return(NA)
    } else {
      if (substr(pesel_chr_to_parse, 3, 3) == 0 | substr(pesel_chr_to_parse, 3, 3) == 1) {
        month_from_pesel <- as.numeric(substr(pesel_chr_to_parse, 3, 4))
        year_from_pesel <- as.numeric(paste0("19", substr(pesel_chr_to_parse, 1, 2)))
        day_from_pesel <- as.numeric(substr(pesel_chr_to_parse, 5, 6))
        date_from_pesel <- paste0(year_from_pesel, "-", month_from_pesel, "-", day_from_pesel)
        return(date_from_pesel)
      } else if (substr(pesel_chr_to_parse, 3, 3) == 2 | substr(pesel_chr_to_parse, 3, 3) == 3) {
        month_from_pesel <- as.numeric(substr(pesel_chr_to_parse, 3, 4)) - 20
        year_from_pesel <- as.numeric(paste0("20", substr(pesel_chr_to_parse, 1, 2)))
        day_from_pesel <- as.numeric(substr(pesel_chr_to_parse, 5, 6))
        date_from_pesel <- paste0(year_from_pesel, "-", month_from_pesel, "-", day_from_pesel)
        return(date_from_pesel)
      } 
    }
  }
  
  #' APPLYING FUNCTION TO VECTOR
  date_from_pesel <- tryCatch({
    date_from_pesel <- sapply(pesel_to_parse, extract_date_from_pesel, simplify = TRUE)
    date_from_pesel[sapply(date_from_pesel, is.null)] <- NA
    date_from_pesel <- unname(unlist(date_from_pesel))
    date_from_pesel <- as.Date(date_from_pesel)
    return(date_from_pesel)
  },
  error=function(cond) {
    return(NA)
  }
  )
  
  return(date_from_pesel)
  
}

# FIRST MAIN FUNCTION: GET GENDER
get_gender_from_pesel <- function(pesel_to_parse) {
  #' ODD AND EVEN NUMBERS FOR GENDER
  gender_values_men <- c("1", "3", "5", "7", "9")
  gender_values_women <- c("0", "2", "4", "6", "8")
  
  
  #' INNER FUNCTION TO EXTRACT DATA
  extract_gender_from_pesel <- function(pesel_chr_to_parse) {
    
    #' filter out exclusions
    if (is.na(pesel_chr_to_parse) |
        is.null(pesel_chr_to_parse) |
        nchar(pesel_chr_to_parse) != 11 |
        grepl("[a-z]", pesel_chr_to_parse) |
        pesel_chr_to_parse == "00000000000") {
      return(NA)
    } else {
      if (substr(pesel_chr_to_parse, 10, 10) %in% gender_values_men) {
        pesel_chr_output <- "man"
        return(pesel_chr_output)
      } else if (substr(pesel_chr_to_parse, 10, 10) %in% gender_values_women) {
        pesel_chr_output <- "woman"
        return(pesel_chr_output)
      } else {
        return(NA)
      }
    }
  }
  
  #' APPLYING FUNCTION TO VECTOR
  gender_from_pesel <- tryCatch({
    gender_from_pesel <- sapply(pesel_to_parse, extract_gender_from_pesel, simplify = TRUE)
    gender_from_pesel[sapply(gender_from_pesel, is.null)] <- NA
    gender_from_pesel <- unname(unlist(gender_from_pesel))
    return(gender_from_pesel)
  },
  error=function(cond) {
    return(NA)
  }
  )
  
  return(gender_from_pesel)
  
}
 


