
#FUNCTIONS -----
#get nonUS/nonCarn inst
fix_non_carn <- function(x){
  case_when(
    str_detect(x, "Calagary") ~ "University Of Calgary",
    str_detect(x, "Curie") ~ "Pierre And Marie Curie University",
    str_detect(x, "Z?rich|Zurich") ~ "University Of Zurich",
    str_detect(x, "Qub") ~ "Queens University Belfast",
    str_detect(x, "Max Planck") ~ "Max Planck Institute",
    str_detect(x, "Oklahoma Medical Research") ~ "Oklahoma Medical Research Foundation",
    str_detect(x, "Tifr") ~ "National Center For Biological Sciences University Tifr India",
    str_detect(x, "Scripps Research") ~ "Scripps Research Institute",
    str_detect(x, "Imp Vienna") ~ "Imp University Vienna",
    TRUE ~ x
  )
}