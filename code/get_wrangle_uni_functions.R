#correllary to %in% function
`%not_in%` <- Negate(`%in%`)

#function to repair abbreviated college and university names
fix_inst_name <- function(x){
  
  x <- x
  
  if(str_detect(x, "College|University") == FALSE){
    
    case_when(
      str_detect(x, "College Of St Benedict Saint Johns University") ~ "College Of St Benedict",
      str_detect(x, "Sam Houston State$|Oregon Health And Science$|Tennessee State$|Bridgewater State$|North Carolina State$|Michigan State$|Kennesaw State$|Sam Houston State$|Georgia State$|Wichita State$|Auburn$|Arkansas Tech$|Colgate$|Vanderbilt$|Rockefeller$") ~ paste0(x, " University"),
      str_detect(x, "Springfield$|Dalton State$|Occidental$|Kentucky Wesleyan$") ~ paste0(x, " College"),
      str_detect(x, "State Billings") ~ "Montana State University Billings",
      str_detect(x, "New School .*") ~ "New School",
      str_detect(x, mult_loc_uni_last_list) ~ str_replace(x, ",", " University"),
      str_detect(x, mult_loc_uni_of_list) ~ paste0("University Of", x),
      str_detect(x, mult_loc_coll_of_list) ~ paste0("College Of", x),
      str_detect(x, mult_loc_college_last_list) ~ str_replace(x, ",", " College"),
      str_detect(x, x_college_list) ~ paste0(x, " College"),
      str_detect(x, x_uni_list) ~ paste0(x, " University"),
      TRUE ~ x
    )}else{
      x
    }
}

#function to expand common institutional acronyms
fix_inst_abbrv <- function(x){
  case_when(
    str_detect(x, "Geisinger") ~ "Geisinger Health System",
    str_detect(x, "ern Miss") ~ "University Of Southern Mississippi",
    str_detect(x, "Uk |Germany$") ~ str_remove(x, "Uk |Germany$"),
    str_detect(x, "Crick") ~ "Francis Crick Institute",
    str_detect(x, "Sickkid") ~ "Hospital For Sick Children",
    str_detect(x, "Ramapo") ~ "Ramapo College Of New Jersey",
    str_detect(x, "Memorial") ~ "Memorial University of Newfoundland And Labrador",
    str_detect(x, "Nih") ~ str_replace(x, "Nih", "National Institutes Of Health"),
    str_detect(x, "West Chester") ~ "West Chester University Of Pennsylvania",
    str_detect(x, "Cold Spring|Watson School") ~ "Cold Spring Harbor Laboratory",
    str_detect(x, "Wake Forest") ~ "Wake Forest University",
    str_detect(x, "Wurzburg") ~ "Julius Maximilian University Of Würzburg",
    str_detect(x, "Sask") ~ "University of Saskatchewann",
    str_detect(x, "Marie Curie") ~ "Sorbonne University",
    str_detect(x, "Liège") ~ "Université De Liège",
    str_detect(x, "Eindhoven") ~ "Eindhoven University of Technology",
    str_detect(x, "Stowers") ~ "Stowers Institute for Medical Research",
    str_detect(x, "Sloan") ~ "Memorial Sloan Kettering Cancer Center",
    str_detect(x, "Salk") ~ "Salk Institute For Biological Studies",
    str_detect(x, "Riken") ~ "Institute Of Physical And Chemical Research (Riken)",
    str_detect(x, "^Oxford") ~ "University Of Oxford",
    str_detect(x, "Ocad") ~ "Ontario College Of Art And Design University",
    str_detect(x, "Notre Dame") ~ "University Of Notre Dame",
    str_detect(x, "Mcgill") ~ "Mcgill University",
    str_detect(x, "Liverpool") ~ "University Of Liverpool",
    str_detect(x, "Jamstec") ~ "Japan Agency For Marine Earth Science And Technology",
    str_detect(x, "Humber") ~ "Humber Institute Of Technology And Advanced Learning",
    str_detect(x, "Hec Montreal|De Montreal|Of Montreal") ~ "Université De Montréal",
    str_detect(x, "Fred Hutch") ~ "Fred Hutchinson Cancer Research Center",
    str_detect(x, "Embl|European Bioinformatics Institute") ~ "European Molecular Biology Laboratory",
    str_detect(x, "Cern") ~ "European Organization For Nuclear Research CERN",
    str_detect(x, "Iitbombay|Iit$") ~ "Indian Institute Of Technology Bombay",
    str_detect(x, "Cdc") ~ "Centers For Disease Control And Prevention",
    str_detect(x, "Hhmi Janelia") ~ "Janelia Research Campus",
    str_detect(x, "Niehs") ~ "National Institute Of Environmental Health Sciences",
    str_detect(x, "Calagary") ~ "University Of Calgary",
    str_detect(x, "Z?rich|Zurich") ~ "University Of Zurich",
    str_detect(x, "Qub") ~ "Queens University Belfast",
    str_detect(x, "Max Planck") ~ "Max Planck Institute",
    str_detect(x, "Oklahoma Medical Research") ~ "Oklahoma Medical Research Foundation",
    str_detect(x, "Tifr") ~ "Tata Institute of Fundamental Research",
    str_detect(x, "Imp Vienna") ~ "Research Institute of Molecular Pathology",
    str_detect(x, "Rcpi") ~ "Royal College Of Physicians Of Ireland",
    str_detect(x, "Igc Oeiras") ~ "Instituto Gulbenkian De Ciencia",
    str_detect(x, "^Cide") ~ "Centro De Investigacion Y Docencia Economicas",
    str_detect(x, "Lmcb|Ucl$") ~ "University College London",
    str_detect(x, "Upenn|Hospital Of Philadelphia") ~ "University Of Pennsylvania",
    str_detect(x, "Emory$|Emory University (?!Oxford.*)") ~ "Emory University",
    str_detect(x, "^Berkeley|Uc Berkeley") ~ "University Of California Berkeley",
    str_detect(x, "Nyit") ~ "New York Institute Of Technology",
    str_detect(x, "Uams") ~ "University of Arkansas For Medical Sciences",
    str_detect(x, "Of Nj") ~ str_replace(x, "Of Nj", "Of New Jersey"),
    str_detect(x, "^Us ") ~ str_replace(x, "^Us ", "United States "),
    str_detect(x, "Usc$") ~ "University Of Southern California",
    str_detect(x, "Ucsd|Scripps.* Oceanography") ~ "University Of California San Diego",
    str_detect(x, "Ucla") ~ "University Of California Los Angeles",
    str_detect(x, "Ucsf") ~ "University of California San Francisco",
    str_detect(x, "Ucsb") ~ "University Of California Santa Barbara",
    str_detect(x, "Uiuc") ~ "University Of Illinois Urbana Champaign",
    str_detect(x, "Uab") ~ "University Of Alabama Birmingham",
    str_detect(x, "Unlv") ~ "University Of Nevada Las Vegas",
    str_detect(x, "Umaine") ~ "University Of Maine",
    str_detect(x, "Buffalo State") ~ "Suny Buffalo State",
    str_detect(x, "Suny Upstate") ~ str_remove(x, "Suny "),
    str_detect(x, "Cuny Graduate Center") ~ "Cuny Graduate School And University Center",
    str_detect(x, "Rose Hulman") ~ "Rose Hulman Institute Of Technology",
    str_detect(x, "Uthscsa") ~ "University Of Texas Health Science Center San Antonio",
    str_detect(x, "Uthsc") ~ "University Of Tennessee Health Science Center",
    str_detect(x, "University.*(?=Albany)") ~ "SUNY Albany",
    str_detect(x, "Uofsc") ~ "University Of South Carolina",
    str_detect(x, "Smu") ~ "Southern Methodist University",
    str_detect(x, "William And Mary") ~ "College Of William And Mary",
    str_detect(x, "Rutgers Medical School") ~ "Rutgers University Newark",
    str_detect(x, "Utsw|Ut (?=South)|Texas Southwestern$") ~ "University Of Texas Southwestern Medical Center",
    str_detect(x, "Md Anderson") ~ "The University Of Texas Md Anderson Cancer Center",
    str_detect(x, "Unc ") ~ str_replace(x,"Unc,?", "University Of North Carolina "),
    str_detect(x, "Cal State|Csu ") ~ str_replace(x, "Cal State|Csu", "California State University"),
    str_detect(x, "Umbc") ~ "University Of Maryland Baltimore County",
    str_detect(x, "Umn" ) ~ "University Of Minnesota",
    str_detect(x, "Mit|Broad Institute" ) ~ "Massachusetts Institute Of Technology",
    str_detect(x, "Vcu" ) ~ "Virginia Commonwealth University",
    str_detect(x, "Utep") ~ "University Of Texas El Paso",
    str_detect(x, "Ut (?=Dallas)|Ut (?=Austin)") ~ str_replace(x,"Ut", "University Of Texas"),
    str_detect(x, "Cc") ~  str_replace(x, "Cc", "Community College"),
    str_detect(x, "Nc\\s?") ~ str_replace(x,"Nc","North Carolina"),
    str_detect(x, "Nw") ~  str_replace(x,"Nw", "Northwest"),
    str_detect(x, "Nyu") ~ "New York University",
    str_detect(x, "Uw")  ~ str_replace(x,"Uw", "University Of Wisconsin"),
    str_detect(x, "Uc (?=Denver)|Uc (?=Boulder)") ~ str_replace(x,"Uc", "University Of Colorado"),
    str_detect(x, "Um (?=Ann Arbor)|Um (?=Dearborn)|Um (?=Flint)") ~ str_replace(x, "Um", "University Of Michigan"),
    str_detect(x, "Uchic") ~ "University Of Chicago",
    str_detect(x, "Umich") ~ "University Of Michigan Ann Arbor",
    str_detect(x, "Uc ")  ~ str_replace(x,"Uc", "University Of California"),
    str_detect(x, "Unh") ~ "University Of New Hampshire",
    str_detect(x, "St Bonaventure") ~ "St Bonaventure University",
    str_detect(x, "Penn |Penn$") ~ str_replace(x, "Penn |Penn$", "Pennsylvania "),
    str_detect(x, "Uva") ~ "University Of Virginia",
    str_detect(x, "Uvm") ~ "University Of Vermont",
    str_detect(x, "Tulane") ~ "Tulane University Of Louisiana",
    str_detect(x, "Lsu|Louisiana State University$") ~ "Louisiana State University And Agricultural And Mechanical College",
    str_detect(x, "Umass") ~ str_replace(x, "Umass", "University Of Massachusetts"),
    str_detect(x, "Ky$") ~ str_replace(x, "Ky$", "Kentucky"),
    str_detect(x, "Wi ") ~ str_replace(x, "Wi ", "Wisconsin "),
    str_detect(x, "Mn|Minn ") ~ str_replace(x, "Mn|Minn", "Minnesota"),
    str_detect(x, "Mich ") ~ str_replace(x, "Mich ", "Michigan "),
    str_detect(x, "Cal Poly San Luis") ~ str_replace(x, "Cal Poly", "California Polytechnic State University San Luis Obispo"),
    str_detect(x, "Cal Poly|University Pomona") ~ "California State Polytechnic University Pomona",
    str_detect(x, "Dc") ~ str_replace(x, "Dc", "District Of Columbia"),
    str_detect(x, "Albany Med$") ~ "Albany Medical College",
    str_detect(x, "Wash University Stl|Rehabilitation And Participation Science") ~ "Washington University St Louis",
    str_detect(x, "Carolina Ch") ~ str_replace(x, "Ch$", "Chapel Hill"),
    str_detect(x, "Virginia Tech|Virginia Polytechnic") ~ "Virginia Polytechnic Institute And State University",
    str_detect(x, "Wake Forest") ~ "Wake Forest University", 
    str_detect(x, "Harvard|Boston Children") ~ "Harvard University",
    str_detect(x, "Mount Sinai") ~ "Icahn School Of Medicine Mount Sinai",
    str_detect(x, "Duke") ~ "Duke University",
    str_detect(x, "Johns Hopkins|Hopkins$|Kennedy Krieger") ~ "Johns Hopkins University",
    str_detect(x, "Caltech") ~ "California Institute Of Technology",
    str_detect(x, "Mayo") ~ "Mayo Clinic College Of Medicine And Science",
    str_detect(x, "Weill Cornell") ~ "Weill Cornell Medical College",
    str_detect(x, "Columbia U|Columbia University$") ~ "Columbia University City Of New York",
    str_detect(x, "Yale") ~ "Yale University",
    str_detect(x, "Stanford") ~ "Stanford University",
    str_detect(x, "North Carolina Agricultural And Technical") ~ "North Carolina A And T State University",
    str_detect(x, "A And T") ~ paste0(x, " State University"),
    str_detect(x, "Aandm|AAndM") ~ str_replace(x, "Aandm|AAndM", "A And M"),
    str_detect(x, "Aandt|AAndT") ~ str_replace(x, "Aandt|AAndT", "A And T"),
    str_detect(x, "Wayne") ~ "Wayne State University",
    str_detect(x, "Of Texas Med") ~ "University Of Texas Medical Branch",
    str_detect(x, "St Mary") ~ "St Marys College Of California",
    str_detect(x, "Of Massachusetts Medical") ~ paste0(x, " Worcester"),
    str_detect(x, "Merced") ~ "University Of California Merced",
    str_detect(x, "Western Michigan.*Medicine$") ~ "Western Michigan University Homer Stryker Md School Of Medicine",
    str_detect(x, "Of Rochester|Georgetown") ~ str_remove(x, " Medical Center"),
    str_detect(x, "West Virginia") ~ str_replace(x, "School", "Institute"),
    str_detect(x, "Champaign Urbana") ~ str_replace(x, "Champaign Urbana", "Urbana Champaign"),
    str_detect(x, "California Institute Of Teaching|California Sta.*sity$") ~ "California State University Long Beach",
    TRUE ~ x
  )
}

#appends campus information to main/primary institutional names
fix_campus <- function(x){
  case_when(
    str_detect(x, "Concordia University$") ~ "Concordia University Montreal",
    str_detect(x, "Helsinki.*") ~ str_remove(x, "(?<=Helsinki).*"),
    str_detect(x, "Basque.*") ~ str_remove(x, "(?<=Country).*"),
    str_detect(x, "College London .*") ~ str_remove(x, "(?<=London).*"),
    str_detect(x, "St Josephs College") ~ "St Josephs College New York",
    str_detect(x, "Scripps Research") ~ "Scripps Research Institute",
    str_detect(x, "Scripps Florida") ~ "University of Florida", 
    str_detect(x, "Planck Florida") ~ "Max Planck Florida Institute For Neuroscience",
    str_detect(x, "Max Planck In.*|Mpi Dresden") ~ "Max Planck Institute",
    str_detect(x, "Uni.* Arkansas$|Arkansas Fayetteville$") ~ "University Of Arkansas",
    str_detect(x, "Sacramento State") ~ "California State University Sacramento",
    str_detect(x, "Idaho Moscow") ~ "University Of Idaho",
    str_detect(x, "Of Kansas .*") ~ str_remove(x, "(?<=Kansas).*"),
    str_detect(x, "Of Southern California .*") ~ str_remove(x, "(?<=California).*"),
    str_detect(x, "Louisiana Stare") ~ "Louisiana State University And Agricultural And Mechanical College",
    str_detect(x, "New York Uni.*|Northeastern Uni.*|Villanova Uni.*|Tufts Uni.*|Vanderbilt Uni.*|	
Kennesaw State") ~ str_remove(x, "(?<=University).*"),
    str_detect(x, "Rhode Island Graduate School.*") ~ str_remove(x, "Graduate.*"),
    str_detect(x, "Arizona State University$|Arizona State$") ~ "Arizona State University Tempe",
    str_detect(x, "Pennsylvania State University$|Pennsylvania State$|University Park$") ~ "Pennsylvania State University Main",
    str_detect(x, "Indiana University$") ~ "Indiana University Bloomington",
    str_detect(x, "Indiana University School Of Medicine") ~ "Indiana University Purdue University Indianapolis",
    str_detect(x, "Miami University") ~ "Miami University Oxford",
    str_detect(x, "Uni.* Arizona$|Arizona Tucson$") ~ "University Of Arizona",
    str_detect(x, "Uni.* South Carolina$") ~ "University Of South Carolina Columbia",
    str_detect(x, "Uni.* Michigan$") ~ "University Of Michigan Ann Arbor",
    str_detect(x, "Uni.* Wisconsin$|Uni.* Wiscsonsin$") ~ "University Of Wisconsin Madison",
    str_detect(x, "Midwestern University$") ~ "Midwestern University Downers Grove",
    str_detect(x, "Uni.* Minnesota$|Minnesota Minneapolis$|Minnesota School|Minnesota Medical School") ~ "University Of Minnesota Twin Cities",
    str_detect(x, "Uni.* Hawaii$|Uni.* Hawai'i$") ~ "University Of Hawaii Manoa",
    str_detect(x, "Brigham Young University$|Byu$") ~ "Brigham Young University Provo",
    str_detect(x, "Ohio State University$|Ohio State$|Ohio.* Columbus") ~ "Ohio State University Main",
    str_detect(x, "Uni.* Cincinnati$|Cincinnati Children") ~ "University Of Cincinnati Main",
    str_detect(x, "Rutgers Newark") ~ "Rutgers University Newark",
    str_detect(x, "Rutgers$|Rutgers University.?") ~ "Rutgers University New Brunswick",
    str_detect(x, "Pennsylvania State Uni.* Behrend|Pennsylvania State Behrend") ~ "Pennsylvania State University Erie Behrend College",
    str_detect(x, "Pennsylvania State Scranton") ~ "Pennsylvania State University Worthington Scranton",
    str_detect(x, "Northwest Missouri") ~ "Northwest Missouri State University",
    str_detect(x, "Uni.* Of Missouri") ~ "University Of Missouri Columbia",
    str_detect(x, "Missouri State Uni.*$") ~ "Missouri State University Springfield",
    str_detect(x, "Uni.* Of Montana Missoula") ~ str_remove(x, "Missoula"),
    str_detect(x, "Dartmouth|Dartmouth School Of Medicine") ~ "Dartmouth College",
    str_detect(x, "Uni.* Maryland Medical School|Uni.* Maryland School Of Medicine") ~ "University Of Maryland Baltimore",
    str_detect(x, "Uni.* Maryland$|Maryland College") ~ "University Of Maryland College Park",
    str_detect(x, "Hostos|Queens College") ~ paste0("Cuny ", x),
    str_detect(x, "Stony Brook") ~ "Stony Brook University",
    str_detect(x, "Georgia I|Georgia Te") ~ "Georgia Institute Of Technology Main",
    str_detect(x, "Akron$") ~ "University Of Akron Main",
    str_detect(x, "North Carolina State University") ~ "North Carolina State University Raleigh",
    str_detect(x, "Of North Carolina$|Chapel Hill .*") ~ "University Of North Carolina Chapel Hill",
    str_detect(x, "Of Tennessee$|Uni.* Of Knoxville") ~ "University Of Tennessee Knoxville",
    str_detect(x, "Anschutz|Colorado Denver|Colorado$|Colorado Medical|Colorado .* Medicine") ~ "University Of Colorado Denver Anschutz Medical",
    str_detect(x, "Purdue$|Purdue University$|Purdue .* West Lafayette") ~ "Purdue University Main",
    str_detect(x, "(?<!North )Texas$") ~ "University Of Texas Austin",
    str_detect(x, "(?<!Central )Oklahoma$") ~ "University Of Oklahoma Norman",
    str_detect(x, "Pittsburgh$") ~ "University Of Pittsburgh Pittsburgh",
    str_detect(x, "South Florida|Moffitt Cancer") ~ "University Of South Florida Main",
    str_detect(x, "Washington State") ~ "Washington State University",
    str_detect(x, "Colorado State") ~ "Colorado State University Fort Collins",
    str_detect(x, "Washington University") ~ "Washington University St Louis",
    str_detect(x, "Of Washington") ~ "University Of Washington Seattle",
    str_detect(x, "Case Western") ~ "Case Western Reserve University", 
    str_detect(x, "Hampshire") ~ "University Of New Hampshire Main",
    str_detect(x, "New Mexico (?=State)|Of New Mexico (?=Health)|Arkansas State|North Dakota|Of Virginia|Bowling Green|Wright State University$|Oklahoma State University$|Ohio University$|Uni.*New Mexico ") ~ paste(x, " Main"),
    str_detect(x, "Southern Ill") ~ "Southern Illinois University Carbondale",
    str_detect(x, "Embry") ~ "Embry Riddle Aeronautical University Daytona Beach",
    str_detect(x, "Texas A And M$|Texas A And M University$") ~ "Texas A And M University College Station",
    str_detect(x, "Texas A And M .* Galveston$") ~ "Texas A And M University Texarkana",
    str_detect(x, "Of Nebraska$") ~ paste(x, " Lincoln"),
    str_detect(x, "Health Shreveport") ~ "Louisiana State University Health Sciences Center Shreveport",
    str_detect(x, "Kent State") ~ "Kent State University Kent",
    str_detect(x, "Mcgovern|Texas Health$") ~ "University Of Texas Health Science Center Houston",
    str_detect(x, "Emmanuel|Ithaca") ~ str_remove(x, " Boston| Ithaca"),
    str_detect(x, "Nevada Reno") ~ "University Of Nevada Reno",
    str_detect(x, "Central Lakes C") ~ "Central Lakes College Brainerd",
    str_detect(x, "University Of Illinois$") ~ "University Of Illinois Urbana Champaign",
    str_detect(x, "University Of Massachusetts$") ~ "University Of Massachusetts Amherst",
    TRUE ~ x
  )
}


#clean up SUNY/CUNY university names
replace_uny <- function(x){
  
  x <- x
  
  if(str_detect(x, "Cuny|Suny|City University Of New York|State University Of New York") == TRUE){
    
    which_uny <- if_else(str_detect(str_to_title(x), "Cuny|City Of New|City.* New"), "CUNY", "SUNY")
    
    if(which_uny == "CUNY"){
      no_cuny <- str_remove_all(x, " \\(Cuny\\)|CUNY$|Cuny$|^Cuny|Cuny|City University of New York")
      replace <- case_when(
        str_detect(x, "City College") ~ "Cuny City College",
        str_detect(x, "Graduate Center") ~ "Cuny Graduate School And University Center",
        str_detect(x, "Advanced Science Research Center") ~ "Cuny Graduate School And University Center",
        TRUE ~ paste0("Cuny", no_cuny)
      )
    }else{
      move <- str_replace(x, "(?<=Suny) .+ At|(?<=Suny) At|State University Of New York", "Suny")
      replace <- case_when(
        str_detect(x, "Suny Esf") ~ "Suny College Of Environmental Science And Forestry",
        str_detect(move, "(?<!University) Buffalo$|(?<!Of) Buffalo$") ~ str_replace(move, "(?<!University)  Buffalo$", "Buffalo State"),
        str_detect(move, "Brockport") ~ "Suny College Brockport",
        str_detect(x, "Suny Upstate") ~ "Upstate Medical University",
        TRUE ~ move
      )
    }
    
    return(replace)
    
  }else{
    return(x)
    }
  
}