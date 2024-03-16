# This is a script used to all the stats.

# clear environment
rm( list = ls() )

# list packages to be used
pkgs <- c("here","tidyverse","gt","psych")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare folders for the outcomes
sapply( c("tabs","figs"), function(i) if( !dir.exists(i) ) dir.create(i) )


# DATA READ ----

d1 <- read.csv( here("_data","long_df.csv"), sep = "," )
d2 <- read.csv( here("_data","wide_df.csv"), sep = "," )


# IN-HOUSE FUNCTIONS ----

# numbers shinaningans
rprint <- function( x, dec=2 ) sprintf( paste0("%.",dec,"f"), round( x, dec ) ) # printing rounded numbers
zerolead <- function(x) sub( "0.", ".", x, fixed = T ) # get rid of leading zero

# collapse table to a cell
tabcol <- function(x,na=F) paste( table( x, useNA = if(na) "always" ), collapse = "/" )

# summarise central tendency and variability
cenvar <-
  
  function(x, ct = "mean", var = "sd", dec = 2, sep = " ± ", end = "" ) {
    
    paste0(
      rprint( do.call( ct,list(x, na.rm = T) ), dec), sep,
      rprint( do.call(var,list(x, na.rm = T) ), dec), end
    )
    
  }


# DESCRIPTIVE STATS ----

# prepare descriptive stats table
t1 <-
  
  lapply(
    
    c("pre","post"),
    function(i)
      
      data.frame(
        
        var = c("Time from surgery (years)",
                "Age (years)",
                "Education (years)",
                "Sex (females/males)",
                "Disease duration (years)",
                "LEDD (mg)",
                "Weight (kg)",
                "BMI (kg/m2)",
                "BDI-II (range 0-63)",
                "DRS-2 (range 0-144)"
        ),
        
        val = c( sapply( c("stimtime_years","age_years","edu_years"), function(j) cenvar( unlist( d1[ d1$event==i, j] ) ) ),
                 tabcol(d2$sex),
                 sapply( c("pd_dur","ledd_mg","weight","bmi","bdi","drsii"), function(j) cenvar( unlist( d1[d1$event==i, j] ) ) )
        ),
        
        event = i
        
      )
    
  ) %>%
  
  do.call( rbind.data.frame, . ) %>%
  pivot_wider( names_from = "event", values_from = "val" ) %>%
  
  gt() %>%
  cols_align( columns = -1, align = "center" ) %>%
  cols_label(
    var ~ "",
    pre ~ "Pre-surgery",
    post ~ "Post-surgery"
  ) %>%
  tab_source_note( source_note = "LEDD = levodopa equivalent daily dose, BMI = body mass index, BDI-II = Beck Depression Inventory, second edition, DRS-2 = Mattis Dementia Rating Scale, second edition, values represent means ± standard deviations for contiuous variables and frequencies for nominal variables." )

# save it
gtsave( t1, here("tabs","descriptives.docx") )


# HYPOTHESES TESTING ----

# prepare data matrix for correlations
d3 <-
  d2[ , c("bdi_gain","weight_gain","drs_gain","ledd_gain") ] %>%
  rename( "BDI-2" = "bdi_gain", "Weight" = "weight_gain", "DRS-2" = "drs_gain", "LEDD" = "ledd_gain" )

# compute correlations
corr <- cor.ci( d3, p = .05, method = "pearson", plot = F )

# 
pairs.panels(
  d3,
  method = "pearson", # correlation method
  lm = T, ci = T, alpha = .05,
  hist.col = "#00AFBB",
  breaks = 15,
  stars = T,
  density = F, # no show density plots
  ellipses = T # show correlation ellipses
)
  


