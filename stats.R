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

# printing rounded numbers
rprint <- function( x, dec = 2 ) sprintf( paste0("%.",dec,"f"), round( x, dec ) )

# collapse table to a cell
tabcol <- function( x, na = F ) paste( table( x, useNA = if(na) "always" ), collapse = "/" )

# summarise central tendency and variability
cenvar <-
  
  function(x, ct = "mean", var = "sd", dec = 2, sep = " ± ", end = "" ) {
    
    paste0(
      rprint( do.call( ct,list(x, na.rm = T) ), dec), sep,
      rprint( do.call(var,list(x, na.rm = T) ), dec), end
    )
    
  }

# change scores t-test values extraction
text <-
  
  function(x, d = 2, ci = .95) {
    
    t <- t.test( x, mu = 0, alternative = "two.sided", conf.level = ci )
    
    return(
      with(
        t,
        c(
          change = paste0( rprint( estimate, d ), " [", paste( rprint( conf.int, d ), collapse = ", " ), "]" ),
          test = paste0( "t(", parameter, ") = ", rprint( statistic, d ), "; p ", ifelse( p.value < .001, "< 0.001", paste0( "= ", rprint( p.value, 3 ) ) ) )
        )
      )
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
  mutate( post = ifelse( var %in% c("Education (years)","Sex (females/males)","Disease duration (years)"), "-", post ) ) %>%
  mutate( change = c( rep("-",5), sapply( c("ledd_gain","weight_gain","bmi_gain","bdi_gain","drs_gain"), function(i) text(d2[[i]])["change"] ) ) ) %>%
  mutate( test = c( rep("-",5), sapply( c("ledd_gain","weight_gain","bmi_gain","bdi_gain","drs_gain"), function(i) text(d2[[i]])["test"] ) ) )

# save it
write.table( t1, file = here("tabs","descriptives.csv"), sep = "\t", row.names = F, quote = F )


# HYPOTHESES TESTING ----

# prepare data matrix for correlations
d3 <-
  d2[ , c("bdi_gain","weight_gain","bmi_gain","drs_gain","ledd_gain") ] %>%
  rename( "BDI-2" = "bdi_gain", "BMI" = "bmi_gain", "Weight" = "weight_gain", "DRS-2" = "drs_gain", "LEDD" = "ledd_gain" )

# compute correlations
corr <- corr.test( d3, alpha = .05, method = "pearson", ci = T )

# prepare jpeg device for figure saving
jpeg( here("figs","corrmat.jpg"), units = "in", width = 10.9, height = 10.9, res = 300, quality = 100 )

# plot pairs
pairs.panels(
  d3,
  method = "pearson", # correlation method
  lm = T, ci = T, alpha = .05,
  hist.col = "#00AFBB", breaks = 15, # histogram
  stars = T, scale = F, cex.cor = .6,
  density = F, # no show density plots
  ellipses = T, # show correlation ellipses
  cex = 2, cex.axis = 1.5
)

# save it
dev.off()

# prepare a table with correlation results
t2 <-
  
  with( corr, cbind( ci, t = t[ lower.tri(t) ] ) ) %>%
  apply( ., 2, rprint, 3 ) %>%
  apply( ., 2, function(x) sub( ".", ",", x, fixed = T ) ) %>%
  as.data.frame() %>%
  mutate( pair = rownames(corr$ci), .before = 1 ) %>%
  mutate( n = with(corr, n[ lower.tri(n) ] ), ci = paste0("[",lower,"; ",upper,"]") ) %>%
  select( pair, r, ci, t, n, p )

# save it
write.table( t2, file = here("tabs","corrs.csv"), sep = "\t", row.names = F, quote = F )


# SESSION INFO -----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "stats_envir.txt" )


# panel of correlations
#panel.corr <- function(x, y) {
#  usr <- par("usr"); on.exit(par(usr))
#  par(usr = c(0, 1, 0, 1))
#  r <- cor(x, y)
#  txt <- paste0("Corr: ", r)
#  text(0.5, 0.5, txt, cex = 1)
#}

# panel of histograms
#panel.hist <- function(x, ...){
#  usr <- par("usr"); on.exit(par(usr))
#  par(usr = c(usr[1:2], 0, 1.5) )
#  h <- hist(x, plot = FALSE)
#  breaks <- h$breaks
#  len <- length(breaks)
#  y <- h$counts/max(h$counts)
#  rect(breaks[-len], 0, breaks[-1], y, col = "lightblue")
#}

# panel of scatterplots
#panel.scat <- function(x, y){
#  points(x,y, pch = 19, cex = 1, col = "coral")
#}

# plot
#pairs(mtcars[, c(1,3:7)],  
#      lower.panel = panel.scat,
#      upper.panel = panel.corr,
#      diag.panel = panel.hist,
#      labels = c("Miles","Displacement","Horsepower",
#                 "Rear axle ratio","Weight","1/4 mile time"),
#      gap = 0.3, 
#      main = "Scatterplot matrix of `mtcars`")
