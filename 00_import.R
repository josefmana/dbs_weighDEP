# This is a script used to prepare data sets for analysis.

# clear environment
rm( list = ls() )

# list packages to be used
pkgs <- c("here","tidyverse")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare a data folder for the outcomes
if( !dir.exists("_data") ) dir.create("_data")


# DATA READ ----

# read outcome data
for( i in c("psych","motor") ) {
  
  assign(
    
    paste0("d.",i),
    read.csv( here( "_raw", paste0(i,"_sum_scores.csv") ), sep = "," ) %>%
      filter( event %in% c("screening","y1") ) %>%
      mutate(
        event =
          factor( case_when( event == "screening" ~ "pre", event == "y1" ~ "post" ),
                  levels = c("pre","post"),
                  ordered = T
                  )
      )
  )

}

# read weight data
d.weight <-
  
  # read it
  read.csv( here("_raw","ITEMPO_DATA_2024-03-04_1128.csv"), sep = "," ) %>%
  
  # re-format variables
  mutate( event = case_when( grepl("screening",redcap_event_name) ~ "pre", grepl("r1",redcap_event_name) ~ "post" ) ) %>%
  mutate( bmi = ifelse( bmi == Inf, NA, bmi) ) %>%
  rename( "id" = "study_id", "weight" = "weight_date" ) %>%
  select( id, event, weight, bmi ) %>%
  
  # keep only patients with weight observation both pre- and post-surgery
  pivot_wider( names_from = "event", values_from = c("weight","bmi") ) %>%
  filter( complete.cases(weight_pre) & complete.cases(weight_post) ) %>%
  pivot_longer( cols = -id, values_to = "obs", names_to = c("var","event"), names_sep = "_" ) %>%
  pivot_wider( names_from = "var", values_from = "obs" ) %>%
  mutate( event = factor( event, levels = c("pre","post"), ordered = T ) )

# prepare a long data format data frame
d1 <-
  
  # add psychology
  d.weight %>%
  left_join( d.psych, by = c("id","event") ) %>%
  
  # drop patients/event combinations without BDI-II
  filter( complete.cases(bdi) ) %>%
  
  # add motor assessments
  left_join(
    d.motor %>%
      filter( stim %in% c("none","on") ) %>%
      select( id, event, med, stim, updrs_iii ) %>%
      pivot_wider( values_from = updrs_iii, names_from = c("med","stim"), names_prefix = "updrs_iii_" ) %>%
      mutate( updrs_iii_off_none = ifelse( event == "post", updrs_iii_off_on, updrs_iii_off_none ) ) %>%
      rename( "updrs_iii_med_off" = "updrs_iii_off_none", "updrs_iii_med_on" = "updrs_iii_on_none" ) %>%
      select( -updrs_iii_off_on ),
    by = c("id","event")
  )

# keep only patients with both pre- and post-surgery BDI-II
d1 <- d1[ d1$id %in% names( which( table(d1$id) == 2 ) ), ]

# prepare gain scores data set
d2 <-
  
  d1 %>%
  
  # calculate postop. gains
  mutate( across( c("stimtime_years","ledd_mg","weight","bmi","bdi","drsii"), ~ ifelse( event == "pre", -.x, .x ) ) ) %>%
  group_by(id) %>%
  summarise(
    time = sum(stimtime_years),
    ledd_gain = sum(ledd_mg),
    weight_gain = sum(weight),
    bmi_gain = sum(bmi),
    bdi_gain = sum(bdi),
    drs_gain = sum(drsii)
  ) %>%
  
  # add pre-surgery data
  left_join( d1[ d1$event == "pre", c("id","ledd_mg","weight","bdi","drsii","sex","hy","type_pd","asym_park") ] ) %>%
  rename( "ledd_pre" = "ledd_mg", "weight_pre" = "weight", "bmi_pre" = "bmi", "bdi_pre" = "bdi", "drs_pre" = "drsii" ) %>%
  mutate( weight_increased = ifelse( weight_gain > 0, 1, 0 ) )

# DATA SAVE ----

write.table( d1, here("_data","long_df.csv"), sep = ",", row.names = F, quote = F )
write.table( d2, here("_data","wide_df.csv"), sep = ",", row.names = F, quote = F )


# SESSION INFO -----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "import_envir.txt" )
