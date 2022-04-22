
library(devtools)

devtools::install_github("psrc/psrccensus")

Sys.getenv("CENSUS_API_KEY")


library(psrccensus)
library(tidyverse)
library(srvyr)

so <- get_psrc_pums(5, 2019, "p", c("RAC1P","RAC2P","HISP"), labels=TRUE)

df %>% filter(A == 1 & B == 3 | A == 3 & B ==2)

so <- filter(so, HISP != 'True' & (RAC1P=='Black or African American alone' | RAC2P=="Black or African American alone")
             


rs <- psrc_pums_count(so)



recs<- get_psrc_pums(span = 5,                          # Denoting ACS 5-year estimates; 1-year also available
                   dyear = 2019,                      # Last data year of span
                   level = "p",                       # Unit of analysis == person; "h" used for household
                   vars = c("RAC1P","POBP"),           # PUMS household income & housing tenure variables; using tenure later
                   ftp = TRUE)                        # ftp option avoids Census API downtime
psrc_pums_count(recs,  "POBP", "RAC1P")            # Median Household income (without regard to tenure)


pums_check<-psrc_pums_count(span = 5,                  
                             dyear = 2019,
                             target_var = "VEH",       
                             group_var = "RAC1P")
write.table(pums_check, "clipboard", sep="\t", row.names=FALSE)

write.table(pums_check, "clipboard", sep="\t", row.names=FALSE)




age_sex_acs18 <- get_acs_recs(geography = 'county',
                              table.names = c('B01001'), # Age group
                              years=c(2018),
                              acs.type = 'acs1')
age_sex_acs19 <- get_acs_recs(geography = 'county',
                              table.names = c('B01001'), # Age group
                              years=c(2019),
                              acs.type = 'acs1')
df18 <- group_recs_test(age_sex_acs18, 'Age Group')
df19 <- group_recs_test(age_sex_acs19, 'Age Group')

age_sex_acs18_nona<-age_sex_acs18 %>% na.omit('moe')
df18_nona <- group_recs(age_sex_acs18_nona, 'Age Group')


group_recs_test <- function(tbl, this_group_name){
  # this is kind of a hard code for the file name and location, may want to revisit
  variables_groupings<-read.csv(system.file('extdata', 'variables_groupings.csv', package='psrccensus'))
  this_variable_grouping<- variables_groupings%>%dplyr::filter(.data$group_name==!!this_group_name)
  
  tbl_w_cats<-merge(tbl, this_variable_grouping)
  
  #the column names between acs and census are slightly different
  # for acs:
  if("estimate" %in% colnames(tbl_w_cats)){
    tbl_grouped <- tbl_w_cats%>%
      dplyr::group_by(dplyr::across(-c(estimate, moe,label, variable )))%>%
      dplyr::summarise(estimate=sum(estimate),
                       moe=tidycensus::moe_sum(moe,estimate, na.rm=TRUE))
  }
  # for census:
  else{
    tbl_grouped <- tbl_w_cats%>%
      dplyr::group_by(dplyr::across(-c(value,label, variable )))%>%
      dplyr::summarise(value=sum(value))
    
  }
  tbl_grouped
  
}

tract.race.big.tbl <- psrccensus::get_acs_recs(geography='tract',table.names=c('B03002'),years=c(2019), acs.type='acs5')

county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('B03002'),years=c(2019), acs.type='acs1')

AIAN_age_df<-get_acs_recs(geography = 'county',
                          table.names = c('B01002C'),
                          years=c(2019),
                          acs.type = 'acs5')
tract.tbl <- tract.big.tbl %>%
filter(label=='Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone')

get_acs_recs(geography = 'county',
             table.names = 'B03002',
             years = 2019,
             acs.type = 'acs1')



gdb.nm <- paste0("MSSQL:server=",
"AWS-PROD-SQL\\Sockeye",
";database=",
"ElmerGeo",
";trusted_connection=yes")

spn <-  2285

tract_layer_name <- "dbo.tract2010_nowater"

tract.lyr <- st_read(gdb.nm, tract_layer_name, crs = spn)
?create_tract_map
m<-create_tract_map(tract.tbl=tract.tbl, tract.lyr=tract.lyr, map.title='Black, non-Hispanic Population', 
                 map.subtitle='by Census Tract',map.title.position='topleft', legend.title='Black, Non-Hispanic Population', legend.subtitle='by Census Tract')


group_recs <- function(tbl, this_group_name){
  # this is kind of a hard code for the file name and location, may want to revisit
  variables_groupings<-read.csv(system.file('extdata', 'variables_groupings.csv', package='psrccensus'))
  this_variable_grouping<- variables_groupings%>%filter(group_name==!!this_group_name)
  
  tbl_w_cats<-merge(tbl, this_variable_grouping)
  
  #the column names between acs and census are slightly different
  # for acs:
  if("estimate" %in% colnames(tbl_w_cats)){
    tbl_grouped <- tbl_w_cats%>%
      dplyr::group_by(dplyr::across(-c(estimate, moe,label, variable )))%>%
      dplyr::summarise(estimate=sum(estimate),
                       moe=tidycensus::moe_sum(moe,estimate))
  }
  # for census:
  else{
    tbl_grouped <- tbl_w_cats%>%
      dplyr::group_by(dplyr::across(-c(value,label, variable )))%>%
      dplyr::summarise(value=sum(value))
    
  }
  tbl_grouped
  
  
}
inc_poverty<-get_acs_recs(geography = 'county',
             table.names = c('C17002'),
             years=c(2019),
             acs.type = 'acs5')
grouped<-group_recs(inc_poverty, 'Poverty Group 100 Percent' )

county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('C17002'),years=c(2019), acs.type='acs1')

write.table(county.big.tbl, "clipboard", sep="\t", row.names=FALSE)

edu_race<-psrccensus::get_acs_recs(geography='county',table.names=c('S1501'),years=c(2018,2019), acs.type='acs1')


edu_race_2019<-psrccensus::get_acs_recs(geography='tract',table.names=c('S1501'),years=c(2019), acs.type='acs1')
black_bachelors<-edu_race_2019%>%filter(variable=='S1501_C02_036')

library(sf)
gdb.nm <- paste0("MSSQL:server=",
                 "AWS-PROD-SQL\\Sockeye",
                 ";database=",
                 "ElmerGeo",
                 ";trusted_connection=yes")

spn <-  2285

tract_layer_name <- "dbo.tract2010_nowater"

tract.lyr <- st_read(gdb.nm, tract_layer_name, crs = spn)

create_tract_map(tract.tbl=black_bachelors, tract.lyr=tract.lyr)

psrccensus::create_tract_map(edu_race_2019)

ts<-get_time_series(edu_race, 'S1501_C01_036')
get_time_series()
write.table(ts$data, "clipboard", sep="\t", row.names=FALSE)
get_time_series  = function(table,
                            variable,
                            ts.title = NULL,
                            print.table = FALSE) {
  variable = rlang::enquo(variable)
  
  df_for_plot = table %>%
    dplyr::rename_at(dplyr::vars(matches("value")), function(x)
      "estimate") %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::filter(variable == (!!variable))
  
  if (print.table == TRUE) {
    df_for_plot = df_for_plot %>%
      select(name, variable, estimate, moe, label, year)
    return(df_for_plot)
  }
  
  if (missing(ts.title)) {
    ts.title = stringr::str_replace_all(df_for_plot$label[1], "[!:]", " ")
    ts.title = stringr::str_replace_all(ts.title, "   ", " ")
  }
  
  legend.title = df_for_plot$census_geography[1]
  
  m = ggplot2::ggplot(df_for_plot, ggplot2::aes(
    x = year,
    y = estimate,
    group = name,
    color = name
  )) +
    ggplot2::geom_line(size = 1) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::geom_point() +
    ggplot2::ggtitle(ts.title) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(color = legend.title)
  return (m)
}



county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('C17002'),years=c(2019), acs.type='acs1')

write.table(county.big.tbl, "clipboard", sep="\t", row.names=FALSE)

county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('B18130'),years=c(2019), acs.type='acs1')

write.table(county.big.tbl, "clipboard", sep="\t", row.names=FALSE)

county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('B03002'),years=c(2019), acs.type='acs1')

write.table(county.big.tbl, "clipboard", sep="\t", row.names=FALSE)


county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('C16001'),years=c(2019), acs.type='acs1')

write.table(county.big.tbl, "clipboard", sep="\t", row.names=FALSE)


county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('B25045'),years=c(2019), acs.type='acs1')

write.table(county.big.tbl, "clipboard", sep="\t", row.names=FALSE)

db.connect <- function() {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\Sockeye",
                                database = "Elmer",
                                trusted_connection = "yes"
  )
}

ni<- get_psrc_pums(span = 5,                          # Denoting ACS 5-year estimates; 1-year also available
                   dyear = 2019,                      # Last data year of span
                   level = "h",                       # Unit of analysis == person; "h" used for household
                   vars = c("HINCP","TEN"),           # PUMS household income & housing tenure variables; using tenure later
                   ftp = TRUE)                        # ftp option avoids Census API downtime
psrc_pums_median(ni, target_var = "HINCP")   




t <- get_acs_recs(geography = 'county',
                  table.names = 'B03002',
                  years=2018,
                  acs.type = 'acs1')
t2 <- group_recs(t, 'Race Group')
