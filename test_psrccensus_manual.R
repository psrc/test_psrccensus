library(devtools)

devtools::install_github("psrc/psrccensus", force=TRUE)

library(sf)
library(dplyr)
library(psrccensus)
library(tidycensus)

Sys.getenv("CENSUS_API_KEY")

tract.big.tbl <- psrccensus::get_acs_recs(geography='tract',table.names=c('B03002'),years=c(2019))

county.big.tbl <- psrccensus::get_acs_recs(geography='county',table.names=c('B03002'),years=c(2019), acs.type='acs1')

tract.tbl <- tract.big.tbl %>%
filter(label=='Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone')

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

age_sex<-get_acs_recs(geography = 'county',
             table.names = c('B01001'),
             years=c(2019),
             acs.type = 'acs1')
group_recs(age_sex, 'Broad Age' )

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
