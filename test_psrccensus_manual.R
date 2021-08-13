library(devtools)

devtools::install_github("psrc/psrccensus", force=TRUE)

library(sf)
library(dplyr)
library(psrccensus)

Sys.getenv("CENSUS_API_KEY")

tract.big.tbl <- psrccensus::get_acs_recs(geography='tract',table.names=c('B03002'),years=c(2019))
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
create_tract_map(tract.tbl=tract.tbl, tract.lyr=tract.lyr, map.title='Black, non-Hispanic Population', 
                 map.subtitle='by Census Tract',map.title.position='topleft', legend.title='Black, Non-Hispanic Population', legend.subtitle='by Census Tract')
