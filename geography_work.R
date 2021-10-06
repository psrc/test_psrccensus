library(odbc)
library(DBI)

library(dplyr)
library(psrccensus)


elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "AWS-PROD-SQL\\Sockeye",
                              database = "Elmer",
                              trusted_connection = "yes")

# options for type are
# options for year are

get_equity_geographies<- function(elmer_connection, equity_type='equity_geog_vs_50_percent',equity_group='racial_equity_geographies',year='2016'){
  query_geo=paste0('SELECT geoid, ', equity_type, ' FROM census.', equity_group,'(',year, ', \'Tract\')')
  table_equity_geo<-dbGetQuery(elmer_connection, SQL(query_geo))
  table_equity_geo
}


poverty_tracts<-get_equity_geographies(elmer_connection, 'equity_geog_vs_reg_total', 'poverty_equity_geographies','2018')



means_transport<-get_acs_recs('tract',table.names=c('S0801_C01'),   years=c(2019),
                              acs.type = 'acs1')


tot_workers<-means_transport%>%filter(variable=='S0801_C01_001')
transit<-means_transport%>%filter(variable=='S0801_C01_009')

poverty_wrkrs<-merge(poverty_tracts, tot_workers, by.x='geoid', by.y='GEOID')
poverty_transit<-merge(poverty_wrkrs, transit, by.x='geoid', by.y='GEOID')

poverty_transit_cals<-poverty_transit%>%mutate(transit=estimate.x*estimate.y/100)%>%mutate(workers=estimate.x)%>%select('geoid', 'transit', 'workers', 'equity_geog_vs_reg_total')

poverty_transit_summary<-poverty_transit_1%>%group_by(equity_geog_vs_reg_total)%>%summarise(sum_workers=sum(workers), sum_transit=sum(transit, na.rm=TRUE))%>%mutate(transit_share=sum_transit/sum_workers)
poverty_transit_summary


library(sf)
library(dplyr)
library(psrccensus)
library(tidycensus)
gdb.nm <- paste0("MSSQL:server=",
                 "AWS-PROD-SQL\\Sockeye",
                 ";database=",
                 "ElmerGeo",
                 ";trusted_connection=yes")

spn <-  2285

tract_layer_name <- "dbo.tract2010_nowater"

tract.lyr <- st_read(gdb.nm, tract_layer_name, crs = spn)

equity_geog_list<-c('racial_equity_geographies', 'poverty_equity_geographies',
                    'disability_equity_geographies', 'elderly_equity_geographies', 'limited_english_equity_geographies')

for(geog in equity_geog_list){
  get_equity_geographies(elmer_connection, 'equity_geog_vs_reg_total', geog,'2018')
  create_tract_map(tract.tbl=tract.tbl, tract.lyr=tract.lyr,
    legend.title=geog,
    legend.subtitle='by Census Tract')
}


