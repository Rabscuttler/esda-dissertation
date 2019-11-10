library(tidyverse)
library(dplyr)
library(lubridate)
library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(sf)
library(forcats)
library(reshape2)
tmap_mode("view")

# Get FiT data  ---------------------------------------------------
# FIT data comes in 3x ~25mb files - Read and combine all 3 
# fits1 <- readxl::read_excel('data/fit_installation_report_-_april_2019_part_1_final.xlsx', skip=5)
# fits2 <- readxl::read_excel('data/fit_installation_report_-_april_2019_part_2_final.xlsx', skip=5)
# fits3 <-readxl::read_excel('data/fit_installation_report_-_april_2019_part_3_final.xlsx', skip=5)
# fits <- rbind(fits1, fits2, fits3) # Combine

# Rename columns for easier manipulation
# fits <- rename(fits,
#                extension = `Extension: Y/N`,
#                postcode = `Installation Postcode`,
#                tech = `Technology`,
#                cap = `Installed capacity` ,
#                net_cap = `Declared net capacity`,
#                app_date = `Application date`,
#                comm_date = `Commissioning date`,
#                msc_date = `MCS issue date`,
#                export = `Export status`,
#                tariff = `Tariff code`,
#                tariff_desc = `Tariff description`,
#                type = `Installation type`,
#                country = `Country`,
#                la = `Local authority`,
#                govt_region = `Government office region`,
#                accred = `Accreditation Route`,
#                mpan = `METER NUMBER (MPAN)`,
#                school = `Community/school category`,
#                lsoa = `LLSOA code`)

# write_csv(fits, 'data/fits_data_april_2019_combined.csv')

fits <- read_csv('data/fits_data_april_2019_combined.csv')

# Other pre-prep to FiT Database




######################### Get Renewable Energy Planning Database (REPD)
repd <- readxl::read_xlsx('data/renewable-energy-public-database-q1-2019.xlsx', sheet = 'Database', skip = 6)
# Drop missing values
repd <- repd[!is.na(repd$`Installed Capacity (MWelec)`),]

# Filter for solar only
repd <- repd %>% filter(`Technology Type`=='Solar Photovoltaics')

# Create factors
repd$`Development Status (short)` <- as_factor(repd$`Development Status (short)`)

# Capacity by status
repd %>% group_by(`Development Status (short)`) %>% 
  summarise(capacity=sum(`Installed Capacity (MWelec)`, status = `Development Status (short)`)) %>%
  mutate(x=fct_reorder(`Development Status (short)`, capacity) %>% fct_rev()) %>%
  ggplot(aes(x, y=capacity)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Operational only
repd <- repd %>% filter(`Development Status (short)`=='Operational')

# I forgot to remove duplicates as those in REPD receiving FIT payments are also in the FIT database
repd_fit <- repd[!is.na(repd$`FiT Tariff (p/kWh)`),]

sum(repd_fit$`Installed Capacity (MWelec)`)
#  224.901 MW down as receiving FiT data

# let's drop those values from repd that should exist in the FIT
repd <- repd[is.na(repd$`FiT Tariff (p/kWh)`),]

# Drop most columns
repd <- repd %>% dplyr::select('Ref ID', 'Site Name', 'Installed Capacity (MWelec)', 'Mounting Type for Solar', 'Address', 'County', 'Region', 'Post Code', 'X-coordinate', 'Y-coordinate', 'Operational')

# Rename sensibly
repd <- rename(repd, 
              id = 'Ref ID',
              name = 'Site Name',
              cap = 'Installed Capacity (MWelec)',
              mounting = 'Mounting Type for Solar',
              address = 'Address',
              county = 'County',
              region = 'Region',
              postcode = 'Post Code',
              x = 'X-coordinate',
              y = 'Y-coordinate',
              start_date = 'Operational')

repd$mounting <- as_factor(repd$mounting)
repd$county <- as_factor(repd$county)
repd$region <- as_factor(repd$region)

# Capacity by region
repd %>% group_by(region) %>% summarise(cap = sum(cap)) %>% ggplot() + geom_col(aes(region, cap)) +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# EDA on FiT---------------------------------------------------------------------

# Initial exploration of FiT Database
# Bear in mind FIT database has technology mix, not just solar!
# fits %>% group_by(tech) %>% summarise(count=n())

# Take only solar
fits <- fits %>% filter(tech=='Photovoltaic')
# Drop the tech column
fits <- fits %>% select(-tech)

# Size range
range(fits$cap)
# [1] 0.02 4999.80

# Histogram of installations by size
b = c(0, 0.5, 1.5, 2, 2.5, 3.5, 4, 5, 10, 50, 500, 5000) 
ranges = paste(head(b,-1), b[-1], sep=" - ")
freq = hist(fits$cap, breaks=b, include.lowest=TRUE, plot=FALSE)
df <- data.frame(range = ranges, frequency = freq$counts, breaks=freq$breaks[-1])
df

# Plot
df$range <- factor(df$range, levels = df$range[order(df$breaks)]) # Order by the breaks
df %>% 
  ggplot(aes(x=range, y=frequency)) + geom_bar(stat="identity") +
  theme_minimal()

# Banding due to FiT thresholds
fits %>% filter(cap > 50 & cap <= 1000) %>%
  ggplot(aes(cap)) + geom_histogram()

fits %>% filter(cap > 1000) %>%
  ggplot(aes(cap)) + geom_histogram()

# Counts 
fits %>% 
  # group_by(Country) %>%
  summarise(capacity = sum(cap), count=n())

# capacity  count
# <dbl>  <int>
#   1 4965576. 839694 
# This is a little low, certainly on capacity. BEIS reports 13GW of solar, this has 4.9GW
# Total count is 839,694 installations. BEIS has 960,000 GB (985,000 UK)

# From the BEIS deployment stats, FiT only covers 5GW. Rest is Renewables Obligation (RO) (6.3GW), CfDs (tiny), Unaccredited (0.85GW)



# Join FiT and REPD -------------------------------------------------------

# Cut out crust
fits <- fits %>% dplyr::select(c(postcode,cap,net_cap,comm_date,export,tariff,tariff_desc,type,country,la,govt_region,school,lsoa))
repd <- repd %>% dplyr::select(id, name, cap, `RO Banding (ROC/MWh)`, `FiT Tariff (p/kWh)`, mounting, `Development Status (short)`, 
                       address, county, region, Country, postcode, x, y, `Planning Authority`, start_date)
fits$id <- NA # Create dummy ID column to match with REPD
fits$source <- "fitsDB"
repd$source <- "REPD"

# Correct the fact that REPD plants have capacity in MW, not kW
repd$cap <- repd$cap * 1000

# Join up dbs
pv <- full_join(fits, repd, by=c("source", "postcode", "cap", "id"))

# Check all rows have gone in separately
nrow(pv) == count(fits) + count(repd)

# I suspect there are duplicates in here. Challenge will be to identify them.

# Try and merge region information
pv$region <- as.character(pv$region)

pv %>% group_by(region) %>% summarise(count = n())
pv %>% group_by(govt_region) %>% summarise(count = n())
# Find intersection of regions
intersect(unique(pv$region), unique(pv$govt_region))
# Regions not matching
union(setdiff(unique(pv$region), unique(pv$govt_region)), setdiff(unique(pv$govt_region), unique(pv$region))) 

# Set Region from REPD to govt_region from FITs
pv$region[pv$region == "Eastern"] <- "East of England"
pv$region[pv$region == "Yorkshire and Humber"] <- "Yorkshire and The Humber"

# Turn unknown to NA
pv$govt_region[pv$govt_region == "Unknown"] <- NA

# Merge region columns
pv$region[is.na(pv$region)] <- pv$govt_region[is.na(pv$region)]

# Drop govt_region col
pv <- pv %>% dplyr::select(-govt_region)

# Net cap vs cap?
grouped <- pv %>% group_by(region) %>% 
    summarise(count = n(), cap=sum(cap), net_cap=sum(cap))
grouped   

# melt the data frame for plotting
pv.melt <- melt(grouped, id.vars=c('region'))

# Plot
pv.melt %>%
  ggplot(aes(region, value)) + geom_bar(aes(fill = variable), position="dodge", stat="identity")

# Tariff types?
pv %>% group_by(tariff) %>% 
  summarise(count=n())

# Export tariff
pv %>% group_by(export) %>% 
  summarise(count=n())

# Export tariff description
pv %>% group_by(tariff_desc) %>% 
  summarise(count=n())

# Only a tiny amount of tariff information so perhaps discard?
# Vast majority on Deemed Export Tariff. Small % on No Export and Standard. Discard.

# Type
pv %>% group_by(type) %>% 
  summarise(count=n())
# This is useful

# Country
pv %>% group_by(country) %>% 
  summarise(count=n())
# Ok I guess

# School
pv %>% group_by(school) %>% 
  summarise(count=n())
# Ok I guess

# Mounting
pv %>% group_by(mounting) %>% 
  summarise(count=n())
# Barely any information here, Discard.

# Address
pv %>% group_by(address) %>% 
  summarise(count=n())
# Mostly not but useful where it is.

# County
pv %>% group_by(county) %>% 
  summarise(count=n())
# Too much missing info


# Slim down and export ----------------------------------------------------

# Slim down the file
pv <- pv %>% dplyr::select(-c('net_cap', 'mounting', 'county', 'tariff', 'tariff_desc'))

pv %>% group_by(pv$name) %>% summarise(count=n())

# Write merged file 
write_csv(pv, 'data/fit_repd_pv.csv')

# TODO Fix: Columns (11,12,16) have mixed types.

