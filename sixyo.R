library(tidyverse)
library(haven)
library(Hmisc)



# options -----------------------------------------------------------------
path = "//wsl.localhost/Ubuntu/home/quent/projects/thesis/data/" 
stich = 1990 # year of residence (for independent variables)
age = 6 # age in year of residence (for independent variables)
obs_age = 36 # age at observation year (for dependent variables)
obs_year = stich+obs_age-age



# preparation -------------------------------------------------------------
## ingestion ####
setwd(path)
bioparen_raw <- read_dta('bioparen.dta')
# biobirth_raw <- read_dta('biobirth.dta')
ppath_raw <- read_dta('ppath.dta')


## transformation ####
bdays <- ppath_raw %>% group_by(pid) %>% distinct(gebjahr)

dimension_p <- bioparen_raw %>%
  # filter(westdeutsch) %>%
  filter(mnr > 0 | fnr > 0) %>%
  select(pid, cid, locchildh, locchild1, mnr, fnr, mybirth, fybirth) %>%
  mutate(parent_known = labelled(case_when(
    mnr > 0 & fnr > 0 ~ 1,
    mnr > 0 ~ 2,
    mnr < 0 ~ 3),
    c(both = 1, mom = 2, dad = 3)
  )) %>% left_join(bdays, by = 'pid')

dimension_p %>% count(parent_known)

fact_p <- pl_raw %>% 
  select(pid, hid, cid, syear, plh0182, pla0003, plf0009, plf0010) %>%
  # some potentially interesting variables
  arrange(pid, syear)



# extraction --------------------------------------------------------------
## find parents and their historic residence ####
mothers <- filter(dimension_p,
                  gebjahr == stich-age,
                  mnr > 0)$mnr

fathers <- filter(dimension_p,
                  gebjahr == stich-age,
                  fnr > 0)$fnr

mother_surveys <- fact_p %>% subset(pid %in%mothers) 
father_surveys <- fact_p %>% subset(pid %in%fathers)
# for more variables use pl_raw for fact_p

residences_mt <- mother_surveys %>% 
  filter(syear == stich) %>% 
  select(pid, hid) %>% 
  rename(mnr = pid,
         mres = hid)

residences_ft <- father_surveys %>% 
  filter(syear == stich) %>% 
  select(pid, hid) %>% 
  rename(fnr = pid, 
         fres = hid)

## merge to final subjects ####
fin_sub <- filter(dimension_p, gebjahr == stich-age) %>%
  left_join(residences_mt, by = ('mnr')) %>% 
  left_join(residences_ft, by = ('fnr')) %>% 
  filter(!is.na(mres)|!is.na(fres)) %>%
  mutate(childhood_res = labelled(case_when(
    !is.na(mres) ~ mres,
    TRUE ~ fres),
  label = paste0("Haushalt im Jahr ", stich))
  )
  

# parent_res <- residences_mt %>%
#   full_join(residences_ft, by = c('mres' = 'fres'), multiple = 'all') %>%
#   rename(par_res = mres)
# two missing entries (76 vs 74 for 6, 1990, 36)

# filter(dimension_p, gebjahr == stich-age) %>%
#   left_join(parent_res)
  
observations <- fin_sub %>% inner_join(filter(select(fact_p, -cid), syear == obs_year), by = 'pid')
# contains only subjects where survey exists for syear