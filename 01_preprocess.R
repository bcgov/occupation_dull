library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(crosstalk)
#functions---------------------------
get_current <- function(tbbl){
  tbbl$value[tbbl$name==min(tbbl$name)]
}
get_cagr <- function(tbbl){
  start <- tbbl$value[tbbl$name==min(tbbl$name)]
  end <- tbbl$value[tbbl$name==max(tbbl$name)]
  (end/start)^(1/(max(tbbl$name-min(tbbl$name))))-1
}
#read in the mapping data--------------------------
noc_broad <- read_csv(here("data","noc_broad.csv"), col_types = "c")
noc_teer <- read_csv(here("data","noc_teer.csv"))|>
  mutate(teer_num=as.character(teer_num))
industry_mapping <- read_csv(here("data","lmo64_agg_mapping.csv"))|>
  select(industry=lmo_industry_name, aggregate_industry)
#demand by occupation------------------------
demand_occ <- read_excel(here("data","demand_occupation.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  clean_names()|>
  filter(variable %in% c("Expansion Demand", "Replacement Demand", "Job Openings"))|>
  group_by(noc, description, variable, geographic_area)|>
  summarize(value=sum(value))|>
  mutate(broad_num=str_sub(noc,2,2),
         teer_num=str_sub(noc,3,3))|>
  full_join(noc_broad)|>
  full_join(noc_teer)|>
  ungroup()|>
  select(-broad_num, -teer_num, -noc)|>
  pivot_wider(names_from = variable, values_from = value)|>
  group_by(broad, teer)|>
  arrange(`Job Openings`, .by_group = TRUE)|>
  pivot_longer(cols=contains("Demand"))|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))

#write shareddata objects based on demand by occupation---------------------

demand_occ|>
  group_by(name, geographic_area)|>
  summarize(value=sum(value, na.rm = TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm=TRUE))|>
  filter(`Job Openings`>0)|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","shared_for_pie.rds"))

demand_occ|>
  group_by(broad, geographic_area, name)|>
  summarize(value=sum(value, na.rm=TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm = TRUE))|>
  ungroup()|>
  arrange(geographic_area, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","shared_broad.rds"))

demand_occ|>
  group_by(teer, geographic_area, name)|>
  summarize(value=sum(value, na.rm=TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm = TRUE))|>
  ungroup()|>
  arrange(geographic_area, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","shared_teer.rds"))

demand_occ|>
  group_by(geographic_area, teer, broad, name)|>
  summarize(value=sum(value, na.rm=TRUE),
            `Job Openings`=sum(`Job Openings`, na.rm = TRUE))|>
  ungroup()|>
  arrange(geographic_area, desc(teer), `Job Openings`)|>
  unite(two_digit, broad, teer, sep=": ")|>
  filter(`Job Openings`>0)|>
  SharedData$new(~geographic_area, group="region1")|>
  write_rds(here("out","shared_two.rds"))

demand_occ|>
  ungroup()|>
  arrange(geographic_area, broad, teer, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  SharedData$new(~interaction(geographic_area, broad, teer))|>
  write_rds(here("out","shared_demand_occ.rds"))

#demand by industry--------------------------------------

demand_industry <- read_excel(here("data","demand_industry.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  clean_names()|>
  filter(variable %in% c("Expansion Demand", "Replacement Demand", "Job Openings"))|>
  group_by(industry, variable, geographic_area)|>
  summarize(value=sum(value))|>
  fuzzyjoin::stringdist_full_join(industry_mapping)|> #names do not match exactly
  ungroup()|>
  select(industry=industry.y, aggregate_industry, geographic_area, variable, value)|>
  pivot_wider(names_from = variable, values_from = value)|>
  group_by(geographic_area)|>
  arrange(`Job Openings`, .by_group = TRUE)|>
  pivot_longer(cols=contains("Demand"))|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))

#write shareddata objects based on demand by occupation---------------------

demand_industry|>
  filter(`Job Openings`>0)|>
  group_by(geographic_area)|>
  slice_max(`Job Openings`, n=100)|> #this keeps the top 50!!! industries (2 rows for each industry/region)
  arrange(`Job Openings`, .by_group = TRUE)|>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","shared_demand_ind.rds"))

demand_industry|>
  group_by(aggregate_industry, geographic_area, name)|>
  summarize(`Job Openings`=sum(`Job Openings`, na.rm = TRUE),
            value=sum(value, na.rm=TRUE)
            )|>
  ungroup()|>
  arrange(geographic_area, `Job Openings`)|>
  filter(`Job Openings`>0)|>
  SharedData$new(~geographic_area, group="region_jo_ind")|>
  write_rds(here("out","shared_demand_ind_agg.rds"))


#employment by occupation----------------------------

employment_education <- read_excel(here("data","employment_occupation.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  mutate(name=as.numeric(name))|>
  clean_names()|>
  mutate(broad_num=str_sub(noc,2,2),
         teer_num=str_sub(noc,3,3))|>
  full_join(noc_broad)|>
  full_join(noc_teer)|>
  ungroup()|>
  select(-broad_num, -teer_num, -noc)|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))

#'need to extract first year employment level and 10year cagr for various levels of aggregation
#' (can't do this post filtering in app)

#employment by industry--------------------------------------------

employment_industry <- read_excel(here("data","employment_industry.xlsx"), skip = 3)|>
  remove_constant()|>
  pivot_longer(cols=starts_with("2"))|>
  mutate(name=as.numeric(name))|>
  clean_names()|>
  fuzzyjoin::stringdist_full_join(industry_mapping)|> #names do not match
  ungroup()|>
  select(industry=industry.y, aggregate_industry, geographic_area, name, value)|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))
