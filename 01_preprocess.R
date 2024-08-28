library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(plotly)
library(crosstalk)
#read in the raw data--------------------------
noc_broad <- read_csv(here("data","noc_broad.csv"), col_types = "c")
noc_teer <- read_csv(here("data","noc_teer.csv"))|>
  mutate(teer_num=as.character(teer_num))
industry_mapping <- read_csv(here("data","lmo64_agg_mapping.csv"))|>
  select(industry=lmo_industry_name, aggregate_industry)
#demand by occupation------------------------
read_excel(here("data","demand_occupation.xlsx"), skip = 3)|>
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
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))|>
  write_rds(here("out","demand_occ.rds"))

#demand by industry--------------------------------------

read_excel(here("data","demand_industry.xlsx"), skip = 3)|>
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
  group_by(aggregate_industry, geographic_area)|>
  arrange(`Job Openings`, .by_group = TRUE)|>
  pivot_longer(cols=contains("Demand"))|>
  mutate(geographic_area=str_replace_all(geographic_area, "&", "and"))|>
  write_rds(here("out","demand_ind.rds"))

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
#' (cant do this post filtering in app)

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
