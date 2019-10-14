library(tidyverse)
survey <- read_csv(here::here("data-raw", "MSD_data.csv"))
# %>% 
  # janitor::clean_names()

social_ref <- tibble(
  social_site_id = 1:9, 
  social_site = c("Twitter", "Instagram", "Facebook", "Snapchat", "YouTube", "WhatsApp", "Pinterest", "LinkedIn", "Reddit")
)


survey %>% 
  select(-c(starts_with("please"), x18)) %>% 
  mutate(social_site_id = as.character(social_media_sites_used)) %>% 
  mutate(social_site_id = str_split(social_site_id, "")) %>%
  unnest_longer(social_site_id) %>% 
  mutate(social_site_id = as.integer(social_site_id)) %>% 
  left_join(social_ref) %>% 
  select(-social_site_id) 
  


survey_clean <- survey %>% 
  rename_at(vars(starts_with("Please")), ~{
    social <- .x %>% 
      str_split(" ") %>% 
      map_chr(last)
    
    glue::glue("use_{social}") %>% 
      as.character()
  }) %>% 
  janitor::clean_names() %>% 
  rename_at(vars(instagram:reddit), ~glue::glue("rank_{.x}")) %>% 
  mutate_at(vars(starts_with("use")), ~ifelse(is.na(.x), 0, 1)) %>% 
  select(-x18) %>%
  na.omit() %>% 
  select(-social_media_sites_used)

write_rds(survey_clean, here::here("data", "survey_clean.rds"))
