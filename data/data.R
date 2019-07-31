library(tidyverse)

set.seed(1234)

num_pols <- 1e3
mean <- 10e3
cv <- 0.3
b_0 <- 2
b_1 <- 1 / 10
b_2 <- 1 / 1e3
b_region <- c(-1, 0, 2, 4)

regions <- c('north', 'south', 'east', 'west')

tbl_policy <- tibble(
  id = seq_len(num_pols)
  , effective_date = as.Date('2001-01-01')
  , premium = runif(num_pols, 1e3, 5e3)
  , years_in_operation = runif(num_pols, 0, 30)
  , number_of_employees = rpois(num_pols, 2e3)
  , region = sample(regions, num_pols, replace = TRUE)
) %>% 
  mutate(
    effective_date = effective_date + runif(num_pols, 0, 3650 + 2)
    , expiration_date = effective_date + lubridate::years(1) - lubridate::days(1)
    , region_factor = case_when(
        region == 'north' ~ b_region[1]
        , region == 'south' ~ b_region[2]
        , region == 'east' ~ b_region[3]
        , region == 'west' ~ b_region[4]
    )
    , mu = b_0 + region_factor + b_1 * years_in_operation + b_2 * number_of_employees
    , num_claims = rpois(num_pols, mu)
    
  ) %>% 
  select(
    -mu
    , -region_factor
  ) %>% 
  select(id, region, effective_date, expiration_date, everything())

tbl_claim <- tbl_policy %>% 
  filter(
    num_claims > 0
  ) %>% 
  select(policy_id = id, num_claims, effective_date) %>% 
  mutate(
    claim_amount = map(num_claims, rgamma, 1 / cv^2, scale = mean * cv ^ 2)
  ) %>% 
  unnest(claim_amount) %>% 
  mutate(
    id = seq_len(nrow(.))
    , occurrence_date = effective_date + runif(nrow(.), 0, 364)
  ) %>% 
  select(-num_claims, -effective_date) %>% 
  select(policy_id, id, occurrence_date, claim_amount)

unlink('data/sqlite_data.db')
if (file.exists('data/sqlite_data.db')) {
  stop('You need to delete the sqlite database.')
}

library(DBI)
library(RSQLite)

db <- dbConnect(
  drv = RSQLite::SQLite()
  , dbname = file.path('data', 'sqlite_data.db')
)

tbl_policy %>% 
  mutate(
    effective_date = as.character(effective_date)
    , expiration_date = as.character(expiration_date)
  ) %>% 
  dbWriteTable(
    db
    , 'tbl_policy'
    , value = .
    , overwrite = TRUE
  )

dbWriteTable(
  db
  , 'tbl_claim'
  , tbl_claim
)

save(
  file = file.path('data', 'data.rda')
  , tbl_policy
  , tbl_claim
)