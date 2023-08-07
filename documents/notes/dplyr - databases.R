{ # Data --------------------------------------------------------------------
  
  library(dbplyr)
  library(arrow)
  
  ## .csv
  penguins_data_url = 'https://gist.githubusercontent.com/slopp/ce3b90b9168f2f921784de84fa445651/raw/4ecf3041f0ed4913e7c230758733948bc561f434/penguins.csv'
   
  ## .parquet
  data %>% arrow::write_parquet('penguins.parquet')
  
}

{# Question ----------------------------------------------------------------
 #' Calculate ratio of bill length to depth then calculate rank by species. Return a table 
 #' whose rows are arranged in order by sepcies and contiaining only relevant columns.
}

{ # Human semantics ---------------------------------------------------------
  #' 1. Use penguins as the input data
  #' 2. Group by species
  #' 3. Calculate bill length depth ratio
  #' 4. Arrange rows based on rank
  #' 5. Select columns: species, rank, ratio
  #' 6. Calculate rank of ratio_bill
}

{# Flat File (.csv)  -----------------------------------------------------------------------
  
  ## Import data
  data = read.csv(penguins_data_url)
  
  ## Wrangling (dplyr)
  data %>%
    group_by(species) %>%
    mutate(ratio_bill = bill_length_mm/bill_depth_mm) %>% 
    select(species, ratio_bill ) %>% 
    mutate(rank = rank(desc(ratio_bill ))) %>% 
    arrange(rank)
}

{ # Database (SQLite) ----------------------------------------------------------------
  
  ## Connect to database
  database  <- memdb_frame(data)
  
  ## Wrangling (dplyr)
  query = database %>%
    group_by(species) %>%
    mutate(ratio_bill = bill_length_mm/bill_depth_mm) %>% 
    select(species, ratio_bill ) %>% 
    mutate(rank = rank(desc(ratio_bill ))) %>% 
    arrange(rank)
  query %>%  collect()
  
  ## Equivalent SQL
  query %>% show_query()
}

{ # Columnar storage (parquet) ------------------------------------------------
  
  ## Open Dataset
  dataset = open_dataset('penguins.parquet')

  ## Form query
  dataset %>%
    group_by(species) %>%
    mutate(ratio_bill = bill_length_mm/bill_depth_mm) %>% 
    select(species, ratio_bill ) %>% 
    collect() %>% 
    mutate(rank = rank(desc(ratio_bill ))) %>% 
    arrange(rank)
  
}