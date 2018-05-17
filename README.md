# Fetch one2one orthologues from Ensembl Compara
This function takes two scientific names (e.g., 'homo_sapiens' and 'mus_musculus') and returns a dataframe of orthologues genes based on the MySQL Ensembl Compara database (version 75).

## Dependencies
Requires the following R packages:
1. RMySQL
2. glue
3. dplyr

## Usage
```R
human_mouse <- get_one2one_orthologues(homo_sapiens, mus_musculus)
```
