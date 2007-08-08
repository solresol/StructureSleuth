> module Main where
> import TSV
> import List
> main = 
>  do
>   tables <- make_tables
>   putStrLn (output_dot_for_tables no_urls_for_dot_output tables)
