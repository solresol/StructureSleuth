> module Main where
> import TSV
> main = 
>  do
>   tables <- make_tables
>   mapM (display_analysis (cross_references tables)) tables 
>    
> display_analysis :: [CrossTableRef] -> Table -> IO ()
> display_analysis crossrefs table = 
>   do 
>     putStrLn (table_name table) 
>     putStr (column_analysis crossrefs table)
>     putStrLn ""
>
> column_analysis crossrefs table = concat (map one_column_analysis todays_news)
>  where
>   one_column_analysis x =  (show (column_header_name x)) ++ " " 
>                             ++ (show (column_properties x)) ++ 
>                             " -> " ++ show [ target_table (r) | r <- crossrefs, source_table r == table_name table, source_column r == column_header_name x ]
>                             ++ " <- " ++ show [ source_table (r) | r <- crossrefs, target_table r == table_name table, target_column r == column_header_name x ]
>                             ++ "\n"
>   -- todays_news = interesting_columns table
>   todays_news = columns table
