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
> column_analysis crossrefs table = concat (map one_column_analysis (columns table))
>  where
>   one_column_analysis x 
>    | fromMeStr ++ toMeStr == "" = ""
>    | otherwise  = (show (column_header_name x)) ++ " " ++ description ++ "\n"
>     where fromMe = [ (target_table r, target_column r, partial r) | r <- crossrefs, source_table r == table_name table, source_column r == column_header_name x ]
>           fromMeStr = if (length fromMe == 0) then "" else " -> " ++ show fromMe
>           toMe = [ (source_table r,source_column r, partial r) | r <- crossrefs, target_table r == table_name table, target_column r == column_header_name x ]
>           toMeStr = if (length toMe == 0) then "" else " <- " ++ show toMe
>           colProps = column_properties x
>           properties = if (length colProps == 0) then "" else show colProps
>           description = properties ++ fromMeStr ++ toMeStr