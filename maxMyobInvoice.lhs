> module Main where
> import TSV
> main = do
>   interact (maximum_invoice . make_table)
>    
> invoice_number_column_header = Header "Invoice #"
> maximum_invoice table = 
>  -- everything
>  -- (header_display)  ++ "\n" ++ 
>  -- invoice_number_column_display ++ "\n"
>  (max_invoice) ++ "\n"
>  where
>        invoice_number_column = select_column invoice_number_column_header table
>        invoice_number_column_display = show invoice_number_column
>        max_invoice = column_max invoice_number_column
>        everything = show table
