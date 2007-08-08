> module TSV where
> import Char
> import List
> import System
> import IO
>
> make_tables :: IO [Table]
> select_column :: Header -> Table -> Column
> make_table_from_file :: String -> FilePath -> IO Table
> make_table :: String -> String -> Table
> make_table_from_stdin :: IO [Table]
> make_tables_from_filename_list :: [FilePath] -> IO [Table]
>    
> newtype Header = Header String deriving (Show,Eq,Ord)
> type CellContent = String
> 
> data Cell = Cell { headername :: Header, cell_contents :: CellContent }
>                   deriving (Show,Eq,Ord)
> newtype Row = Row [Cell]  deriving (Show, Eq)
>
> data ColumnProperties = NumericOnly | Distinct | NonstrictlyIncreasing
>                         | AlwaysBlank | Chunked | AlwaysConstant | DateLike
>                         | AlphaNumericOnly | IndexIsAValidValue
>     deriving (Show,Eq)
>  -- I should also add a flag to say "is only either a constant, or blank"
>  -- and is therefore ineligible to be the source or target of a table xref
>
> data Column = Column { column_header_name :: Header,
>                        column_values :: [CellContent],
>                        column_properties :: [ColumnProperties] }
>                              deriving (Show,Eq)
> data Table = Table { rows :: [Row], columns :: [Column], table_name :: String } 
>                              deriving (Show,Eq)
>
> data ChunkedTable = ChunkedTable Row Table
>
> columns_with_property prop table = filter with_prop (columns table)
>    where with_prop x = prop `elem` (column_properties x)
> columns_without_property prop table = filter without_prop (columns table)
>    where without_prop x = not (prop `elem` (column_properties x))
> numeric_columns = columns_with_property NumericOnly
> chunked_columns = columns_with_property Chunked
> nonblank_columns =  columns_without_property AlwaysBlank
> interesting_columns table = filter interesting (columns table)
>   where boring x = AlwaysBlank `elem` (column_properties x) ||
>                    AlwaysConstant `elem` (column_properties x)
>         interesting x = not (boring x)
> select_distinct_from col = [ Cell {headername = column_header_name col,
>                                    cell_contents = content } |
>                               content <- (nub . sort . column_values) col ]
> 
>
> make_row :: [Header] -> [String] -> Row
> make_row headers cell_texts = 
>  Row [Cell {headername = header, cell_contents = this_cell } |
>        (header,this_cell) <- (zip headers cell_texts) ]
>    -- I should check that the headers are unique. Otherwise
>    -- select_field stuffs up
>
> make_column :: [Row] -> Header -> Column
> make_column rows h =
>    Column { column_header_name = h, column_values = cell_values ,
>             column_properties = properties }
>  where cells = map (select_field h) rows 
>        cell_values = map cell_contents cells 
>        properties = analyse_column cell_values
>
> make_table' :: String -> [Header] -> [[String]] -> Table
> make_table' name headers textlines = Table {rows = table_rows, columns = table_columns, table_name = name }
>   where 
>     table_rows  = map (make_row headers) textlines
>     table_columns = map (make_column table_rows) headers
>
> header_match :: Header -> Cell -> Bool
> header_match h c = headername c == h
>
> select_field :: Header -> Row -> Cell
> select_field h (Row cs) = head (filter (header_match h) cs)
>
> row_has_field :: Header -> Row -> Bool
> row_has_field h (Row cs) = any (header_match h) cs
>
> select_column h ts = head  (filter matching_headername (columns ts))
>  where matching_headername x = column_header_name x == h
>
> row_contains :: Cell -> Row -> Bool
> row_contains c r = select_field (headername c) r == c
>
> select_rows_having :: Table -> Cell -> [Row]
> select_rows_having t c = filter (row_contains c) (rows t)
>

Important stuff... how to create 

>
> data LineEndingStyle = MacOS | Unix | Windows  deriving (Bounded,Enum,Eq)
> all_line_ending_styles = [MacOS .. Windows]
>  
> data TSVTokens = Normal Char | EndOfLine | EndOfField deriving Eq
> tokenize :: LineEndingStyle -> String -> [TSVTokens]
> tokenize _ "" = []
> tokenize MacOS ('\r':chars) = EndOfLine : (tokenize MacOS chars)
> tokenize Unix ('\n':chars) = EndOfLine : (tokenize Unix chars)
> tokenize Windows ('\r':('\n':chars)) = EndOfLine : (tokenize Windows chars)
> tokenize os ('\t':chars) = EndOfField : (tokenize os chars)
> tokenize os (c:chars) = (Normal c): (tokenize os chars)
>
> tokLines :: [TSVTokens] -> [[TSVTokens]]
> tokLines [] =  []
> tokLines s  =  let (l, s') = break (== EndOfLine) s
>                  in  l : case s' of
>                            []      -> []
>                            (_:s'') -> tokLines s''
>
> make_tables = do argv <- getArgs
>                  make_tables_from_filename_list argv
>
> make_tables_from_filename_list [] = make_table_from_stdin
> make_tables_from_filename_list args = 
>     mapM (make_table_from_file (longest_common_suffix args)) args
>   

> make_table_from_stdin =
>  do
>    input_data <- hGetContents stdin
>    return [make_table "" input_data]

>

>
> make_table_from_file suffix_to_strip path = 
>    do filecontents  <- readFile path
>       return (make_table (table_name_simplification suffix_to_strip path) filecontents)
>
> make_table name textfiledata = make_table' name headers good_lines
>  where
>   fielding_options = map (fieldify_using_line_ending textfiledata) all_line_ending_styles
>   fieldification_comparison (_,_,l1,x) (_,_,l2,y) 
>      | x < y = GT
>      | y > x = LT
>      | (length l1) > (length l2) = GT
>      | (length l2) < (length l1) = LT
>      | otherwise = EQ
>   (_,headers,good_lines,_) = last (sortBy fieldification_comparison fielding_options) 
> 
>

Make a table using just the rows in another table which contain a field with
the right value.

> make_sub_table :: Table -> Cell -> Table
> make_sub_table t c = Table {rows = relevant_rows, columns = new_columns, table_name = name }
>  where header = headername c
>        name = (table_name t) ++ " (" ++ (show (dehead header)) ++
>               " = " ++ (cell_contents c) ++ ")"
>        headers = (map column_header_name (columns t)) \\ [ header ]
>        relevant_rows = select_rows_having t c
>        new_columns = map (make_column relevant_rows) headers
> 

Is it the case that for a fixed value of column A, column B is also fixed? That is, does 
a == a' => b == b'  . This is trivially true if column A is Distinct. It's very interesting
however, when column A is Chunked, (or even less structured), because it means there is some
relationship between A and B

> constant_columns_within :: Table -> Cell -> [Header]
> constant_columns_within t cell = [ headername (head sc) | sc <- synthetic_columns, length sc == 1 ]
>  where all_headers = map column_header_name (columns t)
>        relevant_rows = select_rows_having t cell
>        synthetic_columns = nub (sort [ [ select_field h r | r <- relevant_rows ] | h <- all_headers ] )
>
> dependant_columns :: Table -> Header -> [Header]
> dependant_columns t h = fold2 intersect all_constant_sets
>  where col = select_column h t
>        primary_cells = select_distinct_from col
>        all_constant_sets = map (constant_columns_within t) primary_cells
>        fold2 f [x] = x
>        fold2 f (x:xs) = f x (fold2 f xs)
>        
>
> fieldify_using_line_ending :: String -> LineEndingStyle -> (LineEndingStyle,[Header],[[CellContent]],Int)
> fieldify_using_line_ending textfiledata os = (os,headers,good_lines,length bad_lines)
>  where (header_line:other_lines) = tokLines (tokenize os textfiledata)
>        number_of_columns = length headers
>        tabsplit_data = map eofSplit other_lines
>        good_lines = [map (deTokenify os) l|l<-tabsplit_data,length l == number_of_columns]
>        bad_lines = [l|l<-tabsplit_data,length l /= number_of_columns]
>        header_strings = map (deTokenify os) (eofSplit header_line)
>        headers = header_correction (map Header header_strings)
>
> deTokenify :: LineEndingStyle -> [TSVTokens] -> CellContent
> deTokenify _ [] = []
> deTokenify os ((Normal c):others) = c : deTokenify os others
> -- that's what it will mostly be... the next 4 lines just stop warnings
> deTokenify os ((EndOfField):others) = '\t' : deTokenify os others
> deTokenify Unix ((EndOfLine):others) = '\n' : deTokenify Unix others
> deTokenify MacOS ((EndOfLine):others) = '\r' : deTokenify MacOS others
> deTokenify Windows ((EndOfLine):others) = "\r\n" ++ (deTokenify Windows others)

> eofSplit :: [TSVTokens] -> [[TSVTokens]]
> eofSplit toks
>  | after == [] = []
>  | otherwise = before:(eofSplit (tail after))
>  where (before,after) = break (EndOfField ==) toks
>


Column Analysis..............




>
> analyses = [(all_numeric,NumericOnly),
>             (distinctive_and_not_trivial,Distinct),
>             (nonstrict_increasing_list,NonstrictlyIncreasing),
>             (all_blank,AlwaysBlank),
>             (chunky_list,Chunked),
>             (all_the_same,AlwaysConstant),
>             (datelike,DateLike),
>             (all_alphanumeric,AlphaNumericOnly),
>             (index_occurs,IndexIsAValidValue)]
>
> causatives = [(AlwaysBlank,[NumericOnly,AlwaysConstant,Chunked,NonstrictlyIncreasing]),
>               (Distinct,[Chunked]),
>               (AlwaysConstant,[Chunked,NonstrictlyIncreasing]),
>               (NumericOnly,[AlphaNumericOnly])]
>               
>
> analyse_column :: [CellContent] -> [ ColumnProperties ]
> analyse_column c = foldl simplify verbose_properties causatives
>   where
>         verbose_properties = concat (map analyse_column' analyses)
>         analyse_column' (analysis,flag) = if (analysis c) then [flag] else []
>         simplify cur_props (cause,effects) 
>                   | cause `elem` cur_props = cur_props \\ effects 
>                   | otherwise = cur_props
>   
>
> all_numeric :: [CellContent] -> Bool
> all_numeric = all (all isDigit) 
> all_blank = all blank_cell
> blank_cell x =   x `elem` zeros || (all isSpace x) 
> at_least_one_blank = any blank_cell
> all_alphanumeric = all (\x -> (length x /= 0) && (all isAlphaNum x))
> zeros = ["$0.00","0","0.00",""]
>
> all_the_same [] = True
> all_the_same [x] = True
> all_the_same (x:(y:ys)) = x==y && all_the_same (y:ys)
>
> index_occurs = elem "index"
>
> distinctive_list [] = True
> distinctive_list (x:xs) = not (x `elem` xs) && distinctive_list xs
>
> distinctive_and_not_trivial [] = False
> distinctive_and_not_trivial [x] = False
> distinctive_and_not_trivial xs = distinctive_list xs
>
> chunky_list [] = True
> chunky_list (x:xs) = not (x `elem` others) && chunky_list others
>    where others = dropWhile (x==) xs
>
> nonstrict_increasing_list [] = True
> nonstrict_increasing_list [x] = True
> nonstrict_increasing_list (u:(v:vs)) = u <= v && nonstrict_increasing_list (v:vs)
>
> datelike [] = True
> datelike (x:xs) = (has_two_slashes || has_two_dashes) && three_numeric_parts
>  where has_two_slashes = (filter ('/' ==) x) == "//"
>        has_two_dashes = (filter ('-' ==) x) == "--"
>        bothDigits a b = (isDigit a) && (isDigit b)
>        x_without_spaces = filter (\x -> not (isSpace x)) x
>        numeric_parts = filter (all isDigit) (groupBy bothDigits x_without_spaces)
>        three_numeric_parts = length numeric_parts == 3


Just generally useful things to know...............

(Could probably be optimised by checking NonstrictlyIncreasing property first,
perhaps.)


> column_max :: Column -> CellContent
> column_max c = maximum (column_values c)
>
> column_min :: Column -> CellContent
> column_min c = minimum (column_values c)


> data CrossTableRef = 
>        CrossTableRef { source_table :: String, source_column :: Header ,
>                        target_table :: String, target_column :: Header ,
>                        partial :: Bool -- i.e. does the source also contain blanks, which would
>                                           -- not necessarily match up to anything in the target table
>                      }
>              deriving (Eq,Show)
> -- target_table.target_column is either a unique column or a chunked one.
>
> cross_references :: [Table] -> [CrossTableRef]
> cross_references tables =
>   [ CrossTableRef { source_table = table_name t1, source_column = column_header_name c1, 
>                     target_table = table_name t2, target_column = column_header_name c2,
>                     partial = any blank_cell (column_values c1) } |
>      t1 <- tables,
>      t2 <- tables,
>      t1 /= t2,
>      c1 <- columns t1,
>      c2 <- columns t2,
>      not (AlwaysBlank `elem` (column_properties c1)),
>      not (AlwaysConstant `elem` (column_properties c1)),
>      -- not (AlwaysConstant `elem` (column_properties c1)),
>      -- column_header_name c1 == column_header_name c2 || isPrefixOf (dehead (column_header_name c1)) (dehead (column_header_name c2)) || isSuffixOf (dehead (column_header_name c1)) (dehead (column_header_name c2)) ||  isPrefixOf (dehead (column_header_name c2)) (dehead (column_header_name c1)) || isSuffixOf (dehead (column_header_name c2)) (dehead (column_header_name c1)) ,
>      Distinct `elem` (column_properties c2) || Chunked `elem` (column_properties c2) ,
>      AlwaysConstant `notElem` (column_properties c2),
>      AlwaysBlank `notElem` (column_properties c2),
>      nub (column_values c1) /= ["$0.00"],
>      all (\x -> (blank_cell x) || x `elem` (column_values c2)) (column_values c1)
>   ]
> 

Could improve that with the statistical likelihood of all of c1 appearing
in c2. e.g. if the range of c1 is quite small, perhaps that could be coincidence
Maybe column_header_names sharing words makes things very probable as well.

> dehead :: Header -> String
> dehead (Header x) = x
>
> header_correction :: [Header] -> [Header]
> header_correction hs = header_correction' "" hs
> header_correction' _ [] = []
> header_correction' leader ((Header h):hs) 
>  | isSpace (head h) = (Header combination) : (header_correction' leader hs)
>  | otherwise = (Header h) : header_correction' h hs
>      where (whitespace,real_stuff) = span isSpace h
>            first_letter = head real_stuff 
>            combination = case (findIndex (first_letter ==) leader) of
>                              Nothing -> (take (length whitespace) leader) ++ real_stuff
>                              (Just n) -> (take n leader) ++ real_stuff
>       
>
> table_name_simplification suffix_to_strip name = unshortened
>  where
>        suffix_length = length suffix_to_strip
>        suffix_part_of_name = reverse (take suffix_length (reverse name))
>        name' = if (suffix_part_of_name == suffix_to_strip) then
>                 reverse (drop suffix_length (reverse name))
>                 else name
>        unshortened =  map toUpper basename
>        validChars x = isAlphaNum x || isSpace x || x == '_'
>        basename = reverse (takeWhile validChars (reverse name'))
>        
> 

> chop_common_suffixes :: [String] -> [String]
> chop_common_suffixes strs
>   | length (nub (map last strs)) == 1 = chop_common_suffixes (map init strs)
>   | otherwise = strs
>
>
> longest_common_suffix [] = error "Can't get the longest common suffix of nothing."
> longest_common_suffix (x:[]) = ""
> longest_common_suffix (x:(y:[])) = map fst (reverse (takeWhile (\(u,v) -> u==v) (zip (reverse x) (reverse y))))
> longest_common_suffix (x:xs) = longest_common_suffix (x:(longest_common_suffix xs):[])

dot format, as described by  http://www.graphviz.org/ 


> output_dot_for_tables urlfunc tables = output_dot_for_tables_and_crossrefs urlfunc tables crossrefs
>  where crossrefs = cross_references tables
> output_dot_for_tables_and_crossrefs urlfunc tables crossrefs = 
>   "digraph untitled {\n rankdir=LR;\n" ++ (unlines table_stuff) ++ "\n" ++ (unlines crossref_stuff) ++ "\n}\n"
>  where all_crossref_dot_outs = concat (map (dot_outputs tables) crossrefs)
>        crossref_stuff = nub (sort all_crossref_dot_outs)
>        table_stuff = concat (map (dot_table_output urlfunc crossrefs) (zip tables [1..]))
>
> no_urls_for_dot_output _ _ = ""
>
> dot_table_output urlfunc crossrefs (t,id) 
>  | column_headers == [] = []
>  | otherwise =
>   ["subgraph cluster" ++ (show id) ++ " {"] ++
>   [" style=filled;","color=lightgrey;fontcolor=white;label="++(quote t_name)++";"] ++
>   (if (urlfunc t_name "") == "" then [] else [ "URL=\"" ++ (urlfunc t_name "") ++ "\";" ]) ++
>   --[(quote t_name) ++ " [color=lightgrey,style=filled,fontcolor=white,label=" ++ (quote t_name) ++ "];"] ++
>   (map (nodedisplay urlfunc t_name) c_names) ++
>   [ (q_my_h h) ++ " -> " ++ (q_my_h dh) ++ " [style=dotted,arrowhead=none];"
>    | h <- column_headers, dh <- dependant_columns t h , h /= dh, dh `elem` column_headers,
>      h `notElem` (dependant_columns t dh) || h < dh  ] ++
>   --[ (quote t_name) ++ " -> " ++ (q_my_h h) ++ " [style=invis,weight=1];" | h <- column_headers ]  ++
>   --[ (q_my_h h1) ++ " -> " ++ (q_my_h h2) ++ " [style=invis,weight=2];" | (h1,h2) <- adjacents] ++
>   ["}"]
>  where t_name = table_name t
>        column_headers = [column_header_name c | c <- columns t,
>                                 -- AlwaysConstant `notElem` (column_properties c),
>                                 AlwaysBlank `notElem` (column_properties c),
>                                 (column_header_name c)  `elem` column_names_mentioned
>                          ]
>        c_names = map dehead column_headers
>        q_my_h h = qualify_name t_name h
>        adjacents = zip column_headers (tail column_headers)
>        column_names_mentioned =
>          [ source_column c | c <- crossrefs, source_table c == t_name, not (partial c) ] ++
>          [ target_column c | c <- crossrefs, target_table c == t_name, not (partial c) ]
>    
> nodedisplay urlfunc t_name c_name =
>    (quote uniquename)  ++ " [shape=none,label=" ++ (quote displayname) ++  urlstr ++ "];"
>   where uniquename = t_name ++ "." ++ c_name
>         displayname = c_name
>         url_details = urlfunc t_name c_name
>         urlstr = if url_details == "" then "" else ",URL=\""++url_details++"\""
>
> dot_outputs tables crossref
>  | AlwaysBlank `elem` (column_properties c1)  ||
>    AlwaysBlank `elem` (column_properties c2)  -- ||
>    --AlwaysConstant `elem` (column_properties c1)  ||
>    -- AlwaysConstant `elem` (column_properties c2)  
>                        = []
>  | otherwise =
>     [ arrow (partial crossref) (source_table crossref,source_column crossref) 
>                                (target_table crossref,target_column crossref)
>     ]
>  where 
>    arrow False (tx,cx) (ty,cy) = (qualify_name tx cx) ++ " -> " ++ (qualify_name ty cy) ++ " ;"
>    -- arrow True (tx,cx) (ty,cy) = (qualify_name tx cx) ++ " -> " ++ (qualify_name ty cy) ++ " [style=dashed];"
>    arrow True (tx,cx) (ty,cy) = ""
>    t1 = head (filter (\x -> table_name x == source_table crossref) tables)
>    t2 = head (filter (\x -> table_name x == target_table crossref) tables)
>    c1 = select_column (source_column crossref) t1
>    c2 = select_column (target_column crossref) t2
> qualify_name t_name c_name = quote (t_name ++ "." ++ (dehead c_name))
>
> quote x = "\"" ++ x ++ "\"" -- should really backslash any " or \ characters in x, too.

