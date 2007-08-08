> module Main where
> import TSV
> import List
> import System
> import Directory
> import Yaht
> import Hashmere (hashString)

This program is a bit of a mess. It only just barely works.

Features for the future:
 - next and previous buttons on value pages
 - a few more "take me back up..." links
 - document this code, and make it a whole lot less like spaghetti
 - remove some of the stupid performance issues caused by me not caring about
   gobsmacking inefficiency
 - have a command-line flag to say you want to use graphviz or not (and perhaps
   even let you say where dot is, rather than relying on it being in your path)


>
> data OutputGrouping = WholeTableChunk | ChunkedBy [Header]
> data Location = Location { filesys :: FilePath , url :: String }
> data Weave = Weave { base :: Location , refs :: [CrossTableRef], tbl :: Table , all_tables :: [ Table ] }
> chunking :: [CrossTableRef] -> Table -> OutputGrouping
> parseCmdLine :: [String] -> IO (Location,[String])
>
>
>
> columnDataDirectory :: Weave -> Header -> Location
> indexPageName :: Weave -> Header -> Location
> valuePageName :: Weave -> Cell -> Location
>
> outputTableChunkedByColumn :: Weave -> OutputGrouping -> IO ()
> outputTableChunkedOnce :: Weave -> Header -> IO ()
>
> outputIndexPage :: Weave -> Column -> IO ()
> outputValuePage :: Weave -> Cell -> IO ()
>
> outputTopLevelIndexPage :: [(Weave,OutputGrouping)] -> IO ()
> outputDotFormat :: [Weave] -> IO ()
>
> main = 
>  do
>   argv <- getArgs
>   (output,filenames) <- parseCmdLine argv
>   tables <- make_tables_from_filename_list filenames
>   crossrefs  <- return (cross_references tables)
>   weaves <- return [Weave { base = output, refs = crossrefs, tbl = t, all_tables = tables }| t <- tables]
>   weaves_and_chunks <- return [(w,chunking (refs w) (tbl w))  | w <- weaves]
>   outputDotFormat weaves
>   outputTopLevelIndexPage weaves_and_chunks
>   mapM (\(w,g) -> outputTableChunkedByColumn w g) weaves_and_chunks
>

I really should use a proper getopts package here, but it just grew without
planning....

> parseCmdLine [] = do  cwd <- getCurrentDirectory
>                       return (Location { filesys = cwd, url = ""} ,[])
> parseCmdLine ("-d" : (outdir : (remainder))) = 
>   do (output,remainder) <- parseCmdLine remainder
>      return (output { filesys = outdir },remainder)
> parseCmdLine ("-u" : (outurl : (remainder))) = 
>   do (output,remainder) <- parseCmdLine remainder
>      return (output { url = outurl },remainder)
> parseCmdLine (x:xs) =
>   do (outdir,remainder) <- parseCmdLine xs 
>      return (outdir,x:remainder) 
>
> chunking crossrefs t 
>  | all_interesting_columns == []   = WholeTableChunk
>  | otherwise = ChunkedBy all_interesting_columns
>   where
>    t_name = table_name t
>    source_refs = [ source_column x| x <- crossrefs, source_table x == t_name ]
>    target_refs = [ target_column x| x <- crossrefs, target_table x == t_name ]
>    prechunked = map column_header_name (chunked_columns t)
>    distinctive = map column_header_name (columns_with_property Distinct t)
>    increasing = map column_header_name (columns_with_property NonstrictlyIncreasing t)
>    datelike = map column_header_name (columns_with_property DateLike t)
>    all_interesting_columns = nub (sort (source_refs ++ target_refs ++ prechunked ++ distinctive ++ increasing ++ datelike))
>  

> outputTableChunkedByColumn weave WholeTableChunk =
>  do
>    tabledirExists <- doesDirectoryExist tabledir
>    if tabledirExists then return () else createDirectory tabledir
>    putStrLn t_name
>    writeFile (filesys (tableIndexPageName weave)) contents
>   where
>     tabledir = filesys (tableDataDirectory weave)
>     t_name = table_name (tbl weave)
>     contents = (htmlBegin "" (lightHtmlProtection t_name)) ++
>                (show_table weave (tbl weave)) ++
>                htmlEnd
> outputTableChunkedByColumn weave (ChunkedBy hs) =
>   do
>    tabledirExists <- doesDirectoryExist tabledir
>    if tabledirExists then return () else createDirectory tabledir
>    writeFile (filesys (tableIndexPageName weave)) contents
>    lots_of_nulls <- mapM (outputTableChunkedOnce weave) hs
>    return ()
>   where 
>     tabledir = filesys (tableDataDirectory weave)
>     t_name = table_name (tbl weave)
>     contents = (htmlBegin "" (lightHtmlProtection t_name)) ++
>                (htmlList "" (map chunkinglink hs)) ++
>                htmlEnd
>     chunkinglink h = (htmlLinkToTableIndex weave h) ++ (show_refs weave h)
>     
> 
> show_refs weave h = (show_refs' weave " <-> " bothrefs) ++
>                     (show_refs' weave " <- " (inrefs \\ bothrefs)) ++ 
>                     (show_refs' weave " -> " (outrefs \\ bothrefs))
>  where outrefs = outward_references weave h
>        inrefs = inward_references weave h
>        bothrefs = outrefs `intersect` inrefs
> show_refs' :: Weave -> String -> [(String,Header,Bool)] -> String
> show_refs' _ _ [] = ""
> show_refs' weave introtext xrefs = 
>    small (htmlList "" (map (show_ref' weave introtext) xrefs))
> show_ref' :: Weave -> String -> (String,Header,Bool) -> String
> show_ref' weave introtext (other_table_name,other_header,is_partial)
>  | is_partial = introtext ++ (htmlItalic basic_link)
>  | otherwise = introtext ++ basic_link
>  where basic_link = htmlLink desturl anchortext
>        desturl = url (indexPageName new_weave other_header)
>        anchortext = lightHtmlProtection (other_table_name ++ "." ++ (dehead other_header))
>        new_weave = weave { tbl = tablename2table weave other_table_name }
>
> tablename2table weave t_name =
>   head (filter (\x -> table_name x == t_name) (all_tables weave))
>
> outward_references weave h = 
>    [ (target_table xref, target_column xref, partial xref) 
>          |  xref <- refs weave, 
>             source_table xref == table_name (tbl weave),
>             source_column xref == h ]
>
> inward_references weave h = 
>    [ (source_table xref, source_column xref, partial xref) 
>          |  xref <- refs weave, 
>             target_table xref == table_name (tbl weave),
>             target_column xref == h ]
>
> 
>
> outputTableChunkedOnce weave h =
>   do
>     putStrLn (filesys (indexPageName weave h)) -- just to show progress
>     outputIndexPage weave col
>     mapM (outputValuePage weave) (select_distinct_from col)
>     return ()
>   where col = select_column h (tbl weave)
>
>
> below :: Location -> String -> Location
> below baseloc subthing = 
>   Location {
>      filesys = (filesys baseloc) ++ "/" ++ subthing,
>      url = (url baseloc) ++ "/" ++ subthing
>   }
>
> tableDataDirectory weave = below (base weave) protected_table_name
>   where protected_table_name = htmlProtect (table_name (tbl weave))
> columnDataDirectory weave h = below parent protected_header_name
>   where protected_header_name = htmlProtect (dehead h)
>         parent = tableDataDirectory weave
> tableIndexPageName weave = below (tableDataDirectory weave) "index.html"
> indexPageName weave h = below (columnDataDirectory weave h)  "index.html"
> valuePageName weave cell = below (columnDataDirectory weave h) pagename
>   where pagename = if (AlphaNumericOnly `notElem` column_props) &&
>                       (NumericOnly `notElem` column_props) &&
>                       (IndexIsAValidValue `notElem` column_props)
>                     then (show (hashString v)) ++ ".html"
>                     else v ++ ".html"
>         h = headername cell
>         v = cell_contents cell
>         t = tbl weave
>         column_props = column_properties (select_column h t)
>
> outputIndexPage weave col =
>  do
>    dirExists <- doesDirectoryExist outputdir
>    if dirExists then return () else createDirectory outputdir
>    writeFile (filesys (indexPageName weave h)) content
>  where h = column_header_name col
>        outputdir = (filesys (columnDataDirectory weave h))
>        content = (htmlBegin "" title) ++
>                  (show_refs weave h) ++ 
>                  (htmlTable whole_schambula) ++
>                  htmlEnd
>        tname = table_name (tbl weave)
>        hname = dehead (column_header_name col)
>        title = tname ++ " by " ++ hname
>        contents = select_distinct_from col
>        t = tbl weave
>        other_column_headers = (dependant_columns t h) \\ [h]
>        lotsa_headings = (prettifyHeader weave h) : map (prettifyHeader weave) other_column_headers
>        make_my_row c = head (select_rows_having t c)
>        make_my_row_html c = (map (\h' -> htmlTd (cell_contents (select_field h' (make_my_row c)))) other_column_headers)
>        row_with_links c = htmlRow ( (htmlTd (valueLink weave c))  : (make_my_row_html c))
>        whole_schambula = (htmlRow lotsa_headings) : (map row_with_links contents)
>
>
> valueLink weave cell = htmlLink desturl anchortext
>   where h = headername cell
>         v = cell_contents cell
>         desturl = url (valuePageName weave cell)
>         anchortext = if (v == "") then "<i>(null)</i>" 
>                                   else (lightHtmlProtection v)
>
> outputValuePage weave cell = 
>  do
>    putStrLn outfile -- just to show progress
>    -- safely assume the directory exists
>    writeFile outfile content
>  where h = headername cell
>        v = cell_contents cell
>        outfile = filesys (valuePageName weave cell)
>        content = (htmlBegin "" title) ++
>                  (small (htmlLink parent parent_name)) ++
>                  (show_table weave sub_table) ++
>                  htmlEnd
>        title = lightHtmlProtection (t_name ++ "." ++ (dehead h) ++ ": " ++ v)
>        t_name = table_name (tbl weave)
>        sub_table = make_sub_table (tbl weave) cell
>        parent = url (tableIndexPageName weave)
>        parent_name = "Back to " ++ (lightHtmlProtection t_name)

> show_table weave sub_table =
>   (constants_output non_zero_constants) ++
>   others_output ++
>   (constants_output zero_constants) ++
>   blanks_output 
>  where
>    constants = columns_with_property AlwaysConstant sub_table
>    non_zero_constants = [c | c <- constants, 
>                             head (column_values c) `notElem` zeros]
>    zero_constants = [c | c <- constants, 
>                             head (column_values c) `elem` zeros]
>    zeros = ["$0.00","","0.00"]
>    blanks = columns_with_property AlwaysBlank sub_table
>    others = interesting_columns sub_table
>    constants_output consts = 
>     htmlTable [ htmlRow [ prettifyHeader weave (column_header_name x) , 
>                                htmlTd (head (column_values x))
>                              ]
>                       | x <- consts]
>    blanks_output =  ""
>    other_output_headers =
>        [ prettifyHeader weave (column_header_name col') | col' <- others ]
>    others_output = 
>         htmlTable (
>           [htmlRow other_output_headers ] ++ 
>           [
>           htmlRow [ htmlTd (cell_contents (select_field (column_header_name col') row))  | col' <- others ]
>             | row <- rows sub_table
>           ]
>         )
>
>
>
> prettifyHeader weave header 
>  | isChunkedByThis weave header = htmlTh (htmlLinkToTableIndex weave header)
>  | otherwise = htmlTh (dehead header)

>
> isChunkedByThis weave header = answer (chunking (refs weave) (tbl weave))
>   where answer WholeTableChunk = False
>         answer (ChunkedBy hs) = header `elem` hs


> outputDotFormat weaves =
>  do
>    topdirExists <- doesDirectoryExist outputdir
>    if topdirExists then return () else createDirectory outputdir
>    writeFile dotloc content
>    system ("dot -Tcmapx -o " ++ maploc ++ " " ++ dotloc)
>    system ("dot -Tpng -o" ++ imgloc ++ " " ++ dotloc) 
>    return ()
>  where 
>    weave = head weaves
>    outputloc = base weave
>    outputdir = filesys outputloc
>    all_tbls = all_tables weave
>    dotloc = filesys (below outputloc "index.dot")
>    maploc = filesys (below outputloc "index.map")
>    imgloc = filesys (below outputloc "index.png")
>    xrefs = refs weave
>    content = output_dot_for_tables_and_crossrefs urlfunc all_tbls xrefs
>    urlfunc t_name c_name 
>     | c_name /= "" = url (indexPageName (weave {tbl=t}) h)
>     | otherwise = url (tableIndexPageName (weave {tbl=t}))
>        where
>         h = Header c_name
>         t = head (filter (\x -> table_name x == t_name) all_tbls)
>
> outputTopLevelIndexPage weaves_and_chunks = 
>  do 
>    topdirExists <- doesDirectoryExist outputdir
>    if topdirExists then return () else createDirectory outputdir
>    mapdata <- readFile maploc
>    writeFile (filesys (below outputloc "index.html")) 
>     (content ++ mapdata ++ "<IMG SRC=\"index.png\" USEMAP=\"#untitled\">"
>                                         ++ htmlEnd)
>  where weaves = map fst weaves_and_chunks
>        outputloc = base (head weaves)
>        maploc = filesys (below outputloc "index.map")
>        outputdir = filesys outputloc
>        content = (htmlBegin "" "Index to all tables") ++
>                  (htmlList "" tablelinks)
>        tablelinks = map tableLink weaves_and_chunks
>        tableLink (w,WholeTableChunk) = (linkToTable w) 
>        tableLink (w,ChunkedBy hs)= (linkToTable w)  ++ " " ++  dashsep (map (chunkLinks w) hs)
>        linkToTable w = (htmlLink (url (tableIndexPageName w)) (lightHtmlProtection (table_name (tbl w)))) 
>        dashsep stuff = concat (intersperse " -- " stuff)
>        chunkLinks weave h = small (htmlLinkToTableIndex weave h)
>        -- I should also do something here with an image map and image from
>        -- dot (assuming they've got graphviz installed or equivalent)
>
> htmlLinkToTableIndex weave h = 
>   htmlLink (url (indexPageName weave h)) (lightHtmlProtection (dehead h))
