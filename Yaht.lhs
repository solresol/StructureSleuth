> module Yaht where
> import Char
> import List (intersperse)

> htmlBegin stylesheet title =
>  "<HTML>\n <HEAD>\n  <TITLE>" ++ title ++ "</TITLE>\n  <LINK HREF=\"" ++ stylesheet ++ "\" REL=\"stylesheet\" TYPE=\"text/css\">\n </HEAD>\n <BODY>\n  <H1>" ++ title ++ "</H1>\n"
> htmlList id stuff = 
>  "  <ul id=\"" ++ id ++ "\">\n" ++ (concat ["   <li>" ++ s ++ "</li>\n" | s <- stuff ]) ++ "</ul>\n"
> htmlLink target anchortext = "<a href=\"" ++ target ++ "\">" ++ anchortext ++ "</a>"
> htmlEnd = " </BODY>\n</HTML>\n"
> safeLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
> justDropLetters = "'/."
>
> htmlProtect :: String -> String
> htmlProtect "" = ""
> htmlProtect (c:cs) 
>   | c `elem` safeLetters = toUpper (c) : htmlProtect cs
>   | c `elem` justDropLetters = htmlProtect cs
>   | otherwise = '_' : htmlProtect cs
>

The following list is incomplete

> html_escapes = [('<',"&lt;"), ('>',"&gt;"), ('&',"&amp;")]
>
> lightHtmlProtection (c:cs) = lhp' (lookup c html_escapes)
>  where lhp' Nothing = c:lightHtmlProtection cs
>        lhp' (Just x) = x ++ lightHtmlProtection cs
> lightHtmlProtection "" = ""
>
> small x = "<small>" ++ x ++ "</small>"
> htmlItalic x = "<i>" ++ x ++ "</i>"
>
> htmlTable :: [String] -> String
> htmlTable rws = "<table><TR>" ++ (concat (intersperse "</TR><TR> "rws)) ++ "</TR></table>\n"
>
> htmlRow :: [String] -> String
> htmlRow tds = concat tds
> 
> htmlTh :: String -> String
> htmlTh x = "<TH>" ++ x ++ "</TH>"
> 
> htmlTd :: String -> String
> htmlTd x = "<TD>" ++ x ++ "</TD>"
