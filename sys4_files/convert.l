## converts FORTH.f to a buffer ##

(let data (in 'FORTH.4 (make (while (link (rd 1)) ) ) )
	(out 'scr.4 
		(prinl (pack "init_buff db " (glue "," data) " 0")) ) )
(bye)		