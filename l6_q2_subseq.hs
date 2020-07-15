-- start: inclusive start index
-- end: exclusive end index
subseq start end list = take (end-start) (drop start list)