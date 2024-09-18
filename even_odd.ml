let rec even n = if n=0 then "even" else odd (n-1)
 and odd n = if n=0 then "odd" else even (n-1);;