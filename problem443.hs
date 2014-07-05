g n = if (n <= 4) then 13
      else if (n > 4) then
	     let prev = g (n-1)
	     in prev + gcd(n, prev)
	  else 0