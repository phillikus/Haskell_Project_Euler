g k = if (k < 2000) then 1
      else g (k-2000) + g (k-1999)