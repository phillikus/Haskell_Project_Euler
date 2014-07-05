days "FEBL" = 29
days "FEB" = 28
days "SEP" = 30
days "APR" = 30
days "JUN" = 30
days "NOV" = 30
days _ = 31

daysInMonths = [31,28,31,30,31,30,31,31,30,31,30,31]

isLeapYear 1900 = False
isLeapYear 2000 = True
isLeapYear n = n `mod` 4 == 0

countSundays start end = let f month index = if 