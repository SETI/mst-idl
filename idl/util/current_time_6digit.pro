function current_time_6digit

now = systime()
month = strmid(now,4,3)
day = strmid(now,8,2)
day = string( fix(day), fo='(I02)' )  ; Ensure leading zero if single-digit
year = strmid(now,22,2)
case month of
  'Jan':  month = '01'
  'Feb':  month = '02'
  'Mar':  month = '03'
  'Apr':  month = '04'
  'May':  month = '05'
  'Jun':  month = '06'
  'Jul':  month = '07'
  'Aug':  month = '08'
  'Sep':  month = '09'
  'Oct':  month = '10'
  'Nov':  month = '11'
  'Dec':  month = '12'
  else: stop, 'Unrecognized month'
endcase

now_6digit = strjoin([ year, month, day ])
return, now_6digit

end
