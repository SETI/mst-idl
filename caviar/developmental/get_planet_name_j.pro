pro get_planet_name_j,planvar,plan_name_string

num=n_elements(planvar)

plan_name_string=strarr(num)

for i=0,num-1 do begin

if planvar[i] eq 599 then plan_name_string[i]='Jupiter'
if planvar[i] eq 501 then plan_name_string[i]='Io'
if planvar[i] eq 502 then plan_name_string[i]='Europa'
if planvar[i] eq 503 then plan_name_string[i]='Ganymede'
if planvar[i] eq 504 then plan_name_string[i]='Callisto'
if planvar[i] eq 505 then plan_name_string[i]='Amalthea'
if planvar[i] eq 506 then plan_name_string[i]='Himalia'
if planvar[i] eq 507 then plan_name_string[i]='Elara'
if planvar[i] eq 508 then plan_name_string[i]='Pasiphae'
if planvar[i] eq 509 then plan_name_string[i]='Sinope'
if planvar[i] eq 510 then plan_name_string[i]='Lysithea'
if planvar[i] eq 511 then plan_name_string[i]='Carme'
if planvar[i] eq 512 then plan_name_string[i]='Amanke'
if planvar[i] eq 513 then plan_name_string[i]='Leda'
if planvar[i] eq 514 then plan_name_string[i]='Thebe'
if planvar[i] eq 515 then plan_name_string[i]='Adrastea'
if planvar[i] eq 516 then plan_name_string[i]='Metis'

endfor

return
end
