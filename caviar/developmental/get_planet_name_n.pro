pro get_planet_name_n,planvar,plan_name_string

num=n_elements(planvar)

plan_name_string=strarr(num)

for i=0,num-1 do begin

if planvar[i] eq 8 then plan_name_string[i]='Neptune'
if planvar[i] eq 801 then plan_name_string[i]='Triton'
if planvar[i] eq 802 then plan_name_string[i]='Nereid'
if planvar[i] eq 803 then plan_name_string[i]='Naiad'
if planvar[i] eq 804 then plan_name_string[i]='Thalassa'
if planvar[i] eq 805 then plan_name_string[i]='Despina'
if planvar[i] eq 806 then plan_name_string[i]='Galatea'
if planvar[i] eq 807 then plan_name_string[i]='Larissa'
if planvar[i] eq 808 then plan_name_string[i]='Proteus'

endfor

return
end
