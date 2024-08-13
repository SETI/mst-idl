pro get_planet_name_u,planvar,plan_name_string

num=n_elements(planvar)

plan_name_string=strarr(num)

for i=0,num-1 do begin

if planvar[i] eq 799 then plan_name_string[i]='Uranus'
if planvar[i] eq 701 then plan_name_string[i]='Ariel'
if planvar[i] eq 702 then plan_name_string[i]='Umbriel'
if planvar[i] eq 703 then plan_name_string[i]='Titania'
if planvar[i] eq 704 then plan_name_string[i]='Oberon'
if planvar[i] eq 705 then plan_name_string[i]='Miranda'
if planvar[i] eq 706 then plan_name_string[i]='Cordelia'
if planvar[i] eq 707 then plan_name_string[i]='Ophelia'
if planvar[i] eq 708 then plan_name_string[i]='Bianca'
if planvar[i] eq 709 then plan_name_string[i]='Cressida'
if planvar[i] eq 710 then plan_name_string[i]='Desdemona'
if planvar[i] eq 711 then plan_name_string[i]='Juliet'
if planvar[i] eq 712 then plan_name_string[i]='Portia'
if planvar[i] eq 713 then plan_name_string[i]='Rosalind'
if planvar[i] eq 714 then plan_name_string[i]='Belinda'
if planvar[i] eq 715 then plan_name_string[i]='Puck'
if planvar[i] eq 725 then plan_name_string[i]='Perdita'
if planvar[i] eq 726 then plan_name_string[i]='Mab'
if planvar[i] eq 727 then plan_name_string[i]='Cupid'

endfor

return
end
