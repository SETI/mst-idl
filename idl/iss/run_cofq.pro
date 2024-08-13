nqq = 20
qq = (dindgen(nqq)+1)/nqq
cc = dblarr(nqq)
for j=0,nqq-2 do cc[j] = cofq(qq[j])
solid_circles
plot, qq, cc, ps=-8
save, qq, cc, filename='cofq.sav'

end
