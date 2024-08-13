; Plot created while reviewing paper by Lewis and Stewart (2006)

vratio = [2.7,2.7,2.7,3.7,4.8,replicate(3.6,7)]
mratio = [1.25,1.34,1.29,1.78,2.81,1.89,2.04,2.14,1.71,1.81,1.56,1.98]
mrationg = [1.2,1.17,1.2,0,0,1.49,1.43,1.61,1.55,1.86,1.51,1.63]

plot, vratio, mratio ,ps=4, $
      xtit='Volume Ratio (R!DHill!N/R!DMoonlet!N)!U3!N',$
      ytit='Mass Ratio after Clustering', yr=[1,3]
oplot, vratio, mrationg, ps=6

end
