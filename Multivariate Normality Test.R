norMUL<-function(data)
{ 
  cat("UJI NORMALITAS MULTIVARIAT \n")
  cat("------------------------------\n")
  cat("H0 : data berdistribusi normal multivariat\n")
  cat("H1 : data tidak berdistribusi normal multivariat\n")
  b=nrow(data)
  k=ncol(data)
  d=rep(0,b)
  m=rep(0,k)
  S=cov(data)
  SS=solve(S)
  min=rep(0,k)
  for(i in 1:k)
  {
    m[i]=mean(data[,i])
  }
  for(i in 1:b)
  {
    min=t(data[i,])-m
    d[i]=t(min)%*%SS%*%min
  }
  cat("---------------------------------\n")
  cat("Jumlah data :",b,"\n")
  cat("Jumlah Variabel :",k,"\n")
  a=as.numeric(readline("masukkan taraf Signifikansi (a): "))
  cat("------------------------------------------\n")
  cat("menghitung nilai Chi-sq(a,df)\n")
  df=k
  chitab=qchisq(1-a,df)
  cat("Nilai Chi-sq (",a,",",df,") : ",chitab,"\n")
  cat("----------------------------------------------\n")
  cat("Vektor rata-rata\n")
  print(m)
  cat("------------------------------------------------\n")
  cat("Matriks Varian kovarian \n")
  print(S)
  cat("------------------------------------------------\n")
  cat("invers Matriks Varian kovarian \n")
  print(SS)
  cat("------------------------------------------------\n")
  cat("Nilai d[i]^2 adalah :\n")
  print(d)
  cat("\n")
  cat("H0 diterima jika terdapat lebih dari 50% nilai d[i]^2 yang kurang dari Chi-sq(a,df)\n")
  j=0
  l=0
  for(i in 1:b)
  {
    if(d[i]<chitab) j=j+1
    else l=l+1 
  }
  prosen=(j/b)*100
  prosen1=(l/b)*100
  cat("\nnilai d[i]^2 yang kurang dari Chi-sq(a,df) sebanyak ",j,"\n")
  cat("nilai d[i]^2 yang lebih dari Chi-sq(a,df) sebanyak ",l,"\n")
  if(prosen>(b/2)) cat("H0 diterima, data berdistribusi normal multivariat\n")
  else cat("Data tidak berdistribusi normal multivariat\n")
}

