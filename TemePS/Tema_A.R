#Tema_A
#A1_a
calcul_probabilitati=function(lambda,p,n,m,k)
{
  poisson=dpois(k:m,lambda);
  geometric=dgeom(k:m,p);
  binomial=dbinom(k:m,n,p);
}
k=2
m=11
n=10
lambda=3
p=0.4
calcul_probabilitati(3,0.4,10,11,2)

#A1_b
grafic=function(lambda,p,n,m,k)
{
  barplot(dpois(k:m,lambda));
  barplot(dgeom(k:m,p));
  barplot(dbinom(k:m,n,p));
}
grafic(3,0.4,10,11,2)

#A1_c
gaseste_k0=function(lambda)
{
  x=1-10^(-6);
  k0=0;
  while(ppois(k0,lambda)<=x)
    k0=k0+1;
  
  return(k0);
}
#ppois->funcția de distribuție cumulativă

#A2_a
note_data=function(file_name)
{
  note=scan(file_name);
  
  #Esantioanele
  P=note[['P']];
  S=note[['S']];
  
  frecv_abs_P=table(P);
  frecv_abs_S=table(S);
  print(frecv_abs_P);
  print(frecv_abs_S);
  
  frecv_rel_P=as.vector(prop.table(frecv_abs_P));
  frecv_rel_S=as.vector(prop.table(frecv_abs_S));
  print(frecv_rel_P);
  print(frecv_rel_S);
  
  #mediile esantioanelor
  mean_P=mean(P);
  mean_S=mean(S);
  print(mean_P);
  print(mean_S);
  
}
#A2_b
clean_outliers=function(file_name,sample_name)
{
  x=scan(file_name);
  m=mean(x);
  
  s=sd(x);
  # deviatia standard a val din x
  y=vector();
  h=1;
  for(i in 1:length(x)){
    if (x[i] < m + 2 * s & x[i] > m - 2 * s) {
      y[h] <- x[i]
      h <- h + 1
    }
  }
  print("Eșantionul fără valori aberante:")
  print(y)
  
  #hist cu intervalele de adaugat
 }








