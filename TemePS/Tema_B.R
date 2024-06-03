#Tema_B
#B1
volum_tor=function(R,r,N)
{
  C=0;
  for(i in 1:N)
  {
    #generează un număr aleator uniform pt x1 si x2 si z
    x1=runif(1,-R-r,R+r)
    x2=runif(1,-R-r,R+r)
    z=runif(1,-r,r)
    if(z^2+(sqrt(x1^2+x2^2)-R)^2<r^2)
      C=C+1;
  }
  #Calculează volumul exact al torului
  v_exacta=2*pi^2*R*r^2;
  v_total=(2 * (R + r))^2 * (2 * r);
  v_estimata=C/N*v_total;
  
  
  cat("Valoarea estimata este egala cu", v_estimata, "iar valoarea exacta este egala cu", v_exacta, "\n")
  cat("Eroarea relativa pentru esantionul de dimensiune", N, "este egala cu", abs(v_exacta - v_estimata)/abs(v_estimata), "\n")
  cat("Eroarea absoluta pentru esantionul de dimensiune", N, "este egala cu", abs(v_exacta - v_estimata), "\n")
  
}
volum_tor(10,3,10000)
volum_tor(10,3,20000)
volum_tor(10,3,50000)

#B2
triunghi=function(N)
{
  c=0;
  for(i in 1:N)
  {
    #generam un nr aleator in intervalul dat pt x si y
    x=runif(1,0,2);
    y=runif(1,0,12/5);
    if(y>=0 && y<=2*x && y<=(6-3*x))
      c=c+1;
  }
  cat("Aria lui T este egala cu",c/N*(2*(12/5))*2);
}
triunghi(20000)

#B3_a
ex_B3_a=function(N)
{
  sum=0;
  for(i in 1:N){
    x=runif(1,-1,1);
    sum=sum+(2*x-1)/(x^2-x-6);
  }
  cat("Valoarea estimata este", sum / N, "iar valoarea actuala este egala cu", log(3)-log(2),"\n")
}
ex_B3_a(100000)

#B3_b
ex_B3_b=function(N,a)
{
  sum=0;
  for (i in 1:N) {
    x = runif(1, a, 11)
    sum= sum + (x + 4) / sqrt(3 * x - 3)
  }
  
  cat("Valoarea estimata este", sum / N, "iar valoarea actuala este egala cu",61.2,"\n")
  }

ex_B3_b(100000, 100)

#B3_c
ex_B3_c=function(N,a){
  sum=0;
  for(i in 1:N){
    x=runif(1,1,a);
    sum=sum+x*e^(-x^2);
  }
  cat("Valoarea estimata este", s / N, "iar valoarea actuala este egala cu",1/2,"\n")
}
ex_B3_c(100000)

#B4_a


