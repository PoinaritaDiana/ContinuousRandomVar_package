#EXERCITIUL 1 - CONSTANTA DE NORMALIZARE

constantaNorm <- function(f) {
  #Primeste functia ca parametru
  x <- seq(-1000, 1000, 0.1)
  if(!all(f(x) >0)) {
    #o verificare pur orientativa a pozitivitatii
    #deoarece o verificare pe R nu este posibila
    #lasand adevarata verificare in seama utilizatorului

    return(NULL)
  }

  integrala <- integrate(f,-Inf, Inf)
  #presupunand ca e integrabila, o integrez pe R

  rez <- 1
  #rez reprezinta valoarea constantei de norm
  if(integrala$value != 1 && integrala$value != 0) {
    rez<-1/integrala$value
    #1/integrala pentru ca reprezinta acea valoare
    #cu care trb sa inmultim integrala
    #sa obtinem 1
  }
  else {
    rez <- NULL
    #null pentru ca nu exista
  }
  rez
}



#EXERCITIUL 2 - VERIFICARE DENSITATE DE PROBABILITATE

esteDensitate <- function(f) {
  x <- seq(-1000,1000,0.1)
  #Evident, o verificare pe intreg R-ul este imposibila
  #asa ca lasam la mana utilizatorului sa determine daca f(x) > 0 pentru orice x din R
  #aceasta verificare fiind pur orientativa
  este <- TRUE
  if(!all(f(x) > 0)) {
    return(FALSE)
  }
  integrala <- integrate(f,-Inf, Inf)
  if(integrala$value == 1) {
    este <- TRUE
  }
  este
}



#EXERCITIUL 3
#M-am gandit ca pentru un obiect trebuie sa construiesc o clasa
#Construiesc o clasa "Variabila Aleatoare" in care retin tipul variabilei
#si densitatea de repartitie impreuna cu intervalul pe care este definita (daca avem pe ramuri)
setClass("variabilaAleatoare", slots=list(intervaleDensitate="vector",          #vector de intervale
                                          functiiDensitate="vector"))           #vector de functii pe intervale
#valoarea functiei pe cazul "altfel" se considera 0


#Functie pentru a construi o noua variabila aleatoare (de la tastatura)
creareVariabilaTastatura <- function(tipVariabila)
{
  #Un nou obiect din clasa variabilaAleatoare
  var <- new("variabilaAleatoare")

  #Ii cer utilizatorului numarul de intervale pe care este definita densitatea
  nr_intervale_f_densitate <- as.integer(readline("Introduceti numarul de intervale/ramuri: "))-1

  #dimensiunea este = nr_intervale_f_densitate-1 pentru ca tratez separat cazul "altfel"
  #nu este nevoie sa retin o functie cu un interval aferent, se considera automat valoarea 0
  #Un vector in care retinem intervalele pe care este definita densitatea de repartitie (vector de liste)
  var@intervaleDensitate <- vector(mode = "list", nr_intervale_f_densitate)

  #Un vector in care retinem functia densitatii pe intervalul corespunzator
  var@functiiDensitate <- vector(mode = "list", nr_intervale_f_densitate)


  #Se citeste fiecare interval impreuna cu densitatea corespunzatoare
  for(i in 1:nr_intervale_f_densitate)
  {
    #Se retine functia (ca un string - ??Nu am stiut cum altfel)
    var@functiiDensitate[[i]] <- readline( "Introduceti functia: ")

    #Se retine si intervalul pe care aceasta este definita
    startIntervalx <- as.integer(readline("Introduceti inceputul intervalului pentru x: "))
    finishIntervalx <- as.integer(readline("Introduceti sfarsitul intervalului pentru x: "))

    #Daca este variabila aleatoare bidimensionala, trebuie sa retinem si intervalul pentru y
    if(tipVariabila==2)
    {
      startIntervaly <- as.integer(readline("Introduceti inceputul intervalului pentru y: "))
      finishIntervaly <- as.integer(readline("Introduceti sfarsitul intervalului pentru y:"))

      var@intervaleDensitate[[i]] <- list(startIntervalx, finishIntervalx, startIntervaly, finishIntervaly)
    }
    else{
      var@intervaleDensitate[[i]] <- list(startIntervalx, finishIntervalx)
    }
  }

  #Returnez noul obiect construit (variabila aleatoare)
  return(var)
}


#Construire obiect cu parametru dat
creareVariabilaParam <- function(vectorIntervale,vectorFunctii, valoareOutIntervale)
{
  #Un nou obiect din clasa variabilaAleatoare
  var <- new("variabilaAleatoare")
  var@intervaleDensitate <- vectorIntervale
  var@functiiDensitate <- vectorFunctii

  return(var)
}


#Daca se doreste o variabila aleatoare continua unidimensionala -> se apeleaza creareVariabila(1)
#Daca se doreste o variabila aleatoare continua bidimensionala -> se apeleaza creareVariabila(2)
#numarul de "ramuri" ale functiei = length(varX@intervaleDensitate) (+1 daca se doreste si cazul "altfel")
#Daca nu se doreste citirea de la tastatura, se va trimite ca parametru: vector de intervale si vector de functii,
#precum si valoarea pentru cazul "altfel"



#EXERCITIUL 4 - REPREZENTAREA GRAFICA A DENSITATII SI A FUNCTIEI DE REPARTITIE

afisareDensitate <- function(f,a,b,s) {
  x <- seq(a,b,length=s)
  d <- density(f(x))
  plot(d)
}

afisareDistributie <- function(f,a,b,s){
  domain <- seq(a,b,length=s)
  values <- c()

  for(x in domain){
    aux <- integrate(f,-Inf, x)$value
    values<-append(values,aux)
  }
  plot(x = domain, y = values)
}



#EXERCITIUL 5 - Calculul momentelor initiale si centrate pana la ordinul 4

calcMoment<-function(variabila,limInf=-Inf,limSup=Inf){
  ordin=1
  momenteInitiale<-c()        #Cream un vector de momente initiale pe care sa il returnam
  momenteCentrate<-c()        #Cream un vector de momente centrate pe care sa il returnam
  ok1=0
  ok2=0

  while(ok1==0 && ok2==0 && ordin<=4)   #Calculez momementele pana la ordinul 4 sau pana acestea se mai pot calcula
  {
    g<-function(x){x^ordin}     #Pentru formula avem nevoie de x^ordin * f(x)
    check<-tryCatch({
      media<-calcMedieCont(variabila,g,limInf,limSup)
    },error=function(err){
      print(paste('Nu se mai pot calcula momente initiale!Incepand cu ordinul:',ordin))
      return(-1)

    })
    if(check==-1)
    {

      rez<-rbind(momenteInitiale,momenteCentrate)
      return(rez)
      stop()
      break
      exit()
    }
    else{
      media<-calcMedieCont(variabila,g,limInf,limSup)
      momenteInitiale<-c(momenteInitiale,media) #Adaugam momentul initial de ordin r in vector in cazul in care s-a putut calcula
      #Daca am reusit sa calculam momentul initial de ordin r calculam mai departe
      h<-function(x){(x-media)^ordin}
      ok2=0
      check2<-tryCatch({
        momCentrat<-calcMedieCont(variabila,h,limInf,limSup)#Adaugam momentul centrat de ordin r in vector in cazul in care s-a putut calcula
      },error=function(err){
        print(paste('Nu se mai pot calcula momente centrate!Incepand cu ordinul:',ordin))
        return(-1)
      })
      if(check2==-1)                 #Daca nu s-a putut calcula, oprim functia
      {
        rez<-rbind(momenteInitiale,momenteCentrate)
        return(rez)
        stop()
        break
      }
      else{                       #In caz contrar mergem mai departe
        momCentrat<-calcMedieCont(variabila,h,limInf,limSup)#Adaugam momentul centrat de ordin r in vector in cazul in care s-a putut calcula
        momenteCentrate<-c(momenteCentrate,momCentrat)
        ordin=ordin+1
      }
    }
  }

  rez<-rbind(momenteInitiale,momenteCentrate)
  return(rez)
}



#EXERCITIUL 6 -Calculul mediei si dispersiei unei variabile aleatoare g(X)

calcMedieCont<-function(func,G,limInf=-Inf,limSup=Inf){    #Calculez media->ca valori primim functia si
  #Eventual capetele intervalului,in caz contrar va ramane
  #Integrala standard de la minus infinit la infinit

  parametru<-function(x){G(x)*func(x)}
  rez<-integrate(parametru,lower=limInf,upper=limSup)$value #Aplicam integrala din functia primita ca parametru*repartitia
  return(rez)
}


#Pentru calculul dispersiei vom folosi acelasi principiu
#Pentru a obtine media apelam functia definita anterior
#Aplicam formula pentru dispersie
calcDispCont<-function(func,G,limInf=-Inf,limSup=Inf)    #Calculam dispersia=momentul centrat de ordin 2
{
  media<-calcMedieCont(func,G,limInf,limSup)
  parametru<-function(x){((x-media)^2)*func(x)}
  rez<-integrate(parametru,lower=limInf,upper=limSup)$value
  return(rez)
}



#EXERCITIUL 8
# Afisarea unei "fise de sinteza" care sa contina informatii de baza despre respectiva
# repartitie. Relevant aici ar fi sa precizati pentru ce e folosita in mod uzual
# acea repartitie, semnificatia parametrilor, media, dispersia etc.

#Fiecare repartite este reprezentata de un vector de string-uri cu informatiile aferente

#Functia primeste ca parametru o repartitie si afiseaza informatii desprea aceasta
afisare_fisa_sinteza <- function(nr_fisa){

  if(nr_fisa==1){
    cat( c("REPARTITIA UNIFORMA\n
                         O variabila aleatoare continua X este repartizata uniform pe intervalul (a,b) daca admite densitatea de repartitie:\n
                         f(x) = 1/(b-a), x este in intervalul (a,b)\n
                              = 0, in caz contrar\n
                         Notatie:\n
                         X ~ U((a,b)), unde (a,b) este intervalul pe care este repartizata variabila aleatoare X\n
                         Media:\n
                         E[X] = integrala de la a la b din x*f(x) dx = (a+b)/2\n
                         Dispersia:\n
                         Var(X) = E[X^2]-E[x]^2\n
                         E[X^2] = integrala de la a la b din (x^2)*f(x) dx = (a^2 + a*b + b^2)/3\n
                         Prin urmare, Var(X) = ((b-a)^2)/12\n
                         Sursa: Probabilitati si Statistica Curs 9, Prof. Alexandru Amarioarei"))
  }

  if(nr_fisa==2){
    cat( c("REPARTITIA EXPONENTIALA\n
                              O variabila aleatoare continua X este repartizata exponential de parametru lambda > 0 daca admite densitatea de repartitie:\n
                              f(x) = lambda * e^(-lambda*x), x > 0\n
                                   = 0, in caz contrar\n
                              Utilizare: Modeleaza timpul de asteptare pana la aparitia unui eveniment de interes.\n
                              Notatie:\n
                              X ~ Exp(lambda)\n
                              Media:\n
                              E[X] = integrala de la -Inf la +Inf din x*f(x) dx = 1/lambda \n
                              Dispersia:\n
                              Var(X) = E[X^2]-E[x]^2\n
                              E[X^2] = integrala de la -Inf la +Inf din (x^2)*f(x) dx = 2/(lambda^2)\n
                              Prin urmare, Var(X) = 1/(lambda^2)\n
                              Sursa: Probabilitati si Statistica Curs 10, Prof. Alexandru Amarioarei"))
  }

  if(nr_fisa==3){
    cat( c("REPARTITIA NORMALA\n
                            O variabila aleatoare continua X este repartizata normal (sau Gaussian) de parametru miu si sigma^2 daca admite densitatea de repartitie:\n
                            f(x) = (1/(sqrt(2pi)*sigma))*e^(-(x-miu)^2/2*sigma^2), x in R \n
                            Notatie:\n
                            X ~  N(miu,sigma^2)\n
                            In cazul in care miu = 0 si sigma = 1 spunem ca v.a. X este repartizata normal standard si notam X ~ N(0,1) si putem calcula:\n
                            Media:\n
                            E[X] = integrala de la -Inf la +Inf din x*f(x) dx = 0 \n
                            Dispersia:\n
                            Var(X) = E[X^2]-E[x]^2\n
                            E[X^2] = integrala de la -Inf la +Inf din (x^2)*f(x) dx = 1\n
                            Prin urmare, Var(X) = 1\n
                            Sursa: Probabilitati si Statistica Curs 10, Prof. Alexandru Amarioarei"))
  }


  if(nr_fisa==4){
    cat(c("\nREPARTITIA GAMMA\n
                        O variabila aleatoare X este distribuita gamma cu parametrii lambda si p daca are densitatea de probabilitate de forma:\n
                        f(x) = (lambda^p * x^(p-1) * e^(-lambda*x))/gamma(p), x > 0 \n
                        Notatie:\n
                        X ~ Gamma [p, lambda]\n
                        Media:\n
                        E[X] = integrala de la -Inf la +Inf din x*f(x) dx = p/lambda \n
                        Dispersia:\n
                        Var(X) = E[X^2]-E[x]^2 = p/lambda^2\n
                        Sursa: http://math.etc.tuiasi.ro/rstrugariu/cursuri/SPD2015/c7.pdf"))
  }

  if(nr_fisa==5){
    cat(c("\nREPARTITIA X-PATRAT\n
                    O variabila aleatoare X este distribuita X-patrat cu n grade de libertate daca are densitatea de probabilitate de forma:\n
                    f(x) = (1/(2^(n/2) * gamma(n/2))) * x^((n/2)-1) * e^(-x/2), x > 0 \n
                    Notatie:\n
                    X ~ X^2(n), n reprezentand numarul de grade de libertate\n
                    Media:\n
                    E[X] = integrala de la -Inf la +Inf din x*f(x) dx = n \n
                    Dispersia:\n
                    Var(X) = E[X^2]-E[x]^2 = 2n \n
                    Sursa: https://cismasemanuel.files.wordpress.com/2020/04/seminar-variabile-aleatoare-continue-1.pdf"))
  }

  if(nr_fisa==6){
    cat(c("\nREPARTITIA STUDENT\n
                    O variabila aleatoare X este distribuita Student cu n grade de libertate daca are densitatea de probabilitate de forma:\n
                    f(x) = (gamma((n+1)/2))/(gamma(n/2) * sqrt (n*pi)) * (1+x^2/n)^(-(n+1)/2), x apartine R \n
                    Notatie:\n
                    X ~ T(n), n reprezentand numarul de grade de libertate\n
                    Media:\n
                    E[X] = integrala de la -Inf la +Inf din x*f(x) dx = 0 \n
                    Dispersia:\n
                    Var(X) = E[X^2]-E[x]^2 = n/(n-2) \n
                    Sursa: http://math.etc.tuiasi.ro/rstrugariu/cursuri/SPD2015/c7.pdf"))
  }

}


#EXERICITIUL 10
#nu merge agregarea functiilor

#Calcul medie (ca valori primim functia si intervalul)
medie<-function(func,limInf=-Inf,limSup=Inf){
  parametru<-function(x){x*func(x)}
  rez<-integrate(parametru,lower=limInf,upper=limSup)$value
  return(rez)
}

#Calcul dispersie = momentul centrat de ordin 2
varianta<-function(func,limInf=-Inf,limSup=Inf)
{
  #Calculez media
  media<-medie(func,limInf,limSup)
  parametru<-function(x){((x-media)^2)*func(x)}
  rez<-integrate(parametru,lower=limInf,upper=limSup)$value
  return(rez)
}

# Calcul covarianta
covarianta <- function(fx,fy,f_dens_comuna, limInf=Inf, limSup=Inf) {
  #Pachet pentru "integral2"
  install.packages('pracma')
  library('pracma')

  #Fx si Fy ar trebui sa fie densitatile marginale ale lui X si Y
  #Integrarea ma constrange sa dau o valoarea, deci densitatea calculata la exercitiul 11 nu va fi o functie,
  #ci o valoare numerica, de aceea nu o pot folosi
  #De aceea, functiile trebuie date ca parametru separat

  mediaX <- as.numeric(medie(fx,limInf, limSup))
  mediaY <- as.numeric(medie(fy,limInf, limSup))

  #cov <- medie((x-mediaX)*(y-mediaY),limInf, limSup)
  #Covarianta = E[XY] - E[X]*E[Y] = E[(X-E[X])(Y-E[Y])]

  #Covarianta = integrala dubla din (x-E[x])(y-E[y])*f(x,y) dx dy
  functCov <- function(x,y){(x-mediaX)*(y-mediaY)*noquote(trimws(deparse(f_dens_comuna)[3]))}   #nu merge agregarea functiilor
  cov <- integral2(functCov,limInf, limSup, limInf, limSup)

  return (cov)
}


# Calculul coeficientului de corelatie
coeficient_corelatie <- function(fx, fy, limInf=Inf, limSup=Inf)
{
  #Coeficientul de corelatie = covarianta(X,Y) / (sqrt(varianta(X)) * sqrt(varianta(Y)))
  cov <- covarianta(fx, fy, limInf, limSup)
  return (cov / (sqrt(varianta(fx,limInf, limSup)) * sqrt(varianta(fy,limInf, limSup))))
}



#EXERCITIUL 11
#Pornind de la densitatea comuna a doua variabile aleatoare continue,
#construirea densitatilor marginale si a densita??ilor conditionate.

#Densitatea marginala
#fx(x) = integrala(fx,y(x,y)dy, -Inf, Inf)
#fy(y) = integrala(fx,y(x,y)dx, -Inf, Inf)

densitatea_marginala <- function(f_dens_comuna, startInterval = -Inf, finishInterval =Inf, punctValoareX=1, punctValoareY=1)
{
  #Sunt obligata sa dau o valoare pentru x/y
  densitateMarginalaX<-function(x){f_dens_comuna(x,y=punctValoareY)}
  densitateMarginalaY<-function(y){f_dens_comuna(y,x=punctValoareX)}

  #Construirea densitatilor marginale
  densitate_marginala_x <- integrate(densitateMarginalaY, lower = startInterval, upper = finishInterval)$value #trebuie sa derivez in functie de y
  densitate_marginala_y <- integrate(densitateMarginalaX, lower = startInterval, upper = finishInterval)$value  #trebuie sa derivez in functie de x

  return(c(densitate_marginala_x, densitate_marginala_y))
}


densitatea_conditionata <- function(f_dens_comuna,  punctValoareX=1, punctValoareY=1,startInterval=-Inf, finishInterval=Inf)
{
  #Contruirea densitatii conditionate
  densitati_marginale <- densitatea_marginala(f_dens_comuna, startInterval, finishInterval, punctValoareX, punctValoareY)
  densitate_marginala_x <- as.numeric(densitati_marginale[1])
  densitate_marginala_y <- as.numeric(densitati_marginale[2])

  dens_com_XY <- f_dens_comuna(punctValoareX,punctValoareY)
  densitate_condit_x_la_y <- (dens_com_XY / densitate_marginala_y)
  densitate_condit_y_la_x <- (dens_com_XY / densitate_marginala_x)

  return(c(densitate_condit_x_la_y, densitate_condit_y_la_x))
}

#EXERCITIUL 12
# Construirea sumei si diferentei a doua variabile aleatoare continue independente

# Suma Z = X + Y
# fz(z) = integrala de la -Inf la Inf din (fx(z-y)fy(y)dy)
# fx = functia de densitate a lui x
# fy = functia de densitate a lui y

sumaVarIndep <- function(fx,fy, limInf= -Inf, limSup =Inf){
  function(z) (integrate(function(y,z) fy(y)*fx(z-y),-limInf,limSup,z)$value)
}

# Test - nu merge
#fx <- function(x) dnorm(x,1,0.5)
#fy <- function(y) dlnorm(y,1.5, 0.75)
#fz <- sumaVarIndep(fx,fy)
#Vectorize(fz)


# Diferenta Z = X - Y
#fz(z) = integrala de la -Inf la Inf din (fx(y-z)fy(y)dy)
difVarIndep <- function(fx,fy, limInf= -Inf, limSup =Inf){
  function(z) (integrate(function(y,z) (fx(y-z) * fy(y)), limInf, limSup,z)$value)
}









