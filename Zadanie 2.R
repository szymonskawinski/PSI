# 5. Stwórz funkcję będącą najprostszą wersją kalkulatora 
# (dodawanie, odejmowanie, mnożenie albo dzielenie dwóch liczb).

kalkulator = function(a, dzialanie, b){
  if(dzialanie == "+"){
    wynik = a+b
  } else if (dzialanie == "-"){
    wynik = a-b
  } else if (dzialanie == "*"){
    wynik = a*b
  } else if (dzialanie == "/"){
    if(b == 0) {wynik = ("BŁĄD. NIE MOŻNA DZIELIĆ PRZEZ 0!!!")}
    else {wynik = a/b}
  } else {wynik = ("NIEPRAWIDŁOWE DZIAŁANIE. SPRÓBUJ PONOWNIE.")}
  return(wynik)
}
kalkulator(2,"+",9)
kalkulator(2,"-",9)
kalkulator(2,"*",9)
kalkulator(2,"/",9)
kalkulator(2,"^",9)
kalkulator(2,"/",0)