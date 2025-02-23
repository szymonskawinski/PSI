# 7. Stwórz funkcję obliczającą podatek w zależności od dochodu. 
# Przyjmij następujące założenia:
# a) Jeżeli podatnik rozlicza się liniowo, wtedy niezależnie od kwoty płaci 19% podatku.
# b) Jeżeli podatnik rozlicza się na zasadach ogólnych, wtedy:
# - poniżej kwoty 85528zł płaci 18% podatku minus kwota zmniejszająca, czyli 556zł;
# - powyżej kwoty 85528zł płaci 14839zł + 32% nadwyżki powyżej 85528zł.

podatek = function(kwota, typ){
  if(typ == "L"){
    pod = 0.19 * kwota
  } else if (kwota > 85528){
    pod = 14839 + (kwota-85528)*0.32
  } else{
    pod = 0.18 * kwota - 556
    if(pod < 0){
      pod = 0;
    }
  }
  return(pod)
}
podatek(85528,"O")
podatek(85528,"L")
podatek(234453,"O")
podatek(1000,"O")