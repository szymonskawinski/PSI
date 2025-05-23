# Wczytaj dane tekstowe
# Wczytaj plik tekstowy z lokalnego dysku
text <- readLines(file.choose(),encoding="UTF-8")
text

frequent_terms <- freq_terms(text)
frequent_terms
frequent_terms <- freq_terms(text, stopwords = Top200Words)
plot(frequent_terms)

# Bardziej atrakcyjna wizualizacja
ggplot(frequent_terms, aes(x = FREQ, y = reorder(WORD, FREQ))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue", alpha = 0.8) +
  labs(x = "Cz�sto��", y = "S�owo") +
  ggtitle("Wykres cz�sto�ci s��w") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), # Dostosowanie rozmiaru czcionki etykiet na osi Y
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Wy�rodkowanie i stylizacja tytu�u wykresu
        panel.grid.major.y = element_blank(), # Usuni�cie g��wnych linii siatki poziomej
        panel.grid.minor.y = element_blank(), # Usuni�cie mniejszych linii siatki poziomej
        axis.line = element_line(color = "black")) # Dostosowanie linii osi

# Tworzenie chmury s��w za pomoc� pakietu wordcloud
install.packages("wordcloud")
library(wordcloud)


# Utw�rz chmur� s��w
wordcloud(frequent_terms$WORD, frequent_terms$FREQ)

# Opcje chmury s��w
?wordcloud
# Zmiana warto�ci min.freq i max.words w celu wy�wietlenia mniejszej/wi�kszej liczby s��w.
# min.freq: s�owa o cz�sto�ci poni�ej tej warto�ci nie b�d� wy�wietlane
# max.words: maksymalna liczba s��w do wy�wietlenia

# Ograniczenie liczby s��w w chmurze poprzez okre�lenie minimalnej cz�sto�ci
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)



# Ograniczenie liczby s��w w chmurze poprzez okre�lenie maksymalnej liczby s��w
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5)

# Optymalizacja i dostosowanie wynik�w
# Dodanie koloru do chmury s��w dla lepszej wizualizacji
# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))
# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))
?brewer.pal
brewer.pal.info

# Dodanie r�nych palet kolorystycznych
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens"))



