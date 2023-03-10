# Analiza skupie?

mydata <- read.csv(file.choose(), header=T)
str(mydata)
head(mydata)
pairs(mydata[2:9]) # wykres bazowy (base plot) dla wszystkich mo?liwych kombinacji dw?ch zmiennych

# Wykres punktowy - punktami s? firmy

plot(mydata$Fuel_Cost~ mydata$Sales, data = mydata)
with(mydata,text(mydata$Fuel_Cost ~ mydata$Sales, labels=mydata$Company,pos=4))

# Normalizacja - je?li warto?? jest bliska 0 tym warto?? jest bli?sza ?redniej, okre?lamy wiersze i kolumny za pomoc? nawias?w kwadratowych
# nie chcemy pierwszej kolumny bo to jest nazwa firmy, wi?? wykluczamy ze zbioru, liczymy ?redni? dla ka?dej kolumny (u?yli?my)
# liczby 2 poniewa? to oznacza kolumn? (1 oznacza wiersz), normalizujemy dane i m?wimy skali ?e centrum powinno by? srednimi
# i podzielone przez odchylenie standardowe 

z <- mydata[,-c(1,1)]
means <- apply(z,2,mean)
means
sds <- apply(z,2,sd)
sds
nor <- scale(z,center=means,scale=sds)
nor

# Mierzenie dystansu macierzy - Dystans mi?dzy 1 a drug? firm? wynosi 3.1, mo?emy w ten spos?b por?wna? firmy

distance = dist(nor)
print(distance, digits = 3)

# Hierarchiczne grupowanie  Hierarhiczny model grupowania rysuje dla nas ?adny dendrogram dzieki czemu mozemy zasadniczo zobaczy?
#na podstawie struktur ?e kazda obserwacja jest oznaczona na dole a nast?pnie ka?da z nich jest grupowana
#ze swoimi najbardziej podobnymi s?siadami i wtedy mo?na zobaczyc ro?ne skupienia, wiele obserwacji jest 
#zgrupowanych razem i gdy poruszamy sie wy?ej na drzewie mamy coraz wi?ksze skupienia wi?c jest to cz?sto
#przydatne je?li interesuje nas hierarhiczna struktura danych

mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Company,main='Default from hclust')
plot(mydata.hclust,hang=-1)

# Hierarchiczne grupowanie z wykorzystaniem ??redniego? powi?zania - teraz maksymalna odleg?o?? wynosi oko?o 4,5
# grupujemy klastry na 5 rodzaj?w klastr?w

mydata.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1)
rect.hclust(mydata.hclust, k = 4)

# Przynale?no?? klastr?w

member = cutree(mydata.hclust,4)
table(member)

# Charakteryzowanie klastr?w Mo?emy sprawdzi? ze w klastrze 1 jest 18 zmiennych w 2 klastrze 1 zmienna i w 3 klastrze s? 
# 3 zmienne, mo?emy r?wnie? por?wna? za pomoc? funkcji aggregate, kt?re zmienne wnosz? wi?kszy wk?ad w tworzenie klastr?W
# a kt?re wnosz? mniejszy, te zmienne kt?re maj? wi?kszy wk?ad maj? mia?y wi?ksze ?rednie r??nice, czyli na przyk?ad
# cost b?dzie mia? du?y wp?yw bo najwi?ksza warto?? to 1.3389101 a najmniejsza to -0.2552757, mo?emy powiedzie? ?e cost
# bardziej przyczynia sI? do tworzenia klastra ni? fixed charge, poniewa? ma mniejsz? r?znic? mi?dzy minimaln? a maksymaln?
# warto?ci?

aggregate(nor,list(member),mean)
aggregate(mydata[,-c(1,1)],list(member),mean)

# Silhouette Plot (wykres silhouette) CHcemy zobaczy? jak skutecznie stworzyli?my nasz klaster, do tego pomo?e nam Silhouette
# Plot, mo?emy zobaczy? ?e niekt?rzy cz?onkowie klastra dobrze pasuj? do tego klastra a niekt?rzy nie pasuj? a? tak dobrze i s?
# nieco w?tpliwe

library(cluster) 
plot(silhouette(cutree(mydata.hclust,4), distance)) 

# Scree Plot (Wielowymiarowy wykres piargowy) Pierwsza kropka od g?ry to 1 klaster, oanacza to ?e je?li wszystkie 22 firmy s?
# w jednym skupieniu to jaka b?dzie zmienno?c w obr?bie klastra, b?dzie wie? bardzo wysoka, bo nie ka?da firma jest blisko
# innych firm, wiec w ramach grupy suma kwadrat?w b?dzie wysoka, szukamy miejsca gdzie jest za?amanie bo to oznacza ?e mamy
# odpowiedni? ilo?c klastr?w, poniewa? nast?puje tam solidna redukcja w sumie kwadrat?W

wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Liczba klastr?w", ylab="Sumy kwadrat?w wewn?trz grup") 

# Algorytm centroid?w (k-means clustering) Informuje nas ?e np. ?e firma pierwsza nale?y do klastra 2, dowiadujemy sIe r?wnie?
# o stosunku roznicy sumy kwadrat?w do sumy kwaratow, widzimy ?e klaster 3 i 4 ma ma?e warto?ci  9.533522 i 10.177094 co zonacza ?e radz? sobie
# dobrze poniewa? im mniejsza warto?? tym lepiej, poniewa? klastry nie mog? by? blisko siebie, poniewa? moze wyst?powa?
# nak?adanie si? dw?ch klastr?W, wysz?o nam 52.2 %, to oznacza ?e nasze klastry s? w miar? daleko od siebie, im wi?ksza
# jest ta warto?c tym klastry s? dalej od siebie a do tego d??ymy, wsp??czynnik daje nam wyobra?enie o jako?ci klastr?W

set.seed(123)
kc<-kmeans(nor,4)
kc
clusplot(mydata,
         kc$cluster,
         color = T,
         shade = T,
         labels = 2,
         lines = 0)

