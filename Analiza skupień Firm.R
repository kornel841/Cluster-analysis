# Analiza skupieñ

mydata <- read.csv(file.choose(), header=T)
str(mydata)
head(mydata)
pairs(mydata[2:9]) # wykres bazowy (base plot) dla wszystkich mo¿liwych kombinacji dwóch zmiennych

# Wykres punktowy - punktami s¹ firmy

plot(mydata$Fuel_Cost~ mydata$Sales, data = mydata)
with(mydata,text(mydata$Fuel_Cost ~ mydata$Sales, labels=mydata$Company,pos=4))

# Normalizacja - jeœli wartoœæ jest bliska 0 tym wartoœæ jest bli¿sza œredniej, okreœlamy wiersze i kolumny za pomoc¹ nawiasów kwadratowych
# nie chcemy pierwszej kolumny bo to jest nazwa firmy, wiêæ wykluczamy ze zbioru, liczymy œredni¹ dla ka¿dej kolumny (u¿yliœmy)
# liczby 2 poniewa¿ to oznacza kolumnê (1 oznacza wiersz), normalizujemy dane i mówimy skali ¿e centrum powinno byæ srednimi
# i podzielone przez odchylenie standardowe 

z <- mydata[,-c(1,1)]
means <- apply(z,2,mean)
means
sds <- apply(z,2,sd)
sds
nor <- scale(z,center=means,scale=sds)
nor

# Mierzenie dystansu macierzy - Dystans miêdzy 1 a drug¹ firm¹ wynosi 3.1, mo¿emy w ten sposób porównaæ firmy

distance = dist(nor)
print(distance, digits = 3)

# Hierarchiczne grupowanie  Hierarhiczny model grupowania rysuje dla nas ³adny dendrogram dzieki czemu mozemy zasadniczo zobaczyæ
#na podstawie struktur ¿e kazda obserwacja jest oznaczona na dole a nastêpnie ka¿da z nich jest grupowana
#ze swoimi najbardziej podobnymi s¹siadami i wtedy mo¿na zobaczyc ro¿ne skupienia, wiele obserwacji jest 
#zgrupowanych razem i gdy poruszamy sie wy¿ej na drzewie mamy coraz wiêksze skupienia wiêc jest to czêsto
#przydatne jeœli interesuje nas hierarhiczna struktura danych

mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Company,main='Default from hclust')
plot(mydata.hclust,hang=-1)

# Hierarchiczne grupowanie z wykorzystaniem „œredniego” powi¹zania - teraz maksymalna odleg³oœæ wynosi oko³o 4,5
# grupujemy klastry na 5 rodzajów klastrów

mydata.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1)
rect.hclust(mydata.hclust, k = 4)

# Przynale¿noœæ klastrów

member = cutree(mydata.hclust,4)
table(member)

# Charakteryzowanie klastrów Mo¿emy sprawdziæ ze w klastrze 1 jest 18 zmiennych w 2 klastrze 1 zmienna i w 3 klastrze s¹ 
# 3 zmienne, mo¿emy równie¿ porównaæ za pomoc¹ funkcji aggregate, które zmienne wnosz¹ wiêkszy wk³ad w tworzenie klastróW
# a które wnosz¹ mniejszy, te zmienne które maj¹ wiêkszy wk³ad maj¹ mia³y wiêksze œrednie ró¿nice, czyli na przyk³ad
# cost bêdzie mia³ du¿y wp³yw bo najwiêksza wartoœæ to 1.3389101 a najmniejsza to -0.2552757, mo¿emy powiedzieæ ¿e cost
# bardziej przyczynia sIê do tworzenia klastra ni¿ fixed charge, poniewa¿ ma mniejsz¹ róznicê miêdzy minimaln¹ a maksymaln¹
# wartoœci¹

aggregate(nor,list(member),mean)
aggregate(mydata[,-c(1,1)],list(member),mean)

# Silhouette Plot (wykres silhouette) CHcemy zobaczyæ jak skutecznie stworzyliœmy nasz klaster, do tego pomo¿e nam Silhouette
# Plot, mo¿emy zobaczyæ ¿e niektórzy cz³onkowie klastra dobrze pasuj¹ do tego klastra a niektórzy nie pasuj¹ a¿ tak dobrze i s¹
# nieco w¹tpliwe

library(cluster) 
plot(silhouette(cutree(mydata.hclust,4), distance)) 

# Scree Plot (Wielowymiarowy wykres piargowy) Pierwsza kropka od góry to 1 klaster, oanacza to ¿e jeœli wszystkie 22 firmy s¹
# w jednym skupieniu to jaka bêdzie zmiennoœc w obrêbie klastra, bêdzie wieæ bardzo wysoka, bo nie ka¿da firma jest blisko
# innych firm, wiec w ramach grupy suma kwadratów bêdzie wysoka, szukamy miejsca gdzie jest za³amanie bo to oznacza ¿e mamy
# odpowiedni¹ iloœc klastrów, poniewa¿ nastêpuje tam solidna redukcja w sumie kwadratóW

wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Liczba klastrów", ylab="Sumy kwadratów wewn¹trz grup") 

# Algorytm centroidów (k-means clustering) Informuje nas ¿e np. ¿e firma pierwsza nale¿y do klastra 2, dowiadujemy sIe równie¿
# o stosunku roznicy sumy kwadratów do sumy kwaratow, widzimy ¿e klaster 3 i 4 ma ma³e wartoœci  9.533522 i 10.177094 co zonacza ¿e radz¹ sobie
# dobrze poniewa¿ im mniejsza wartoœæ tym lepiej, poniewa¿ klastry nie mog¹ byæ blisko siebie, poniewa¿ moze wystêpowaæ
# nak³adanie siê dwóch klastróW, wysz³o nam 52.2 %, to oznacza ¿e nasze klastry s¹ w miarê daleko od siebie, im wiêksza
# jest ta wartoœc tym klastry s¹ dalej od siebie a do tego d¹¿ymy, wspó³czynnik daje nam wyobra¿enie o jakoœci klastróW

set.seed(123)
kc<-kmeans(nor,4)
kc
clusplot(mydata,
         kc$cluster,
         color = T,
         shade = T,
         labels = 2,
         lines = 0)

