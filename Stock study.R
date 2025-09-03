
# Tema Microeconomie ------------------------------------------------------

#Importarea datelor din fisierul csv in R 
setwd("C:\\Users\\Maria\\OneDrive\\Desktop\\FAC\\AN2_SEM2\\MICRO MANAGERIALA\\proiect")
preturi <- read.csv("preturi.csv", header=T, sep=",", dec=".")
attach(preturi)
#Am folosit functia attach pentru a putea referi variabilele direct cu denumirile lor fara specificarea numelui df-ului

#Am rulat options(scipen=999) pentru a afisa numerele in formatul obisnuit, nu in cel stiintific

#Calcularea rentabilitatilor pe baza formulei Rt = (Pret t/ Pret t-1)-1
#Pe prima linie din tabelul cu rentabilitati vom avea NA, deoarece nu putem calcula rentabilitatea primei zile neavand valoarea anterioara
rentabilitateBKNG <- c(NA, (PBKNG[-1]/PBKNG[-length(PBKNG)])-1)
rentabilitateABNB <- c(NA, (PABNB[-1]/PABNB[-length(PABNB)])-1)
rentabilitateNDAQ <- c(NA, (PNDAQ[-1]/PNDAQ[-length(PNDAQ)])-1)

#Crearea data-frame-ului cu rentabilitati
rentabilitati <- data.frame(rentabilitateBKNG, rentabilitateABNB, rentabilitateNDAQ)

#Summary Statistics 
#Pentru preturi:
summary(preturi[,-1])
#[,-1] pentru a elimina prima coloana (Date)
#Am creat vectori in care am stocat statisticile pentru fiecare entitate in parte si am creat o matrice pe baza acestora
statisticiPBKNG <- summary(preturi[,2])
statisticiPABNB <- summary(preturi[,3])
statisticiPNDAQ <- summary(preturi[,4])
SummaryStatisticsPreturi <- matrix(c(statisticiPBKNG,statisticiPABNB,statisticiPNDAQ), nrow=6, ncol=3, byrow=F)
rownames(SummaryStatisticsPreturi) <- c("Min.", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max.")
colnames(SummaryStatisticsPreturi) <- c("PBKNG","PABNB","PNDAQ")

#Interpretare Summary Statistics-Preturi:
#Distributia preturilor pentru compania Booking prezinta o medie si o mediana relativ apropiate, in jurul valorii 2900; 
#Preturile sunt distribuite intr-un interval larg cuprins intre 2348 si 3617, ceea ce indica o variatie semnificativa a preturilor actiunilor Booking. 
#Conform cuartilelor, aproximativ jumatate dintre preturi se afla intre 2631 si 3132, cu o dispersie semnificativa in jurul valorii medii.
#Pentru seria de preturi a companiei Airbnb, media si mediana se situeaza in jurul valorii 128, aratand o tendinta centrala stabila;
#Datele pentru Airbnb sunt distribuite intr-un interval mai restrans in comparatie cu Booking Holdings, variand intre valorile 104.4 si 153.3. 
#Dispersia datelor este mai scazuta in comparatie cu competitorul, cuartilele indicand ca preturile se situeaza intre 118.5 si 137.1.
#Pentru indicele de pret NASDAQ se observa o crestere de la valoarea minima de 47.25 la cea maxima de 60.57, avand o tendinta centrala stabila cu media si mediana apropiate, in jurul valorii 53.5;
#Majoritatea preturilor NADSAQ sunt concentrate intre 50.76 si 55.85, seria avand o dispersie relativ mica a datelor in jurul mediei, ceea ce indica stabilitatea seriei de preturi.

#Pentru rentabilitati:
summary(rentabilitati)
#Am creat vectori in care am stocat statisticile pentru fiecare entitate si apoi am creat matricea
statisticiRentabilitateBKNG <- summary(rentabilitati[,1])
statisticiRentabilitateABNB <- summary(rentabilitati[,2])
statisticiRentabilitateNDAQ <- summary(rentabilitati[,3])
SummaryStatisticsRentabilitati <- matrix(c(statisticiRentabilitateBKNG,statisticiRentabilitateABNB,statisticiRentabilitateNDAQ),
                                         nrow=7, ncol=3, byrow=F)
rownames(SummaryStatisticsRentabilitati) <- c("Min.", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max.", "NA")
colnames(SummaryStatisticsRentabilitati) <- c("RentBKNG", "RentABNB", "RentNDAQ")

#Interpretare Summary Statistics-Rentabilitati:
#Rentabilitatea pentru Booking Holdings variaza de la -0.043 la 0.078, cu o rentabilitate medie pozitiva (0.0016) ceea ce sugereaza o utilizare eficienta a capitalului pentru a genera profit.
#Valoarea maxima inregistrata a fost de 0.078, indicand posibilitatea unor perioade de crestere semnificativa a profitului;
#Rentabilitatea medie a Airbnb este pozitiva, ceea ce indica o utilizare eficienta a capitalului, insa prezinta valori minime foarte negative (-0.109) ce indica posibilitatea unor perioade de pierderi semnificative care pot afecta performanta la bursa.
#Cuartilele sugereaza o variatie mare a rentabilitatilor, intre valorile -0.014 si 0.014, ceea ce arata o volatilitate mai mare in comparatie cu concurentul sau. 
#Rentabilitatea Airbnb poate varia semnificativ de la o perioada la alta in functie de diversi factori din industrie si din economie, asadar compania poate avea atat perioade cu castiguri ridicate, cat si cu pierderi semnificative, mai des decat Booking Holdings;
#In ceea ce priveste indicele de piata NASDAQ, rentabilitatea medie negativa (-0.0000551) sugereaza ca performanta in generarea profitului este una modesta, echilibrata (fiind apropiata de zero). 
#Intervalul larg de rentabilitati, intre valoarea minima -0.118 si valoarea maxima 0.040, indica o variatie semnificativa in rentabilitate si sensibilitate la factorii externi si la fluctuatiile pietei.
#Rentabilitatea maxima cea mai mare se intalneste la Airbnb, avand un potential de castig mai mare pentru aceasta companie;
#Rentabilitatea minima cea mai scazuta este pentru indicele de piata, ceea ce sugereaza ca in perioada de timp analizata NASDAQ a avut o performanta mai putin favorabila in comparatie cu celelalte entitati analizate;

#Standard Deviation/Abaterea standard
#Am creat vectori pentru abaterile standard pentru preturi, respectiv rentabilitati, folosind functia apply pentru a aplica sd pe fiecare variabila
sdPreturi <- apply(preturi[,-1], 2, sd)  #In cazul preturilor am folosit [,-1] pentru a elimina prima coloana, cea cu datele
sdRentabilitati <- apply(rentabilitati[-1,], 2, sd) #In cazul rentabilitatilor am folosit [-1,] pentru a elimina prima linie pe care aveam valorile lipsa(NA)
print(sdPreturi)
print(sdRentabilitati)

#Activarea pachetului moments pentru functiile skewness si kurtosis
library(moments)

#Skewness/Asimetrie
#Am aplicat functia skewness pe fiecare entitate in parte, stocand rezultatele in vectori, atat pentru preturi, cat si pentru rentabilitati
skPreturi <- apply(preturi[,-1], 2, skewness)
skRentabilitati <- apply(rentabilitati[-1,], 2, skewness)
print(skPreturi)
print(skRentabilitati)

#KurtoskRentabilitati#Kurtosis/Aplatizare
#Am aplicat functia kurtosis pe fiecare entitate in parte, stocand rezultatele in vectori, atat pentru preturi, cat si pentru rentabilitati
kPreturi <- apply(preturi[,-1], 2, kurtosis)
kRentabilitati <- apply(rentabilitati[-1,], 2, kurtosis)
print(kPreturi)
print(kRentabilitati)

#Coeficientul de variatie = abatere standard/medie
#Am calculat coeficientul de variatie pentru fiecare entitate si apoi am creat un vector in care am stocat cele 3 rezultate (atat la preturi, cat si la rentabilitati)
#Pt preturi:
cvBKNG <-  sd(PBKNG)/mean(PBKNG)
cvABNB <-  sd(PABNB)/mean(PABNB)
cvNDAQ <-  sd(PNDAQ)/mean(PNDAQ)
cvPreturi <- c(cvBKNG,cvABNB,cvNDAQ)
print(cvPreturi)
#Pt rentabilitati:
#Am folosit na.rm=T pentru a ignora valorile de tip NA (valori lipsa)
cvRentBKNG <- sd(rentabilitateBKNG, na.rm=T)/mean(rentabilitateBKNG, na.rm=T)
cvRentABNB <- sd(rentabilitateABNB, na.rm=T)/mean(rentabilitateABNB, na.rm=T) 
cvRentNDAQ <- sd(rentabilitateNDAQ, na.rm=T)/mean(rentabilitateNDAQ, na.rm=T)
cvRent <- c(cvRentBKNG,cvRentABNB,cvRentNDAQ)
print(cvRent)

#Matrice cu abaterea standard, asimetrie, aplatizare si coef. de variatie
#Am creat 2 matrici (una pentru preturi, una pentru rentabilitati) in care am stocat statisticile calculate anterior pentru toate cele 3 entitati analizate

#Pentru preturi:
matriceAnalizaPreturi <- matrix(c(sdPreturi, skPreturi, kPreturi, cvPreturi), nrow=4, ncol=3, byrow=T)
rownames(matriceAnalizaPreturi) <- c("Abatere standard", "Asimetrie", "Aplatizare", "Coef. de variatie")
colnames(matriceAnalizaPreturi) <- c("PBKNG", "PABNB", "PNDAQ")
print(matriceAnalizaPreturi)

#Interpretare preturi Booking:
#Seria de preturi pentru Booking prezinta o abatere de 335.913, ce indica cât de mult se abat valorile individuale din setul de date de la media lor. Majoritatea preturilor se afla intr-un interval de aproximativ +/- 335.913 de la media 2909, asadar seria prezinta o dispersie considerabila a valorilor in jurul mediei.
#Asimetria de 0.4 arata faptul ca distributia preturilor este usor impinsa spre dreapta, ceea ce inseamna ca pot exista mai multe valori peste medie, avand o asimetrie usor pozitiva.
#Valoarea kurtosis-ului este de 2.047, distributia fiind platikurtica, deoarece este mai mica decat 3. Distributia datelor are cozi mai usoare decat o distributie normala, asadar probabilitatea de a observa valori extreme in afara intervalului in jurul mediei este mai mica decat in cazul distributiilor normale.
#Avand un coeficient de variatie de 0.115, variatia preturilor este relativ mica, deci datele sunt stabile si omogene in raport cu media.

#Interpretare preturi Airbnb:
#Abaterea standard de 11.567 indica faptul ca majoritatea preturilor se incadreaza intr-un interval de +/- 11.567 de la media de 128. Majoritatea preturilor Airbnb se afla in jurul mediei, dispersia fiind relativ mica in comparatie cu media.
#Asimetria de 0.158 sugereaza ca distributia este usor asimetrica la dreapta, existand o usoara tendinta ca valorile mari sa fie mai frecvente decat valorile mici, insa asimetria nu este foarte pronuntata.
#Aplatizarea de 2.157 indica o distributie platikurtica, fiind mai aplatizata decat o distributie normala, cu un kurtosis de 3. Exista mai putine valori extreme/outliere decat in cazul distributiilor normale.
#Coeficientul de variatie de 0.09 indica o variatie relativ mica a datelor in raport cu media, datele fiind relativ stabile si omogene.

#Interpretare preturi NASDAQ:
#Abaterea standard de 3.119 inseamna ca preturile din setul de date se abat, in medie, cu 3.119 unitati fata de media de 53.49, asadar dispersia datelor este relativ mica.
#Asimetria de 0.13 indica o coada lunga a distributiei in partea dreapta,  deci o usoara asimetrie pozitiva spre dreapta, fiind mai frecvente valorile mari decat valorile mici.
#Aplatizarea de 1.99 sugereaza o aplatizare mai mica decat a unei distributii normale, adica distributia de date este mai plata si mai dispersata in jurul valorii medii, cu cozi mai lungi.
#Coeficientul de variatie de 0.058 indica o abatere standard relativ mica in comparatie cu media, ceea ce inseamna ca preturile sunt grupate in jurul mediei, datele fiind consistente.

#Pentru rentabilitati:
matriceAnalizaRentabilitati<- matrix(c(sdRentabilitati, skRentabilitati, kRentabilitati, cvRent), nrow=4, ncol=3, byrow=T)
rownames(matriceAnalizaRentabilitati) <- c("Abatere standard", "Asimetrie", "Aplatizare", "Coef. de variatie")
colnames(matriceAnalizaRentabilitati) <- c("rentBKNG", "rentABNB", "rentNDAQ")
print(matriceAnalizaRentabilitati)

#Interpretare rentabilitati Booking:
#Abaterea standard de 0.0156 inseamna ca valorile rentabilitatii se abat in medie cu 0.0156 de la media generala de 0.0016. Distributia datelor este relativ concentrata in jurul valorii medii, ceea ce sugereaza stabilitatea seriei de date in timp.
#Asimetria de 0.534 este o asimetrie pozitiva, distributia are coada mai lunga in partea dreapta si o concentrare mai mare a valorilor in partea stanga a distributiei, adica valorile mai mari decat media sunt mai frecvente.
#Aplatizarea de 5.253 indica o distributie leptokurtica extrem de aplatizata si cu cozi foarte lungi, asadar distributia are o concentrare mare a datelor in jurul mediei. Aplatizarea mare poate indica existenta unor outliere in setul de date.
#Coeficientul de variatie de 9.664 sugereaza o mare variabilitate a datelor. Impreuna cu abaterea standard de 0.156, se poate constata ca, in comparatie cu media, datele variaza semnificativ, dar se afla intr-un interval mic.

#Interpretare rentabilitati Airbnb:
#Abaterea standard de 0.025 indica faptul ca valorile sunt relativ apropiate de medie, dar tot exista o oarecare variabilitate.
#Asimetria de 0.466 este pozitiva si indica faptul ca distributia rentabilitatilor este deformata spre dreapta ceea ce inseamna ca exista mai multe valori in partea dreapta decat in partea stanga a distributiei.
#Valoarea ridicata a kurtosis (6.548) arata ca distributia este leptokurtica, cu cozi mai lungi decat o distributie normala, ceea ce inseamna ca exista valori extreme/outliere in setul de date.
#Coeficientul de variatie de 19.375 indica gradul de variabilitate relativ ridicat in comparatie cu media.

#Interpretarea rentabilitatii NASDAQ:
#Abaterea standard de 0.0146 indica o dispersie relativ mica a datelor in raport cu media lor.
#Asimetria negativa de -2.047 indica o asimetrie puternica spre stanga, asadar distributia are o coada mai lunga in partea stanga si o concentrare mai mare a valorilor in partea dreapta a distributiei.
#Aplatizarea de 18.878 indica o distributie extrem de aplatizata, cu cozi foarte lungi, ceea ce evidentiaza prezenta unor outliere in setul de date.
#Coef. de variatie este negativ, deoarece setul de date contine multe valori extreme/outliers negative, iar abaterea standard este mai mare in comparatie cu media.

#Reprezentare histograme
#windows()
par(mfrow=c(3,2))
hist(PBKNG, main="Distributia Preturilor BOOKING")
#Histograma preturilor Booking indica o asimetrie pozitiva, existand mai multe valori peste medie. Majoritatea preturilor sunt cuprinse intre 2600 si 3200 (aprox.)
hist(rentabilitateBKNG, main="Distributia Rentabilitatilor BOOKING")
#Histograma rentabilitatii Booking indica o distributie relativ normala, usor asimetrica spre dreapta, media, mediana si modul sunt aproximativ egale. Majoritatea rentabilitatilor depasesc valoarea mediei (0.001615688).
hist(PABNB, main="Distributia Preturilor AIRBNB")
#Histograma preturilor Airbnb indica o asimetrie pozitiva, majoritatea rentabilitatilor fiind situate in jumatatea pozitiva a distributiei. Sunt prezente de asemenea si valori extreme in partea pozitiva.
hist(rentabilitateABNB, main="Distributia Rentabilitatilor AIRBNB")
#Histograma rentabilitatilor Airbnb indica o distributie relativ normala, usor asimetrica (media, modul si mediana fiind aproape egale). Majoritatea valorilor sunt situate in jurul mediei, insa exista si valori extreme.
hist(PNDAQ, main="Distributia Preturilor NASDAQ")
#Histograma preturilor NASDAQ indica o asimetrie pozitiva, cu valori concentrate intr-un interval restrans cuprins intre 50 si 55.
hist(rentabilitateNDAQ, main="Distributia Rentabilitatilor NASDAQ")
#Histograma rentabilitatilor NASDAQ indica o asimetrie negativa puternica, valorile fiind concentrate in partea dreapta a distributiei, fiind vizibile si outliere.

#Reprezentare BoxPlot-uri
#windows()
par(mfrow=c(3,2))
boxplot(PBKNG, horizontal=T, main="BoxPlot Preturi BOOKING")
#Boxplot preturi Booking: Distributia este pozitiva, distanta dintre mediana si Q3 fiind mai mare decat cea dintre mediana si Q1. Nu exista outliere.
boxplot(rentabilitateBKNG, horizontal=T, main="BoxPlot Rentabilitati BOOKING")
#BoxPlot Rentabilitati BOOKING: Distributia este normala, cu outliere atat in partea pozitiva(0.06; 0.08), cat si in cea negativa(-0.04)
boxplot(PABNB, horizontal=T, main="BoxPlot Preturi AIRBNB")
#BoxPlot Preturi AIRBNB: Distributie pozitiva, fara outliere
boxplot(rentabilitateABNB, horizontal=T, main="BoxPlot Rentabilitati AIRBNB")
#BoxPlot Rentabilitati AIRBNB: Distributie pozitiva, prezentand destul de multe outliere in partea pozitiva (intre 0.05 si 0.10 majoritatea, si inca un outlier peste 0.10), dar si in partea negativa(-0.10;-0.05)
boxplot(PNDAQ, horizontal=T, main="BoxPlot Preturi NASDAQ")
#BoxPlot Preturi NASDAQ: Distributie pozitiva, nu exista outliere
boxplot(rentabilitateNDAQ, horizontal=T, main="BoxPlot Rentabilitati NASDAQ")
#BoxPlot Rentabilitati NASDAQ: Distributie negativa, prezinta outliere majore in partea stanga (chiar minimul seriei fiind un outlier= -0.11), cat si in dreapta 

#Identificarea outlierilor
#Pentru Rentabilitatea Booking:
Q1 <- quantile(rentabilitateBKNG, 0.25, na.rm=T)
Q3 <- quantile(rentabilitateBKNG, 0.75, na.rm=T)
IQR <- Q3-Q1
outliersRentBKNG <- rentabilitateBKNG[rentabilitateBKNG<(Q1-1.5*IQR)|rentabilitateBKNG>(Q3+1.5*IQR)]
print(outliersRentBKNG)
#Pentru Rentabilitatea Airbnb:
Q1 <- quantile(rentabilitateABNB, 0.25, na.rm=T)
Q3 <- quantile(rentabilitateABNB, 0.75, na.rm=T)
IQR <- Q3-Q1
outliersRentABNB <- rentabilitateABNB[rentabilitateABNB<(Q1-1.5*IQR)|rentabilitateABNB>(Q3+1.5*IQR)] 
print(outliersRentABNB)
#Pentru Rentabilitatea NASDAQ:
Q1 <- quantile(rentabilitateNDAQ, 0.25, na.rm=T)
Q3 <- quantile(rentabilitateNDAQ, 0.75, na.rm=T)
IQR <- Q3-Q1
outliersRentNDAQ <- rentabilitateNDAQ[rentabilitateNDAQ<(Q1-1.5*IQR)|rentabilitateNDAQ>(Q3+1.5*IQR)] 
print(outliersRentNDAQ)

#Pozitiile outlierilor (Zilele din setul de date cand s-au inregistrat outlieri)
pozitiiOutlieriBKNG <- which(rentabilitateBKNG<(Q1-1.5*IQR)|rentabilitateBKNG>(Q3+1.5*IQR))
pozitiiOutlieriABNB <- which(rentabilitateABNB<(Q1-1.5*IQR)|rentabilitateABNB>(Q3+1.5*IQR))
pozitiiOutlieriNDAQ <- which(rentabilitateNDAQ<(Q1-1.5*IQR)|rentabilitateNDAQ>(Q3+1.5*IQR))
print(pozitiiOutlieriBKNG)
print(pozitiiOutlieriABNB)
print(pozitiiOutlieriNDAQ)

library(corrplot)
#Matricea de corelatie in format numeric si grafic
windows()
corelatiePreturi <- cor(preturi[,-1])
corrplot(corelatiePreturi, method="shade", type="upper")
print(corelatiePreturi)

#Corelatia dintre preturile Booking si cele Airbnb este foarte puternica, activand in aceeasi industrie si fiind concurenti principali.
#Corelatia dintre preturile Booking si cele Nasdaq este scazuta, aproape de valoarea 0, intre cele doua nu exista influente semnificative.
#Corelatia dintre preturile Airbnb si cele Nasdaq este una negativa, ceea ce inseamna ca nu exista legaturi directe intre evolutia Airbnb si cea a indicelui de piata.

corelatieRentabilitate <- cor(rentabilitati[-1,])
corrplot(corelatieRentabilitate, method="square", type="upper")
print(corelatieRentabilitate)

#Corelatia dintre rentabilitatea actiunilor Booking si rentabilitatea actiunilor Airbnb este destul de puternica, ambele entitati operand in aceeasi industrie, 
#asadar profitabilitatea acestora poate fi afectata de aceeasi factori externi (tendintele de calatorie, preferintele consumatorilor) sau de aceleasi evenimente majore care afecteaza industria calatoriilor.
#Corelatia dintre rentabilitatea Booking si rentabilitatea indicelui de piata este pozitiva, deoarece factorii de risc de pe piata bursiera, cum ar fi schimbarile economice si schimbarile legislative si politice, pot afecta atat Booking, cat si Nasdaq.
#Corelatia dintre rentabilitatea Airbnb si rentabilitatea indicelui de piata este pozitiva, dar mai mica decat cea dintre Booking si Nasdaq, deoarece nu este la fel de dependenta ca competitorul sau de evolutiile si schimbarile pietei tehnologice.

#Reprezentarea evolutiilor seriilor de date

#Evolutia Booking in raport cu indicele de piata NASDAQ
#windows()
par(mfrow=c(2,2))
ts.plot(PBKNG, col="blue", main="Evolutia Preturilor BOOKING")
ts.plot(rentabilitateBKNG, col="cyan", main="Evolutia Rentabilitatilor BOOKING")
ts.plot(PNDAQ, col="green", main="Evolutia Preturilor NASDAQ")
ts.plot(rentabilitateNDAQ, col="aquamarine", main="Evolutia Rentabilitatilor NASDAQ")
#Analizand evolutiile preturilor pentru Booking Holdings si NASDAQ se poate observa ca intre primele 150 de observatii (de la 01/02/2023 la 06/09/2023) nu exista o corelatie, insa apoi se poate observa o scadere brusca atat in cazul Booking, cat si NASDAQ, urmata de o crestere. Scaderea a fost cauzata de amenintarea razboiului in fasia Gaza, iar cresterea a fost una organica, odata cu trecerea perioadei tensionate.
#In ceea ce priveste rentabilitatile, cele doua grafice au o legatura slaba, singura asemanare fiind fluctuatiile in jurul valorii 0, ramanand relativ constante, cu usoare modificari (Booking a inregistrat o crestere brusca intre zilele 100-150, iar Nasdaq a inregistrat o scadere intre 50-100)

#Evolutia Airbnb in raport cu indicele de piata NASDAQ
#windows()
par(mfrow=c(2,2))
ts.plot(PABNB, col="red", main="Evolutia Preturilor AIRBNB")
ts.plot(rentabilitateABNB, col="pink", main="Evolutia Rentabilitatilor AIRBNB")
ts.plot(PNDAQ, col="green", main="Evolutia Preturilor NASDAQ")
ts.plot(rentabilitateNDAQ, col="aquamarine", main="Evolutia Rentabilitatilor NASDAQ")
#Analizand evolutiile preturilor pentru Airbnb si NASDAQ se poate observa ca intre primele 150 de observatii nu exista o legatura prea evidenta, insa apoi se poate observa o scadere, urmata de crestere in cazul ambelor entitati analizate. La fel ca in cazul actiunilor companiei Booking, scaderea a fost cauzata de iminenta razboiului, iar cresterea a urmat natural, odata cu depasirea tensiunilor politice.
#Intre graficele evolutiilor rentabilitatilor putem observa modificari asemanatoare, intre zilele 50-100 (intre 13-04-23 si 26-06-23) suferind scaderi pana la -0.10, insa ambele evolutii raman constante in jurul valorii 0.

#Evolutia preturilor
#windows()
par(mfrow=c(3,1))
ts.plot(PBKNG, col="blue", main="Evolutia Preturilor BOOKING")
ts.plot(PABNB, col="red", main="Evolutia Preturilor AIRBNB")
ts.plot(PNDAQ, col="green", main="Evolutia Preturilor NASDAQ")

#Evolutia rentabilitatilor
#windows()
par(mfrow=c(3,1))
ts.plot(rentabilitateBKNG, col="cyan", main="Evolutia Rentabilitatilor BOOKING")
ts.plot(rentabilitateABNB, col="pink", main="Evolutia Rentabilitatilor AIRBNB")
ts.plot(rentabilitateNDAQ, col="aquamarine", main="Evolutia Rentabilitatilor NASDAQ")

#ziua cand Booking a avut pretul de inchidere maxim->19-01-24
which.max(PBKNG)
preturi[which.max(PBKNG),1]
#ziua cand Booking a avut pretul de inchidere minim->10-02-23
preturi[which.min(PBKNG),1]
#ziua cand Airbnb a avut pretul de inchidere maxim->28-07-23
preturi[which.max(PABNB),1]
#ziua cand Airbnb a avut pretul de inchidere minim->25-05-23
preturi[which.min(PABNB),1]
#ziua cand NASDAQ a avut pretul de inchidere maxim->02-02-23
preturi[which.max(PNDAQ),1]
#ziua cand NASDAQ a avut pretul de inchidere minim->03-10-23
preturi[which.min(PNDAQ),1]


