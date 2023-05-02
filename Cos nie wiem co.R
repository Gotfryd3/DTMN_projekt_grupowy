library(caret)
library(ROCR)

library(car)
#Wczytanie danych
weights = read.csv('C:/Users/Adi/Documents/GitHub/DTMN_projekt_grupowy/ObesityDataSet_raw_and_data_sinthetic.csv')
#Dodanie kolumny pozwalającej przeprowadzić regresję logiczną
#Uznajemy, iż jeśli w parametrze "NObeyesdad" znjaduje się opis "Normal_Weight"
#to w "Logic" wpisujemy "No" jako oznaczenie osoby nie otyłej
# wprzeciwnym wypadku osobe uznajemy jako otyłą i ma oznaczenie "Yes"
for (i in 1 : nrow(weights))
{
	if (weights[i, "NObeyesdad"] == 'Normal_Weight')
	{
	
		weights[i, "Logic"] <- 0
	}
	else
	{
		weights[i, "Logic"] <- 1
	}
}
print(colnames(weights))
#Opis wszystkich parametrów określających wybraną bazę danych:
#Frequent consumption of high caloric food (FAVC) - częstość spożywania wysokokalorycznych potraw;
#Frequency of consumption of vegetables (FCVC) - częstotliwość jedzenia warzyw;
#Number of main meals (NCP) - liczba posiłków;
#Consumption of food between meals (CAEC) - jedzenie pomiędzy posiłkami;
#Consumption of water daily (CH20) - ilość spożywanej wody;
#Family_history_with_overweight - czy wśród członków rodziny są osoby z nadwagą;
#Smoking (SMOKE) - palenie;
#Consumption of alcohol (CALC) - częstość spożywania alkoholu;
#Calories consumption monitoring (SCC) - monitorowanie zużycia kalorii;
#Physical activity frequency (FAF) - częstotliwość aktywności fizycznej;
#Time using technology devices (TUE) - czas spędzony na korzystaniu z urządzeń komputerowych;
#Transportation used (MTRANS) - rodzaj przemieszania się;
#Gender - płeć;
#Age - wiek;
#Height - wzrost;
#Weight - waga
#NObeyesdad - oznaczenie stopnia otyłości (na podstawie tego paramtetru 
#ustanowiliśmy własny parametr 0/1 - 'Logic', który posłuży nam do przeprowadzenia
#regresji logistycznej).

#Regresja logistyczna jest przydatna w sytuacjach, w których wymagane 
#jest przewidywanie obecności lub braku cechy bądź wyniku na podstawie 
#wartości zestawu predyktorów. Jest podobna do modelu regresji liniowej
#(w przypadku regresji liniowej prognozowana wartość Y może wyjść poza 
#przedział 0 - 1, w przypadku regresji logistycznej wartość ta musi być z 
#przedziału 0 - 1),ale nadaje się dla modeli, w których zmienna zależna 
#jest dychotomiczna (przyjmuje maksymalnie 2 wartości). 
#Współczynniki regresji logistycznej mogą być używane do oszacowania 
#ilorazów szans dla każdej zmiennej niezależnej w modelu. Regresja 
#logistyczna ma zastosowanie do szerszego zakresu sytuacji badawczych 
#niż analiza dyskryminacyjna.
#Opis matematyczny takiego modelu opisany jest wzorem:
#   logistic(t) = exp(t) / (1 + exp(t)),
#gdzie t określone na przedziale (- inf, + inf);
#Wykres takiej funkcji przyjmuje formę dużej litery 'S', czyli
#funkcja ta charakteryzuje się tym, iż taka sama zmiana zmiana wartości t
#spowoduje gwałtowniejszą zmianę prawdopodobieństwa logistic(t), gdy jest ono 
#bliskie 0.5 niż, gdy funkcja ta osiąga wartości bliskie skrajnym (0 bądź 1).
#Model regresji logistycznej zakłada spełnienie innych warunków niż 
#model regresji liniowej. Nie obowiązują w nim na przykład założenia 
#dotyczące normalności czy homoskedastyczności. Wymagane jest jednak, aby
#obserwacje były od siebie niezależne.

#Podsumowanie całej bazy
summary(weights)

#W celu przeprowadzenia regresji logistycznej dodane parametry numeryczne 
#opisujące  parametry nie numeryczne.

for (i in 1 : nrow(weights))
{
	if (weights[i, "Gender"] == 'Female')
	{
	
		weights[i, "NumGender"] <- 0
	}
	else
	{
		weights[i, "NumGender"] <- 1
	}
}
for (i in 1 : nrow(weights))
{
	if (weights[i, "family_history_with_overweight"] == 'yes')
	{
	
		weights[i, "NumHistory"] <- 1
	}
	else
	{
		weights[i, "NumHistory"] <- 0
	}
}
for (i in 1 : nrow(weights))
{
	if (weights[i, "FAVC"] == 'yes')
	{
	
		weights[i, "NumFAVC"] <- 1
	}
	else
	{
		weights[i, "NumFAVC"] <- 0
	}
}
for (i in 1 : nrow(weights))
{
	if (weights[i, "CAEC"] == 'no')
	{
	
		weights[i, "NumCAEC"] <- 0
	}
	else if (weights[i, "CAEC"] == 'Sometimes')
	{
		weights[i, "NumCAEC"] <- 1
	}
	else if (weights[i, "CAEC"] == 'Frequently')
	{
		weights[i, "NumCAEC"] <- 2
	}
	else
	{
		weights[i, "NumCAEC"] <- 3
	}
}
for (i in 1 : nrow(weights))
{
	if (weights[i, "SMOKE"] == 'yes')
	{
	
		weights[i, "NumSMOKE"] <- 1
	}
	else
	{
		weights[i, "NumSMOKE"] <- 0
	}
}
for (i in 1 : nrow(weights))
{
	if (weights[i, "SCC"] == 'yes')
	{
	
		weights[i, "NumSCC"] <- 1
	}
	else
	{
		weights[i, "NumSCC"] <- 0
	}
}
for (i in 1 : nrow(weights))
{
	if (weights[i, "CALC"] == 'no')
	{
	
		weights[i, "NumCALC"] <- 0
	}
	else if (weights[i, "CALC"] == 'Sometimes')
	{
		weights[i, "NumCALC"] <- 1
	}
	else if (weights[i, "CALC"] == 'Frequently')
	{
		weights[i, "NumCALC"] <- 2
	}
	else
	{
		weights[i, "NumCALC"] <- 3
	}
}
for (i in 1 : nrow(weights))
{
	if (weights[i, "MTRANS"] == 'Walking')
	{
	
		weights[i, "NumMTRANS"] <- 0
	}
	else if (weights[i, "MTRANS"] == 'Bike')
	{
	
		weights[i, "NumMTRANS"] <- 1
	}
	else if (weights[i, "MTRANS"] == 'Motorbike')
	{
	
		weights[i, "NumMTRANS"] <- 2
	}
	else if (weights[i, "MTRANS"] == 'Automobile')
	{
	
		weights[i, "NumMTRANS"] <- 3
	}

	else
	{
	
		weights[i, "NumMTRANS"] <- 4
	}
}
#Usuwamy kolumny z danych nienumerycznymi, żeby było łatwiej kontrolować
#działania na pozostałej bazie danych
weights <- subset(weights, select = -c(Gender, MTRANS, family_history_with_overweight,
						   NObeyesdad, FAVC, SMOKE, CALC, CAEC, SCC))
#Podział danych 
intrain <- createDataPartition(y = weights$Logic, p= 0.7, list = FALSE)
training <- weights[intrain,]
testing <- weights[-intrain,]

logistic <- glm(Logic ~ NumGender + Age + Height + Weight + NumFAVC + NumCAEC 
			+ NumHistory + FCVC + CH2O + NumSCC + NumMTRANS + NCP + NumSMOKE
			+ FAF + TUE + NumCALC, data = training)
summary(logistic)
pred <- predict(logistic, testing)
print(testing)
pred <- ifelse(pred > 0.5, 1, 0)
table(testing$Logic, pred) 
#Na podstawie tablicy błędów widać, iż występuje duża ilość błędów II rodzaju
#czyli osoby posiadające nadwagę zostały uznane za zdrowe. W przypadku
#pierwszego rodzaju błędu, ilość błędów można przyjąć za możliwą.
print(pred)
ROCPred <- prediction(pred, testing$Logic) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                             x.measure = "fpr")
auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]

plot(ROCPer, colorize = TRUE,  
     main = "ROC CURVE")
abline(a = 0, b = 1)
auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)
#Na podstawie krzywej ROC możemy stwierdzić, iż powstały model nie jest zbyt 
#dobry, gdyż obszar pod krzywą (czerwono-zielona linia) jest mała, dla 
#porównania wyznaczono linią czarną pseudolosowe dopasowanie, które dokonuje
#w 50% poprawnego dopasowania, widać więc, iż nasz model nie działa w sposób
#wyuczony, lecz w sposób bliski losowości, co tylko potwierdza wartość AUC
#równa ~57%.
#MOŻNA TU JESZCZE DODAĆ JAKIES INNE WYKRESY POKAZUJACE JAKIE TO JEST GÓWNO
#JAKIES WYKRESY INFLUENCE LUB RESIDUÓW(CHYBA W CW 3 Z PROJ INDYWIDUALNEGO)
#Redukcja
linear <- lm(Logic ~ NumGender + Age + Height + Weight + NumFAVC + NumCAEC 
			+ NumHistory + FCVC + CH2O + NumSCC + NumMTRANS + NCP + NumSMOKE
			+ FAF + TUE + NumCALC, data = weights)
cooke <- cooks.distance(linear)
plot(cooke)
influential <- as.numeric(names(sort(cooke, decreasing = TRUE)[1:10]))
weights <- weights[-influential,]

linear <- lm(Logic ~ NumGender + Age + Height + Weight + NumFAVC + NumCAEC 
			+ NumHistory + FCVC + CH2O + NumSCC + NumMTRANS + NCP + NumSMOKE
			+ FAF + TUE + NumCALC, data = weights)
cooke <- cooks.distance(linear)
plot(cooke)
#Odjęcie 10 obserwacji o najwyższej wartości odległości Cooka, powoduje jej 
#spadek o prawie połowe
#Sprawdzenie p-value bez 10 obserwacji i usunięcie paremetru o największym p - value
logistic <- glm(Logic ~ NumGender + Age + Height + Weight + NumFAVC + NumCAEC 
			+ NumHistory + FCVC + CH2O + NumSCC + NumMTRANS + NCP + NumSMOKE
			+ FAF + TUE + NumCALC, data = training)

pVal <- summary(logistic)$coefficients[,4]
weights <- weights[-which(max(pVal) == pVal)]
logistic <- glm(Logic ~ NumGender + Age + Height + Weight + NumFAVC + NumCAEC 
			+ NumHistory + FCVC + CH2O + NumSCC + NumMTRANS + NCP + NumSMOKE
			+ FAF + TUE + NumCALC, data = training)
summary(logistic)
pred <- predict(logistic, testing)
pred <- ifelse(pred > 0.5, 1, 0)
table(testing$Logic, pred) 
ROCPred <- prediction(pred, testing$Logic) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                             x.measure = "fpr")
auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
plot(ROCPer, colorize = TRUE,  
     main = "ROC CURVE")
abline(a = 0, b = 1)
auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)
#Dalsza redukcja liczby zmiennych poprzez pVal obniżała wartość AUC
influencePlot(logistic, main="Influence Plot", sub="Circle size is proportial to Cook's Distance")

#Na podstawie powyższego plota widać, iż największy wpływ na model ma obserwacja 233 (MOŻNA SPRÓBOWAĆ JĄ USUNĄĆ I ZOBACZYĆ CO TO DA)
#wiele obserwacji ma nikły wpływ na model - mały promień okręgu i wartość residuów studentyzowanych bliska zeru.
#NIE WIEM O CO CHODZI W PUNKCIE 6 