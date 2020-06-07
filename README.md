# R-for-food-price-analysis
This project presents a detailed analysis of the behaviour of food prices in selected markets in Ghana. The prices under study are cassava, maize, plantain, rice and yam, with markets being Accra, Cape Coast, Ho, Kumasi, Mankessim, Obuasi, Sekondi-Takoradi, Sunyani, Tamale and Techiman.
Using the R programming language and monthly data from January 2008 to July 2017, I collect, clean, visualise and model the time-series properties of these prices.  To conduct a detailed analysis, I conduct commodity price analysis by market and market analysis by commodity. The rationale for conducting market analysis by commodity is to ascertain the extent to which prices in one market (market A) interact with and are transmitted to prices in another market (market B).

NOTES ON DATA TRANSFORMATION
The units are different. We have 100 KG, 50 KG, 91 KG, Bunch and 100 yams. So I convert Bunch and Yams to KG. For yams, I use 100 yams = 15 KG (Refer to Ghana yam report). What of 22.5 KG to 12 tubers based on Ghana Yam Sector Development Report. I use 100 Tubers = 375 KG based on average of 2.5 kg and 5 kg. Means that 1 tuber = 3.75 KG (Refer to yam brief).
With respect to plantain, I use 1 bunch = 24.5 KG. This is based on the conclusion (Ref: Honduran Association for Agricultural Research (FHIA) Report 21 - FHIA-21) that the bunch of plantain is between 22 and 27 KG. I took the average of the two. This calculation is based on the assumption that a bunch of plantain = the average size of a bunch of FHIA plantain. To do this, I create a new variable that uses the above rates.

MODELLING
Linear regression: Normal equation
Analysis 1: Commodity price analysis by market
-	The goal here is to model the interaction between food prices in in each market. 
-	Such analysis is justified in that all foods are staples, thus regularly eaten by households
-	Modelling technique: Panel data modelling

