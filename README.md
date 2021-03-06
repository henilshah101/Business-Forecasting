# Retail Gas Price Prediction

summary:
This project seeks to understand the change in gasoline prices based on consumer behavior over a span of 18 years: namely, to compare gasoline prices to the number of registrations of conventional cars, sales of Hybrid Electric Vehicles (HEVs), and vehicle fuel consumption (all in the U.S.). The objective is to be able to accurately predict the gasoline price in the US market for the coming years. We use reported data tracked on a monthly basis from January 2000 through December 2017. Our task is to forecast the gasoline price in 2018 based on analysis of past performance and dependencies on consumer behaviour using fpp module in R. We will measure the accuracy of the forecast by comparing our results to actual data published for 2018 using RMSE error technique.

Data information:
The gas pricing data was obtained from Kaggle, which linked to the U.S. Energy Administration Information dataset for U.S. gasoline prices. Conventional car registration data was found at the U.S. Department of Transportation's Federal Highway Administration's Office of Highway Policy Information website. Sales of Electric Vehicles were obtained from the U.S. Department of Energy's Alternative Fuels Data Center, and natural gas vehicle fuel consumption was found on the U.S. Energy Administration Information's website. 

Links: 
https://www.kaggle.com/mruanova/us-gasoline-and-diesel-retail-prices-19952021
https://www.eia.gov/dnav/pet/PET_PRI_GND_A_EPMRU_PTE_DPGAL_A.htm
https://www.energy.gov/sites/prod/files/2016/09/f33/NY_Energy%20Sector%20Risk%20Profile_0.pdf 
https://www.fhwa.dot.gov/policyinformation/statistics/2019/mv1.cfm 
https://afdc.energy.gov/data/?q=electric+vehicles 
https://www.fhwa.dot.gov/policyinformation/statistics.cfm
https://www.eia.gov/dnav/ng/hist/n3025us2m.htm

Collaborators:
https://github.com/lgheit
https://github.com/smgomez12

Insights:
The problem statement was choosen to solve a real world problem for the following stakeholders:
a) Car manufacturers - Helping them decide when to launch the new cars.
b) Consumers - Helping them budget for expenditures, car purchases & lifestyle
c) Economists, Government Entities - Helping them predict the direction of a country's growth
d) Tourism Industry - Helping them plan trips in the months where gasoline is more affordable

The data ranges from 2000 to 2017, predicting the values for 2018. 
We decided to not use the entire data set as there are some major dips in 2008 and 2015, we've considered a subset of data which helps us get very accurate predictions. 
On testing various forecasting models, we would like to recommend the following models:
a) Seasonal Naive Bayes 
b) Moving Average of Order 12 months

The presentation gives a visual represenation of each model and technique used to derive the best performing model.
https://docs.google.com/presentation/d/1RNdw-PgjJxKFk6plIvpk6g_l8SZTqTZG6NCQEpNb1f0/edit#slide=id.p
