# windandpower

This project won the 2019 Duke MQM Summer Data Competition to engineer a reliable data set that could be used for follow on analysis. I scraped, cleaned, and engineered features for two years of energy generation data from the ERCOT website with the intention of using the data to answer the question, "What effect does a 1MW load increase/decreasse change the price that MW can be sold for?"

## Problem
If renewable energy sources like wind are to supplant oil and gas they must become cost effective for energy generators and consumers. Government subsidies are a major contributor to rises in US wind and solar power but margins in the energy market are still thin. Renewable energy companies must accurately price the power they generate in day-ahead markets to earn a profit. Additionally, they must be prepared to supply energy during times of high demand to take advantage of increased prices in real-time markets. Historical pricing trends in day-ahead and real-time markets, energy production rates, and the associated weather data are critical components to accurate price forecasting. A data set combining these parameters is essential to a renewable energy company’s strategy. 
The Electric Reliability Council of Texas (ERCOT) is one ten electricity markets in the US and oversees financial transactions between energy generators and distributors for approximately 90% of Texas residents. Texas has more installed wind capacity than any state in the US. ERCOT publishes data on wind energy production and pricing but the data is separated into various tables. Additionally, ERCOT only publishes its weather data for the current and previous month. These obstacles make forecasting future wind energy prices difficult.
This project’s goal was to create a database capable of three things:
Provide enhanced understanding of the relationship between weather and wind energy generation.
Provide insight into how changes in demand for energy affect wind energy prices.
Predict future settlement prices in the day-ahead and real-time ERCOT markets. 

## Method
Annual wind generation, monthly day-ahead market, and monthly real-time market data from January 1st, 2016 – December 31, 2017 was downloaded from ERCOT’s website. The fragmented data was first bound together to create wind energy generation, day-ahead market, and real-time market data tables covering 2016 – 2017.
Left joins were used to combine these three tables in order to take advantage of a many to one relationship between the real-time market data and the day-ahead market and wind generation tables. The resulting table contained the day-ahead settlement price and the real-time market price for each node for each day in 2016 and 2017. The data was then filtered to include only the regional nodes. 
In order accurately represent the weather at each regional node, climate data covering 2016-2017 for an international airport within each ERCOT region was requested from NOAA’s National Centers for Environmental Information. This data contained temperature and wind data at each regional airport. 
The weather data was joined to the wind generation and market table by first creating lists of key-value pairs matching regional nodes with their corresponding airport. The matching airport was then appended to the wind generation and market table. Finally, the two tables were joined on matching dates and airport names. The result is a data table consisting of historical pricing trends, wind generation statistics, and weather data for 2016 and 2017. 

## Bibliography
  * ERCOT. (2018, 09 03). ERCOT Foundations. Retrieved from ERCOT: http://www.ercot.com/services/training/course/151432#schedule

  * Federal Energy Regulatory Commission. (2017, December 4). Electric Power Markets: Texas (ERCOT). Retrieved from Federal Energy Regulatory Commission: https://www.ferc.gov/market-oversight/mkt-electric/texas.asp

  * Humphrey, G. (2018, August - September). Email exchange discussing wind energy generation and pricing. (T. Wileman, Interviewer)
