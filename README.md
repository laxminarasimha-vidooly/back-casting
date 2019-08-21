# back-casting
Obective: Back forecast the viewership of channel which is missing from daily tracking

Tool: R

Technique: ARIMA modelling

Process:

1. Daily tracking of channels fetched through API for which we need the backcasted values

2. Preprocess the data for isolating viewership and date from the log file

3. Run ARIMA modelling on the data

4. Visualize the data using actual and forecasted values 

Accuracy: Model performed better with non transformed data and the results were pretty close to the actual values

