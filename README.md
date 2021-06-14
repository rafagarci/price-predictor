# Stock Price Predictor

A script that predicts stock prices based on the selection of an appropriate ARIMA + GARCH model.

## Running the script

You need to have R installed. Then you can do

```r
Rscript script.R n
```

where `n` is the number of ahead values to predict.

## Running in docker

Build and start the container with:
```
docker-compose up
```