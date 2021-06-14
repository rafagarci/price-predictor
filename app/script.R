# Load packages
if(!require("rugarch")){
    install.packages("rugarch")
    library("rugarch")
}
if(!require("R.utils")){
    install.packages("R.utils")
    library("R.utils")
}
if(!require("grDevices")){
    install.packages("grDevices")
    library("grDevices")
}

# Import https://gist.github.com/ivannp/5198580
# source("./5198580/garchAuto.R")

# Get BitCoin data
if(system("python3 script.py")!= 0){
    stop("Error while getting data")
}

DATA = read.csv("data.csv")

# Log returns
DATA = data.frame(time = DATA$time, log.returns = log(DATA$close/DATA$open))

# Choose best ARMA model
# https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R/
final.aic <- Inf
final.order <- c(0,0,0)
for (p in 0:5) for (q in 0:5) for (d in 0:2){
    if ( p == 0 && q == 0) {
        next
    }

    arimaFit = tryCatch( arima(DATA$log.returns, order=c(p, d, q)),
                        error=function( err ) FALSE,
                        warning=function( err ) FALSE )

    if( !is.logical( arimaFit ) ) {
        current.aic <- AIC(arimaFit)
        if (current.aic < final.aic) {
            final.aic <- current.aic
            final.order <- c(p, d, q)
            final.arima <- arima(DATA$log.returns, order=final.order)
        }
    } else {
        next
    }
}

# Fit GARCH
spec = ugarchspec(
            variance.model=list(garchOrder=c(1,1)),
            mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
            distribution.model="sged")

# Take difference of values to add parameter d to ARMA model.
# DATA2 = diff(DATA$log.returns, final.order[2])

fit = tryCatch(
        ugarchfit(
            spec, DATA$log.returns, solver = 'hybrid'
        ),
        error=function(e) e, warning=function(w) w
)

# Create model diagnostic plots
# pdf("./Rplots.pdf")
# plot(fit)
# dev.off()

# Clear terminal, meant to be used on linux
system("clear")

# forecast
n = as.numeric(commandArgs()[6])
result = ugarchforecast(fit, n.ahead = n)
mu = fitted(result)
sig = sigma(result)
Tplus = c()
for(i in 1:n){
    Tplus = c(Tplus, paste("T+", i, sep = ""))
}

# Percentage changes
percent = data.frame(Series = exp(mu))
colnames(percent) = c("% Change")

# Print all values
print("Log returns forecast")
print(result)
print("Estimated real value percentage change")
print(percent)

