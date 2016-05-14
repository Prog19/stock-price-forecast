library(quantmod)
library(caTools)
library(PerformanceAnalytics)

run.model <- function(stock.name) {
	#GET STOCK DATA
	#stock.price = getSymbols(stock.name, auto.assign = F) 
		#requires internet for the NASDAQ symbol of the company to fetch from repository
	#stock.price = stock.price['2007::2012']
	#stock.price = stock.price[,4] #extract closing price - header column 4
	#write.table(stock.price, file, file.path(getwd(), "stock-price-forecast", stock.name, ".txt"))
	
	#Read the same dump - header removed
	stock.price = read.table(file.path(getwd(), "stock-price-forecast", stock.name, ".txt"))
	stock.price = stock.price[,5]

	#DAILY LOG RETURNS
	#stock.returns = dailyReturn(stock.price, type='log') 
	stock.returns=c()
	for (i in 1:length(stock.price)-1)
	{
		stock.returns[i] = log(stock.price[i+1]/stock.price[i])
	}		
		
	#SIMPLE VOLATILITY
	stock.vol = runsd(stock.returns,20)  #20-day window		
	plot(stock.vol, type='l', main = paste("Stock ",stock.name), xlab = "Time", ylab = "20-day Volatility")

	#STOCHASTIC VOLATILITY - REGRESSION MODELS
	y = c(0,stock.vol) #20-day mean returns 
	x = c() 
	x = stock.price
	options(scipen=999) #disable scientific notation

	#Linear model
	vol.model1 = lm(formula = y ~ x)
	print(summary(vol.model1))
	plot(y=y,x=x,ylab="Volatility",xlab="Stock price",main=paste("Stock ",stock.name)) #datapoints
	#abline(vol.model1)		
	lines(x,fitted(vol.model1)) #estimated regression line
	plot(vol.model1) #diagnostic plots

	vol.model3 = lm(formula = y ~ log(x))
	print(summary(vol.model3))
	plot(y=y,x=x,ylab="Volatility",xlab="Stock price",main=paste("Stock ",stock.name))
	lines(x,fitted(vol.model3))
	plot(vol.model3)

	#vol.model4 = lm(formula = y ~ log(x))
	#print(summary(vol.model4))
	#plot(y=y,x=x,ylab="Volatility",xlab="Stock price",main=paste("Stock ",stock.name))
	#lines(x,fitted(vol.model4))
	#plot(vol.model4)

	#Quadratic model		
	vol.model2 = lm(formula = y ~ poly(x,2))
	print(summary(vol.model2))
	plot(y=y,x=x,ylab="Volatility",xlab="Stock price",main=paste("Stock ",stock.name))
	lines(x,fitted(vol.model2))
	plot(vol.model2)

	#Residual standard error
	err1 = summary(vol.model1)$sigma 
	err2 = summary(vol.model2)$sigma 
	err3 = summary(vol.model3)$sigma
	cat("Model errors : ", err1, "\t", err2, "\t", err3, "\n")
	#Model selection
	e = err2
	coeffs = coef(vol.model2)


	#Stock price simulation - extended from GBM
	s=c()
	#latest stock price
	s[1] = stock.price[length(stock.price)-1] 
	s[2] = stock.price[length(stock.price)]
	ret=log(s[2]/s[1])
	stoch.vol=c()
	stoch.vol[1] = 0
	all.stock = stock.returns
	mu = mean(all.stock) #average so far
	#For the next 1 month
  	for(i in 2:30)
	{
	  #stoch.vol[i]=stoch.vol[i-1]+(coeffs[1]+coeffs[2]*ret+coeffs[3]*ret**2+e)
	  stoch.vol[i]=coeffs[1]+coeffs[2]*ret+coeffs[3]*ret**2+e #Chosen volatility model
	  #cat("stoch.vol ",stoch.vol[i], "\n")
	  s[i] = s[i-1]*(exp(mu-0.5*stoch.vol[i]**2)+(stoch.vol[i]*rnorm(1,-1,1))) #GBM Stock price model
	  ret=log(s[i]/s[i-1])
	  all.stock=c(all.stock,ret)
	  mu=mean(all.stock)
	}
	plot(s, xlab="Time (in days)", ylab="Stock price", type='o')
	sim.ret=c()
	for (i in 1:length(s)-1)
	{
		sim.ret[i] = log(s[i+1]/s[i])
	}	
	
	#RISK CALCULATION
	val.at.risk=VaR(sim.ret, p = 0.9, 
	method = "gaussian",
	clean = "none",
	portfolio_method = "single",
	weights = NULL, mu = mean(s), sigma = var(s),
	m3 = skewness(s), m4 = kurtosis(s), invert = FALSE)
	
	conditional.VaR=ES(sim.ret, p = 0.9, 
	method = "gaussian",
	clean = "none",
	portfolio_method = "single",
	weights = NULL, mu = mean(s), sigma = var(s),
	m3 = skewness(s), m4 = kurtosis(s), invert = FALSE)
	cat("Value at risk : ", val.at.risk, "\tConditional Value at Risk : ", conditional.VaR)
}
run.model('IBM')
	#Model errors :  0.006415972    0.005765206     0.00623856 
	#Value at risk :  0.04787755     Conditional Value at Risk :  0.05744611

#run.model('INFY')
	#Model errors :  0.009595642      0.009018464     0.009250758 
	#Value at risk :  0.1264683      Conditional Value at Risk :  0.1540751