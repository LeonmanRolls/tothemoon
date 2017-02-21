(ns core.core)

;#To the moon
;
;The goal of this project is try out different automated trading strategies for trading various instruments 
;from FOREX to crypto-currencies.  
;
;##Up and running
;
;Type 'boot repl' into the command line to get started. 
;
;(dev) To watch for file changes and run tests and recompile docs
;
;### Simple strat candle ratio
;
;These are a group of strategies that modify the decision to trade or not based on the ratio of the body of a candle 
;to its wicks.
;
;# An experiment in heavy documentation and inline testing
;
;After returning to this project after a few months off I found it difficult to jump back in. One week I happened to 
;read various articles and other minor events that lead me to belive going heavy on documentation would be cool thing 
;to try. Having tests inline is also a form of documentation. The primary concern here would be that large tests 
;would obscure the source code and make navigating around more diffcult. My pet theory was that if the funcitons 
;could be kept small and single purposed enough then large tests wouldn't be necessary. Even if they were their 
;details could be abstracted away into functions. This could provide motivation to abstract relentlessly. 
;
;#Backtesting
;
;Backtesting is implemented as a reducer function. The reducer takes a map representing a trading account 
;and a time series of OHLC data (candles). The reducer then runs and returns the final account value. 
