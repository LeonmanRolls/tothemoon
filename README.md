#To the moon

This is the implementation of a very simple trading strategy based on candlesticks. 
If the last candle was green, buy, and put a stop loss at the low of the last candle. 
If the last candle was red, sell, and put a stop loss at the high of the last candle. 

##Up and running

Type 'boot repl' into the command line to get started. Available commands reside in 
build.boot. 

##Terminology

Simple strat - The name for the most basic implementation of the strategy mentioned above.
