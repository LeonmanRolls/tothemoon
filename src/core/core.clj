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
;## Simple strat
;
;This is the implementation of a very simple trading strategy based on candlesticks.  
;If the last candle was green, buy, and put a stop loss at the low of the last candle, on the sell side, sell and put 
;a stop loss at the high of the last candle. 
;
;### Simple strat candle ratio
;
;These are a group of strategies that modify the decision to trade or not based on the ratio of the body of a candle 
;to its wicks.
