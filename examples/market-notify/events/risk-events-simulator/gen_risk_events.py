'''
This module generates stock risk events for the COAST market-notify example
It takes an input file of the format:

<number of events to generate>
<min risk value> <max risk value>
<min time delay> <max time delay>
[<stock symbol> <start risk_value> <delta change> ]*

e.g.

200
0 100
1 5
GOOG 3 1
YHOO 10 2
FB 15 3
IBM 20 4

Sample generated output will look like this:

delay 3
IBM risk 20
delay 4
FB risk 15
delay 1
IBM risk 20
delay 2
GOOG risk 4
delay 5
YHOO risk 8
delay 1
GOOG risk 3
delay 2
IBM risk 22
delay 2
YHOO risk 7
delay 2
IBM risk 19
delay 1

'''

import random
import time

event_count = 0
max_delay = min_delay = 0
stock_risks = {}

def init_stock_data(infile):
    # read event count
    event_count = int(infile.readline()[:-1]);
    # read min and max risk values
    min_risk_str, max_risk_str = infile.readline()[:-1].split(" ");
    min_risk = int(min_risk_str)
    max_risk = int(max_risk_str)
    # read min and max delays
    min_delay_str, max_delay_str = infile.readline()[:-1].split(" ");
    min_delay = int(min_delay_str)
    max_delay = int(max_delay_str)

    for line in infile:
        stock_info = line.replace("\n", "").split(" ");
        init_risk_value = int(stock_info[1])
        delta_change = int(stock_info[2])
        stock_risks[stock_info[0]] = [init_risk_value, delta_change]

    return event_count, min_risk, max_risk, min_delay, max_delay, stock_risks

def generate_stock_events(outfile, event_count, min_risk, max_risk, \
                          min_delay, max_delay, stocks):
    for i in range(event_count):
        delay = random.randint(min_delay, max_delay)
        symbol = random.choice(list(stocks.keys()))
        current_risk= stocks[symbol][0]
        delta = stocks[symbol][1]
        diff = random.randint(-delta, delta)
        new_risk = current_risk + diff
        if min_risk <= new_risk <= max_risk:
            stocks[symbol][0] = new_risk
        print("delay %d" % delay, file=outfile)
        print("%s %s %d" % (symbol, "risk", stocks[symbol][0] ), file=outfile)    

def main():
    in_filename = "risk_input.txt"
    infile = open(in_filename, "r")
    event_count, min_risk, max_risk, min_delay, max_delay, stock_risks = \
        init_stock_data(infile)
    print("event count:", event_count)
    print("min risk value:", min_risk, "max risk value:", max_risk)
    print("min delay:", min_delay, "max delay:", max_delay)    
    print("Risk info:", stock_risks)
    
    out_filename = "risk_events.txt"
    outfile = open(out_filename, "w")
    generate_stock_events(outfile, event_count, min_risk, max_risk,\
                          min_delay, max_delay, stock_risks)
    infile.close()
    outfile.close()

    
if __name__ == "__main__":
    main()
