'''
This module generates stock events for the COAST market-notify example
It takes an input file of the format:

<number of events to generate>
<min time delay> <max time delay>
<min purchase quantity> <max purchase quantity>
[<stock symbol> <start price> <delta change> <list of possible buyers> <list of posssible sellers>]*

e.g.

200
1 5
1 500
GOOG 644 10 [A,B] [A,B]
YHOO 39 5 [A] [B]
FB 94 2 [A] [B]
IBM 161 3 [A] [B]

Sample generated output will look like this:

delay 3
IBM order 16150 139 A B
delay 3
GOOG order 55242 172 B A
delay 2
IBM order 15977 417 A B
delay 1
GOOG order 55842 256 A A
delay 4
FB order 8371 444 A B
delay 2
GOOG order 56301 446 A B
delay 2
FB order 8425 214 A B
delay 5
IBM order 16187 440 A B
delay 3
GOOG order 56154 120 B B
delay 5
YHOO order 30248 484 A B

'''

import random
import time

event_count = 0
max_delay = min_delay = 0
stocks = {}

def init_stock_data(infile):
    # read event count
    event_count = int(infile.readline()[:-1]);
    # read min and max delays
    min_delay_str, max_delay_str = infile.readline()[:-1].split(" ");
    min_delay = int(min_delay_str)
    max_delay = int(max_delay_str)
    min_quantity_str, max_quantity_str = infile.readline()[:-1].split(" ");
    min_quantity = int(min_quantity_str)
    max_quantity = int(max_quantity_str)

    for line in infile:
        stock_info = line.replace("\n", "").split(" ");
        sellers = (stock_info[3][1:-1]).split(",")
        buyers = (stock_info[4][1:-1]).split(",")
        init_price = int(stock_info[1])*100 # convert to cents
        delta_change = int(stock_info[2])*100 # convert to cents
        stocks[stock_info[0]] = [init_price, delta_change, sellers, buyers]

    return event_count, min_delay, max_delay, \
           min_quantity, max_quantity, stocks

def generate_stock_events(outfile, event_count, min_delay, max_delay, \
                          min_quantity, max_quantity, stocks):
    for i in range(event_count):
        delay = random.randint(min_delay, max_delay)
        quantity = random.randint(min_quantity, max_quantity)
        symbol = random.choice(list(stocks.keys()))
        seller = random.choice(stocks[symbol][2])
        buyer = random.choice(stocks[symbol][3])
        current_price = stocks[symbol][0]
        delta = stocks[symbol][1]
        diff = random.randint(-delta, delta)
        new_price = current_price + diff
        # don't allow the price to go negative!
        if new_price < 0:
            new_price = 0
        stocks[symbol][0] = new_price
        print("delay %d" % delay, file=outfile)
        print("%s %s %d %d %s %s" % (symbol, "order", new_price, quantity,
                                     seller, buyer), file=outfile)    

def main():
    in_filename = "stock_input.txt"
    infile = open(in_filename, "r")
    event_count, min_delay, max_delay, min_quantity, max_quantity, stocks = \
        init_stock_data(infile)
    print("event count:", event_count)
    print("min delay:", min_delay, "max delay:", max_delay)
    print("min quantity:", min_quantity, "max quantity:", max_quantity)
    
    print("Stock info:", stocks)
    
    out_filename = "stock_events.txt"
    outfile = open(out_filename, "w")
    generate_stock_events(outfile, event_count, min_delay, max_delay, \
                          min_quantity, max_quantity, stocks)
    infile.close()
    outfile.close()

    
if __name__ == "__main__":
    main()
