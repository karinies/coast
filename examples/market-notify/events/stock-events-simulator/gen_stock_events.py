import random
import time

event_count = 0
max_delay = min_delay = 0
stocks = {}

def init_stock_data(infile):
    # read event count
    event_count = int(infile.readline()[:-1]);
    # read min and max delays
    min_delay, max_delay = infile.readline()[:-1].split(" ");
    min_quantity, max_quantity= infile.readline()[:-1].split(" ");

    for line in infile:
        stock_info = line.replace("\n", "").split(" ");
        sellers = (stock_info[3][1:-1]).split(",")
        buyers = (stock_info[4][1:-1]).split(",")
        init_price = int(stock_info[1])*100 # convert to cents
        delta_change = int(stock_info[2])*100 # convert to cents
        stocks[stock_info[0]] = [init_price, delta_change, sellers, buyers]

    return event_count, int(min_delay), int(max_delay), \
           int(min_quantity), int(max_quantity), stocks

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
    print("min delay:", min_delay, "max_delay:", max_delay)
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
