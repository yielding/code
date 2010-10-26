WHERE = /opt/local/lib/ruby/1.8/i686-darwin10
CFLAGS=-I$(WHERE) -g
CC=gcc
LDFALGS=-L$(WHERE) -lruby -ldl -lobjc
embed: embed.o
	$(CC) -o embed embed.o $(LDFALGS)
