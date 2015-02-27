LIBDIR = /usr/local/lib
LIBSODIUM = ./zeromq/libsodium-0.4.5
ZEROMQ = ./zeromq/zeromq4-x
CZMQ = ./zeromq/czmq
ZYRE = ./zeromq/zyre
MAKELIB = sudo ./configure; make; make check; sudo make install; sudo ldconfig /user/local/lib

SUBDIRS = . \
bindings bindings/curl/libcurl bindings/curl/tests \
curl \
examples \
include \
Island \
Motile Motile/capability Motile/compile Motile/generate Motile/persistent Motile/tests Motile/tests/splash-2012 \
persistent persistent/queue \
tests \
transport transport/channels transport/gates transport/transports

all: all-libs raco-make

clean: clean-libs clean-racket

clean-racket: 
	find . -name compiled -type d | xargs rm -rf

clean-libs: 
	find zeromq -name ".deps" -type d | xargs rm -rf
	find zeromq -name ".libs" -type d | xargs rm -rf
	find zeromq -name "*.o" -exec rm -rf {} \; 
	find zeromq -name "*.lo" -exec rm -rf {} \; 

raco-make:
	for dir in $(SUBDIRS); do \
            raco make *.rkt; \
        done
		
all-libs: $(LIBDIR)/libsodium.so $(LIBDIR)/libzmq.so $(LIBDIR)/libczmq.so $(LIBDIR)/libzyre.so

$(LIBDIR)/libsodium.so: 
	cd $(LIBSODIUM); $(MAKELIB)
$(LIBDIR)/libzmq.so:
	cd $(ZEROMQ); $(MAKELIB)
$(LIBDIR)/libczmq.so:
	cd $(CZMQ); $(MAKELIB)
$(LIBDIR)/libzyre.so:
	cd $(ZYRE); $(MAKELIB)
