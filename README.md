COmputAtional State Transfer
============================

COmputAtional State Transfer (COAST) is an architectural style based upon computational exchange for secure, adaptive, decentralized systems developed by the [Institute for Software Research](http://isr.uci.edu) at the [University of California, Irvine](http://uci.edu).

# COAST Architectural Principles

#### Computations

* All services are computations which execute within the confines of some execution site ‹E,B› where E is an execution engine and B a binding environment. In other words, a COAST computation is the execution of a closure c by the execution engine E of its execution site in the context of B.
* E may enforce site-specific semantics: for example, limits on the consumption of resources such as processor cycles, memory, storage, or network bandwidth; rate-throttling of the same; logging; or adaptations for debugging.

#### Communication

* The sole means of interaction among computations is the asynchronous messaging of closures (functions plus their lexical-scope bindings), continuations (snapshots of execution state) and binding environments (maps of name/value pairs). The receival of a message may result in the execution of some functionality, the sending of a message to another computation, or in the creation of a new computation.

#### Addressability

* Communication capability is both granted and constrained by capability URLs (called CURLs). A CURL is a capability that conveys the ability for two computations to communicate. A CURL c issued by a computation x, is an unguessable, unforgeable and tamper-proof reference to x that grants to any computation y holding c the power to transmit messages to x. A CURL contains cryptographic material identifying x and is signed by x's underlying execution host. CURLs are used to constrain interactions with a computation and also to bound the services that it offers.

#### Security

* COAST relies on two security principles: the Principle of Least Authority (POLA) and capability-based security. POLA dictates that security is a cross-product of the authority given to a principal (the functional power made available) and the rights given to the principal (the rights of use conferred with respect to that authority). At each point within a system a principal must be simultaneously confined with respect to both authority and rights. In keeping with POLA, each binding environment B of an execution site should contain just the functional capability that visiting mobile code requires to implement a confined segment of service.
* A capability is an unforgeable reference whose possession confers both authority and rights to a principal. A principal can be an individual, group of individuals, or a computation running on his or their behalf. The set of all closures and primitives reachable from binding environment B is the initial functional capability of a closure c. No local computation beyond that permitted by B is possible since, for any closure c, B is the only resolver for the free variables of c, hence, the only provider of the primitives that c's definition requires.

# The Motile/Island Platform

Motile/Island is a reference implementation of the COAST architectural style. Motile is a language for mobile computation, and Island is the infrastructure supporting the deployment and execution of mobile computations. Motile and Island are closely integrated and although they are not the same thing, they are interrelated up to the point that we usually call them the Motile/Island Platform.

## Start working the Motile/Island Platform

Learn the [basics](http://isr.uci.edu/projects/coast/gettingstarted.html#basics) of COAST, [download the Motile/Island Platform](http://isr.uci.edu/projects/coast/about.html#download), and take a look at the [examples](http://isr.uci.edu/projects/coast/gettingstarted.html#hello) we created.

# Impact of COAST

Our research team has published several papers, journal articles, and other reports about COAST and other related technologies. Take a look at the [complete list of publications](http://isr.uci.edu/projects/coast/about.html#publications).
