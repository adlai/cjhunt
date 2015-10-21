# CoinJoin Hunter #

Tool for (crudely) tracking and (eventually) analyzing joinmarket activity.

See http://github.com/chris-belcher/joinmarket/issues/19

# How do you turn this on #

You'll need a Common Lisp compiler, such as [Steel Bank](http://sbcl.org) or
[Clozure](http://ccl.clozure.com/) (please let me know if others work for you).

The easiest way to pull in the dependencies is via
[Quicklisp](https://www.quicklisp.org/beta/). Once you've installed it, clone
this repository under the ```quicklisp/local-projects/``` directory, then run
```(ql:quickload "cjhunt")```.

You'll also need [Bitcoin](https://github.com/bitcoin/bitcoin) with the
[transaction index enabled](http://bitcoin.stackexchange.com/a/11876/21107). You
may also want ```blocknotify=curl -s http://localhost:5000/blockjoins```, which
will scan for new coinjoins in each new block, once you ```(cjhunt:start)```.

... at which point you'll likely want to either setup
[SLIME](https://common-lisp.net/project/slime/) and get hacking, or incentivize
others to continue the project on your behalf. Preferably via JoinMarket itself.
