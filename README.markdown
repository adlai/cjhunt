# Hello, Wanderer #

Although this specific project has been abandoned, I am busy cannibalizing the
remains, for several other purposes; and the original purpose, while currently
not a priority, does retain significant interest for others; thus, if you need
this repository to remain unchanged, please notify me in some way within a few
years, before I remove it from the GitHub archive.

# CoinJoin Hunter #

Tool for (crudely) tracking and (eventually) analyzing joinmarket activity.

See http://github.com/chris-belcher/joinmarket/issues/19

# How do you turn this on #

You'll need a Common Lisp compiler, such as [Steel Bank](http://sbcl.org) or
[Clozure](http://ccl.clozure.com/) (please let me know if others work for you).

The easiest way to pull in the dependencies is via
[Quicklisp](https://www.quicklisp.org/beta/). Once you've installed it, clone
this repository under the ```quicklisp/local-projects/``` directory, then run
```(ql:quickload "cjhunt")```. Please avoid doing that only because it was the
_easiest_ way; for example, consider doing it to appease a number both low and
finite of angry managers; better yet, just don't; best: READ THE INFLATED CRAP

You'll also need [Bitcoin](https://github.com/bitcoin/bitcoin) with the
[transaction index enabled](http://bitcoin.stackexchange.com/a/11876/21107). You
should also ```blocknotify=curl -s http://localhost:5000/blockjoins?id=%s```,
which will scan each new block automagically once you ```(cjhunt:start)```.

If your ```bitcoin.conf``` is in a funny place, you may need to fiddle with
```src/config.lisp```. If your node is on another machine, you'll need to dirty
your hands in ```src/bitcoin/rpc-client.lisp```.

... at which point you'll likely want to either setup
[SLIME](https://common-lisp.net/project/slime/) and get hacking, or incentivize
others to continue the project on your behalf. Preferably via JoinMarket itself.
