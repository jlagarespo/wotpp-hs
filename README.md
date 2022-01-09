# wotpp-hs
Haskell port of the [original wot++](https://github.com/wotpp/wotpp) programming
language. Technically more of a "redesign+rewrite" than a "port". As of right now it's really just
old-style wot++ with pattern matching and very crappy string lexing.

## The Zen of wot++
Inspired by [the Zen of Python](https://www.python.org/dev/peps/pep-0020/), I decided to write some
design principles for wot++, to drive all design decisions for at least this port. Someone should
come back periodically and check on these, to ensure they're being followed.

* Strings are the only type you need.
* Pattern matching is the only conditional you need.
* In general, there should be only one way to do it.
* Minimalism is good as long as it doesn't hinder readability.
* Explicit is better than implicit as long as it doesn't hinder readability.
* Special cases should be minimized, but those that make it shoud be handled explicitly.
* Keep it simple stupid!
* Errors should never pass silently.
* Optimization is good if it doesn't get in the way of design.
