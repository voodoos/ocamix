# OCAMIX

OCAMIX is yet another music client for servers like Jellyfin. Focus is made on
handling large libraries (>100000 tracks) and the app relies on a local database
to perform correctly.

Right now this is still an early work-in-progress:

- The feature set is very limited: browsing and playing libraries sorted by
  random.
- Code quality varies but is mostly quite low since most parts of the software
  are still in a fast-moving exploration phase.

This experiment is mostly an excuse to play with the somewhat-new [`brr-lwd`
library](https://ocaml.org/p/brr-lwd/latest) which provides reactive web
programming (:heart:).

This project features bindings to
[IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) that
will be released independently at some point.
