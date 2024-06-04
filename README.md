# OCAMIX

OCAMIX is yet another music client for servers like Jellyfin. Focus is made on
handling large libraries (>100000 tracks) and the app relies on a local database
to perform custom sorting and filteriing as fast as possible.

Right now this is still an early work-in-progress:

- The feature set is very limited: browsing and playing libraries sorted by
  random.
- Most parts of the executable and its library are still in a fast-moving exploration phase.
- Some features are implemented multiple times in different ways to assess which API is the best.
- Focus is now on core-functionnality, not UI / UX. Yet, a lovely font was
  chosen to bring peace to the user's eyes.

This experiment is mostly an excuse to play with the somewhat-new [`brr-lwd`
library](https://ocaml.org/p/brr-lwd/latest) which provides reactive web
programming (❤️).

This project features bindings to
[IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) that
will be released independently at some point.
