Notes about a possible server side database:

- Syncing a database with a filesystem hierarchy is a common problem.
- Possible approach: decouple the fs-syncing information in itself and the
  actual database objects that are application-specific.
- Make a lib that take care of the syncing and the watching:
- Use Irmin_watch for watching ?
- Use SQB DB CTE for recursive-hierarchical queries ?
