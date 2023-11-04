opam exec -- dune build @default --watch --terminal-persistence=preserve &
DUNE_PID=$!
yarn run dev &
SERVER_PID=$!

stop() {
 echo '\n' Killing servers;
 kill ${SERVER_PID}
 kill ${DUNE_PID}
 exit
}
trap stop SIGINT

wait ${SERVER_PID}
kill ${DUNE_PID}
