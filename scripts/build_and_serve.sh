# This is required to workaround a node issue
# see https://github.com/nodejs/node/issues/48444
#export UV_USE_IO_URING=0

opam exec -- dune build @ocaml-index @default --profile=dev --watch --terminal-persistence=preserve &
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
