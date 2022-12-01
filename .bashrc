source .env

function erl() {
    docker run --rm -it -v $(pwd):$(pwd) -w $(pwd) ${ERLANG_IMAGE} erl "$@"
}

function rebar3() {
    docker run --rm -it -v $(pwd):$(pwd) -w $(pwd) ${ERLANG_IMAGE} rebar3 "$@"
}
