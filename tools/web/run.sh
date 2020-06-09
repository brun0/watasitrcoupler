#!/bin/bash

ctrlc() {
    echo "SIGINT caught"
    kill -9 $PID
    echo "$PID killed"
}

trap 'ctrlc' SIGINT

python3 -m http.server &
PID=$!
firefox http://localhost:8000
wait $PID
