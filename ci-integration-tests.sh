#!/bin/sh

cd app
ws --spa index.html &
server_pid=$!
cd ..
cypress run
exit_code=$?
kill $server_pid
return $exit_code
