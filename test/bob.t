Tests about the bob binary
  $ ./sched.exe <<EOF
  > send --seed 8H93erb5Kvo= not-found 2>stderr 1>stdout
  > EOF
  bob send --seed 8H93erb5Kvo= not-found 1>stdout 2>stderr -> EXITED(124)
  $ cat stdout
  Password: soarfly-bril
  $ cat stderr
  send: <path> argument: not-found does not exist
  Usage: send [OPTION]… <path>
  Try 'send --help' for more information.
  $ cat >example <<EOF
  > Hello World!
  > EOF
  $ ./free.exe closed 127.0.0.1 9000
  $ bob relay -p pid &
  $ ./free.exe opened 127.0.0.1 9000
  $ ./sched.exe <<EOF
  > send -r 127.0.0.1 --password toto example
  > recv -r 127.0.0.1 -y toto -o out
  > EOF
  bob send -r 127.0.0.1 --password toto example -> EXITED(0)
  bob recv -r 127.0.0.1 -y toto -o out -> EXITED(0)
  $ diff example out
  $ kill -INT $(cat pid)
