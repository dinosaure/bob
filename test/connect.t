Tests about the connect
  $ cat >example <<EOF
  > Hello World!
  > EOF
  $ bob send --quiet -r 127.0.0.1 example
  bob: connect(): Connection refused.
  [1]
  $ bob recv --quiet -r 127.0.0.1 toto
  bob: connect(): Connection refused.
  [1]
