Tests about the connect
  $ cat >example <<EOF
  > Hello World!
  > EOF
  $ bob send --quiet -r 10.0.0.1 example 2> /dev/null
  [125]
  $ bob recv --quiet -r 10.0.0.1 toto 2> /dev/null
  [125]
