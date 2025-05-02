Tests about the connect
  $ cat >example <<EOF
  > Hello World!
  > EOF
  $ bob send --quiet -r 10.0.0.1 --connect-delay 1s --connect-timeout 5ms example 2> /dev/null
  [1]
  $ bob recv --quiet -r 10.0.0.1 --connect-delay 1s --connect-timeout 5ms toto 2> /dev/null
  [1]
