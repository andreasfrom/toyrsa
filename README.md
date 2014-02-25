toyrsa
========

Simple demonstration of RSA and XOR-encryption with Web-GUI in Haskell.

** NOT SECURE IN ANY WAY :) **

# crypt.hs
The Web-GUI, uses [threepenny-gui](https://hackage.haskell.org/package/threepenny-gui) for a pure Haskell, HTML-interface.

Starts a webserver with:
    ./crypt

# rsacrypt.hs
Just the RSA-"implementation" for the command line.

Generate keys with `keys` and a bit-length.
    > ./rsacrypt keys 16
    Public:
    n: 56977
    e: 6757
    Private:
    d: 393

Encrypt something via stdin and `n` and `e` from above.
    > cat hopefully-not-very-secret.txt | ./rsacrypt encrypt 56977 6757 > encrypted.txt

Decryption works the same way, but with the private key `d`.
    > cat encrypted.txt | ./rsacrypt decrypt 56977 393

# xorcrypt.hs
Simple `xor`'ing against the input and a user-supplied key.
    > echo "hej" | ./xorcrypt encrypt "secret" | ./xorcrypt decrypt "secret"
    hej

# Copyright and license
Copyright (c) 2014 Andreas H. From

Licensed under the MIT License (see the file LICENSE)
