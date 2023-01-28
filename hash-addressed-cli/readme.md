The `hash-addressed` executable is a command-line interface for maintaining a
directory wherein each file's name is a hash of its content.


Initialization
-------------------------------------------------------------------------

In this demonstration, we initialize a SHA-256 hash-addressed directory at
`/tmp/demo`.

```
$ hash-addressed initialize --directory /tmp/demo --hash-function sha256
```

This created a configuration file:

```
$ cat /tmp/demo/.hash-addressed/config
version: 1
hash function: sha256
```


Writing
-------------------------------------------------------------------------

We can now use the `hash-addressed write` command to write files into this
directory. In the output we see the path to which the content was written.

```
$ echo "Test file 1" | hash-addressed write --target-directory /tmp/demo
/tmp/demo/ad7a409054a68314812ba3ad0e523a66593ab3404c81700d6f5c3601f0da830e
```

```
$ echo "Test file 2" | hash-addressed write --target-directory /tmp/demo
/tmp/demo/20b39ca6ca85b53be73920532fd6f9cc164317646995839e2e54a6871dc13bf7
```

We can verify that the file was written and that indeed its name matches its
SHA-256 checksum:

```
$ cat /tmp/demo/20b39ca6ca85b53be73920532fd6f9cc164317646995839e2e54a6871dc13bf7
Test file 2
```

```
$ sha256sum /tmp/demo/20b39ca6ca85b53be73920532fd6f9cc164317646995839e2e54a6871dc13bf7
20b39ca6ca85b53be73920532fd6f9cc164317646995839e2e54a6871dc13bf7
```


Symbolic links
-------------------------------------------------------------------------

A place to put links for demonstration:

```
$ mkdir --parents /tmp/demo-links
```

You can use the `--link` option with the `write` command to create symbolic
links to the hash-addressed content.

```
$ echo "whatever" | hash-addressed write --target-directory /tmp/demo --link /tmp/demo-links/link-1
/tmp/demo/cd293be6cea034bd45a0352775a219ef5dc7825ce55d1f7dae9762d80ce64411
```

Observing that the link was written:

```
$ readlink /tmp/demo-links/link-1
/tmp/demo/cd293be6cea034bd45a0352775a219ef5dc7825ce55d1f7dae9762d80ce64411
```

```
$ cat /tmp/demo-links/link-1
whatever
```


Verbosity
-------------------------------------------------------------------------

Use the `--verbose` flag to get some additional information, printed to the
standard error stream.

```
$ echo "Test file 3" | hash-addressed write --target-directory /tmp/demo --verbose
The hash function is sha256
/tmp/demo/efdbe264574c7440b80a2c4aaf15c18787a125b6223d05300841f32f46361e7f
One new file was added to the store.
```

```
$ echo "Test file 3" | hash-addressed write --target-directory /tmp/demo --verbose
The hash function is sha256
/tmp/demo/efdbe264574c7440b80a2c4aaf15c18787a125b6223d05300841f32f46361e7f
The file was already present in the store; no change was made.
```

Use the `--quiet` flag to suppress what is normally printed to the standard
output stream.

```
$ echo "Test file 4" | hash-addressed write --target-directory /tmp/demo --quiet
```

Fatal errors resulting in non-zero status code are always printed to the
standard error stream regardless of what command-line options are given. In the
following demonstration, the `--link` instruction succeeds the first time
(printing nothing because we use `--quiet`), but we see an error message the
second time because the link already exists.

```
$ echo "Test file 4" | hash-addressed write --target-directory /tmp/demo --quiet --link /tmp/demo-links/link-2
$ echo "Test file 4" | hash-addressed write --target-directory /tmp/demo --quiet --link /tmp/demo-links/link-2
Failed to create link /tmp/demo-links/link-2
```


File copying
-------------------------------------------------------------------------

Instead of reading from the standard input stream, `hash-addressed` can also
copy a file into the store using the `--source-file` option.

```
$ echo "file content" > /tmp/demo-file
```

```
$ hash-addressed write --source-file /tmp/demo-file --target-directory /tmp/demo
/tmp/demo/694b27f021c4861b3373cd5ddbc42695c056d0a4297d2d85e2dae040a84e61df
```

```
$ cat /tmp/demo/694b27f021c4861b3373cd5ddbc42695c056d0a4297d2d85e2dae040a84e61df
file content
```
