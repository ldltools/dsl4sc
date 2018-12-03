To run test cases in this directory, you need a small utility program called [shelltest](https://github.com/simonmichael/shelltestrunner),
which can be installed
by running `apt-get install shelltestrunner` (on Debian/Ubuntu).

Once installed, you can run test cases defined in \*.conf files as follows.

```
$ shelltest echo.conf  
$ shelltest ping\_pong.conf  
$ ...
```

or more simply:

```
$ make test
```
