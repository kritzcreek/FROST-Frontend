# FROST - Frontend

React.js View >>= RX Binding >>= Purescript Controller.

There is a hosted timetable at [OpenSpace](http://frost.kritzcreek.me/instance/0).

## Building

A global `gulp` installation is assumed. (`npm i -g gulp`)

```
npm i && bower i
gulp
```

## Running

The Result of the build process is supposed to be run with the FROST-Backend. If your directory structure includes a FROST-Backend directory in the same directory as FROST-Frontend you can run `gulp deploy` to copy the build artifacts.
