# reactive-psc

React.js View >>= RX Binding >>= Purescript Controller.

There is a hosted version at [OpenSpace](http://178.62.90.204/build/). Since this version is updated manually it might not always be up-to-date.

## Required Tooling:
 - [psc/psc-make](http://www.purescript.org/)
 - [npm](http://nodejs.org/)
 - [bower](http://bower.io/)
 - [grunt](http://gruntjs.com/)

## Building

```
npm install
bower update
grunt
grunt browser
```

Point your browser at **localhost:9001/static**

## Websocket support

In order to use the websocket features you first need to start https://github.com/kRITZCREEK/open-space-socket.

Once it's up and running your actions get shared across multiple sessions. Open up two Browsertabs and see them synchronize.

**This is still in the experimental Phase**
