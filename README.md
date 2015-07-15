## Voice API

[![Circle CI](https://circleci.com/gh/aria42/voice-api/tree/master.svg?style=svg&circle-token=88cec8dcf43023fa6d6f420dcac4eba16cc6b1d1)](https://circleci.com/gh/aria42/voice-api/tree/master)

## Getting Started

Required [homebrew](http://brew.sh) packages:

```bash
brew install scala
brew install sbt
```

## How to run unit-tests

```bash
sbt clean test
```

## How to compute testing coverage

```bash
sbt clean coverage test
sbt coverageReport
open target/scala-2.11/scoverage-report/index.html
```
## How to start server

```bash
sbt
// This starts the server
> container:start
// Open browser to localhost and port
> browse
// After you make a change to code, you can 
> container:restart
// Then you can reload the webpage
```

## Current Servlets

There are two servlets:

* `/app/`: The webapp
* `/api/0.1/`: The JSON api route

## How to use an IDE

It's vastly easier to code Scala in an IDE; IntelliJ is generally considered the strongest IDE (not just for Java, Scala, 
but also for Python, TypeScript, etc.). 

In IntelliJ, import the `build.sbt` project and the rest should just work. 