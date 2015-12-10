# logplex-statsd

Report Heroku platform errors to a statsd collector for aggregation and storage.

## Summary

Heroku injects platform errors into your application’s Logplex stream. You can view
the last 24hrs worth of these errors in the application’s dashboard but can’t build
on top of them.

If you want to graph these error trends over a longer time scale, apply functions to them
or configure nagios alerts based on their occurrence, you need to dump them into a database.

logplex-statsd allows you to parse and egress these errors into persistent storage like
Graphite’s [carbon](https://github.com/graphite-project/).

## Components

logplex-statsd builds on two primary components:

- [keithduncan/logplex-parse](https://github.com/keithduncan/logplex-parse)
- [keithduncan/statsd-client](https://github.com/keithduncan/statsd-client)

These are tied together using [Scotty](http://github.com/scotty-web/scotty) to receive, handle
and respond to incoming requests from Logplex.

## Development

`script/development-bootstrap` checks your machine for the required components,
creates a cabal sandbox and installs the dependencies.

logplex-statsd uses `foreman` in development to run the processes for testing.
Run `script/debug-server` to start the web server and two statsd listener
processes.

Running `script/debug-server` you should see output like this when sending
logplex documents to localhost:

```
Keiths-MacBook-Pro:logplex-statsd keith$ script/debug-server
20:18:32 web.1           | started with pid 19983
20:18:32 listener-8126.1 | started with pid 19984
20:18:32 listener-8127.1 | started with pid 19985
20:18:32 listener-8126.1 | Listening on port 8126...
20:18:32 listener-8127.1 | Listening on port 8127...
20:18:32 web.1           | Preprocessing executable 'logplex-statsd' for logplex-statsd-0.1.0.0...
20:18:33 web.1           | Running logplex-statsd...
20:18:34 web.1           | Setting phasers to stun... (port 3000) (ctrl-c to quit)
20:18:34 web.1           | POST /heaven/logs
20:18:34 web.1           |   Request Body: 106 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - [foo bar="baz"] error=1234597 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - - Error R99 heroku97 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - - Error R89 heroku97 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - - Error L99 heroku122 <10>123 2015-12-03T23:12:17+11:00 keiths-macbook-pro.local logplex-parse 420 - - at=error code=H12 desc="a thing happened"
20:18:34 web.1           |   Accept:
20:18:34 web.1           |   Status: 201 Created 0.026851s
20:18:34 listener-8126.1 | ["heaven.heroku.errors.R89:1|c", #<Addrinfo: 127.0.0.1:52122 UDP>, 0]
20:18:34 listener-8127.1 | ["heaven.heroku.errors.R99:1|c", #<Addrinfo: 127.0.0.1:53383 UDP>, 0]
20:18:34 listener-8127.1 | ["heaven.heroku.errors.L99:1|c", #<Addrinfo: 127.0.0.1:53383 UDP>, 0]
20:18:34 listener-8127.1 | ["heaven.heroku.errors.H12:1|c", #<Addrinfo: 127.0.0.1:53383 UDP>, 0]
```

## Deploy to Heroku

TODO Heroku Button

## Piping a Heroku application's logs to logplex-parse

TODO logplex HTTPS drain instructions, add per-app authentication credentials
too
