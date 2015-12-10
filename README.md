# logplex-statsd

Report Heroku platform errors to a statsd collector for aggregation and storage.

## Summary

Heroku injects platform errors into your application’s Logplex stream. You can view
the last 24hrs worth of these errors in the application’s dashboard.

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

## Deploy to Heroku

TODO Heroku Button
