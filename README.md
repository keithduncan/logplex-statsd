# logplex-statsd

Report Heroku platform errors to a statsd collector for aggregation and storage.

## Summary

Heroku injects platform errors into your application’s Logplex stream. You can view
the last 24hrs worth of these errors in the application’s dashboard but can’t build
on top of them.

If you want to graph these error trends over a longer time scale, apply functions to them
or configure nagios alerts based on their occurrence, you need to dump them into a database.

logplex-statsd is a Heroku deployable application that can be configured as a log drain
destination for your other Heroku applications. It will parse these errors from the logs
and egress them over UDP into persistent storage like Graphite’s [carbon](https://github.com/graphite-project/)
for analysis.

## Components

logplex-statsd builds on two primary components:

- [keithduncan/logplex-parse](https://github.com/keithduncan/logplex-parse)
- [keithduncan/statsd-client](https://github.com/keithduncan/statsd-client)

These are tied together using [Scotty](http://github.com/scotty-web/scotty) to receive, authenticate
and respond to incoming logs from Logplex.

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

Before you can deploy to Heroku you need to familiarise yourself with the
expected configuration variables.

### Metrics Cluster Configuration

You first need to configure where the UDP packets containing the statsd metrics
should be sent.

The `METRICS_CLUSTER` environment variable is a comma separated list of
collector URIs. It defaults to two localhost collectors which you will want to
replace with something useful.

logplex-statsd uses [keithduncan/statsd-client](https://github.com/keithduncan/statsd-client)
which supports consistently routing metrics between multiple collectors using a
modulus of a `CRC32(stat name)` and the number of collectors. By distributing
the metrics it helps prevents any single metrics collector from becoming
overloaded.

A cluster of one host is an acceptable configuration, all stats
will be sent to the cluster's single member. Measuring the load of your
collector cluster will indicate whether the collector cluster should be scaled
horizontally.

### Amazon AWS S3 Configuration

Deployment currently uses Heroku’s buildpack system however the app cannot be
built inside the quotas applied to the git push process. Instead deployment will
prepare a slug containing the buildpack source that can be used to build the
app dependencies and upload these to Amazon S3 for reuse.

A postdeploy script is used to download GHC and Cabal, update the Cabal package
list, create a Cabal sandbox and install the dependencies. These artefacts are
then uploaded to S3 for reuse on subsequent deploys.

This requires an AWS S3 bucket to be configured along with an AWS IAM user. The
IAM user's access should be tightly scoped using an IAM policy such as this with
the `bucket_name` replaced:

```
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "s3:DeleteObject",
                "s3:GetObject",
                "s3:GetObjectAcl",
                "s3:ListBucket",
                "s3:PutObject",
                "s3:PutObjectAcl"
            ],
            "Resource": [
                "arn:aws:s3:::bucket_name*"
            ]
        }
    ]
}
```

Heroku’s deployment process will prompt you to configure the required fields.

Once this postdeploy script has completed you will need to deploy the app again
using `git commit --amend --no-edit && git push --force heroku` to force a slug
containing the newly built environment and the app to be generated, phew.

This build process is less than ideal and I plan to replace it with Heroku’s
Docker based workflow.

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)

## Piping a Heroku application's logs to logplex-parse

Once you have logplex-statsd deployed you’ll want to configure your other apps
to send their logs to logplex-statsd.

1. Prepare a new set of per-app credentials
    - `password="$(ruby -rsecurerandom -e '$stdout.puts SecureRandom.hex(40)')"`
2. Set these in the environment of your logplex-statsd
    - `heroku config:set API_CREDENTIALS_MYAPP="logplex:$password" --app my-logplex-statsd`
3. Configure the drain for `MYAPP`, assuming your logplex-statsd is available over https
    - `heroku drain:add "https://logplex:${password}@$(heroku domains --app my-logplex-statsd --json | jq --raw-output .[0].hostname)/myapp/logs" --app my-app`
