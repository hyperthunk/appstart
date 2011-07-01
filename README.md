# AppStart

This small library aims to help with OTP application startup and shutdown.

## Overview

Appstart aims to provide the following (hopefully useful) features.

- Simple multi-app startup in a development (or non-OTP release environment)
- Additional bootstrapping and configuration options for OTP applications
- Co-ordinated concurrent (and distributed) application/cluster startup

### Development (and non-standard release) Tools

The basic premise is similar to the OTP concept of included\_applications (see the [docs](http://www.erlang.org/doc/design_principles/included_applications.html)).
What differs about appstart, is that it provides a means to let your application
depend on other primary applications.

With included\_applications, OTP provides a degree of support for
application\_process synchronisation during startup, via the *start_phase*
application callback mechanism. Your chosen included\_applications however, are
not started by OTP, but rather they're meant to be initialised by the top level
supervisor of your *primary application*, meaning that their processes actually
become part of your application, hence the term *included*.

With appstart, you have the ability to start other applications (on which your
app will depend) as primary applications in their own right, with their own start
types and configuration data. Naturally this starting of dependant application (in
proper order) is best handled by bundling applications into releases, so the 
*startup* feature is really intended for development purposes.

#### API

Start up your application and all its dependencies:

```erlang
appstart:start(myapp).
```

Specify a startup type:

```erlang
appstart:start(myapp, permanent).
```

Start up you application's dependencies without starting your app:

```erlang
appstart:start_deps(myapp).
```

### Bootstrapping and Configuration Tools

Appstart implements a couple of simple features at this stage. The first of these
is the `appstarter` module, which provides a generic, .app configuration based 
replacement for custom application callback modules. Intended for only the simple
cases (at the moment), you can configure the module to deal with no-brainer cases
such as starting your application's top level supervisor and so on.

Configure appstart to act as your `<name>_app` module for simple cases:

```erlang
{application,
 example,
 [{description,"Example using appstart to load my app"},
  {vsn,"0.0.1"},
  {mod,{appstarter,[]}},
  {modules,[example_sup, example_shutdown_handler, example_srv]},
  {registered,[]},
  {applications,[kernel,stdlib]},
  {env,[
    {appstart, [
        {start, [example_sup, start_link, [{custom, "args"}]]},
        {stop,  [example_shutdown_handler, shutdown, []]}
    ]}
  ]}]}.
```

In future releases we hope to extend the `appstarter` module to provide "out of 
the box" support for declarative concurrent/distributed startup management,
application and process synchronisation and other co-ordination measures and so 
on.

The `appstarter` module also provides support for automatically configuring the
[fastlog](https://github.com/hyperthunk/fastlog) for your application, which can
be done with the following config:

```erlang
{mod,{appstarter,[{fastlog, configure}]}}
```

## Status

This project is in early alpha at the moment.

## License

This project is distributed under a BSD-style license (please see the accompanying
LICENSE document for details).

## Versioning

This project uses [Semantic Versioning](http://semver.org). All major and minor
versions will be tagged for release. Release candidates (i.e., revision builds)
might be tagged. Binary releases will be provided once we get to version 1.0.0 and
onwards.

## Installation

See the accompanying INSTALL file.

## Issue Tracking

Please register issues against the [Repository Issue Tracker](https://github.com/hyperthunk/appstart/issues).
