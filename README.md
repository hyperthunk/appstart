# AppStart

This small library aims to help with OTP application startup and shutdown.

## Usage

Start up your application and all its dependencies:

	appstart:start(myapp).

Specify a startup type:

	appstart:start(myapp, permanent).
	
Start up you application's dependencies without starting your app:

	appstart:start_deps(myapp).

Configure appstart to act as your `<name>_app` module for simple cases:
	
	{application,
	 example,
	 [{description,"Example using appstart to load my app"},
	  {vsn,"0.0.1"},
	  {mod,{appstarter,[]}},
	  {modules,[example_sup, example_srv]},
	  {registered,[]},
	  {applications,[kernel,stdlib]},
	  {env,[
	    {appstart, [
	        {start, [example_sup, start_link, [{custom, "args"}]]}
	    ]}
	  ]}]}.

## Status

This project is in alpha at the moment.

## License

This project is distributed under a BSD-style license (please see the accompanying LICENSE document for details).

## Versioning

This project uses [Semantic Versioning](http://semver.org). All major and minor versions will be tagged for release. Release candidates (i.e., revision builds) might be tagged.

## Installation

See the accompanying INSTALL file.

## Issue Tracking

Please register issues against the [Repository Issue Tracker](https://github.com/hyperthunk/appstart/issues).
