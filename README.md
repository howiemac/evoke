# evoke 5

This is the python 3 version of EVOKE, currently in _active_ development.

EVOKE is a simple and powerful python web framework with pythonic "evo" templating.

The evoke module allows you to create twisted evoke apps (server applications) which:

- use mysql for data storage, and present the data to you as python objects
- produce HTML output via evoke's own "evo" templating
- use twisted webserver (optionally proxied via apache) to serve the data


## requirements

- python3 (tested on 3.6.2)
- linux (should work on BSD and MacOS also - but no yet tested)
- mysql

## installation

    pip3 install python

## usage

Evoke is a longstanding and stable system, that has been in use for commercial
mission-critical systems since its inception in 2001.

However, python packaging and automated install are a recent work in progress.

For now: manually configure evoke and create your evoke app(s):

- pip will have installed the "evoke" module at eg: `usr/lib/python3.6/site-packages` (or `usr/local/lib..` etc.)

  - create an `evoke/config_site.py` file, similar to `evoke/config_site.example`, but with your mysql connect parameters

  - create an app with a name of your choice (say `yourapp`) using the `evoke/create_app` script:

    ./create_app yourapp

  - create a `yourapp/config_site.py` file, similar to `yourapp/config_site.example`:
    - specify the `port` if you don't wnat the default of `8080`
    - specify the `domains` if you don't want default of `127.0.0.1`

- pip will have installed the `site` folder at eg: `usr/site` (or `usr/local/site` )

  - create a subfolder there named `yourapp` (or whatever app name you are using)

  - symlink `yourapp/site` to that subfolder `site/yourapp`


- start the app:

    cd yourapp
    ./start

- stop the app

    yourapp/stop

- restart the app:

    cd yourapp
    ./restart

When you first start an app, the mysql database for that app will be created.

The app will be visible at the domain and port specified, eg (using the defaults):

    http://127.0.0.1:8080/


contents
==

application (app) generation 
--
- app: prototype application
- create_app: script to create an app

library routines
--
- lib: library routines, including data types  
- data: database interface
- render: .evo html templating
- serve: application server

evoke classes for use in apps
--
- security classes
  - User: 
  - Reset:
  - Permit:
  - Session:
- foundation classes
  - Page: page hierarchy, including image and file handling
  - Var.py: system variables

application support
--
- evo: default system-wide templates
- site: flat file resources common to all apps 

system configuration
--
 - config_base.py: defaults
 - config_site.py: overrides for this server
 - config_multi.py: multi-app server config (see below)

multi-app server (optional, as apps may be run individually)
--
 - devstart: development start script (runs in foreground) 
 - start: production start script (runs in background)
 - stop: production stop script
 - multiserve.py: twisted application - called by the above scripts
