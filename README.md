# evoke

EVOKE is a simple and powerful python web framework with pythonic "evo" templating.

The evoke module allows you to create evoke apps, which are twisted web-server-applications which:

- use twisted webserver (optionally proxied via apache) to serve the data
- use mysql for data storage, and present the data to you as python objects
- produce HTML output via evoke's own "evo" templating, by default using bootstrap 4 for CSS 

## versions

This is version 7 of EVOKE: the experimental development version (for python 3).

For the current stable python 3 version, see [evoke 6](https://github.com/howiemac/evoke6).

For the historic stable python 2 version see [evoke 4](https://github.com/howiemac/evoke4).

## requirements

- python3 (tested on 3.6.2)
- linux (should work on BSD and MacOS also - but not yet tested)
- mysql

## installation (version 6)

    pip3 install evoke

## usage

Evoke is a longstanding and stable system, which has been in use for commercial
mission-critical systems since its inception in 2001.

However, python packaging and automated install are a recent work in progress.

For now: manually configure evoke and create your evoke app(s):

## manual install

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


### start the app:

    cd yourapp
    ./start

### stop the app

    yourapp/stop

### restart the app:

    cd yourapp
    ./restart


When you first start an app, the mysql database for that app will be created.

The app will be visible at the domain and port specified, eg (using the defaults):

    http://127.0.0.1:8080/


