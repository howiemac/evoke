"""a setuptools based setup module.

 python3 setup.py bdist_wheel

the wheel will then be in the dist folder

Then, from a virtualenv:

 pip install dist/<wheelname>

"""

# Always prefer setuptools over distutils
from setuptools import setup, find_packages

from os import path, walk
from shutil import copyfile

# get the evoke_version from VERSION file
evoke_version=open("VERSION").read().split('=')[1].strip()
# and copy version file to evoke/
copyfile("VERSION","evoke/VERSION")

# get the long description from the README file
long_description ="""
The evoke module allows you to create evoke apps, which are twisted web-server-applications which:

- use twisted webserver (optionally proxied via apache) to serve the data
- use mysql for data storage, and present the data to you as python objects
- produce HTML output via evoke's own "evo" templating

requirements
------------

- python3 (tested on 3.6.2)
- linux (should work on BSD and MacOS also - but not yet tested)
- mysql

caution
-------

Evoke is a stable system, which has been in production use for commercial mission-critical systems since its inception in 2001.

However, python packaging and automated install are a recent (October 2017) work in progress, and some manual configuration is currently required. (see the file README.md )
"""

#here = path.abspath(path.dirname(__file__))
#with open(path.join(here, 'README.md')) as f:
#    long_description = f.read()

# find all of the site data files
def site_files():
  tree=walk('site')
  files=[]
  for (dirpath,dirnames,filenames) in tree:
    print(dirpath,filenames)
    if filenames:
      files.append((dirpath,[(dirpath+'/'+f) for f in filenames]))
  return files

#print (site_files())

# the main setup:
setup(
    name='evoke',

    # Versions should comply with PEP440
    version=evoke_version,
    description='a simple and powerful python web framework with pythonic "evo" templating',
    long_description=long_description,

    # The project's main homepage.
    url='https://github.com/howiemac/evoke',

    # Author details
    author='The Evoke Foundation',
    author_email='howiemac@gmail.com',

    # Choose your license
    license='modified BSD',

    # See https://pypi.python.org/pypi?%3Aaction=list_classifiers
    classifiers=[
        # How mature is this project? Common values are
        #   3 - Alpha
        #   4 - Beta
        #   5 - Production/Stable
        'Development Status :: 5 - Production/Stable',

        # Indicate who your project is intended for
        'Intended Audience :: Developers',
        'Topic :: Software Development',

        # Pick your license as you wish (should match "license" above)
        'License :: OSI Approved :: BSD License',

        'Programming Language :: Python :: 3.6',
    ],

    # What does your project relate to?
    keywords='development http html twisted mysql',

    # You can just specify the packages manually here if your project is
    # simple. Or you can use find_packages().
#    packages=find_packages(exclude=[]),
    packages=[
    'evoke',
    'evoke.app',
    'evoke.data',
    'evoke.lib',
    'evoke.render',
    'evoke.nevo',
    'evoke.serve',
    'evoke.Page',
    'evoke.Permit',
    'evoke.Reset',
    'evoke.Session',
    'evoke.User',
    ],
    #+find_packages(where='evoke',exclude=[]),

    # Alternatively, if you want to distribute just a my_module.py, uncomment
    # this:
    #   py_modules=["my_module"],

    # List run-time dependencies here.  These will be installed by pip when
    # your project is installed. For an analysis of "install_requires" vs pip's
    # requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    install_requires=['twisted','mysqlclient','pillow','markdown','munch'],

    # List additional groups of dependencies here (e.g. development
    # dependencies). You can install these using the following syntax
    # for example:
    # $ pip install -e .[dev,test]
    extras_require={},

    # If there are data files included in your packages that need to be
    # installed, specify them here.
    # package_data={
    #    'sample': ['package_data.dat'],
    #},
    package_data={
        '': ['evo/*.evo']+['devstart','README'],
        'evoke': [
          'CONTENTS.md',
          'VERSION',
          'create_app',
          'restart-multi',
          'start-multi',
          'stop-multi',
          'config_multi.py.example',
          'config_site.py.example',
          ],
        'evoke.app': ['config_site.py.example','restart','start','stop','CONTENTS'],
    },

#    package_data={
#        'evoke': ['evo/*.evo']+[
#          'create_app',
#          'devstart','restart','start','stop',
#          'config_multi.py.example','config_site.py.example',
#          'README.md','LICENSE'
#          ],
#        'evoke.app': ['README'],
#        'evoke.app.code': [
#          'README',
#          'config_site.py.example',
#          'devstart','restart','start','stop',
#          ],
#        'evoke.Page':['evo/*.evo'],
#        'evoke.User':['evo/*.evo'],
#    },


    # Although 'package_data' is the preferred approach, in some case you may
    # need to place data files outside of your packages. See:
    # http://docs.python.org/3.4/distutils/setupscript.html#installing-additional-files # noqa
    # In this case, 'data_file' will be installed into '<sys.prefix>/my_data'
    # data_files=[('my_data', ['data/data_file'])],
    data_files=site_files(),

    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # pip to create the appropriate form of executable for the target platform.
    entry_points={}
#        'console_scripts': [
#            'sample=sample:main',
#        ],
#    },

)

