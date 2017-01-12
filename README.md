ParTAGe4XMG
===========

**ParTAGe4XMG** is a command-line tool which allows to use [ParTAGe][partage], a
Haskell library dedicated to parsing *tree adjoining grammars* (TAGs), with
[XMG][xmg]-generated TAG grammars.


Installation
------------

It is recommanded to install *partage* using the [Haskell Tool Stack][stack],
which you will need to download and install on your machine beforehand.
Then:
    * Create an empty directory, which will be dedicated for the ParTAGe toolset,
    * Clone the `simple` branch of the [ParTAGe][partage] repository into this empty directory,
    * Clone the `simple` branch of *this* repository into the same directory,
    * Run `stack install` in the local copy of the `partage4xmg` repository.

Under linux, you can use the following sequence of commands to set up ParTAGe4XMG:

    mkdir partage-src
    cd partage-src
    git clone -b simple https://github.com/kawu/partage.git
    git clone -b simple https://github.com/kawu/partage4xmg.git
    cd partage4xmg
    stack install

The final command will by default install the `partage4xmg` command-line tool in
the `~/.local/bin` directory.  You can either add this directory to your `$PATH`,
or use the full path to run `partage4xmg`:

    $ ~/.local/bin/partage4xmg --help


Examples of usage
-----------------
