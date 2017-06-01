ParTAGe4XMG
===========

**ParTAGe4XMG** is a command-line tool which allows to use [ParTAGe][partage], a
Haskell library dedicated to parsing *tree adjoining grammars* (TAGs), with
[XMG][xmg]-generated TAG grammars.


Installation
------------

It is recommanded to install *ParTAGe4XMG* using the
[Haskell Tool Stack][stack], which you will need to download and install on your
machine beforehand.
Then:
* Create an empty directory, which will be dedicated for the ParTAGe source code,
* Clone the `simple` branch of the [ParTAGe][partage] repository into this empty directory,
* Clone the `simple` branch of [this][this] repository into the same directory,
* Run `stack install` in the local copy of the `partage4xmg` repository.

Under linux, you can use the following sequence of commands to set up ParTAGe4XMG:

    mkdir partage-src
    cd partage-src
    git clone -b simple https://github.com/kawu/partage.git
    git clone https://github.com/kawu/partage4xmg.git
    cd partage4xmg
    stack install

The final command will by default install the `partage4xmg` command-line tool in
the `~/.local/bin` directory.  You can either add this directory to your `$PATH`,
or use the full path to run `partage4xmg`:

    $ ~/.local/bin/partage4xmg --help
    

### Update

To update the *ParTAGe4XMG* tool, use `git pull` in both repositories downloaded
during installation and run `stack install --force-dirty` in the local copy of
`partage4xmg`.
Under linux, assuming that you are in the `partage-src` directory, you can use
the following sequence of commands to perform the update:

    cd partage
    git pull
    cd ../partage4xmg
    git pull
    stack install --force-dirty

The usage of `--force-dirty` ensures that `stack` does not overlook any of the
modifications pulled from the upstream repositories.


Examples of usage
-----------------

Provided that you have a grammar `grammar.xml` file, a lexicon `lemma.xml` file
and a morphology `morph.xml` file, you can retrieve all the derived trees for a
given sentence using the following command:

    echo "a sentence to parse" | partage4xmg parse -g grammar.xml -l lemma.xml -m morph.xml -s S
    
where the argument of the `-s` option specifies the axiom symbol.
Note that in this mode the parser can take some time to read the grammar,
especially if the input `.xml` files are big.
If you have several sentences to parse, you can write them in a single file and
provide it as input for the parser, which will then read and process them one by
one.


### Derivations

If you want the parser to pretty print derivations rather than derived trees,
use the `-d` (`--derivations`) option, as in:

    echo "a sentence to parse" | partage4xmg parse -g grammar.xml -l lemma.xml -m morph.xml -s S -d


### Feature structures

To enable support for feature structures (FSs), use the `-u` (`--use-features`)
option. Note that at the moment the parser reports FSs only for derivations and
not for derived trees.

Note that currently the parser provides support for *flat* FSs only (with the
*top*/*bottom* distinction).


### Tokenization

*At the moment the command-line tool does not implement any smart tokenization
strategies and it assumes that you supply the input sentences with words
separated by spaces.*




[this]: https://github.com/kawu/partage4xmg
[partage]: https://github.com/kawu/partage#partage
[xmg]: http://dokufarm.phil.hhu.de/xmg/
[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
