Partage4xmg
===========

**Partage4xmg** is a tool which allows to use the [partage][partage] library
with TAG grammars produced by the [XMG][xmg-ng] meta-grammar compiler.

It doesn't provide any fancy functionality for the moment. Nonetheless, it can
be used to test the library on real-size grammars and to obtain statistics
measuring the efficiency of the parser.


Installation
------------

To install *partage4xmg* you will need to download and install the
[Haskell Tool Stack][stack] first.

The package relies on a specific version of the [partage][partage] library, thus
you will need to `git clone` both the [partage][partage] repository and the
current repository to your local directories and then run `stack install` in the
local version of the current repository. The entire process can look like this:

    git clone -b 0.1 https://github.com/kawu/partage.git
    git clone -b 0.1 https://github.com/kawu/partage4xmg.git
    cd partage4xmg
    stack install

Note that the path to the local version of the *partage* library is specified in
the `stack.yaml` file of the current repository. You will have to modify it
before running `stack install` if you clone the *partage* library into a
different directory.


Usage
-----

The *partage4xmg* package provides a command-line tool which can be used to read
TAG grammars stored in the format compatible with [XMG's][xmg-ng] output and to
parse input sentences with such grammars.

### Example

In the example below we assume an existing `grammar.xml` file.
In order to follow this example yourself, you can download the
[TAG grammar][french-tag-valuation] compiled from the [FrenchTAG][french-tag]
meta-grammar.

First, to see all the available options of the command-line tool, run:

    partage4xmg --help

#### Build automaton
    
To see the automaton representation of the grammar, run:

    partage4xmg -i grammar.xml build
    
The tool will output the list of transitions of the automaton, together with the
corresponding labels. By default, the minimal finite-state automaton will be
built, but you can also choose other grammar representations. For instance,
to built a trie representation of the input grammar, run:

    partage4xmg -i grammar.xml build -c trie

#### Generate sample input

To generate all trees (of which the maximum size can be specified by using the
`-m` option) derivable from the input grammar, run:

    partage4xmg -i grammar.xml gen -m 10 > output.txt

The output will consist of the projections of the individual trees to sequences
of terminal leaves. The purpose of the `gen` command is to be able to quickly
generate some sample sentences consistent with the grammar on which the
efficiency of the parser can be later tested.

Note that the result may contain sequences of terminals interleaved with the
anchoring symbols. This happens if the input grammar is *not* fully lexicalized,
i.e. contains some tree templates. Such an output still can be used to test the
parsing efficiency, by treating anchors as special types of terminal symbols.

The tool provides also another generation mode, which can be invoked as follows:

    partage4xmg -i grammar.xml gen-rand -m 10 -a 0.1 -n 10 > output.txt
    
The purpose of this mode is to randomly generate, on the basis of the output of
the simple generation mode described above, larger derived trees, by applying the
substitution operation where needed and the adjoining operations where possible,
with adjunction probability specified by the `-a` parameter. The value of the
`-n` parameter determines the number of trees the tool will try to generate.

#### Parse sentences

To parse samples sentences from the `input.txt` file, run:

    partage4xmg -i grammar.xml stats < input.txt
 
For each input sentence, the output will contain two numbers -- the number of
nodes and the number of edges in the resulting hypergraph. At the end, aggregate
statistics will be shown for different sentence lengths.

Note that parsing may take some time even for small input files. This is due
to the fact that the tool has to create the automaton-based representation of
the grammar first.

A test set consisting of ~13000 sentences randomly generated from
[FrenchTAG][french-tag] is available [here][french-tag-testset] for testing
purposes.


[partage]: https://github.com/kawu/partage/tree/0.1 "partage library"
[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[xmg-ng]: http://dokufarm.phil.hhu.de/xmg "eXtensible MetaGrammar NG (XMG-NG)"
[french-tag]: https://sourcesup.renater.fr/scm/viewvc.php/trunk/METAGRAMMARS/FrenchTAG/?root=xmg "FrenchTAG meta-grammar"
[french-tag-valuation]: http://www.info.univ-tours.fr/~waszczuk/.share_9715348/french-tag.zip "FrenchTAG compiled to TAG"
[french-tag-testset]: http://www.info.univ-tours.fr/~waszczuk/.share_9715348/french-tag-testset.zip "Sample sentences generated from FrenchTAG"
