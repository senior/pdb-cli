# PDB CLI

A native PuppetDB command line client. This project intends to be a
quick way to interact with PuppetDB via the command line. Things like
executing a query, viewing results and eventually import/export.

# Status

This is pretty early yet, more of a hack day project and a proof of
concept. Nodes querying works and can use mutually authenticated SSL
to connect to PuppetDB. Non-SSL connections work as
well. Import/Export worked on a previous prototype, still needs to be
added to the current one. Other endpoints also need to be
added. Likely nodes.rkt will switch to something more general and be a
git style subcommand, i.e. ~pdb nodes ...~.

# Config

The pdb-cli wants a ~/.pdbrc file that contains an alias for a
puppetdb instance along with certificate information. ~/.pdbrc looks
like:

```
{"default_host": "pdb-prod",
 "hosts": {"pdb-prod": {"root_url": "https://<puppetdb host name>:8081",
                        "ca": "/path/to/ca.pem",
                        "cert": "/path/to/<client-cert>.pem",
                        "key": "/path/to/<client-key>.pem"}}}
```

The easiest way to get those is to use puppet's cert generation
facility, copy the public/private/ca certs from puppet and add the
public cert to PuppetDB's certificate whitelist.

The tool also supports non-SSL communication:

```
{"default_host": "pdb-prod",
 "hosts": {"pdb-prod": {...}}
           "pdb-dev":  {"root_url": "http://localhost:8080"}}}
```

# Building/Running

To build this, you only need Racket installed. It will probably work
with earlier versions of Racket, but it was developed and tested with
6.2.1. The example below assume Racket installed and the Racket bin
directory is on your $PATH.

An easy way to run the tool from the command line during development
is to run the following (from this directory):

```
racket nodes.rkt -q "[\"~\",\"certname\",\".*\"]"
```

To build a standalone copy of pdb-cli run:

```
$ raco exe nodes.rkt
...

With this you can run the same code as the racket invocation above:

```
$ ./nodes -q "[\"~\",\"certname\",\".*\"]"
```

This creates a standalone binary, but to distribute it, you need the
supporting racket libraries to be bundled with it. This can be
created with the following command

```
$ raco distribute nodes-dist nodes
```

This will create a new ~nodes-dist~ directory and put the nodes binary
along with the needed supporting libs in that directory. That
directory can be zipped up and ran on another system without
installing Racket. This has been tested on OS X (Yosemite), Debian 7
and Windows 2012.