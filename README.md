# PDB CLI

A native PuppetDB command line client. This project intends to be a
quick way to interact with PuppetDB via the command line. Things like
executing a query, viewing results, importing/exporting etc.

# Status

This is pretty early yet, more of a hack day project and a proof of
concept. Import/export works (only on PuppetDB's current master
branch). Nodes querying works and can use mutually authenticated SSL
to connect to PuppetDB. SSL needs to be added to import/export (and
factored into it's own file). Nodes was where I spiked out the
http-client library's ability to handle mutual auth. I still have some
functionality to fill in for the nodes functionality to be done (CSV
support, loading a query from a file, PQL support).

# Building

From the root of the pdb-cli directory, type

```
chicken-scheme install
```

This will download all of the supporting eggs the PDB cli needs (see
below for mutual authentication hacks if you want SSL).

From there you can use the interpreter

```
csi -s export-script --help
```

To interpret export.scm, or you can compile and run it:

```
csc export-script.scm -o export && ./export --help
```

To build a standalone copy of pdb-cli run the following:

```
$ chicken-install nodes-script
...
$ chicken-install -init nodes-script
...
$ chicken-install -deploy -prefix nodes-script
```

From there you can cd into the nodes-script directory and invoke nodes-script.

# Config

The nodes endpoint wants a ~/.pdbrc file that contains an alias for a
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


# SSL Hacks

I needed to make changes to two libraries to support mutual
authentication that are still waiting to be merged
upstream. Specifically there are two diffs in this repo
intarweb-changes.diff and http-client-changes.diff that need to be
patched before mutual authentication will work. You can use

```
chicken-install -r intarweb http
```

To download the source of the two libraries, then apply the patch to
each and run chicken-install in each of the directories. This will
install the patched version locally.