#!/bin/sh -ex

mirage configure -f config_server.ml --xen --no-opam
mirage build -f config_server.ml
mirage configure -f config_client.ml --xen --no-opam
mirage build -f config_client.ml
