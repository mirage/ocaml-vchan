#!/bin/sh -ex

mirage configure -f config_server.ml -t xen --no-opam
mirage build -f config_server.ml
mirage configure -f config_client.ml -t xen --no-opam
mirage build -f config_client.ml
