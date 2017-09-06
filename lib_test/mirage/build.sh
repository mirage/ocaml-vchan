#!/bin/sh -ex

mirage configure -f config_server.ml -t xen
mirage build -f config_server.ml
mirage configure -f config_client.ml -t xen
mirage build -f config_client.ml
