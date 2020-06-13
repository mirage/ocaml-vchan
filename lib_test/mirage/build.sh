#!/bin/sh -ex

(cd server && mirage configure -t xen)
(cd server && make depends)
(cd server && mirage build)
(cd client && mirage configure -t xen)
(cd client && make depends)
(cd client && mirage build)
