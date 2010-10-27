#!/usr/bin/env bash
erl -sname ekm -pa ebin/ -eval 'application:start(ekm)' -dims 10 10
