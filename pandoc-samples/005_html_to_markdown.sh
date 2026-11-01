#!/bin/bash

# HTML to Markdown, writes to stdout, manual p4

pandoc -f html -t markdown hello.html
