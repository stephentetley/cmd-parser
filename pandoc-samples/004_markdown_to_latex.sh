#!/bin/bash

# Markdown to LaTeX, writes to stdout, manual p4

pandoc -f markdown -t latex hello.txt
