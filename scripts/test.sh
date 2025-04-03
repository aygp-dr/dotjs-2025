#!/bin/bash
echo "Test script working!"
echo "Current directory: $(pwd)"
echo "Directory contents:"
ls -la
echo "Looking for org files:"
find . -name "*.org" | head -n 5