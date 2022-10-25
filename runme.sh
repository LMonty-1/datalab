#!bin/bash
if ./dlc bits.c; then
  echo "dlc tests pass!"
  echo "making..."
  make clean
  make
  make btest
  echo ""
  ./btest
else
  echo "Failed dlc test:"
  ./dlc -e bits.c
  exit 0
fi
exit 0