#!bin/bash
if ./dlc bits.c; then
  echo "dlc tests pass!"
  echo "making btests..."
  make clean
  make btest
  echo ""
  ./btest

else
  echo "Failed dlc test:"
  ./dlc -e bits.c
  exit 1
fi
exit 0