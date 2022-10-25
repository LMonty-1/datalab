#!bin/bash
git fetch --all
git pull origin
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
fi
exit 0