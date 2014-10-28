REMARK
======
  - dd if=/dev/urandom of=random.bin bs=1MB count=1
  - openssl sha1 ./random.bin
