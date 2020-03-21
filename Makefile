install:
	zef install .

install-force:
	zef install --force-install .

test:
	for i in t/*; do raku $$i; done
