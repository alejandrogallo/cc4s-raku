install:
	zef install .

force-install:
	zef install --force-install .

test:
	for i in t/*; do raku $$i; done
