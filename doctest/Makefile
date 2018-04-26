all:
	true

clean:
	rm -rf doc

doc: clean
	haddock -h Hoge.hs -o doc/

test:
	doctest Hoge.hs
