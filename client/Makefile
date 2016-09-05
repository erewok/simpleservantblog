../assets/index.html: Api.elm Main.elm
	elm make Main.elm
	mv index.html ../assets/

Api.elm: GenerateElm.hs ../src/Api.hs
	(cd ../ ; stack exec -- runhaskell -isrc client/GenerateElm.hs)

run-tests: tests.js
	node tests.js

tests.js: Main.elm Api.elm Tests.elm
	elm make Tests.elm --output tests.js
