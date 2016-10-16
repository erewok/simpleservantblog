../assets/index.html: Api.elm Main.elm
	elm make Main.elm --output elm.js
	mv elm.js ../assets/js
	gulp

Api.elm: GenerateElm.hs ../src/Api.hs
	(cd ../ ; stack exec -- runhaskell -isrc client/GenerateElm.hs)

Admin.elm: Admin.elm admin-elm.js
	elm make Admin.elm --output admin-elm.js
	mv admin-elm.js ../assets/js

run-tests: tests.js
	node tests.js

tests.js: Main.elm Api.elm Tests.elm
	elm make Tests.elm --output tests.js
