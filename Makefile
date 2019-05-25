all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean --allow-different-user 
	@stack build --allow-different-user 
	@cp `stack --allow-different-user build` build/
	@cd build && zip function.zip haskell_lambda && rm haskell_lambda && cd ..