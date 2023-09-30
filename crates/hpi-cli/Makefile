version := 0.1.0

target build: src/*rs Cargo.toml
	cross build \
		--target-dir=../target \
		--target=x86_64-unknown-linux-musl \
		--release
	cp ../target/x86_64-unknown-linux-musl/release/hpi-cli .
	docker build . \
		-t mikmuellerdev/hpi-cli:$(version) \
		-t mikmuellerdev/hpi-cli:latest
