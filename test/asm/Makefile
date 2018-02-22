all:
	clang -target arm-linux-gnueabihf -S -c tmp.c -o arm.s
	clang -target aarch64-linux-gnueabihf -S -c tmp.c -o aarch64.s
	clang -target arm64-apple-ios -S -c tmp.c -o aarch64-ios.s
	clang -target armv7-apple-ios -S -c tmp.c -o arm-ios.s 
	x86_64-w64-mingw32-gcc -S -c tmp.c -o x86_64-mingw32.s
	clang -target i386-unknown-linux -S -c tmp.c -o x86-linux.s
	clang -target x86_64-apple-macos -S -c tmp.c -o x86_64-mac.s
	clang -target i386-unknown-linux -S -c tmp.c -o x86-linux.s
	clang -target x86_64-unknown-linux -S -c tmp.c -o x86_64-linux.s
