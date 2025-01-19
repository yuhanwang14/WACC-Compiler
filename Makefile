# NOTE: PLEASE DON'T USE THIS MAKEFILE, IT IS FOR LABTS
# it is *much* more efficient to use `scala compile .` trust me, I'm watching you.
all:
# the --server=false flag helps improve performance on LabTS by avoiding
# downloading the build-server "bloop".
# the --jvm system flag helps improve performance on LabTS by preventing
# scala-cli from downloading a whole jdk distribution on the lab machine
# the --force flag ensures that any existing built compiler is overwritten
# the --power flag is needed as `package` is an experimental "power user" feature (NOTE: use this or --assembly if anything goes wrong)
#	scala --power package . --server=false --jvm system --force -o wacc-compiler
# you can use --assembly to make it built a self-contained jar,
#	scala --power package . --server=false --jvm system --assembly --force -o wacc-compiler
# you can use --native to make it build a native application (requiring Scala Native),
#	scala --power package . --server=false --jvm system --native --force -o wacc-compiler
# or you can use --graalvm-jvm-id graalvm-java21 --native-image to build it using graalvm
	scala --power package . --server=false --jvm system --graalvm-jvm-id graalvm-java21 --native-image --force -o wacc-compiler

clean:
	scala clean . && rm -f wacc-compiler

.PHONY: all clean
